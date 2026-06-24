/* rune_tls.c — a minimal HTTPS GET over BearSSL, for the native backends (C, LLVM,
 * Rust) which ship no TLS of their own. One entry point, rune_tls_get, so the
 * codegen marshals Bin<->C and never touches the TLS state machine. Links the
 * vendored libbearssl.a.
 *
 * Trust: a "no anchor" X.509 wrapper (adapted from BearSSL's tools/certs.c)
 * processes the certificate chain but accepts an untrusted self-signed peer
 * (BR_ERR_X509_NOT_TRUSTED -> 0). This is BearSSL's sanctioned skip-verify; it is
 * for the harness self-signed test server and matches the host backends' skip-
 * verify (Go InsecureSkipVerify, Python _create_unverified_context). */
#include <bearssl.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <errno.h>
#include <unistd.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>

typedef struct {
	const br_x509_class *vtable;
	const br_x509_class **inner;
} rune_noanchor_ctx;

static void na_start_chain(const br_x509_class **ctx, const char *sn) {
	rune_noanchor_ctx *x = (rune_noanchor_ctx *)ctx; (*x->inner)->start_chain(x->inner, sn);
}
static void na_start_cert(const br_x509_class **ctx, uint32_t len) {
	rune_noanchor_ctx *x = (rune_noanchor_ctx *)ctx; (*x->inner)->start_cert(x->inner, len);
}
static void na_append(const br_x509_class **ctx, const unsigned char *buf, size_t len) {
	rune_noanchor_ctx *x = (rune_noanchor_ctx *)ctx; (*x->inner)->append(x->inner, buf, len);
}
static void na_end_cert(const br_x509_class **ctx) {
	rune_noanchor_ctx *x = (rune_noanchor_ctx *)ctx; (*x->inner)->end_cert(x->inner);
}
static unsigned na_end_chain(const br_x509_class **ctx) {
	rune_noanchor_ctx *x = (rune_noanchor_ctx *)ctx;
	unsigned r = (*x->inner)->end_chain(x->inner);
	/* Skip-verify (test only, mirrors Go InsecureSkipVerify / Python's unverified
	 * context): accept an untrusted self-signed peer AND a server-name mismatch. */
	if (r == BR_ERR_X509_NOT_TRUSTED || r == BR_ERR_X509_BAD_SERVER_NAME) {
		r = 0;
	}
	return r;
}
static const br_x509_pkey *na_get_pkey(const br_x509_class *const *ctx, unsigned *u) {
	rune_noanchor_ctx *x = (rune_noanchor_ctx *)ctx; return (*x->inner)->get_pkey(x->inner, u);
}
static const br_x509_class rune_noanchor_vtable = {
	sizeof(rune_noanchor_ctx),
	na_start_chain, na_start_cert, na_append, na_end_cert, na_end_chain, na_get_pkey
};
static void rune_noanchor_init(rune_noanchor_ctx *x, const br_x509_class **inner) {
	x->vtable = &rune_noanchor_vtable; x->inner = inner;
}

static int sock_read(void *ctx, unsigned char *buf, size_t len) {
	int fd = *(int *)ctx;
	for (;;) { ssize_t r = read(fd, buf, len); if (r <= 0) { if (r < 0 && errno == EINTR) continue; return -1; } return (int)r; }
}
static int sock_write(void *ctx, const unsigned char *buf, size_t len) {
	int fd = *(int *)ctx;
	for (;;) { ssize_t w = write(fd, buf, len); if (w <= 0) { if (w < 0 && errno == EINTR) continue; return -1; } return (int)w; }
}

/* HTTPS GET host:port/path. On success returns 0 and sets *out (malloc'd response
 * BODY, headers stripped) and *outlen; nonzero on failure. */
int rune_tls_get(const char *host, int port, const char *path, unsigned char **out, size_t *outlen) {
	*out = NULL; *outlen = 0;
	int fd = socket(AF_INET, SOCK_STREAM, 0);
	if (fd < 0) return -1;
	struct sockaddr_in a; memset(&a, 0, sizeof a);
	a.sin_family = AF_INET; a.sin_port = htons((unsigned short)port); a.sin_addr.s_addr = inet_addr(host);
	if (connect(fd, (struct sockaddr *)&a, sizeof a) < 0) { close(fd); return -1; }

	br_ssl_client_context sc; br_x509_minimal_context xc; rune_noanchor_ctx xwc;
	unsigned char *iobuf = (unsigned char *)malloc(BR_SSL_BUFSIZE_BIDI);
	br_ssl_client_init_full(&sc, &xc, NULL, 0);
	rune_noanchor_init(&xwc, &xc.vtable);
	br_ssl_engine_set_x509(&sc.eng, &xwc.vtable);
	br_ssl_engine_set_buffer(&sc.eng, iobuf, BR_SSL_BUFSIZE_BIDI, 1);
	/* NULL server name: skip-verify (test only). It disables the X.509 server-name
	 * check entirely (and SNI) — mangling that error mid-chain instead leaves the
	 * engine in a bad state (BR_ERR_UNEXPECTED). The noanchor wrapper handles trust. */
	(void)host;
	br_ssl_client_reset(&sc, NULL, 0);
	br_sslio_context ioc;
	br_sslio_init(&ioc, &sc.eng, sock_read, &fd, sock_write, &fd);

	char req[1024];
	int rn = snprintf(req, sizeof req,
		"GET %s HTTP/1.0\r\nHost: %s\r\nConnection: close\r\n\r\n", path, host);
	if (br_sslio_write_all(&ioc, req, rn) < 0) { close(fd); free(iobuf); return -2; }
	br_sslio_flush(&ioc);

	size_t cap = 4096, len = 0; unsigned char *buf = (unsigned char *)malloc(cap);
	for (;;) {
		unsigned char tmp[2048];
		int rc = br_sslio_read(&ioc, tmp, sizeof tmp);
		if (rc < 0) break;
		if (len + (size_t)rc > cap) { while (len + (size_t)rc > cap) cap *= 2; buf = (unsigned char *)realloc(buf, cap); }
		memcpy(buf + len, tmp, (size_t)rc); len += (size_t)rc;
	}
	close(fd); free(iobuf);

	size_t off = 0;
	for (size_t i = 0; i + 3 < len; i++) {
		if (buf[i] == '\r' && buf[i + 1] == '\n' && buf[i + 2] == '\r' && buf[i + 3] == '\n') { off = i + 4; break; }
	}
	size_t bl = len - off;
	unsigned char *body = (unsigned char *)malloc(bl ? bl : 1);
	memcpy(body, buf + off, bl); free(buf);
	*out = body; *outlen = bl;
	return 0;
}
