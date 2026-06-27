package main

import (
	"fmt"
	"io"
	"os"

	"goforge.dev/rune/v3/calm"
	"goforge.dev/rune/v3/control"
	"goforge.dev/rune/v3/internal/session"
	"goforge.dev/rune/v3/ledger"
)

// runCalm implements `rune calm emit|validate`. Both subcommands build the SOURCE
// model from a manifest (the resource graph -> nodes) and a control listing (the
// assurance ledger -> per-control tiers and hashes); the control catalog maps each
// control to its node or relationship. `emit` writes the CALM document; `validate`
// checks a document against the source 1:1 and fails on any mismatch.
//
//	rune calm emit     [--manifest M] [--listing L]
//	rune calm validate FILE [--manifest M] [--listing L]
func runCalm(args []string, out io.Writer) error {
	if len(args) == 0 {
		return fmt.Errorf("usage: rune calm emit|validate [FILE] [--manifest M] [--listing L]")
	}
	sub := args[0]
	rest := args[1:]

	manifest := "examples/wavelet_demo.wav"
	listing := "listings/ch538_control_catalog.rune"
	var file string
	for i := 0; i < len(rest); i++ {
		switch rest[i] {
		case "--manifest":
			i++
			if i < len(rest) {
				manifest = rest[i]
			}
		case "--listing":
			i++
			if i < len(rest) {
				listing = rest[i]
			}
		default:
			if file == "" {
				file = rest[i]
			}
		}
	}

	source, err := buildSourceModel(manifest, listing)
	if err != nil {
		return err
	}

	switch sub {
	case "emit":
		return calm.Emit(source, out)
	case "validate":
		if file == "" {
			return fmt.Errorf("usage: rune calm validate FILE [--manifest M] [--listing L]")
		}
		data, err := os.ReadFile(file)
		if err != nil {
			return err
		}
		doc, err := calm.Parse(data)
		if err != nil {
			return fmt.Errorf("not a valid CALM document: %w", err)
		}
		errs := calm.Validate(doc, source)
		for _, e := range errs {
			fmt.Fprintln(os.Stderr, e.Error())
		}
		if len(errs) > 0 {
			return fmt.Errorf("CALM validation failed: %d mismatch(es)", len(errs))
		}
		fmt.Fprintln(out, "CALM document validates against the source 1:1.")
		return nil
	default:
		return fmt.Errorf("rune calm: unknown subcommand %q (want emit or validate)", sub)
	}
}

// buildSourceModel loads the resource graph + the control ledger and assembles the
// source model the catalog binds together.
func buildSourceModel(manifest, listing string) (calm.Model, error) {
	rs, err := parseManifest(manifest)
	if err != nil {
		return calm.Model{}, err
	}
	src, err := os.ReadFile(listing)
	if err != nil {
		return calm.Model{}, err
	}
	s := session.New()
	if _, err := s.LoadSource(string(src)); err != nil {
		return calm.Model{}, err
	}
	entries := ledger.Build(s)
	return calm.BuildModel(rs, control.Catalog(), entries)
}
