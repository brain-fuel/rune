CREATE TABLE lexicon (strong TEXT PRIMARY KEY, lemma TEXT, translit TEXT, lang TEXT, pos TEXT, root TEXT);
INSERT OR IGNORE INTO lexicon(strong,lemma,translit,lang,pos,root) VALUES ('G0001','Α','A','grc','N','G0001');
INSERT OR IGNORE INTO lexicon(strong,lemma,translit,lang,pos,root) VALUES ('G0002','ab''c',NULL,'grc',NULL,NULL);
INSERT OR IGNORE INTO lexicon(strong,lemma,translit,lang,pos,root) VALUES ('H0001','אב','av','hbo','N',NULL);
INSERT OR IGNORE INTO lexicon(strong,lemma,translit,lang,pos,root) VALUES (NULL,'λόγος',NULL,'grc',NULL,NULL);
CREATE INDEX idx_lexicon_lemma ON lexicon(lemma);
