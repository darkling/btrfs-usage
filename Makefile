TGT_DIR := build/assets/

ELM_SOURCE_FILES := $(wildcard src/*.elm)
STATIC_FILES := $(wildcard static/*.html static/*.css static/*.ttf)

STATIC_TARGET := $(addprefix $(TGT_DIR),$(subst static/,,$(STATIC_FILES)))
DOC_TARGET := doc/theory.pdf

.PHONY: all
all: $(TGT_DIR)/elm.js $(STATIC_TARGET) $(DOC_TARGET)

$(TGT_DIR)/elm.js: $(ELM_SOURCE_FILES)
	elm make --output=$(TGT_DIR)/elm.js $(ELM_SOURCE_FILES)

build/assets/%: static/%
	cp $< $@

doc/theory.pdf: doc/theory.tex
	which rubber && rubber --into doc --pdf doc/theory.tex

.PHONY: test
test:
	elm-test

.PHONY: test-watch
test-watch:
	elm-test --watch

.PHONY: webserver
webserver: all
	python3 -m http.server --directory build/assets/
