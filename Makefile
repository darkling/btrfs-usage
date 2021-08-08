TGT_DIR := build/assets/

ELM_SOURCE_FILES := $(wildcard src/*.elm)
STATIC_FILES := $(wildcard static/*.html static/*.css)

STATIC_TARGET := $(addprefix $(TGT_DIR),$(subst static/,,$(STATIC_FILES)))

all: $(TGT_DIR)/elm.js $(STATIC_TARGET)

$(TGT_DIR)/elm.js: $(ELM_SOURCE_FILES)
	elm make --output=$(TGT_DIR)/elm.js $(ELM_SOURCE_FILES)

build/assets/%: static/%
	cp $< $@
