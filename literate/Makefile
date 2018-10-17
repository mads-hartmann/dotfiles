BUILD_DIR := _build
QUIET ?= @

ORG_FILES := $(wildcard org/**/*.org)
TANGLE_TARGETS := \
	$(patsubst %.org,$(BUILD_DIR)/%.tangled,$(ORG_FILES))


build: build-website build-files

# TODO: Improve this so it only generates the files that have changed. This should
# be possible through a simple %.html: %.org rule as there's a direct connection
# between the two.
build-website: $(ORG_FILES)
	$(QUIET)docker run \
		-v $(shell pwd)/..:/home/babel/dotfiles:rw \
		mads379/dotfiles-ci:0.0.2 \
			emacs \
			--script dotfiles/literate/etc/project.el \
			--eval "(org-publish-project \"everything\" t)"


tangle: $(TANGLE_TARGETS)

shell:
	$(QUIET)docker run \
		-ti \
		-v $(shell pwd)/..:/home/babel/dotfiles:rw \
		mads379/dotfiles-ci:0.0.2 bash

clean:
	rm -rf output

# rules

# There isn't a direct connection between the source files and the tangled output
# so we create this imaginary one instead.
$(BUILD_DIR)/%.tangled: %.org
	$(QUIET)mkdir -p $(dir $@)
	$(QUIET)docker run \
		-ti \
		-v $(shell pwd)/..:/home/babel/dotfiles:rw \
		mads379/dotfiles-ci:0.0.2 \
			emacs \
			--script dotfiles/literate/etc/project.el \
			--eval "(org-babel-tangle-file \"/home/babel/dotfiles/literate/$<\")"
	$(QUIET)touch $@

# Debug.

print-%: ; @echo $* is $($*)
