QUIET ?= @ # use QUIET= make xyz to get command output

ORG_FILES := $(shell find org -name "*.org")

TANGLE_TARGETS := \
	$(patsubst %.org,.build/%.tangled,$(ORG_FILES))

WEAVE_TARGETS := \
	$(patsubst org/%.org,.website/%/index.html,$(ORG_FILES))

COPY_TARGETS := \
	$(patsubst static/%,.website/static/%,$(shell find static -name "*"))

# TODO: Hardcoded for now. Later I'll do something smart.
LINKS := .zsh .zshrc .zshenv .bashrc .ctags.cnf
LINK_TARGETS := \
	$(foreach link,$(LINKS),$(HOME)/$(link))

all: weave tangle
weave: $(WEAVE_TARGETS) $(COPY_TARGETS)
tangle: $(TANGLE_TARGETS)
link: tangle $(LINK_TARGETS)

shell:
	$(QUIET)docker run \
		-ti \
		-v $(shell pwd)/..:/home/babel/dotfiles:rw \
		mads379/dotfiles-ci:0.0.2 bash

clean:
	rm -rf .website .build .home

serve: weave
# TODO: Use nginx or some other docker image instead. I mean, why not.
	cd .website && serve 80

#
# rules
#

${HOME}/%: .home/%
	ln -s $(abspath $<) $@

# Instead of having org-mode copy over static files as many people do
# I just use make instead.
.website/static/%: static/%
	$(QUIET)mkdir -p $(dir $@)
	cp $< $@

.website/%/index.html: org/%.org
	$(QUIET)docker run \
		-ti \
		-v $(shell pwd)/..:/home/babel/dotfiles:rw \
		mads379/dotfiles-ci:0.0.2 \
			emacs \
			--script dotfiles/literate/etc/project.el \
			--eval "(org-publish-file \"/home/babel/dotfiles/literate/$<\" nil nil)"
# To the best of my knowledge there isn't a good way to have org-mode
# publish name/index.html instead of name.html so instead I do all
# this horrible hackery ¯\_(ツ)_/¯
	$(if $(shell [[ $< == "org/index.org" ]] && echo "skip"),\
		$(QUIET)echo "skipping", \
		$(QUIET)mkdir -p $(dir $@) && mv .website/$*.html $@)

# There isn't a direct connection between the source files and the tangled output
# so we create this imaginary one instead.
.build/%.tangled: %.org
	$(QUIET)mkdir -p $(dir $@)
	$(QUIET)docker run \
		-ti \
		-v $(shell pwd)/..:/home/babel/dotfiles:rw \
		mads379/dotfiles-ci:0.0.2 \
			emacs \
			--script dotfiles/literate/etc/project.el \
			--eval "(org-babel-tangle-file \"/home/babel/dotfiles/literate/$<\")"
	$(QUIET)touch $@

#
# Debug.
#

# make print-VARIABLE_NAME will print the contents of the variable
print-%: ; @echo $* is $($*)
