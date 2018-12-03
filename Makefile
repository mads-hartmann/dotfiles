#
# This Makefile takes care of tangling, weaving, copying, and linking
# everything into the right place.
#

# use QUIET= make xyz to get command output
QUIET ?= @

# All the files that I want to have linked into ~ from .home
LINKS := \
	.zsh .zshrc .zshenv \
	.bashrc \
	.ctags.cnf \
	.cheat \
	.gitconfig \
	.editorconfig \
	.ssh/config

ILLITERATE := \
	$(patsubst \
		illiterate/%,\
		${HOME}/%,\
		$(shell find illiterate -type d -maxdepth 1 -name ".*"))

# I strip the file extension.
SCRIPTS := \
	$(basename $(patsubst \
		illiterate/bin/%,\
		/usr/local/bin/%,\
		$(shell find illiterate/bin -type f)))

ORG_FILES := \
	$(shell find literate/org -name "*.org")

TANGLE_TARGETS := \
	$(patsubst %.org,.build/%.tangled,$(ORG_FILES))

WEAVE_TARGETS := \
	$(patsubst \
		literate/org/%.org,\
		.website/%/index.html,\
		$(ORG_FILES))

COPY_TARGETS := \
	$(patsubst \
		literate/static/%,\
		.website/static/%,\
		$(shell find literate/static -name "*"))

LINK_TARGETS := \
	$(foreach link,$(LINKS),$(HOME)/$(link))

all: weave tangle
weave: $(WEAVE_TARGETS) $(COPY_TARGETS)
tangle: $(TANGLE_TARGETS)
link: tangle $(LINK_TARGETS) $(SCRIPTS) $(ILLITERATE)

shell:
	$(QUIET)docker run \
		-ti \
		-v $(shell pwd)/..:/home/babel/dotfiles:rw \
		mads379/dotfiles-ci:0.0.2 bash

clean:
	rm -rf .website .build .home

# TODO: Use nginx or some other docker image instead. I mean, why not.
serve: weave
	cd .website && serve 80

#
# rules
#

${HOME}/%: .home/%
	$(call print, Linking, $@ -> $<)
	$(QUIET)ln -s $(abspath $<) $@

${HOME}/%: illiterate/%
	$(call print, Linking, $@ -> $<)
	@$(QUIET)ln -sf "$(abspath $<)" "$@"

/usr/local/bin/%: illiterate/bin/%*
	$(call print, Linking, $@ -> $<)
	$(QUIET)@ln -sf "$(abspath $<)" "$@"

# Instead of having org-mode copy over static files as many people do
# I just use make instead.
.website/static/%: literate/static/%
	$(call print, Copying over $<)
	$(QUIET)mkdir -p $(dir $@)
	$(QUIET)cp $< $@

.website/%/index.html: literate/org/%.org
	$(QUIET)./scripts/buildtool.sh weave "$<"
# To the best of my knowledge there isn't a good way to have org-mode
# publish name/index.html instead of name.html so instead I do all
# this horrible hackery ¯\_(ツ)_/¯
	$(if $(shell [[ $< == "literate/org/index.org" ]] && echo "skip"),\
		$(QUIET)echo "skipping", \
		$(QUIET)mkdir -p $(dir $@) && mv .website/$*.html $@)

# There isn't a direct connection between the source files and the tangled output
# so we create this imaginary one instead.
.build/%.tangled: %.org
	$(QUIET)mkdir -p $(dir $@)
	$(QUIET)./scripts/buildtool.sh tangle "$<"
	$(QUIET)touch $@

#
# Functions.
#

# $(call print-rule, variable, extra)
#   Used to decorate the output before printing it to stdout.
define print
	@echo "[$(shell date +%H:%M:%S)] $(strip $1): $(strip $2)"
endef

#
# Debug.
#

# make print-VARIABLE_NAME will print the contents of the variable
print-%: ; @echo $* is $($*)
