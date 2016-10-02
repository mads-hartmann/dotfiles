# This Makefile takes care of symlinking, installing
# dependencies and generally configuring everything.

QUIET := @

build.dir := _build
backup.dir := _backup

symlinks := $(patsubst %.symlink,$(build.dir)/%.linked, $(shell find . -name "*.symlink"))

### Target Rules

setup: $(symlinks)
clean: ; rm -rf $(build.dir)
print-%: ; @echo $* is $($*)


### Pattern Rules

_build/%.linked: %.symlink
	$(call print,Linking $(HOME)/.$(notdir $*) → $(abspath $<))
	$(QUIET)ln -fs $(abspath $<) $(HOME)/.$(notdir $*)
	$(call touch,$@)


### Useful functions

# $(call touch, file)
#   Touch a file, making sure to create parent directories first if
#   they dont exist.
define touch
	@mkdir -p $(dir $1)
	@touch $1
endef

# $(call print, text)
#   Used to decorate the output before printing it to stdout.
define print
	@echo "[$(shell date +%H:%M:%S)] $(strip $1)"
endef
