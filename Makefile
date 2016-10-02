# This Makefile takes care of symlinking, installing
# dependencies and generally configuring everything.

QUIET := @

build.dir := _build

# Find all the files that I'd like to have symlinked
symlinks := $(addprefix $(HOME)/.,$(shell ls home))

### Target Rules

setup_targets := \
	$(symlinks) \
	$(HOME)/.config/fish \
	$(build.dir)/homebrew.installed \
	$(build.dir)/npm.installed \
	$(build.dir)/apm.installed \
	$(build.dir)/gems.installed

setup: $(setup_targets)
clean: ; rm -rf $(build.dir)
print-%: ; @echo $* is $($*)

### Pattern Rules

# Symlink all files ending in .symlink
$(HOME)/.%: home/%
	$(call print,Linking $(HOME)/.$* → $(abspath $<))
	$(QUIET)ln -fs $(abspath $<) $(HOME)/.$*

# Install all homebrew packages.
$(build.dir)/homebrew.installed: requirements/Brewfile
	$(call print,Installing Homebrew packages)
	$(QUIET)cd requirements && brew bundle -v
	$(call touch, $@)

# Installs global NPM dependencies.
$(build.dir)/npm.installed: requirements/npm-packages.txt
	$(call print,Installing global NPM packages)
	$(QUIET)npm install -g $(shell cat $<)
	$(call touch, $@)

# Installs global gems.
$(build.dir)/gems.installed: requirements/gems.txt
	$(call print,Installing global gems)
	$(QUIET)apm install $(shell cat $<)
	$(call touch, $@)

# Installs atom packages.
$(build.dir)/apm.installed: requirements/atom-packages.txt
	$(call print,Installing Atom packages)
	$(QUIET)apm install $(shell grep -v '\#' $<)
	$(call touch, $@)

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
