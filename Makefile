# This Makefile takes care of symlinking, installing
# dependencies and generally configuring everything.

ifeq ($(shell which brew),)
    $(error Cant find brew in $$PATH. Please install it.)
endif


QUIET := @

build.dir := _build

# Find all the files that I'd like to have symlinked
symlinks := $(addprefix $(HOME)/.,$(shell ls home))

### Target Rules

setup_targets := \
	$(symlinks) \
	$(build.dir)/homebrew.installed \
	$(build.dir)/npm.installed \
	$(build.dir)/apm.installed \
	$(build.dir)/gems.installed \
	$(build.dir)/pips.installed \
	$(build.dir)/oh-my-zsh.installed

lint_targets := \
	$(addprefix $(build.dir)/lint/, $(shell ls ./bin/*))

setup: $(setup_targets)
lint: setup $(lint_targets)
clean: ; rm -r $(build.dir)

print-%: ; @echo $* is $($*)

### Pattern Rules

# Lint bash scripts - zsh isn't supported
$(build.dir)/lint/%: %
	$(call print,Linting $<)
	$(QUIET)shellcheck $<
	$(call touch,$@)

# Symlink all files ending in .symlink
$(HOME)/.%: home/%
	$(call print,Linking $(HOME)/.$* → $(abspath $<))
	$(QUIET)ln -fs $(abspath $<) $(HOME)/.$*

# Install oh-my-zsh
$(build.dir)/oh-my-zsh.installed:
	$(call print,Installing oh-my-zsh)
	$(QUIET)sh -c "$$(curl -fsSL https://raw.githubusercontent.com/robbyrussell/oh-my-zsh/master/tools/install.sh)"
	$(call touch,$@)

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
	$(QUIET)gem install $(shell cat $<)
	$(call touch, $@)

# Installs global pip packages.
$(build.dir)/pips.installed: requirements/pip-packages.txt
	$(call print,Installing pip packages)
	$(QUIET)pip install $(shell cat $<)
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
