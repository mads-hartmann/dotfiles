# This Makefile takes care of symlinking, installing
# dependencies and generally configuring everything.

QUIET := @
.DEFAULT_GOAL := setup

build.dir := _build

# Find all the files that I'd like to have symlinked
symlinks := $(addprefix $(HOME)/.,$(shell ls home))
git-pr := /usr/local/share/zsh/site-functions/_git-pr /usr/local/bin/git-pr

install_targets =

setup_targets = \
	$(build.dir)/oh-my-zsh.installed \
	$(build.dir)/zsh-syntax-highlighting.installed \
	$(symlinks) \
	$(git-pr)

lint_targets = \
	$(addprefix $(build.dir)/lint/, $(shell ls ./bin/*))

#
# Import the appropriate setup.
#
ifeq ($(shell uname),Linux)
    include Makefile.linux
else
    include Makefile.osx
endif

#
# Targets
#

setup: install $(setup_targets)
install: $(install_targets)
lint: setup $(lint_targets)
clean: ; rm -r $(build.dir)

print-%: ; @echo $* is $($*)

#
# Rules
#

# Lint bash scripts - zsh isn't supported
$(build.dir)/lint/%: %
	$(call print,Linting $<)
	$(QUIET)shellcheck $<
	$(call touch,$@)

# Symlink all files ending in .symlink
$(HOME)/.%: home/%
	$(call print,Linking $(HOME)/.$* → $(abspath $<))
	$(QUIET)ln -fs $(abspath $<) $(HOME)/.$*

/usr/local/share/zsh/site-functions/_git-pr:
	ln -fs $(abspath bin/git-pr.completions.sh) $@

/usr/local/bin/git-pr:
	ln -fs $(abspath bin/git-pr.sh) $@

# Install oh-my-zsh
$(build.dir)/oh-my-zsh.installed:
	$(call print,Installing oh-my-zsh)
	$(QUIET)sh -c "$$(curl -fsSL https://raw.githubusercontent.com/robbyrussell/oh-my-zsh/master/tools/install.sh)"
	$(call touch,$@)

$(build.dir)/zsh-syntax-highlighting.installed:
	$(call print,Installing zsh-syntax-highlighting)
	git clone https://github.com/zsh-users/zsh-syntax-highlighting.git $(HOME)/.oh-my-zsh/custom/plugins/zsh-syntax-highlighting
	$(call touch,$@)

#
# Useful functions
#

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
