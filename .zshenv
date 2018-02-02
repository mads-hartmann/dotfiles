# .zshenv is sourced on all invocations of the shell, unless the -f
# option is set. It should contain commands to set the command search
# path, plus other important environment variables. `.zshenv' should
# not contain commands that produce output or assume the shell is
# attached to a tty.

# Environment variables I don't want to have commited to git goes into that file
[ -f ~/.secrets ] && source ~/.secrets

export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8

export ZSH=$HOME/.oh-my-zsh
export ZSH_CUSTOM=$HOME/.zsh
export ZSH_THEME="hartmann"
export ZSH_HIGHLIGHT_HIGHLIGHTERS=(main brackets pattern cursor)

export DOTFILES_HOME="$HOME/dev/personal/dotfiles"

# Used in some of my git-related scripts.
export DEFAULT_PR_BRANCH="staging"

# Use emacs as the default editor.
# Default to using the GUI. Fallback to the launching emacs in the shell.
export EDITOR="emacsclient -c --socket-name server-gui --alternate-editor=emacsclient"

export ATOM_REPOS_HOME=~/dev/other

# Path.
#   For many of these path updates it would've been nicer to use
#   (brew --prefix <package>) to find the path, however, it's rather
#   slow so when you have a lot of them your boot time gets slow :(

# I have both python3 and python2 installed.
# This makes sure that python2 is on the path.
export PATH="/usr/local/opt/python/libexec/bin:$PATH"

# Coming from bash, I expect these to be in the PATH.
export PATH=/usr/local/bin:$HOME/bin:$PATH

# Homebrew somtimes puts things here.
export PATH="/usr/local/sbin:$PATH"

# Make sure that the homebrew installed emacs comes first in the
# shell.
export PATH="/usr/local/opt/emacs/bin:$PATH"

# I mostly use sed in CI scripts so might as well use the version that
# the CI servers are going to use
export PATH="/usr/local/opt/gnu-sed/libexec/gnubin:$PATH"

# Go
export GOROOT=/usr/local/opt/go/libexec # where homebrew install it.
export GOPATH=~/.go
export PATH=$PATH:$GOROOT/bin
export PATH=$PATH:$GOPATH/bin

# Docker for Mac ships with an out-dated version of docker-machine
# (0.12.0) rather than the one brew has (0.12.2). On top of that it
# seems that Docker for Mac will actually re-install itself over
# any `brew link docker-machine` you might have performed 🤔
# So now we just put the one from homebrew before the one from Docker for Mac
# on the PATH
export PATH="/usr/local/Cellar/docker-machine/0.12.2/bin:$PATH"

# fzf is an extremely useful tool.
# These are just some tweaks to make it even more useful than what
# the defaults are.
export FZF_DEFAULT_COMMAND='ag -g ""'
export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
export FZF_CTRL_T_OPTS="--preview '(highlight -O ansi -l {} 2> /dev/null || cat {} || tree -C {}) 2> /dev/null | head -200'"
export FZF_DEFAULT_OPTS="--height 100% --reverse --color=bg+:#000000,hl:#EAC170,hl+:#EAC170,prompt:#EAC170,pointer:#EAC170"

# cheat
export CHEATCOLORS=true
