set -Ux DOTFILES_HOME "$HOME/dev/personal/dotfiles"

set -Ux DEFAULT_PR_BRANCH "staging"

# Use emacs as the default editor. It expects that an emacs daemon is
# running with the name 'shell-emacs'.
set -Ux EDITOR "emacsclient -c --socket-name=shell-emacs"

# Change the directory where gems are stored. I don't want to use
# sudo when installing gems.
set -Ux GEM_HOME "~/.gems"

# Path.
#   For many of these path updates it would've been nicer to use
#   (brew --prefix <package>) to find the path, however, it's rather
#   slow so when you have a lot of them your boot time gets slow :(

# Make sure my shell scripts are on on the path
set -x PATH $DOTFILES_HOME/bin $PATH

# Make sure that the homebrew installed emacs comes first in the
# shell.
set -x PATH /usr/local/opt/emacs/bin $PATH

# Make sure the hombrew installed PHP comes before the pre-installed
# PHP version. Couldn've used (brew --prefix php70)/bin to get the path
# but it's rather slow.
set -x PATH /usr/local/opt/php70/bin $PATH
