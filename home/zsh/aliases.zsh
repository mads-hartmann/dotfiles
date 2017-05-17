### Aliases.
#
#    various commands that I'm too lazy to type out all the time.

alias reload=src
alias f="famlydev"
alias epoch="date +%s"
alias e="emacsclient -nw --socket-name=shell-emacs"
alias eg="/Applications/Emacs.app/Contents/MacOS/bin/emacsclient -c --no-wait"

# Starts an emacs daemon with a dedicated name. I use this daemon
# whenever I use emacs from the terminal.
alias emacs-daemon="emacs --daemon=shell-emacs"

alias dps="docker ps --format 'table {{.ID}}\t{{.Names}}\t{{.Ports}}\t{{.Status}}'"
alias dc="docker-compose"
