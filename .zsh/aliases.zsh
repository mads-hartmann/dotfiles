### Aliases.
#
#    various commands that I'm too lazy to type out all the time.

alias reload=src
alias f=famlydev
alias epoch="date +%s | tr -d '\n'"
alias e="emacsclient -nw --socket-name=shell-emacs"
alias eg="emacsclient -c --no-wait --socket-name server-gui"
alias gti=git # DWIM version of git.

alias dps="docker ps --format 'table {{.ID}}\t{{.Names}}\t{{.Ports}}\t{{.Status}}'"
alias dc="docker-compose"
alias dm="docker-machine"
