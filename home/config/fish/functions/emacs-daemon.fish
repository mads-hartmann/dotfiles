# Starts an emacs daemon with a dedicated name. I use this daemon
# whenever I use emacs from the terminal.
function emacs-daemon
        emacs --daemon=shell-emacs
end
