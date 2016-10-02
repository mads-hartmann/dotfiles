# Edit a file/folder using the OS X Gui version of Emacs
function editg
        /Applications/Emacs.app/Contents/MacOS/bin/emacsclient -c --no-wait $argv
end
