# TODO

- Have a look at https://github.com/herrbischoff/awesome-osx-command-line
- Create an OS X config file
- Create an iTerm config script (I don't want to use the plist.) It should set the simple things.

# xcode-select --install
https://github.com/jwiegley/git-scripts
https://github.com/jwiegley/git-all


## Race condition when install oh-my-zsh
There seems to be a race condition where oh-my-zsh creates the .zshrc
files before my repo and thus it doesn't get linked. Perhaps it should
simply overwite any files in HOME that isn't already a link like the
one we want.
