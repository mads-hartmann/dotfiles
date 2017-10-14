# TODO

- [ ] Have a look at https://github.com/herrbischoff/awesome-osx-command-line
- [ ] Create an OS X config file
- [ ] Enable having extensions in my `bin` folder and strip them when linking.
- [ ] See if mackup is something for me
- [ ] Do I need to run `xcode-select  --install` at some point?
- [ ] Currently I had to brew link --overwrite. Make sure that happens on setup automatically
- [ ] Switch to Hyper for my terminal? Would need to add theme etc as part of my dotfiles


https://github.com/jwiegley/git-scripts
https://github.com/jwiegley/git-all

## Race condition when install oh-my-zsh
There seems to be a race condition where oh-my-zsh creates the .zshrc
files before my repo and thus it doesn't get linked. Perhaps it should
simply overwite any files in HOME that isn't already a link like the
one we want.

## Key-bindings
Figure out how to automate the creation of `~/Library/KeyBindings/DefaultKeyBinding.dict`

    {
        "~d" = "deleteWordForward:";
        "^w" = "deleteWordBackward:";
        "~f" = "moveWordForward:";
        "~b" = "moveWordBackward:";
    }
