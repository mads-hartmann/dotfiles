# hartmann's dotfiles

This is my attempt at getting my dotfiles in order. I'm still figuring
out how I'd like to structure everything.

Almost everything you see here has been stolen from various other
repositories I found [dotfiles][dotfiles].




## TODO
Move the following into a .gitconfig file.

    git config --global alias.co checkout
    git config --global alias.stats "diff --stat"
    git config --global core.pager "diff-so-fancy | less --tabs=4 -RFX"

~/Library/Keybindings/DefaultKeyBinding.dict -- This actually didn't seem to work

    {
        "~d" = "deleteWordForward:";
        "^w" = "deleteWordBackward:";
        "~f" = "moveWordForward:";
        "~b" = "moveWordBackward:";
    }

[dotfiles]: https://dotfiles.github.io/
