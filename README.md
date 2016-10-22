# hartmann's dotfiles

These are my [dotfiles][dotfiles]. They are nothing out of the
extraordinary, but I like them nonetheless.

In case you want to browse around, here's the general structure
I've tried to follow.

    .
    ├── bin
    │   └── Executable scripts go into this directory. Various small
    │       scripts that I've found over the years.
    ├── home
    │   └── Everything in this folder is prefixed with . and symlinked
    │       to $HOME
    └── requirements
        └── Files that lists various kinds of dependencies. Such as
            global NPM packages, Homebrew packages, etc.

The installation of packages, creation of symlinks etc. are taken care
of my the Makefile. I love Makefiles.

[dotfiles]: https://dotfiles.github.io/
