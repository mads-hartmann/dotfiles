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
    │   ├── bash_profile
    │   │   └── Simple loads .bashrc.
    │   ├── bashrc
    │   │   └── Configuration of bash. Almost nothing in it. Hoping to
    │   │       be able to kill it at some point.
    │   ├── config
    │   │   └── fish
    │   │       └── Configuration of my fish shell environment. Fish is
    │   │           currently my shell of choice.
    │   ├── ctags
    │   │   └── Sometimes, ctags is still a very useful way to navigate
    │   │       a large project.
    │   ├── gitconfig
    │   │   └── Configuration of git, as you might have expected.
    └── requirements
        └── Files that lists various kinds of dependencies. Such as
            global NPM packages, Homebrew packages, etc.

Everything inside of `home` is prefixed with `.` and symlinked to
`$HOME`.

The installation of packages, creation of symlinks etc. are taken care
of my the Makefile. I love Makefiles.

[dotfiles]: https://dotfiles.github.io/
