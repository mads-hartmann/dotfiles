# TODO

- [ ] Fix my PATH
    Sooo, mac has a concept of /etc/paths and /etc/paths.d but is used if I use the login shell
    However if set the shell command in iTerm to be /usr/local/bin/zsh it doesn't read those files
      which results in a PATH that's correct -- in the other case the /etc/paths are prefixed.

- [ ] There are some bootstraping problems
  - [ ] Can't tangle without having Emacs etc. Probably best to have an initial setup script that downloads the latest tangles from S3
    - [ ] How about something like: `sh -c "$(curl -fsSL https://raw.github.com/mads-hartmann/dotfiles/master/sh/install.sh)"`
          It would have to
          1. Install homebrew, zsh, a few basic things
          2. Install oh-my-zsh
          3. Download tangles and link them
          4. Install other apps like 1Password, Emacs, Code, etc.

- [ ] Have a look at https://github.com/herrbischoff/awesome-osx-command-line
- [ ] Create an OS X config file
- [ ] Do I need to run `xcode-select  --install` at some point?
- [ ] Currently I had to brew link --overwrite. Make sure that happens on setup automatically
