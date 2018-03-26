# My dotfiles

These are my [dotfiles][dotfiles]. They are nothing out of the
extraordinary, but I like them nonetheless.

## From scratch

Install homebrew (will also install Xcode Command Line Tools)

```sh
/usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
```

Get 1Password installed so I can get access to all my secrets and my Github
account.

```sh
brew cask install 1password
```

Generate a SSH key, add it to Github.com and the keychain (my `dotfiles` takes
care of `~/.ssh/config`)

```sh
ssh-keygen -t rsa -b 4096 -C "mads379@gmail.com"
ssh-add -K ~/.ssh/id_rsa # add to keychain
cat ~/.ssh/id_rsa.pub | pbcopy
```

Clone my beloved `dotfiles`.

```sh
mkdir -p ~/dev/personal && cd ~/dev/personal
git clone git@github.com:mads-hartmann/dotfiles
cd dotfiles
./install.sh
```

[dotfiles]: https://dotfiles.github.io/
