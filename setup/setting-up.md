From nothing to development.

Install home-brew (will also install Xcode Command Line Tools)
```
/usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
```

Get 1Password installed so I get all my credentials and can get access to my Github account.

```
brew cask install 1password
```

Install Chrome so I can login to Github and add my SSH keys.

```
brew cask install google-chrome
```

Generate a SSH key, add it to Github.com and your keychain

```
ssh-keygen -t rsa -b 4096 -C "mads379@gmail.com"
ssh-add -K ~/.ssh/id_rsa # add to keychain
cat ~/.ssh/id_rsa.pub | pbcopy
```

Clone my beloved dotfiles.

```
mkdir -p ~/dev/personal && cd ~/dev/personal
git clone git@github.com:mads-hartmann/dotfiles
cd dotfiles
./install.sh
```

