#!/usr/bin/env bash
#
# Bootstrap a fresh Macbook.
#

set -euo pipefail

echo ""
echo "Congratulations on getting a new Mac."
echo ""

echo "Install XCode command line tools"
xcode-select --install

echo "Install Homebrew"
/usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"

echo "Install Oh My ZSH!"
sh -c "$(curl -fsSL https://raw.github.com/robbyrussell/oh-my-zsh/master/tools/install.sh)"

echo "Cloning dotfiles"
mkdir -p ~/dev && cd ~/dev
git clone https://github.com/mads-hartmann/dotfiles.git
cd ~/dev/dotfiles

echo "Downloading the latest tangled dotfiles to bootstrap"
curl -o home.zip https://computer.mads-hartmann.com/.bootstrap/home.zip
unzip home.zip && rm home.zip
make link
