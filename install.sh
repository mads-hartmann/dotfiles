#/bin/bash
#
# Usage:
#   homebrew  - Installs homebrew + homebrew packages
#   all       - Installs _everything_
#

set -euo pipefail

# Ensure a symlink exists.
#   Nothing happens if the link already exists.
#   If a file/folder exists and isn't a link it complains.
function symlink {
    local origin="$1"
    local target="$2"

    if [[ -L "${target}" ]]; then
        echo "✓ Link already exists: ${target} "
    elif [[ -e "${target}" ]]; then
        echo "File already exists ${origin} and isn't a link: ${target}"
    else
        echo "✓ Linking ${origin} -> ${target}"
        ln -s "${origin}" "${target}"
    fi
}

# install brew
function install_homebrew {
    local check_status_code

    echo "Homebrew"
    if [[ -z "$(which brew)" ]]; then
        echo "Installing homebrew"
        ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
    else
        echo "✓ Homebrew is installed"
    fi
    set +e
    brew bundle check --file=./requirements/Brewfile > /dev/null
    check_status_code=$?
    set -e
    if [[ ${check_status_code} -gt 0 ]]; then
        echo "Installing homebrew packages"
        brew bundle --no-upgrade --file=./requirements/Brewfile
    else
        echo "✓ All packages satisfied"
    fi
}

# Install oh-my-zsh
function install_oh_my_zsh {
    echo "Oh-my-zsh"
    if [[ -d ~/.oh-my-zsh ]]; then
        echo "✓ oh-my-zsh is already installed"
    else
        echo "Installing oh-my-zsh"
        sh -c "$(curl -fsSL https://raw.githubusercontent.com/robbyrussell/oh-my-zsh/master/tools/install.sh)"
    fi

    if [[ -d ${HOME}/.oh-my-zsh/custom/plugins/zsh-syntax-highlighting ]]; then
        echo "✓ zsh-syntax-highlighting is already installed"
    else
        echo "Installing zsh-syntax-highlighting"
        git clone \
            https://github.com/zsh-users/zsh-syntax-highlighting.git \
            $(HOME)/.oh-my-zsh/custom/plugins/zsh-syntax-highlighting
    fi
}

function install_npm_packages {
    echo "Node"
    npm install -g $(cat requirements/npm-packages.txt)
}

function install_gem_packages {
    echo "Ruby"
    gem install $(cat requirements/gems.txt)
}

function install_pip_packages {
    echo "Python"
    pip install $(cat requirements/pip-packages.txt)
}

function install_atom_packages {
    echo "Atom"
    apm install --packages-file .atom/apm-packages.txt
}

function install_opam_packages {
    echo "OCaml"
    grep -v "#" requirements/opam-packages.txt | grep -v "^$$" | xargs -L 1 opam
}


# Create symlinks for all the .xyz files into $HOME
function create_symlinks {
    local files
    files=$(ls -Al | awk '{ print $9 }' | grep -E '^\.' | grep -v '.DS_Store' | grep -v '.gitignore' | grep -v '.git$')

    echo "Create symlinks for everyhing"
    for file in ${files}; do
        symlink "$(pwd)/$file" "$HOME/$file"
    done

    symlink "${HOME}/Library/Mobile Documents/com~apple~CloudDocs" "${HOME}/iCloudDrive"
}

# Make vscode work well with my dotfiles.
function create_vscode_symlinks {
    echo "Creating vscode symlinks"
    symlink ~/.code/keybindings.json "${HOME}/Library/Application Support/Code/User/keybindings.json"
    symlink ~/.code/settings.json "${HOME}/Library/Application Support/Code/User/settings.json"
    symlink ~/.code/keybindings.json "${HOME}/Library/Application Support/Code - Insiders/User/keybindings.json"
    symlink ~/.code/settings.json "${HOME}/Library/Application Support/Code - Insiders/User/settings.json"
}

function install_all {
    install_homebrew
    install_oh_my_zsh
    install_npm_packages
    install_gem_packages
    install_atom_packages
    install_pip_packages
    install_opam_packages
    create_symlinks
    create_vscode_symlinks
}



function main {
    local cmd="${1:-all}"

    case "${cmd}" in
        "homebrew")
             install_homebrew
             ;;
        *)
            install_all
            ;;
    esac
}

main "$@"
