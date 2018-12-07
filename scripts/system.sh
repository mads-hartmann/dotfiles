#!/usr/bin/env bash
#
# A little CLI for managing whats installed, linked, and configured.
#
#
#

set -euo pipefail

#
# Utility functions
#

function str::strip_extension() {
    echo "${1%.*}"
}

# Ensure a symlink exists.
#   Nothing happens if the link already exists.
#   If a file/folder exists and isn't a link it complains.
function os::symlink() {
    local origin="$1"
    local target="$2"

    mkdir -p $(dirname "$target")

    if [[ -e "${target}" ]]; then
        local msg="File already exists ${target} and isn't a link. Overwriting it with ${origin}"
        rm -rf ${target} && ln -s "${origin}" "${target}"
    else
        echo "✓ Linking ${origin} -> ${target}"
        ln -sf "${origin}" "${target}"
    fi
}

#
# Commands
#

function help() {
    local installables

    installables=$(find .installers -name '*.sh' -exec basename {} +)
    echo $(str::strip_extension what/test.sh)


    cat <<EOF
I know how to install the following things:

${installables}

EOF
}

function link() {
    # Hard-coded for now
    local names="
        .zsh
        .zshrc
        .zshenv
        .bashrc
        .bash_profile
        .ctags.cnf
        .cheat
        .gitconfig
        .editorconfig
        .ssh/config"
    for name in ${names}; do
        os::symlink "$PWD/.home/${name}" "${HOME}/${name}"
    done
}

command="${1:-help}"

case "${command}" in
    help)
       help
    ;;
    link)
        link
    ;;
    *)
        echo "Unknown command: ${command}"
        exit 1
        ;;
esac
