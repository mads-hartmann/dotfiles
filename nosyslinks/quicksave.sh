#!/usr/bin/env bash

set -euo pipefail

BACKUP_LOCATION="./store"

quicksave::expand_tilde() {
    # Expand tilde into the value of $HOME
    # https://stackoverflow.com/questions/3963716/how-to-manually-expand-a-special-variable-ex-tilde-in-bash/27485157#27485157
    local path=$1
    echo "${path/#\~/$HOME}"
}

quicksave::backup_file() {
    local path=$1
    local hash

    path=$(quicksave::expand_tilde "$path")
    hash="$(echo -n "$path" | md5sum | awk '{print $1}')"

    if [ ! -f "$path" ]; then
        echo "File doens't exit: $path. Skipping"
        return
    fi

    echo "Copying file: $path"
    cp "$path" "$BACKUP_LOCATION/$hash"
}

quicksave::backup_folder() {
    local path=$1
    local hash

    path=$(quicksave::expand_tilde "$path")
    hash="$(echo -n "$path" | md5sum | awk '{print $1}')"

    if [ ! -d "$path" ]; then
        echo "Folder doens't exit: $path. Skipping"
        return
    fi

    echo "Copying folder: $path"
    mkdir -p "$BACKUP_LOCATION/$hash"
    cp -R "$path/" "$BACKUP_LOCATION/$hash"
}

quicksave::restore_file() {
    local path=$1
    local hash

    path=$(quicksave::expand_tilde "$path")
    hash="$(echo -n "$path" | md5sum | awk '{print $1}')"

    echo "Copying file: $BACKUP_LOCATION/$hash -> $path"
    mkdir -p "$(dirname "$path")"
    cp "$BACKUP_LOCATION/$hash" "$path"
}

quicksave::restore_folder() {
    local path=$1
    local hash

    path=$(quicksave::expand_tilde "$path")
    hash="$(echo -n "$path" | md5sum | awk '{print $1}')"

    if [ ! -d "$BACKUP_LOCATION/$hash" ]; then
        echo "Folder doens't exit: $BACKUP_LOCATION/$hash. Skipping"
        return
    fi

    echo "Copying folder: $BACKUP_LOCATION/$hash -> $path"
    mkdir -p "$path"
    cp -R "$BACKUP_LOCATION/$hash" "$path/"
}

quicksave::check_prerequisites() {
    # Check that the required programs are installed.
    command -v jq > /dev/null || (echo "Missing program: jq" && exit 1)
    command -v md5sum > /dev/null || (echo "Missing program: md5sum" && exit 1)
}

quicksave::help() {
    cat <<EOF

Usage: quicksave <command>

Where command is one of

- restore
- backup

EOF
}

quicksave::restore() {
    local files
    local folders

    files="$(cat files.json | jq -r '.[] | select(.type == "file") | .path')"
    folders="$(cat files.json | jq -r '.[] | select(.type == "folder") | .path')"

    while read -r path; do
        quicksave::restore_file "$path"
    done <<< "$files"

    while read -r path; do
        quicksave::restore_folder "$path"
    done <<< "$folders"
}

quicksave::backup() {
    local files
    local folders

    files="$(cat files.json | jq -r '.[] | select(.type == "file") | .path')"
    folders="$(cat files.json | jq -r '.[] | select(.type == "folder") | .path')"

    mkdir -p store

    # Note: Using while + read in order to read the paths line by line. Had I used a
    # normal for $path in $folders it would've performed word-splitting on spaces

    while read -r path; do
        quicksave::backup_file "$path"
    done <<< "$files"

    while read -r path; do
        quicksave::backup_folder "$path"
    done <<< "$folders"
}

quicksave::main() {

    # If no arugments were passsed we don't know what do to.
    if [[ $# -lt 1 ]]; then
        echo "Error: Missing command"
        quicksave::help
        exit 1
    fi

    local command="$1"
    case $command in
        restore)
            quicksave::restore
            ;;
        backup)
            quicksave::backup
            ;;
        *)
            echo "Error: Unknown command '$command'"
            quicksave::help
            exit 1
            ;;
    esac
}

quicksave::check_prerequisites
quicksave::main $@
