#!/usr/bin/env bash
#
#
#

set -euo pipefail

quicksave::expand_tilde() {
    # Expand tilde into the value of $HOME
    # https://stackoverflow.com/questions/3963716/how-to-manually-expand-a-special-variable-ex-tilde-in-bash/27485157#27485157
    local path=$1
    echo "${path/#\~/$HOME}"
}

quicksave::copy_file() {
    local path=$1
    local hash

    path=$(quicksave::expand_tilde "$path")
    hash="$(echo -n "$path" | md5sum | awk '{print $1}')"

    if [ ! -f "$path" ]; then
        echo "File doens't exit: $path"
        exit 1
    fi

    echo "Copying file: $path"
    cp "$path" "./quicksave/$hash"
}

quicksave::copy_folder() {
    local path=$1
    local hash

    path=$(quicksave::expand_tilde "$path")
    hash="$(echo -n "$path" | md5sum | awk '{print $1}')"

    if [ ! -d "$path" ]; then
        echo "Folder doens't exit: $path"
        exit 1
    fi

    echo "Copying folder: $path"
    mkdir -p "./quicksave/$hash"
    cp -R "$path/" "./quicksave/$hash"
}

# Check that the required programs are installed.
command -v jq > /dev/null || (echo "Missing program: jq" && exit 1)
command -v md5sum > /dev/null || (echo "Missing program: md5sum" && exit 1)

files="$(cat files.json | jq -r '.[] | select(.type == "file") | .path')"
folders="$(cat files.json | jq -r '.[] | select(.type == "folder") | .path')"

mkdir -p quicksave

# Note: Using while + read in order to read the paths line by line. Had I used a
# normal for $path in $folders it would've performed word-splitting on spaces

while read -r path; do
    quicksave::copy_file "$path"
done <<< "$files"

while read -r path; do
    quicksave::copy_folder "$path"
done <<< "$folders"
