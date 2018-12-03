#!/usr/bin/env bash
#
# A little CLI for building the assets of the site.
#
# If Docker is installed it will use a container to do the processing,
# otherwise it uses your local installation of Emacs - I needed that
# as my old MBA can't run Docker
#
# Example:
#
#     buildtool.sh weave <FILE>
#     buildtool.sh tangle <FILE>

set -uo pipefail

# TODO
# emacs_command="docker run -ti -v $(pwd):/home/babel/dotfiles:rw mads379/dotfiles-ci:0.0.2 emacs"

function tangle() {
    local file="$1"
    emacs \
	-q \
	--script literate/etc/project.el \
	--eval "(org-babel-tangle-file \"${file}\")"
}

function weave() {
    local file="$1"
    emacs \
	-q \
	--script literate/etc/project.el \
	--eval "(org-publish-file \"$(pwd)/${file}\" nil nil)"
}

command="$1"
file="$2"

case "${command}" in
    "weave")
	weave "${file}"
	;;
    "tangle")
	tangle "${file}"
	;;
    *)
	echo "Unknown command: '${command}'"
	exit 1
esac
