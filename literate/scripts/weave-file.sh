#!/usr/bin/env bash

set -uo pipefail

file=$1
emacs_command="emacs"

if [[ "$(which docker)" ]]; then
    emacs_command="docker run -ti -v $(pwd):/home/babel/dotfiles:rw mads379/dotfiles-ci:0.0.2 emacs"
fi

${emacs_command} \
  -q \
  --script etc/project.el \
  --eval "(org-publish-file \"$(pwd)/${file}\" nil nil)"
