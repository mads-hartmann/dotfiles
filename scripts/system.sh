#!/usr/bin/env bash
#
# A little CLI for managing whats installed, linked, and configured.
#
# 
#

set -euo pipefail

function str::strip_extension() {
    echo "${1%.*}"
}

function help() {
    local installables

    installables=$(find .installers -name '*.sh' -exec basename {} +)
    echo $(str::strip_extension what/test.sh)


    cat <<EOF
I know how to install the following things:

${installables}

EOF
}

command="${1:-help}"

case "${command}" in
    help)
       help
    ;;
    *)
        echo "Unknown command: ${command}"
        exit 1
        ;;
esac
