#!/bin/bash

set -euo pipefail

function usage {
    echo "Usage: every <seconds> <command>"
}

function every {
    local seconds=$1
    local command=$2
    local count=0

    # Emulation do-while loop.
    while :;
    do
        clear
        count=$((count+1))
        printf "Running '${command}' every ${seconds} seconds. Count. ${count}\n\n"
        ${command}
        sleep ${seconds}
    done
}

seconds=${1:?You must provide the interval in seconds}
command="${@:2}"

if [[ -z "${command}" ]]
then
    usage
    printf "\nMissing argument <command>\n"
else
    every ${seconds} "${command}"
fi
