#!/usr/bin/env bash

set -euo pipefail

VERSION=0.0.1
TAG=mads379/dotfiles-ci:${VERSION}

function main {
    docker build -t ${TAG} --file docker/Dockerfile docker
    docker push ${TAG}
}

main
