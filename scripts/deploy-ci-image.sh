#!/usr/bin/env bash

set -euo pipefail

VERSION=0.0.3
TAG=mads379/dotfiles-ci:${VERSION}

function main {
    docker build -t ${TAG} .circleci/docker
    docker push ${TAG}
}

main
