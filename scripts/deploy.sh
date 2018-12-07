#!/usr/bin/env bash

set -euo pipefail

bucket="computer.mads-hartmann.com"

function deploy-website-to-s3() {
  echo "Deploying to ${bucket}"

  echo "Performing major hack: Replacing all local urls with production urls"
  find .website -type f -exec \
    sed -i 's/http:\/\/computer.localhost/https:\/\/computer\.mads-hartmann\.com/g' {} +

  aws s3 sync \
    --region eu-central-1 \
    .website/ \
    s3://${bucket}/ \
      --acl public-read \
      --cache-control "max-age=0, no-cache, no-store" \
      --expires "Thu, 01 Jan 1970 00:00:00 GMT"
}

function deploy-home-to-s3() {
  echo "Creating zip file."
  zip -r home.zip .home

  aws s3 sync \
    --region eu-central-1 \
    home.zip \
    s3://${bucket}/.bootstrap/home.zip \
      --acl public-read \
      --cache-control "max-age=0, no-cache, no-store" \
      --expires "Thu, 01 Jan 1970 00:00:00 GMT"

    rm home.zip
}

function invlidate-cache() {
    echo "Invalidating CloudFront distribution"
    aws cloudfront create-invalidation \
        --distribution-id EEV4KGOVYCCTD \
        --paths '/*'
}

function main {
    deploy-website-to-s3
    invlidate-cache
    deploy-home-to-s3
}

main
