# Hartmanns Dotfiles

Writing files `make build-files`
Writing the docs `make build-website`

## TODO

To finish the proof of concept

- [ ] Does deploy remove files that are no longer there?
- [ ] Set up stow or similar thing to handle symlinks
  This guide seems to be very simple to follow: https://alexpearce.me/2016/02/managing-dotfiles-with-stow/

Org-mode configuration guide:
https://orgmode.org/manual/Configuration.html
https://orgmode.org/worg/org-tutorials/org-publish-html-tutorial.html

Might be able to steal some things from these dotfiles as well.
https://github.com/alexpearce/dotfiles

# CI

I've created a Docker image that contains Emacs and the relevant packages.
See `docker/Dockerfile`.

The rest is taken care of by Travis. See `.travis.yml`.

## Debugging CI image

Sometimes it can be useful to debug the CI image. The following will start a
Bash session in the Docker image with the relevant folders mounted.

```sh
make shell
```
