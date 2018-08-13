# Hartmanns Dotfiles

Writing files `make build-files`
Writing the docs `make build-website`

## TODO

To finish the proof of concept

- [ ] Finish CI setup (it's still broken)
- [ ] Does deploy remove files that are no longer there?
- [ ] Set up stow or similar thing to handle symlinks
  This guide seems to be very simple to follow: https://alexpearce.me/2016/02/managing-dotfiles-with-stow/

Org-mode configuration guide:
https://orgmode.org/manual/Configuration.html
https://orgmode.org/worg/org-tutorials/org-publish-html-tutorial.html

Might be able to steal some things from these dotfiles as well.
https://github.com/alexpearce/dotfiles

## Debugging build image

The export it performed by a Docker image that contains Emacs and the relevant
Emacs packages (see `docker/Dockerfile`).

Sometimes it can be useful debug the build image. Run the following to start a
Bash session in the Docker image with the relevant folders mounted.

```sh
make shell
```
- *TODO*: See if there's more to steal [here](https://github.com/binarin/docker-org-export/blob/master/Dockerfile)

## CI

See `.travis.yml`.
