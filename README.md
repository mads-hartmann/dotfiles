# Hartmann's dotfiles

## Bootstrapping a new Mac

```sh
sh -c "$(curl -fsSL https://raw.githubusercontent.com/mads-hartmann/dotfiles/master/scripts/bootstrap.sh)"
```

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

See `.circleci/config.yml`.

## Resources

Org-mode configuration guide:
https://orgmode.org/manual/Configuration.html
https://orgmode.org/worg/org-tutorials/org-publish-html-tutorial.html
