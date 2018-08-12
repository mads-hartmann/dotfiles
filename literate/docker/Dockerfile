FROM debian:jessie

# Install dependencies - just Emacs in this case.
RUN apt-get update && apt-get -y install emacs24

# Create a user. No need to run as root.
RUN useradd -ms /bin/bash babel
USER babel
WORKDIR /home/babel

# Install org-mode and other Emacs packages that I need.
ADD dependencies.el dependencies.el
RUN emacs --batch --load dependencies.el
