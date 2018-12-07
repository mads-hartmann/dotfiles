# TODO: Use a smaller image.
# TODO: Add git/ssh
FROM debian:jessie

# Install system dependencies
RUN apt-get update && \
    apt-get -y install \
        zip \
        git \
        emacs24 \
        python python-pip && \
    pip install awscli

# Create a user. No need to run as root.
RUN useradd -ms /bin/bash babel
USER babel
WORKDIR /home/babel

# Install org-mode and other Emacs packages that I need.
ADD dependencies.el dependencies.el
RUN emacs --batch --load dependencies.el
