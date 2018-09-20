# TODO: Use a smaller image.
FROM debian:jessie

# Install system dependencies
RUN apt-get update && \
    apt-get -y install \
        # For building
        emacs24 \
        # For aws-cli
        python python-pip && \
    pip install awscli

# Create a user. No need to run as root.
RUN useradd -ms /bin/bash babel
USER babel
WORKDIR /home/babel

# Install org-mode and other Emacs packages that I need.
ADD dependencies.el dependencies.el
RUN emacs --batch --load dependencies.el
