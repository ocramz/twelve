FROM debian:buster
LABEL maintainer="Marco Zocca"

ENV DEBIAN_FRONTEND noninteractive

# Install required system packages
RUN apt-get update -y  && \
  apt-get install -y --no-install-recommends \
  ca-certificates \
  curl \
  libtinfo-dev \
  libgmp3-dev

# RECENT STACK
RUN curl -sSL https://get.haskellstack.org/ | sh
RUN chmod 755 /usr/local/bin/stack


CMD ["bash"]
