FROM haskell:8.10.7-slim

RUN apt-get update && \
  apt-get install --assume-yes --no-install-recommends sudo

ARG CABAL_STORE=/cabal-store
RUN mkdir "$CABAL_STORE"
RUN chgrp sudo "$CABAL_STORE"
RUN chmod g+w "$CABAL_STORE"

ARG USER_NAME=haskell
RUN useradd --create-home --groups sudo --shell "$( command -v bash )" "$USER_NAME"
RUN echo "$USER_NAME ALL=(ALL) NOPASSWD: ALL" > "/etc/sudoers.d/$USER_NAME"
WORKDIR "/home/$USER_NAME"

USER "$USER_NAME"
ENV PATH="/home/$USER_NAME/.cabal/bin:$PATH"
RUN cabal user-config init --augment "store-dir: $CABAL_STORE"
