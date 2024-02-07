FROM node:14

ENV PURESCRIPT_DOWNLOAD_SHA1 0dd80d29388eee68ba0ced9ffd002124e10768e9

RUN apt-get update && apt-get install -y libsecret-1-dev \
    && rm -rf /var/lib/apt/lists/*

RUN npm install --unsafe-perm=true --allow-root -g \
    bower parcel-bundler pulp@15.0.0

RUN mkdir -p /opt/bin && cd /opt \
    && wget https://github.com/purescript/purescript/releases/download/v0.15.13/linux64.tar.gz \
    && echo "$PURESCRIPT_DOWNLOAD_SHA1 linux64.tar.gz" | sha1sum -c - \
    && tar -xvf linux64.tar.gz \
    && rm /opt/linux64.tar.gz \
    && wget https://github.com/purescript/spago-legacy/releases/download/0.21.0/Linux.tar.gz \
    && tar -xvf Linux.tar.gz -C bin/ \
    && rm /opt/Linux.tar.gz

ENV PATH /opt/bin:/opt/purescript:/opt/psc-package:$PATH

RUN echo "#!/usr/bin/env bash\n\$@\n" > /opt/entrypoint && \
    chmod a+x /opt/entrypoint

ENTRYPOINT ["/opt/entrypoint"]