FROM debian:bullseye-20250407-slim AS elm

RUN apt-get update \
	&& apt-get install wget unzip zip -y

RUN wget -O - 'https://github.com/elm/compiler/releases/download/0.19.1/binary-for-linux-64-bit.gz' \
	| gunzip -c >/usr/local/bin/elm
RUN chmod +x /usr/local/bin/elm

FROM node:19.9.0 AS dev
COPY --from=elm /usr/local/bin/elm /usr/local/bin/

# NOTE: we need to set HOME, because elm uses $HOME/.elm to save packages.
#       if this doesn't persist between runs, you get CORRUPT BINARY errors.
ENV HOME /mnt

WORKDIR /mnt
VOLUME /mnt
# NOTE: docker-compose mounts code in /mnt

COPY package*.json /mnt/

RUN npm install

CMD ["npm", "run", "start-env"]
