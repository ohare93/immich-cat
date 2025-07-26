FROM jetpackio/devbox:latest

# Installing your devbox project
WORKDIR /app
USER root:root
RUN mkdir -p /app && chown ${DEVBOX_USER}:${DEVBOX_USER} /app
USER ${DEVBOX_USER}:${DEVBOX_USER}

# Copy package.json first to prevent devbox init_hook from failing
COPY --chown=${DEVBOX_USER}:${DEVBOX_USER} package.json package-lock.json ./

# Copy devbox configuration
COPY --chown=${DEVBOX_USER}:${DEVBOX_USER} devbox.json devbox.json
COPY --chown=${DEVBOX_USER}:${DEVBOX_USER} devbox.lock devbox.lock

# Install devbox packages and npm dependencies
RUN devbox run -- echo "Packages installed" && nix-store --gc

# Copy source code
COPY --chown=${DEVBOX_USER}:${DEVBOX_USER} src/ src/
COPY --chown=${DEVBOX_USER}:${DEVBOX_USER} elm.json elm.json
COPY --chown=${DEVBOX_USER}:${DEVBOX_USER} production-server.js production-server.js

# Build the application
RUN devbox run -- elm make src/Main.elm --output=dist/main.js --optimize

# Expose port
EXPOSE 8000

# Start the production server directly
CMD ["devbox", "run", "--", "node", "production-server.js"]
