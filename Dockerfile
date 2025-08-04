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

# Install devbox packages with platform-specific handling
RUN set -e && \
    echo "Building for platform: $(uname -m)" && \
    if [ "$(uname -m)" = "aarch64" ]; then \
        echo "ARM64 detected - using alternative installation method"; \
        export DEVBOX_NO_PROMPT=true; \
        devbox install --quiet; \
    else \
        echo "x86_64 detected - using standard installation"; \
        devbox install; \
    fi && \
    echo "Testing devbox environment..." && \
    devbox run -- elm --version && \
    devbox run -- node --version && \
    echo "Installing npm dependencies..." && \
    devbox run -- npm ci --only=production && \
    echo "Cleaning up..." && \
    nix-store --gc || echo "GC cleanup skipped"

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
