FROM jetpackio/devbox:latest

# Add build platform argument
ARG TARGETPLATFORM

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

# Install packages with platform-specific handling
RUN set -e && \
    echo "Building for target platform: ${TARGETPLATFORM:-unknown}" && \
    echo "Architecture: $(uname -m)" && \
    if [ "$(uname -m)" = "aarch64" ]; then \
        echo "ARM64 detected - using direct nix installation to avoid script issues"; \
        # Install packages directly with nix-env to bypass devbox script issues \
        nix-env -iA nixpkgs.elmPackages.elm nixpkgs.elmPackages.nodejs nixpkgs.elmPackages.elm-test nixpkgs.elmPackages.elm-format nixpkgs.elmPackages.elm-live && \
        echo "ARM64 packages installed directly" && \
        # Install npm dependencies directly \
        npm ci --only=production && \
        echo "npm dependencies installed"; \
    else \
        echo "x86_64 detected - using standard devbox installation"; \
        devbox install && \
        echo "Testing devbox environment..." && \
        devbox run -- elm --version && \
        devbox run -- node --version && \
        echo "Installing npm dependencies..." && \
        devbox run -- npm ci --only=production; \
    fi && \
    echo "Cleaning up..." && \
    nix-store --gc || echo "GC cleanup skipped"

# Copy source code
COPY --chown=${DEVBOX_USER}:${DEVBOX_USER} src/ src/
COPY --chown=${DEVBOX_USER}:${DEVBOX_USER} elm.json elm.json
COPY --chown=${DEVBOX_USER}:${DEVBOX_USER} production-server.js production-server.js

# Build the application with platform-specific commands
RUN set -e && \
    mkdir -p dist && \
    if [ "$(uname -m)" = "aarch64" ]; then \
        echo "ARM64 - building with direct elm command"; \
        elm make src/Main.elm --output=dist/main.js --optimize; \
    else \
        echo "x86_64 - building with devbox"; \
        devbox run -- elm make src/Main.elm --output=dist/main.js --optimize; \
    fi && \
    echo "Build completed successfully"

# Expose port
EXPOSE 8000

# Start the production server with platform-specific commands
CMD if [ "$(uname -m)" = "aarch64" ]; then \
        echo "ARM64 - starting with direct node command" && \
        node production-server.js; \
    else \
        echo "x86_64 - starting with devbox" && \
        devbox run -- node production-server.js; \
    fi
