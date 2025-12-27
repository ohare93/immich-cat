# Deployment Guide

This guide covers installation and configuration of Immich Cat. For usage instructions, see [README.md](README.md).

## Quick Start

### Docker (Recommended)

**Pull and run the latest published image:**

```bash
# Create environment file
cat > .env << EOF
IMMICH_URL=https://your-immich-server.com
IMMICH_API_KEY=your_immich_api_key_here
EOF

# Run the application
docker run -p 8000:8000 --env-file .env ghcr.io/ohare93/immich-cat:latest
```

**Or with docker-compose:**

```yaml
# docker-compose.yml
version: "3.8"
services:
  immich-cat:
    image: ghcr.io/ohare93/immich-cat:latest
    ports:
      - "8000:8000"
    env_file:
      - .env
```

Then run: `docker-compose up`

Open `http://localhost:8000` in your browser.

### Nix/NixOS Installation

**Install and run with Nix flakes:**

```bash
# Run directly (will prompt for configuration)
nix run github:yourusername/image-categoriser

# Run with environment file
nix run github:yourusername/image-categoriser -- --env-file .env

# Install to your profile
nix profile install github:yourusername/image-categoriser
```

**NixOS system configuration:**

```nix
# In your flake.nix inputs
image-categorizer.url = "github:yourusername/image-categoriser";

# In your NixOS configuration
services.image-categorizer = {
  enable = true;
  port = 8000;
  immichUrl = "https://immich.example.com";
  environmentFile = "/run/secrets/image-categorizer.env";
  openFirewall = true;
};
```

See [examples/](examples/) for detailed NixOS and home-manager configurations.

### Development Setup

**For local development or building from source:**

```bash
# Clone the repository
git clone https://github.com/ohare93/immich-cat.git
cd immich-cat

# Set up environment
cp .env.example .env
# Edit .env with your Immich server details

# Option 1: Using Nix (recommended)
nix develop  # Enter development shell
npm run dev

# Option 2: Using devbox
devbox shell
npm run dev

# Option 3: Using Docker Compose (build locally)
docker-compose up --build
```

## Configuration

Set up your Immich connection in `.env`:

```env
IMMICH_URL=https://your-immich-server.com
IMMICH_API_KEY=your_immich_api_key_here
```

Get your API key from Immich web interface → Account Settings → API Keys.

## CORS Configuration

To allow this webapp to access your Immich server's API, you need to configure CORS (Cross-Origin Resource Sharing) on your **Immich server** (not immich-cat). The configuration depends on your reverse proxy setup.

### Traefik Configuration

Add these labels to your **Immich server** container:

```yaml
labels:
  # Your existing router configuration
  - traefik.http.routers.immich.entryPoints=https
  - traefik.http.routers.immich.rule=Host(`photos.example.com`)
  - traefik.http.routers.immich.middlewares=immich-cors

  # CORS middleware - use regex for multiple origins
  - traefik.http.middlewares.immich-cors.headers.accessControlAllowOriginListRegex=^https?://(localhost:8000|immich-cat\.example\.com)$
  - traefik.http.middlewares.immich-cors.headers.accessControlAllowMethods=GET,PUT,POST,DELETE,OPTIONS
  - traefik.http.middlewares.immich-cors.headers.accessControlAllowHeaders=X-Api-Key,User-Agent,Content-Type
  - traefik.http.middlewares.immich-cors.headers.accessControlMaxAge=1728000
  - traefik.http.middlewares.immich-cors.headers.accessControlAllowCredentials=true

  # Private Network Access header (required for local DNS resolution)
  - traefik.http.middlewares.immich-cors.headers.customResponseHeaders.Access-Control-Allow-Private-Network=true

  - traefik.http.services.immich.loadbalancer.server.port=2283
```

> **Important:** Replace `photos.example.com` with your Immich domain and `immich-cat.example.com` with where you host this webapp.

### Authentication Middleware (Authelia, Authentik, etc.)

Authentication middleware can intercept OPTIONS preflight requests before CORS headers are added, causing CORS failures. Add a separate router to bypass auth for OPTIONS requests:

```yaml
labels:
  # Main router with auth
  - traefik.http.routers.immich.middlewares=my-geoblock@file,auth@file,immich-cors

  # OPTIONS-specific router bypasses auth for CORS preflight
  - traefik.http.routers.immich-options.entryPoints=https
  - traefik.http.routers.immich-options.rule=Host(`photos.example.com`) && Method(`OPTIONS`)
  - traefik.http.routers.immich-options.middlewares=immich-cors
  - traefik.http.routers.immich-options.service=immich
  - traefik.http.routers.immich-options.priority=100
```

### Private Network Access (PNA)

If you use local DNS resolution (Pi-hole, AdGuard, etc.) that resolves your Immich domain to a private IP (192.168.x.x), Chrome-based browsers enforce Private Network Access security. The browser sends `Access-Control-Request-Private-Network: true` in preflight requests, and the server must respond with `Access-Control-Allow-Private-Network: true`.

This is already included in the configuration above:
```yaml
- traefik.http.middlewares.immich-cors.headers.customResponseHeaders.Access-Control-Allow-Private-Network=true
```

### Multiple Origins (Development + Production)

Use `accessControlAllowOriginListRegex` instead of `accessControlAllowOriginList` when you need multiple origins. The list variant has a bug where it returns the first origin instead of matching the request:

```yaml
# DON'T: Returns wrong origin for some requests
- traefik.http.middlewares.immich-cors.headers.accessControlAllowOriginList=http://localhost:8000,https://immich-cat.example.com

# DO: Properly matches request origin
- traefik.http.middlewares.immich-cors.headers.accessControlAllowOriginListRegex=^https?://(localhost:8000|immich-cat\.example\.com)$
```

### Troubleshooting CORS Issues

1. **Clear browser cache** - CORS preflight responses are cached (up to 20 days with the config above). After changing CORS settings, clear your browser cache or use incognito mode.

2. **Check the Network tab** - Look at the preflight OPTIONS request and verify response headers include:
   - `Access-Control-Allow-Origin` matching your webapp's origin
   - `Access-Control-Allow-Private-Network: true` (if using local DNS)

3. **Works in Firefox but not Chrome?** - Chrome is stricter about Private Network Access. Ensure the PNA header is present.

4. **After deploying changes** - Restart your Immich container and clear browser cache before testing.
