
# Image Categoriser

An Elm-based image categorization tool that integrates with Immich photo management system. Features keyboard-driven navigation, real-time image/video viewing, and efficient album management.

## Development Setup

### Prerequisites

Install devbox for NixOS environment management:

```sh
# Install devbox if not already installed
curl -fsSL https://get.jetpack.io/devbox | bash
```

### Quick Start

1. **Enter development environment:**
   ```sh
   devbox shell
   ```

2. **Start secure development server:**
   ```sh
   npm run dev
   ```

   This starts a secure development server on `http://localhost:8000` with:
   - 🔒 Environment variables injected in-memory only (never written to files)
   - 🔥 Hot reloading via elm-live
   - 📱 Real-time Elm compilation

3. **Environment Configuration:**
   Create a `.env` file in the project root:
   ```env
   IMMICH_URL=https://your-immich-instance.com/api
   IMMICH_API_KEY=your-api-key-here
   ```

### Build Commands

- `npm run dev` - Secure development server with hot reloading
- `npm run build` - Production build (no secrets embedded)
- `npm run start` - Alternative development server launcher

### Security Features

- ✅ **Zero secret persistence**: API keys never written to files
- ✅ **In-memory injection**: Environment variables loaded at request time
- ✅ **Clean builds**: Production artifacts contain no embedded secrets
- ✅ **Development safety**: Shows first 5 characters of API key for verification

## Docker & Production Deployment

### 🐳 Docker Setup

**Quick Start with Docker:**
```bash
# 1. Copy environment template
cp .env.example .env

# 2. Edit .env with your Immich settings
# IMMICH_API_KEY=your_api_key_here
# IMMICH_URL=https://your-immich-server.com

# 3. Run production build
docker-compose -f docker-compose.prod.yaml up --build
```

**Development with Docker:**
```bash
# Development environment with hot reloading
docker-compose up --build
```

### ❄️ Nix Flake Support

**Development Environment:**
```bash
# Enter development shell with all dependencies
nix develop

# Or use direnv for automatic environment loading
echo "use flake" > .envrc && direnv allow
```

**Production Build:**
```bash
# Build production package
nix build

# Run production server
nix run
```

### 🚀 Home Server Deployment

**Option 1: Docker Compose (Recommended)**
```yaml
# docker-compose.prod.yaml
services:
  image-categorizer:
    image: image-categorizer:latest
    ports:
      - "8000:8000"
    environment:
      - IMMICH_URL=https://your-immich.com
      - IMMICH_API_KEY=your_api_key
    restart: unless-stopped
```

**Option 2: NixOS Service**
```nix
# configuration.nix
{
  services.image-categorizer = {
    enable = true;
    port = 8000;
    immichUrl = "https://your-immich.com";
    immichApiKey = "your_api_key"; # Consider using secrets
  };
}
```

**Option 3: Standalone Binary**
```bash
# Build and run directly
nix run github:user/image-categorizer
```

### 🔐 Production Security

- **Multi-stage Docker builds** - Optimized production images
- **Non-root containers** - Enhanced security
- **Health checks** - Automatic service monitoring
- **Security headers** - CSP, HSTS, XSS protection
- **Resource limits** - Memory and CPU constraints
- **Read-only filesystem** - Immutable production environment

### 📊 Monitoring & Health Checks

Health check endpoint: `http://localhost:8000/health`

```json
{
  "status": "healthy",
  "timestamp": "2024-01-01T00:00:00.000Z",
  "version": "0.1.0",
  "environment": {
    "nodeVersion": "v18.17.0",
    "immichUrl": "https://your-immich.com",
    "hasApiKey": true
  }
}
```

## Architecture

- **Frontend**: Elm with functional programming patterns
- **Development**: Secure Node.js proxy server + elm-live  
- **Production**: Lightweight Node.js static server with environment injection
- **Environment**: NixOS with devbox for reproducible builds
- **Containerization**: Multi-stage Docker builds with security hardening
- **Security**: Zero-persistence environment variable handling
