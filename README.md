
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

## Architecture

- **Frontend**: Elm with functional programming patterns
- **Development**: Secure Node.js proxy server + elm-live
- **Environment**: NixOS with devbox for reproducible builds
- **Security**: Zero-persistence environment variable handling
