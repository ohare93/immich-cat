# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

This is an image categorization tool built with Elm that integrates with the Immich photo management system. It provides a keyboard-driven interface for efficiently categorizing and managing large photo collections.

## Development Commands

### Starting Development Server
```bash
npm run dev
```
Starts a secure development server on port 8000 with hot reloading. Environment variables are injected in-memory only (never written to files).

### Building for Production
```bash
npm run build
```
Compiles the Elm application to JavaScript in the `dist/` directory. Produces clean builds with no embedded secrets.

### Development Environment Setup
Requires devbox (NixOS environment management):
```bash
devbox shell  # Enter development environment
npm run dev   # Start secure development server
```

## Architecture

### Frontend (Elm)
- **Entry Point:** `src/Main.elm` - Main application using Elm Architecture (Model-View-Update)
- **API Integration:** `src/Immich.elm` - Handles communication with Immich photo management API
- **Utilities:** `src/Helpers.elm` - Common utility functions
- **UI Framework:** elm-ui for layout and styling

### Key Features
- Keyboard-driven navigation (Normal/Insert modes similar to Vim)
- Real-time image/video viewing with custom web components
- Album management and asset categorization
- Fuzzy search for albums and assets
- Batch operations for favorites, archives, and deletions
- Asset preloading and caching system

### Custom Web Components
The application uses custom elements defined in `src/index.html`:
- `image-from-api` - Handles authenticated image loading from Immich with preloading
- `video-from-api` - Handles authenticated video playback with selective preloading
- Both create blob URLs for secure asset access and implement caching

### Environment Configuration (Secure)
- **Development:** Custom Node.js proxy server injects environment variables in-memory only
- **Security:** API keys never written to any files on disk
- **Runtime injection:** Environment variables passed via `window.ENV` at request time
- **Configuration:** Development environment variables loaded from `.env` file

### Development Security Features
- ✅ Zero secret persistence - API keys stay in memory only
- ✅ In-memory injection - Variables loaded at HTTP request time
- ✅ Clean builds - Production artifacts contain no embedded secrets
- ✅ Development safety - Shows first 5 characters of API key for verification

## File Structure

```
src/
├── Main.elm              # Application entry point
├── Immich.elm           # Immich API integration
├── Helpers.elm          # Utility functions
└── index.html           # HTML with secure environment variable handling

scripts/
├── dev-server.js        # Secure development server with in-memory env injection
├── start-dev.js         # Development server launcher
└── build.js             # Production build script (clean, no secrets)

dist/                    # Compiled JavaScript output
docs/                    # API documentation
devbox.json             # NixOS environment configuration
```

## Development Notes

- The application uses Elm 0.19.1 with functional programming patterns
- Hot reloading provided by elm-live proxied through secure development server
- All API communication goes through the Immich.elm module
- UI state is managed through the Elm Architecture pattern
- Custom web components handle authenticated asset loading with blob URL caching
- Environment variables handled securely - never persisted to files during development