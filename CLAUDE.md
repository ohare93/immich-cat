# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

This is an image categorization tool built with Elm that integrates with the Immich photo management system. It provides a keyboard-driven interface for efficiently categorizing and managing large photo collections.

## User Preferences

**Important Guidelines for Claude Code:**

- **Always create tests when possible** - Add comprehensive unit, integration, and fuzz tests for new functionality
- **Play a sound when finished** - Run my `beepcomplete` alias
- **Use direct Elm commands** - Run `elm make` and `elm-test` directly instead of npm scripts (npm commands only work in devbox shell)
- **Do not run the server** - The user will run the server, and usually is already doig so with elm-live
- **Refer to the Immich api docs** - https://immich.app/docs/api/
- **Use the example api return json files** - Located at `docs/immich-api/example-returns`

## Development Commands

### Compilation and Testing (Direct Commands)

```bash
# Compile the Elm application
elm make src/Main.elm --output dist/index.html

# Run all tests
elm-test

# Run tests with specific seed for reproducible results
elm-test --seed 12345
```

## Architecture

### Frontend (Elm)

- **Entry Point:** `src/Main.elm` - Main application using Elm Architecture (Model-View-Update)
- **API Integration:** `src/Immich.elm` - Refactored with generic HTTP request builders and minimal duplication
- **Utilities:** `src/Helpers.elm` - Common utility functions
- **Key Bindings:** `src/KeybindingGenerator.elm` - Smart keybinding generation with conflict resolution
- **UI Framework:** elm-ui for layout and styling

### Key Features

- **Keyboard-driven navigation** - Normal/Insert modes similar to Vim with pagination support
- **Real-time image/video viewing** - Custom web components with authenticated asset loading
- **Album management** - Asset categorization with automatic count tracking
- **Smart keybinding system** - Conflict-free album shortcuts with fuzzy matching
- **Search capabilities** - Fuzzy search for albums and smart asset search
- **Batch operations** - Favorites, archives, deletions with proper state management
- **Asset preloading** - Intelligent caching system for smooth navigation
- **External integration** - 'K' keybinding to open assets in Immich web interface

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
├── Main.elm                    # Application entry point with pagination and state management
├── Immich.elm                 # Refactored API integration with generic HTTP builders
├── Helpers.elm                # Utility functions
├── KeybindingGenerator.elm    # Smart keybinding generation system
└── index.html                 # HTML with secure environment variable handling

tests/
├── ImmichTest.elm            # Unit tests for API module (JSON, URL construction)
└── ApiIntegrationTest.elm    # Integration tests with mock responses

scripts/
├── dev-server.js        # Secure development server with in-memory env injection
├── start-dev.js         # Development server launcher
└── build.js             # Production build script (clean, no secrets)

dist/                    # Compiled JavaScript output
docs/                    # API documentation
devbox.json             # NixOS environment configuration
```

## Development Notes

### Core Technologies

- **Elm 0.19.1** with functional programming patterns and strong typing
- **Hot reloading** provided by elm-live proxied through secure development server
- **Elm Architecture** pattern for predictable state management
- **elm-ui** for declarative layout and styling

### Architecture Patterns

- **Generic HTTP builders** - Centralized request handling with ~70% code reduction
- **PropertyChange tracking** - Optimistic UI updates with backend synchronization
- **Smart keybinding system** - Conflict-free album shortcuts with prefix detection
- **Pagination state management** - Screen-height-aware pagination with vim-style navigation
- **Custom web components** - Authenticated asset loading with intelligent caching

### API Integration

- All HTTP communication centralized in `Immich.elm` with generic request builders
- Comprehensive test coverage with unit, integration, and fuzz tests
- Clean URL construction with proper path joining and query parameter handling
- Album asset count tracking with precise increment/decrement operations

### Security & Environment

- Environment variables handled securely - never persisted to files during development
- Clean base URL vs API URL separation for proper external link generation
- API keys remain in memory only during development
