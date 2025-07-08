# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

This is an image categorization tool built with Elm that integrates with the Immich photo management system. It provides a keyboard-driven interface for efficiently categorizing and managing large photo collections.

## Development Commands

### Starting Development Server
```bash
npm run start-env
```
This creates environment-specific files and starts the development server with hot reloading on port 8000.

### Building for Production
```bash
npm run build
```
Compiles the Elm application to JavaScript in the `dist/` directory.

### Running with Docker
```bash
docker compose up -d --remove-orphans --build
```
Starts the complete application stack in containers.

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

### Custom Web Components
The application uses custom elements defined in `src/index.html.template`:
- `image-from-api` - Handles authenticated image loading from Immich
- `video-from-api` - Handles authenticated video playback
- Both create blob URLs for secure asset access

### Environment Configuration
- Uses `envsub` to inject environment variables into HTML template
- Requires Immich API URL and authentication configuration
- Development environment variables are loaded from `.env` file

## File Structure

```
src/
├── Main.elm              # Application entry point
├── Immich.elm           # Immich API integration
├── Helpers.elm          # Utility functions
└── index.html.template  # HTML template with custom elements

dist/                    # Compiled JavaScript output
docs/                    # API documentation
```

## Development Notes

- The application uses Elm 0.19.1 with functional programming patterns
- Hot reloading is available during development via elm-live
- All API communication goes through the Immich.elm module
- UI state is managed through the Elm Architecture pattern
- Custom web components handle authenticated asset loading from Immich API