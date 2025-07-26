# Image Categoriser

A keyboard-driven image categorization tool built with Elm that integrates with the [Immich](https://immich.app/) photo management system. Provides an efficient interface for categorizing and managing large photo collections with vim-style navigation.

## Features

- **Keyboard-driven interface** - Navigate with vim-style keybindings
- **Real-time image/video viewing** - Authenticated asset loading from Immich
- **Smart album management** - Categorize assets with automatic count tracking  
- **Intelligent keybinding system** - Conflict-free album shortcuts with fuzzy matching
- **Advanced search** - Fuzzy search for albums and smart asset filtering
- **Batch operations** - Bulk favorites, archiving, and album management
- **Asset preloading** - Intelligent caching for smooth navigation
- **External integration** - Open assets directly in Immich web interface

## Prerequisites

- [Immich](https://immich.app/) server running and accessible
- Immich API key with appropriate permissions
- [Elm](https://elm-lang.org/) 0.19.1
- [Node.js](https://nodejs.org/) (for development server)

## Quick Start

1. **Clone the repository**
   ```bash
   git clone <repository-url>
   cd image-categoriser
   ```

2. **Set up environment variables**
   ```bash
   cp .env.example .env
   # Edit .env with your Immich server details
   ```

3. **Install dependencies and start development server**
   ```bash
   npm install
   npm run dev
   ```

4. **Open your browser**
   Navigate to `http://localhost:3000`

## Configuration

Create a `.env` file in the project root:

```env
IMMICH_URL=https://your-immich-server.com
IMMICH_API_KEY=your_immich_api_key_here
```

### Getting an Immich API Key

1. Log into your Immich web interface
2. Go to Account Settings → API Keys
3. Create a new API key
4. Copy the key to your `.env` file

## Development

### Commands

```bash
# Compile the Elm application
elm make src/Main.elm --output dist/index.html

# Run tests
elm-test

# Start development server
npm run dev

# Build for production
npm run build
```

### Testing

The project includes comprehensive test coverage:

```bash
# Run all tests
elm-test

# Run tests with specific seed for reproducible results
elm-test --seed 12345
```

## Architecture

### Frontend (Elm)

- **Entry Point:** `src/Main.elm` - Main application using Elm Architecture
- **API Integration:** `src/Immich.elm` - HTTP request builders for Immich API
- **Utilities:** `src/Helpers.elm` - Common utility functions
- **Key Bindings:** `src/KeybindingGenerator.elm` - Smart keybinding generation
- **UI Framework:** elm-ui for layout and styling

### Key Architecture Patterns

- **Elm Architecture** - Predictable state management with Model-View-Update
- **Generic HTTP builders** - Centralized request handling
- **Custom web components** - Authenticated asset loading with caching
- **Smart keybinding system** - Conflict-free shortcuts with prefix detection

### Security Features

- **Zero secret persistence** - API keys never written to disk
- **In-memory injection** - Environment variables loaded at runtime
- **Clean builds** - Production artifacts contain no embedded secrets
- **Secure development** - API key masking in development UI

## Usage

### Navigation Modes

- **Normal Mode** - Default navigation mode with vim-style keybindings
- **Insert Mode** - Text input for search and album creation
- **Scroll View Mode** - For viewing large images that require scrolling

### Key Bindings

#### Navigation
- `h/j/k/l` - Navigate left/down/up/right
- `g/G` - Go to first/last item
- `Ctrl+u/d` - Half page up/down
- `Ctrl+f/b` - Full page up/down

#### Asset Management
- `F` - Toggle favorite
- `D` - Toggle delete/archive
- `K` - Open in Immich (new tab)
- `Y` - Yank (copy to clipboard)
- `R` - Reload albums from server

#### Search and Albums
- `I` - Enter album search mode
- `i` - Insert mode for text input
- `?` - Show help

## File Structure

```
src/
├── Main.elm                    # Application entry point
├── Immich.elm                 # API integration
├── Helpers.elm                # Utility functions
├── KeybindingGenerator.elm    # Keybinding system
├── UpdateAlbums.elm           # Album update logic
├── UpdateAsset.elm            # Asset update logic
├── UpdateMenus.elm            # Menu update logic
├── ViewGrid.elm               # Grid view components
├── HelpText.elm               # Help text components
└── index.html                 # HTML with custom web components

tests/
├── ImmichTest.elm            # Unit tests for API module
└── ApiIntegrationTest.elm    # Integration tests

scripts/
├── dev-server.js             # Development server
├── start-dev.js              # Server launcher
└── build.js                  # Production build

docs/
└── immich-api/               # API documentation and examples
```

## Contributing

1. Fork the repository
2. Create a feature branch
3. Add tests for new functionality
4. Ensure all tests pass: `elm-test`
5. Submit a pull request

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## Related Projects

- [Immich](https://immich.app/) - Self-hosted photo and video management solution
- [Elm](https://elm-lang.org/) - Functional programming language for web applications