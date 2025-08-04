# Immich Cat

A keyboard-driven image categorization tool built with Elm that integrates with [Immich](https://immich.app/). Efficiently categorize and manage large photo collections with vim-style navigation.

[![ko-fi](https://ko-fi.com/img/githubbutton_sm.svg)](https://ko-fi.com/S6S81LJQ7)

## Features

- **Keyboard-driven interface** - Vim-style navigation and shortcuts
- **Real-time viewing** - Authenticated image/video loading from Immich
- **Smart album management** - Categorize assets with automatic count tracking
- **Intelligent keybindings** - Conflict-free album shortcuts with fuzzy matching
- **Advanced search** - Fuzzy search and filtering capabilities
- **Batch operations** - Bulk favorites, archiving, and album management
- **Asset preloading** - Intelligent caching for smooth navigation

## Demo

See Immich Cat in action - this 1-minute demo shows the core workflow of efficiently categorizing photos with single-keypress album assignment:

https://github.com/user-attachments/assets/99238b3a-b2aa-4b6e-ac43-89fb06f3976b

**What you'll see in the demo:**

- Loading albums from Immich and navigating through photos with vim-style keys
- Album keybindings displayed on screen (like `a` for "Animals", `f` for "Family")
- Single-keypress photo categorization - just press a letter to add the current photo to that album
- Search mode for finding albums not shown on screen
- The speed difference: traditional web interfaces require multiple clicks per photo, here it's one keypress per album

## Quick Start

### 🚀 Docker (Fastest - Recommended)

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

### 🛠️ Development Setup

**For local development or building from source:**

```bash
# Clone the repository
git clone https://github.com/your-username/immich-cat.git
cd immich-cat

# Set up environment
cp .env.example .env
# Edit .env with your Immich server details

# Option 1: Using devbox (recommended for development)
devbox shell
npm run dev

# Option 2: Using Docker Compose (build locally)
docker-compose up --build
```

## Configuration

Set up your Immich connection in `.env`:

```env
IMMICH_URL=https://your-immich-server.com
IMMICH_API_KEY=your_immich_api_key_here
```

Get your API key from Immich web interface → Account Settings → API Keys.

### CORS Configuration

To allow this webapp to access your Immich server's API, you need to configure CORS (Cross-Origin Resource Sharing) settings. If you're running Immich behind Traefik, add these middleware labels to your Immich service:

```yaml
- traefik.http.routers.immich.middlewares=immich-cors
- traefik.http.middlewares.immich-cors.headers.accessControlAllowOriginList=http://localhost:8000
- traefik.http.middlewares.immich-cors.headers.accessControlAllowMethods=GET, PUT, POST, DELETE, OPTIONS
- traefik.http.middlewares.immich-cors.headers.accessControlAllowHeaders=X-Api-Key, User-Agent, Content-Type
- traefik.http.middlewares.immich-cors.headers.accessControlMaxAge=1728000
```

> **Note**: Adjust `http://localhost:8000` to match where you're hosting this webapp. For other reverse proxy setups, configure equivalent CORS headers to allow requests from your webapp's domain.

## How it Works

The app connects to your existing Immich server and loads all your albums and assets. You don't need to recreate anything - it works with your current photo organization.

**The key advantage:** In Immich's web interface, adding a photo to an album requires multiple clicks (select photo → add to album → search/select album → confirm). Here, it's **just one keypress** per album thanks to automatically generated keybindings.

> 💡 **See it in action:** Watch the [demo above](#demo) to see exactly how this works!

## Usage

### Getting Started

1. After starting the app, it will load your Immich albums and assets
2. Use vim-style navigation to browse through your photos
3. See your top albums with their auto-generated keybindings displayed on screen (like `a` for "Animals", `v` for "Vacation 2024")
4. Navigate to any photo and press the album's key to instantly add it (e.g., press `a` to add current photo to "Animals")
5. Press `?` anytime to see all available shortcuts

> 📺 **New to the workflow?** Check out the [demo video](#demo) for a visual walkthrough!

### Quick Album Assignment (Main Feature)

The app automatically generates single-key shortcuts for your most-used albums and **displays them on screen at all times**:

- `a` might be "Animals"
- `v` might be "Vacation 2024"
- `f` might be "Family"
- And so on...

Simply navigate to any photo and press the letter - **that's it!** No menus, no clicking, no searching. The keybindings are always visible.

### Interface Modes

- **Normal Mode** (default): Navigate and manage assets with keyboard shortcuts (top album keybindings always visible)
- **Insert Mode**: Search for albums not shown on screen, or create new albums
- **Scroll View Mode**: For viewing large images that need scrolling

### Key Bindings

#### Navigation

- `h/j/k/l` - Move left/down/up/right (vim-style)
- `g/G` - Jump to first/last item
- `Ctrl+u/d` - Page up/down
- `Ctrl+f/b` - Full page forward/back

#### Asset Management

- `F` - Toggle favorite status
- `D` - Toggle archive/delete
- `K` - Open current asset in Immich web interface
- `Y` - Copy asset information
- `R` - Reload albums from server

#### Album Operations

- `a-z` (displayed on screen) - **Instantly add current photo to specific album**
- `I` - Search for albums not visible on screen, or create new albums
- `i` - Insert mode for text input
- `Esc` - Return to normal mode
- `?` - Show help with all current keybindings

> **Example:** With "Animals" = `a` visible on screen, just press `a` while viewing any photo to add it to Animals album. Use `I` only if you need an album that's not displayed.

## License

MIT License - see [LICENSE](LICENSE) file for details.
