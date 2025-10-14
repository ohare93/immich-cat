{
  description = "Elm Image Categorizer - A keyboard-driven image categorization tool for Immich";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        
        # Environment configuration
        # Override these in your environment or .env file
        defaultImmichUrl = "https://localhost";
        defaultPort = "8000";

        # Read .env file if it exists
        envFile = if builtins.pathExists ./.env then ./.env else null;
        envVars = if envFile != null then
          let
            envContent = builtins.readFile envFile;
            lines = builtins.filter (line: line != "" && !(pkgs.lib.hasPrefix "#" line))
                     (pkgs.lib.splitString "\n" envContent);
            parseEnvLine = line:
              let
                parts = pkgs.lib.splitString "=" line;
                key = builtins.head parts;
                value = builtins.concatStringsSep "=" (builtins.tail parts);
              in
                { name = key; value = pkgs.lib.removeSuffix "\r" value; };
          in
            builtins.listToAttrs (map parseEnvLine lines)
        else {};

        # Node.js packages for development
        nodePackages = pkgs.nodePackages;
        
        # Elm packages
        elmPackages = pkgs.elmPackages;

        # Thumbhash library
        thumbhash = pkgs.fetchurl {
          url = "https://unpkg.com/thumbhash@0.1.1/thumbhash.js";
          sha256 = "sha256-IooSXmCOSVWUDL52TgRjhsmVMibSr2alOAJm4Yt4/8o=";
        };

        # Simple wrapper that launches the local devbox version
        image-categorizer = pkgs.writeShellScriptBin "image-categorizer" ''
          #!/usr/bin/env bash
          set -euo pipefail
          
          # Find the image-categoriser directory (assuming it's cloned in common locations)
          POSSIBLE_PATHS=(
            "$HOME/Development/image-categoriser"
            "$HOME/Projects/image-categoriser"
            "$HOME/Code/image-categoriser"
            "$HOME/image-categoriser"
            "$(pwd)"
          )
          
          IMAGE_CAT_DIR=""
          for path in "''${POSSIBLE_PATHS[@]}"; do
            if [[ -f "$path/devbox.json" ]] && [[ -f "$path/src/Main.elm" ]]; then
              IMAGE_CAT_DIR="$path"
              break
            fi
          done
          
          if [[ -z "$IMAGE_CAT_DIR" ]]; then
            echo "❌ Could not find image-categoriser directory with devbox.json and src/Main.elm"
            echo "Please ensure image-categoriser is cloned to one of these locations:"
            printf '   %s\n' "''${POSSIBLE_PATHS[@]}"
            echo ""
            echo "Or run this command from within the image-categoriser directory."
            exit 1
          fi
          
          echo "📂 Found image-categoriser at: $IMAGE_CAT_DIR"
          cd "$IMAGE_CAT_DIR"
          
          # Parse command line arguments
          ENV_FILE=""
          while [[ $# -gt 0 ]]; do
            case $1 in
              --env-file)
                ENV_FILE="$2"
                shift 2
                ;;
              -h|--help)
                echo "Image Categorizer - A keyboard-driven image categorization tool for Immich"
                echo ""
                echo "Usage: image-categorizer [OPTIONS]"
                echo ""
                echo "Options:"
                echo "  --env-file <path>  Path to .env file containing configuration"
                echo "  -h, --help         Show this help message"
                echo ""
                echo "Environment variables:"
                echo "  IMMICH_API_KEY     Immich API key (required)"
                echo "  IMMICH_URL         Immich server URL (default: https://localhost)"
                echo "  PORT               Server port (default: 8000)"
                echo ""
                echo "Note: This wrapper uses the local devbox development environment."
                exit 0
                ;;
              *)
                echo "Unknown option: $1"
                echo "Use --help for usage information"
                exit 1
                ;;
            esac
          done
          
          # Load environment from file if specified
          if [[ -n "$ENV_FILE" ]] && [[ -f "$ENV_FILE" ]]; then
            echo "📄 Loading environment from: $ENV_FILE"
            set -a
            source "$ENV_FILE"
            set +a
          fi
          
          # Check for .env in current directory as fallback
          if [[ -z "$ENV_FILE" ]] && [[ -f ".env" ]]; then
            echo "📄 Loading environment from: .env"
            set -a
            source ".env"
            set +a
          fi
          
          # Check for environment variables
          if [[ -z "''${IMMICH_API_KEY:-}" ]]; then
            echo "❌ ERROR: IMMICH_API_KEY environment variable is required"
            echo ""
            echo "Please provide your Immich API key in one of these ways:"
            echo "  1. Set environment variable: export IMMICH_API_KEY=your_api_key_here"
            echo "  2. Create a .env file in the current directory"
            echo "  3. Use --env-file option: image-categorizer --env-file /path/to/.env"
            echo ""
            echo "Get your API key from: Immich web interface → Account Settings → API Keys"
            exit 1
          fi
          
          echo "🚀 Starting Image Categorizer with devbox..."
          echo "   IMMICH_URL: ''${IMMICH_URL:-https://localhost}"
          echo "   PORT: ''${PORT:-8000}"
          echo ""
          
          # Ensure we have node_modules
          if [[ ! -d "node_modules" ]]; then
            echo "📦 Installing dependencies..."
            ${pkgs.devbox}/bin/devbox run -- npm install
          fi
          
          # Build if needed
          if [[ ! -f "dist/main.js" ]] || [[ "src/Main.elm" -nt "dist/main.js" ]]; then
            echo "🔨 Building Elm application..."
            ${pkgs.devbox}/bin/devbox run -- elm make src/Main.elm --output=dist/main.js --optimize
          fi
          
          # Start the production server
          echo "🌐 Open http://localhost:''${PORT:-8000} in your browser"
          exec ${pkgs.devbox}/bin/devbox run -- node production-server.js
        '';

        # Also provide a simpler package that just contains the scripts
        image-categorizer-scripts = pkgs.stdenv.mkDerivation rec {
          pname = "image-categorizer-scripts";
          version = "1.0.4";

          src = ./.;

          buildInputs = [ pkgs.dos2unix ];

          buildPhase = ''
            # Fix line endings in scripts
            find . -name "*.js" -type f -exec dos2unix {} \;
          '';

          installPhase = ''
            mkdir -p $out/share/image-categorizer
            
            # Copy source files
            cp -r src/ $out/share/image-categorizer/
            cp production-server.js $out/share/image-categorizer/
            cp package.json $out/share/image-categorizer/
            cp elm.json $out/share/image-categorizer/
            cp devbox.json $out/share/image-categorizer/
            cp devbox.lock $out/share/image-categorizer/
            
            # Copy thumbhash library
            mkdir -p $out/share/image-categorizer/node_modules/thumbhash
            cp ${thumbhash} $out/share/image-categorizer/node_modules/thumbhash/thumbhash.js
            
            # Create wrapper script
            cat > $out/bin/image-categorizer << 'EOF'
            #!/usr/bin/env bash
            set -euo pipefail
            
            # Parse command line arguments
            ENV_FILE=""
            while [[ $# -gt 0 ]]; do
              case $1 in
                --env-file)
                  ENV_FILE="$2"
                  shift 2
                  ;;
                -h|--help)
                  echo "Image Categorizer - A keyboard-driven image categorization tool for Immich"
                  echo ""
                  echo "Usage: image-categorizer [OPTIONS]"
                  echo ""
                  echo "Options:"
                  echo "  --env-file <path>  Path to .env file containing configuration"
                  echo "  -h, --help         Show this help message"
                  echo ""
                  echo "Environment variables:"
                  echo "  IMMICH_API_KEY     Immich API key (required)"
                  echo "  IMMICH_URL         Immich server URL (default: ${defaultImmichUrl})"
                  echo "  PORT               Server port (default: ${defaultPort})"
                  echo ""
                  echo "You can also provide an .env file with these variables."
                  exit 0
                  ;;
                *)
                  echo "Unknown option: $1"
                  echo "Use --help for usage information"
                  exit 1
                  ;;
              esac
            done
            
            # Load environment from file if specified
            if [ -n "$ENV_FILE" ] && [ -f "$ENV_FILE" ]; then
              echo "Loading environment from: $ENV_FILE"
              set -a
              source "$ENV_FILE"
              set +a
            fi
            
            # Check for .env in current directory as fallback
            if [ -z "$ENV_FILE" ] && [ -f ".env" ]; then
              echo "Loading environment from: .env"
              set -a
              source ".env"
              set +a
            fi
            
            export NODE_PATH="${nodePackages.nodejs}/lib/node_modules"
            export IMMICH_URL=''${IMMICH_URL:-"${defaultImmichUrl}"}
            export PORT=''${PORT:-"${defaultPort}"}
            
            if [ -z "''${IMMICH_API_KEY:-}" ]; then
              echo "ERROR: IMMICH_API_KEY environment variable is required"
              echo ""
              echo "Please provide your Immich API key in one of these ways:"
              echo "  1. Set environment variable: export IMMICH_API_KEY=your_api_key_here"
              echo "  2. Create a .env file in the current directory"
              echo "  3. Use --env-file option: image-categorizer --env-file /path/to/.env"
              echo ""
              echo "Get your API key from: Immich web interface → Account Settings → API Keys"
              exit 1
            fi
            
            cd $out/share/image-categorizer
            exec ${pkgs.nodejs}/bin/node production-server.js
            EOF
            
            chmod +x $out/bin/image-categorizer
          '';

          meta = with pkgs.lib; {
            description = "A keyboard-driven image categorization tool for Immich";
            homepage = "https://github.com/user/image-categorizer";
            license = licenses.mit;
            platforms = platforms.unix;
            mainProgram = "image-categorizer";
          };
        };

        # Development shell with hot reloading
        devShell = pkgs.mkShell {
          buildInputs = with pkgs; [
            # Core development tools
            nodejs
            nodePackages.npm
            
            # Elm development tools
            elmPackages.elm
            elmPackages.elm-test
            elmPackages.elm-format
            elmPackages.elm-live
            
            # Docker tools
            docker
            docker-compose
            
            # Development utilities
            curl
            wget
            jq
            
            # Git (for version control)
            git
          ];

          shellHook = ''
            echo "🏠 Image Categorizer Development Environment"
            echo "=========================================="
            echo ""
            
            # Load .env file if it exists
            if [ -f .env ]; then
              echo "📄 Loading environment from .env file..."
              export $(grep -v '^#' .env | xargs)
            else
              echo "⚠️  No .env file found. Copy .env.example to .env and configure:"
              echo "   cp .env.example .env"
              echo ""
            fi
            
            # Check for required environment variables
            if [ -z "''${IMMICH_API_KEY:-}" ]; then
              echo "❌ Missing IMMICH_API_KEY - please set in .env file"
            else
              echo "✅ IMMICH_API_KEY is set"
            fi
            
            if [ -z "''${IMMICH_URL:-}" ]; then
              echo "❌ Missing IMMICH_URL - please set in .env file"
              export IMMICH_URL="${defaultImmichUrl}"
            else
              echo "✅ IMMICH_URL is set to: $IMMICH_URL"
            fi
            
            echo ""
            echo "🚀 Available commands:"
            echo "   npm run dev      - Start development server"
            echo "   npm run build    - Build production version"
            echo "   elm-test         - Run tests"
            echo "   docker-compose up - Run in Docker (development)"
            echo ""
            echo "🐳 Docker commands:"
            echo "   docker-compose -f docker-compose.prod.yaml up --build  - Production build"
            echo ""
            echo "📦 Install dependencies:"
            echo "   npm install"
            echo ""
          '';

          # Set environment variables with fallbacks
          IMMICH_URL = envVars.IMMICH_URL or defaultImmichUrl;
          PORT = envVars.PORT or defaultPort;
          NODE_ENV = "development";
          ELM_HOME = ".elm";
        };

      in {
        # Development shell
        devShells.default = devShell;
        
        # Production package
        packages = {
          default = image-categorizer;
          image-categorizer = image-categorizer;
        };

        # Applications
        apps = {
          default = flake-utils.lib.mkApp {
            drv = image-categorizer;
            name = "image-categorizer";
          };
          
          # Development server
          dev = flake-utils.lib.mkApp {
            drv = pkgs.writeShellScriptBin "dev-server" ''
              export PATH="${pkgs.nodejs}/bin:${nodePackages.npm}/bin:$PATH"
              export NODE_ENV=development
              
              # Load environment variables
              if [ -f .env ]; then
                export $(grep -v '^#' .env | xargs)
              fi
              
              # Check dependencies
              if [ ! -d node_modules ]; then
                echo "Installing npm dependencies..."
                npm install
              fi
              
              echo "Starting development server..."
              npm run dev
            '';
            name = "dev-server";
          };
        };

        # NixOS module for easy deployment
        nixosModules.image-categorizer = { config, lib, pkgs, ... }: {
          options.services.image-categorizer = {
            enable = lib.mkEnableOption "Image Categorizer service";
            
            port = lib.mkOption {
              type = lib.types.port;
              default = 8000;
              description = "Port for the Image Categorizer service";
            };
            
            immichUrl = lib.mkOption {
              type = lib.types.str;
              example = "https://immich.example.com";
              description = "URL of your Immich instance";
            };
            
            immichApiKey = lib.mkOption {
              type = lib.types.nullOr lib.types.str;
              default = null;
              description = "Immich API key (consider using immichApiKeyFile for better security)";
            };
            
            immichApiKeyFile = lib.mkOption {
              type = lib.types.nullOr lib.types.path;
              default = null;
              description = "Path to file containing the Immich API key";
            };
            
            environmentFile = lib.mkOption {
              type = lib.types.nullOr lib.types.path;
              default = null;
              description = "Path to .env file containing environment variables";
              example = "/run/secrets/image-categorizer.env";
            };
            
            user = lib.mkOption {
              type = lib.types.str;
              default = "image-categorizer";
              description = "User to run the service as";
            };
            
            group = lib.mkOption {
              type = lib.types.str;
              default = "image-categorizer";
              description = "Group to run the service as";
            };
            
            openFirewall = lib.mkOption {
              type = lib.types.bool;
              default = false;
              description = "Whether to open the firewall for the specified port";
            };
          };

          config = lib.mkIf config.services.image-categorizer.enable (
            let
              cfg = config.services.image-categorizer;
            in {
            assertions = [
              {
                assertion = (cfg.immichApiKey != null) || (cfg.immichApiKeyFile != null) || (cfg.environmentFile != null);
                message = "services.image-categorizer requires either immichApiKey, immichApiKeyFile, or environmentFile to be set";
              }
            ];

            users.users.${cfg.user} = {
              group = cfg.group;
              isSystemUser = true;
            };

            users.groups.${cfg.group} = {};

            networking.firewall.allowedTCPPorts = lib.mkIf cfg.openFirewall [ cfg.port ];

            systemd.services.image-categorizer = {
              description = "Image Categorizer Service";
              wantedBy = [ "multi-user.target" ];
              after = [ "network.target" ];

              environment = {
                PORT = toString cfg.port;
                IMMICH_URL = cfg.immichUrl;
                NODE_ENV = "production";
              } // lib.optionalAttrs (cfg.immichApiKey != null) {
                IMMICH_API_KEY = cfg.immichApiKey;
              };

              serviceConfig = {
                ExecStart = "${image-categorizer}/bin/image-categorizer";
                User = cfg.user;
                Group = cfg.group;
                Restart = "always";
                RestartSec = 10;
                
                # Load environment from files
                EnvironmentFile = lib.optional (cfg.environmentFile != null) cfg.environmentFile;
                LoadCredential = lib.optional (cfg.immichApiKeyFile != null) "api-key:${cfg.immichApiKeyFile}";
                
                # Set API key from credential if using file
                ExecStartPre = lib.optional (cfg.immichApiKeyFile != null) (
                  pkgs.writeShellScript "load-api-key" ''
                    export IMMICH_API_KEY="$(cat "$CREDENTIALS_DIRECTORY/api-key")"
                  ''
                );
                
                # Security settings
                NoNewPrivileges = true;
                ProtectSystem = "strict";
                ProtectHome = true;
                PrivateTmp = true;
                PrivateDevices = true;
                ProtectHostname = true;
                ProtectClock = true;
                ProtectKernelTunables = true;
                ProtectKernelModules = true;
                ProtectKernelLogs = true;
                ProtectControlGroups = true;
                RestrictAddressFamilies = [ "AF_UNIX" "AF_INET" "AF_INET6" ];
                RestrictNamespaces = true;
                LockPersonality = true;
                MemoryDenyWriteExecute = true;
                RestrictRealtime = true;
                RestrictSUIDSGID = true;
                RemoveIPC = true;
              };
            };
          });
        };
      });
}