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

        # Production build derivation
        image-categorizer = pkgs.stdenv.mkDerivation rec {
          pname = "image-categorizer";
          version = "0.1.0";

          src = ./.;

          buildInputs = with pkgs; [
            nodejs
            elmPackages.elm
            elmPackages.elm-test
          ];

          buildPhase = ''
            export HOME=$TMPDIR
            export ELM_HOME=$TMPDIR/.elm
            
            # Install npm dependencies
            npm ci --production --no-audit
            
            # Build Elm application
            elm make src/Main.elm --output=dist/main.js --optimize
          '';

          installPhase = ''
            mkdir -p $out/bin $out/share/image-categorizer
            
            # Copy built assets
            cp -r dist/ $out/share/image-categorizer/
            cp -r node_modules/thumbhash/ $out/share/image-categorizer/
            cp src/index.html $out/share/image-categorizer/
            
            # Create wrapper script
            cat > $out/bin/image-categorizer << 'EOF'
            #!/usr/bin/env bash
            set -euo pipefail
            
            export NODE_PATH="${nodePackages.nodejs}/lib/node_modules"
            export IMMICH_URL=''${IMMICH_URL:-"${defaultImmichUrl}"}
            export PORT=''${PORT:-"${defaultPort}"}
            
            if [ -z "''${IMMICH_API_KEY:-}" ]; then
              echo "ERROR: IMMICH_API_KEY environment variable is required"
              echo "Please set your Immich API key:"
              echo "  export IMMICH_API_KEY=your_api_key_here"
              exit 1
            fi
            
            cd $out/share/image-categorizer
            exec ${pkgs.nodejs}/bin/node - << 'INNER_EOF'
              ${builtins.readFile ./production-server.js}
            INNER_EOF
            EOF
            
            chmod +x $out/bin/image-categorizer
          '';

          meta = with pkgs.lib; {
            description = "A keyboard-driven image categorization tool for Immich";
            homepage = "https://github.com/user/image-categorizer";
            license = licenses.mit;
            platforms = platforms.unix;
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
              type = lib.types.str;
              description = "Immich API key (consider using secrets)";
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
          };

          config = lib.mkIf config.services.image-categorizer.enable {
            users.users.${config.services.image-categorizer.user} = {
              group = config.services.image-categorizer.group;
              isSystemUser = true;
            };

            users.groups.${config.services.image-categorizer.group} = {};

            systemd.services.image-categorizer = {
              description = "Image Categorizer Service";
              wantedBy = [ "multi-user.target" ];
              after = [ "network.target" ];

              environment = {
                PORT = toString config.services.image-categorizer.port;
                IMMICH_URL = config.services.image-categorizer.immichUrl;
                IMMICH_API_KEY = config.services.image-categorizer.immichApiKey;
                NODE_ENV = "production";
              };

              serviceConfig = {
                ExecStart = "${image-categorizer}/bin/image-categorizer";
                User = config.services.image-categorizer.user;
                Group = config.services.image-categorizer.group;
                Restart = "always";
                RestartSec = 10;
                
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
              };
            };
          };
        };
      });
}