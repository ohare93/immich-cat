# Example home-manager configuration for Image Categorizer
# This allows you to run Image Categorizer as a user service

{ config, pkgs, lib, ... }:

let
  # Import the flake
  # You would add this to your flake inputs:
  # inputs.image-categorizer.url = "github:yourusername/image-categoriser";
  # Then use: inputs.image-categorizer.packages.${pkgs.system}.default
  image-categorizer = pkgs.image-categorizer; # placeholder
in
{
  # Option 1: Install the package and run manually
  home.packages = [ image-categorizer ];

  # Create a .env file in your home directory
  home.file.".config/image-categorizer/.env".text = ''
    IMMICH_URL=https://immich.example.com
    IMMICH_API_KEY=your-api-key-here
  '';

  # Option 2: Create a systemd user service
  systemd.user.services.image-categorizer = {
    Unit = {
      Description = "Image Categorizer - Immich photo categorization tool";
      After = [ "network.target" ];
    };

    Service = {
      Type = "simple";
      ExecStart = "${image-categorizer}/bin/image-categorizer --env-file %h/.config/image-categorizer/.env";
      Restart = "on-failure";
      RestartSec = "10s";
      
      # Environment variables (if not using .env file)
      Environment = [
        "PORT=8000"
        "NODE_ENV=production"
      ];
    };

    Install = {
      WantedBy = [ "default.target" ];
    };
  };

  # Option 3: Create convenient shell aliases
  programs.bash.shellAliases = {
    immich-cat = "image-categorizer --env-file ~/.config/image-categorizer/.env";
    immich-cat-dev = "cd ~/Development/image-categoriser && nix develop -c npm run dev";
  };

  programs.zsh.shellAliases = {
    immich-cat = "image-categorizer --env-file ~/.config/image-categorizer/.env";
    immich-cat-dev = "cd ~/Development/image-categoriser && nix develop -c npm run dev";
  };

  # Option 4: Create a desktop entry for GUI users
  xdg.desktopEntries.image-categorizer = {
    name = "Image Categorizer";
    comment = "Keyboard-driven image categorization for Immich";
    exec = "${image-categorizer}/bin/image-categorizer --env-file %h/.config/image-categorizer/.env";
    terminal = true;
    type = "Application";
    icon = "image-viewer";
    categories = [ "Graphics" "Photography" ];
  };

  # Option 5: Use with secret management (e.g., pass)
  systemd.user.services.image-categorizer-with-pass = {
    Unit = {
      Description = "Image Categorizer with password store";
      After = [ "network.target" ];
    };

    Service = {
      Type = "simple";
      ExecStartPre = "${pkgs.writeShellScript "load-secrets" ''
        mkdir -p $HOME/.config/image-categorizer
        cat > $HOME/.config/image-categorizer/.env <<EOF
        IMMICH_URL=$(${pkgs.pass}/bin/pass show immich/url)
        IMMICH_API_KEY=$(${pkgs.pass}/bin/pass show immich/api-key)
        EOF
      ''}";
      ExecStart = "${image-categorizer}/bin/image-categorizer --env-file %h/.config/image-categorizer/.env";
      Restart = "on-failure";
      RestartSec = "10s";
    };

    Install = {
      WantedBy = [ "default.target" ];
    };
  };

  # Development setup for contributors
  home.file.".config/image-categorizer/dev.env".text = ''
    IMMICH_URL=http://localhost:2283
    IMMICH_API_KEY=development-api-key
  '';

  # Quick start script
  home.file.".local/bin/immich-categorizer".source = pkgs.writeShellScript "immich-categorizer" ''
    #!/usr/bin/env bash
    set -euo pipefail

    CONFIG_DIR="$HOME/.config/image-categorizer"
    ENV_FILE="$CONFIG_DIR/.env"

    # Check if env file exists
    if [ ! -f "$ENV_FILE" ]; then
      echo "No configuration found. Creating template..."
      mkdir -p "$CONFIG_DIR"
      cat > "$ENV_FILE" <<EOF
    # Image Categorizer Configuration
    # Update these values with your Immich instance details
    IMMICH_URL=https://immich.example.com
    IMMICH_API_KEY=your-api-key-here
    EOF
      echo "Please edit $ENV_FILE with your Immich credentials"
      exit 1
    fi

    # Start the application
    exec ${image-categorizer}/bin/image-categorizer --env-file "$ENV_FILE" "$@"
  '';

  home.file.".local/bin/immich-categorizer".executable = true;
}