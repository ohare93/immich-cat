# Example NixOS configuration for Image Categorizer
# Add this to your NixOS configuration.nix or as a separate module

{ config, pkgs, lib, ... }:

{
  # Import the Image Categorizer flake
  imports = [
    # If using flakes, you would add this to your flake inputs:
    # inputs.image-categorizer.url = "github:yourusername/image-categoriser";
    # Then use: inputs.image-categorizer.nixosModules.image-categorizer
  ];

  # Option 1: Using environment variables directly (less secure)
  services.image-categorizer = {
    enable = true;
    port = 8000;
    immichUrl = "https://immich.example.com";
    immichApiKey = "your-api-key-here"; # Not recommended for production
    openFirewall = true; # Open port 8000 in the firewall
  };

  # Option 2: Using a secret file (more secure)
  services.image-categorizer = {
    enable = true;
    port = 8000;
    immichUrl = "https://immich.example.com";
    immichApiKeyFile = "/run/secrets/immich-api-key";
    openFirewall = true;
  };

  # Option 3: Using an environment file
  services.image-categorizer = {
    enable = true;
    environmentFile = "/run/secrets/image-categorizer.env";
    # The env file should contain:
    # IMMICH_URL=https://immich.example.com
    # IMMICH_API_KEY=your-api-key-here
    # PORT=8000 (optional, defaults to 8000)
    openFirewall = true;
  };

  # Option 4: Using agenix or sops-nix for secrets management
  # With agenix:
  age.secrets.immich-api-key = {
    file = ../secrets/immich-api-key.age;
    owner = "image-categorizer";
    group = "image-categorizer";
  };
  
  services.image-categorizer = {
    enable = true;
    port = 8000;
    immichUrl = "https://immich.example.com";
    immichApiKeyFile = config.age.secrets.immich-api-key.path;
    openFirewall = true;
  };

  # Option 5: Using systemd credentials (NixOS 23.05+)
  systemd.services.image-categorizer.serviceConfig = {
    LoadCredentialEncrypted = "api-key:/path/to/encrypted/api-key";
  };
  
  services.image-categorizer = {
    enable = true;
    port = 8000;
    immichUrl = "https://immich.example.com";
    immichApiKeyFile = "%d/api-key"; # systemd credentials directory
    openFirewall = true;
  };

  # Additional configuration options
  services.image-categorizer = {
    enable = true;
    port = 8000;
    immichUrl = "https://immich.example.com";
    immichApiKeyFile = "/run/secrets/immich-api-key";
    
    # Custom user/group (optional)
    user = "immich-cat";
    group = "immich-cat";
    
    # Firewall configuration
    openFirewall = true;
  };

  # If you need to create the secret file manually
  # (for testing only - use proper secrets management in production)
  systemd.tmpfiles.rules = [
    "f /run/secrets/immich-api-key 0600 image-categorizer image-categorizer - your-api-key-here"
  ];

  # Nginx reverse proxy example
  services.nginx = {
    enable = true;
    virtualHosts."categorizer.example.com" = {
      forceSSL = true;
      enableACME = true;
      locations."/" = {
        proxyPass = "http://localhost:8000";
        proxyWebsockets = true;
        extraConfig = ''
          proxy_set_header Host $host;
          proxy_set_header X-Real-IP $remote_addr;
          proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
          proxy_set_header X-Forwarded-Proto $scheme;
        '';
      };
    };
  };
}