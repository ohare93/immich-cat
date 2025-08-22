# Example of how to use Image Categorizer in your system flake

{
  description = "My NixOS configuration with Image Categorizer";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    
    # Add Image Categorizer as an input
    image-categorizer = {
      url = "github:yourusername/image-categoriser";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, home-manager, image-categorizer, ... }:
  let
    system = "x86_64-linux"; # adjust for your system
  in {
    # NixOS configuration
    nixosConfigurations.my-host = nixpkgs.lib.nixosSystem {
      inherit system;
      modules = [
        # Your hardware configuration
        ./hardware-configuration.nix
        
        # Import the Image Categorizer module
        image-categorizer.nixosModules.image-categorizer
        
        # Your configuration
        ({ config, pkgs, ... }: {
          # Enable Image Categorizer service
          services.image-categorizer = {
            enable = true;
            port = 8000;
            immichUrl = "https://immich.example.com";
            environmentFile = "/run/secrets/image-categorizer.env";
            openFirewall = true;
          };
          
          # Rest of your system configuration
        })
      ];
    };

    # Home Manager configuration
    homeConfigurations.my-user = home-manager.lib.homeManagerConfiguration {
      pkgs = nixpkgs.legacyPackages.${system};
      modules = [
        ({ config, pkgs, ... }: {
          # Install Image Categorizer for user
          home.packages = [
            image-categorizer.packages.${system}.default
          ];
          
          # Configure it
          home.file.".config/image-categorizer/.env".text = ''
            IMMICH_URL=https://immich.example.com
            IMMICH_API_KEY=your-api-key-here
          '';
          
          # Create an alias
          programs.bash.shellAliases = {
            immich-cat = "image-categorizer --env-file ~/.config/image-categorizer/.env";
          };
        })
      ];
    };

    # Direct usage examples
    # Run directly from the flake:
    # nix run github:yourusername/image-categoriser -- --env-file .env
    
    # Enter development shell:
    # nix develop github:yourusername/image-categoriser
    
    # Build the package:
    # nix build github:yourusername/image-categoriser
  };
}