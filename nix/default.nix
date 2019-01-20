{ fetch ? import ./fetch.nix, nixpkgs ? fetch.nixpkgs }:

import nixpkgs {
  overlays = [ (import ./overlay.nix fetch) ];
}
