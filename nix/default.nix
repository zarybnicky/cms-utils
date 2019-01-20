{ nixpkgs ? (import ./fetch.nix).nixpkgs }:

import nixpkgs {
  overlays = [ (import ./overlay.nix) ];
}
