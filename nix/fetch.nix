builtins.mapAttrs (_: spec: with spec;
  builtins.fetchTarball {
    inherit sha256;
    url = "https://github.com/${owner}/${repo}/archive/${rev}.tar.gz";
  }
) (builtins.fromJSON (builtins.readFile ./versions.json))
