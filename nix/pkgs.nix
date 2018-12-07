# let rev = "153d32cd9fac7e885979426b0e86b560a661a1ac";
let rev = "61deecdc34fc609d0f805b434101f3c8ae3b807a";
in  import (builtins.fetchTarball { url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz"; }) {}
