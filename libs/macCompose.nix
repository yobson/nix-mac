{ lib, pkgs, config, home, ... }:
with lib;

let 
  cfg = config.macCompose;
  test = {
    "^1" = "¹";
    "^2" = "²";
  };
  expand = rec {
    f = (attrs: lib.attrsets.foldlAttrs g {} attrs);
    expand = key: conf: toString key (f conf);
    g = acc: name: v: if (builtins.stringLength name == 1)
                then let nn = clean name; in lib.attrsets.recursiveUpdate acc { "${nn}" = v; }
                else let
                  h = clean (builtins.substring 0 1 name);
                  t = builtins.substring 1 (builtins.stringLength name - 1) name;
                  r = f {"${t}" = v; };
                in lib.attrsets.recursiveUpdate acc { ${h} = r; };
    clean = str: if str == "^" then
                  "\\\\^"
                 else if str == "~" then
                  "\\\\~"
                 else
                  str;
    toString = (key: attrs: lib.strings.concatStrings ["{\"${key}\" = " (foo 2 attrs) "}"]);
    mkindent = i: if i == 0 then "" else " " + (mkindent (i - 1));
    foo = indent: as: 
      if builtins.isString as
        then "(\"insertText:\", \"${as}\");\n"
        else lib.strings.concatStrings
          [ "{\n"
          (lib.strings.concatStrings (lib.attrsets.mapAttrsToList (c: v: 
             lib.strings.concatStrings
             [ (mkindent indent) "\"${c}\" = "
               (foo (indent + 2) v)
             ]) as))
             (mkindent (indent - 2))
            "};\n"
          ];
  }.expand;
in {
  options.macCompose = {
    enable = mkEnableOption "Write Compose File";
    mapping = mkOption {
      type = types.attrsOf types.str;
      default = {};
    };
    composeKey = mkOption {
      type = types.str;
      default = "§";
    };
  };
  
  config = mkIf cfg.enable {
    home.file."Library/KeyBindings/DefaultKeyBindingNix.dict" = {
      text = expand cfg.composeKey cfg.mapping;
      onChange = ''
        rm -f ${config.home.homeDirectory}/Library/KeyBindings/DefaultKeyBinding.dict
        cp ${config.home.homeDirectory}/Library/KeyBindings/DefaultKeyBindingNix.dict ${config.home.homeDirectory}/Library/KeyBindings/DefaultKeyBinding.dict
        chmod 764 ${config.home.homeDirectory}/Library/KeyBindings/DefaultKeyBinding.dict
      '';
    };
  };
}


