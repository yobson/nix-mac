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
    expand = key: conf: if pkgs.stdenv.isDarwin 
      then toString key (f conf)
      else toStringLinux conf;
    g = acc: name: v: if (builtins.stringLength name == 1)
      then let nn = clean name; in lib.attrsets.recursiveUpdate acc { "${nn}" = v; }
    else let
        h = clean (builtins.substring 0 1 name);
        t = builtins.substring 1 (builtins.stringLength name - 1) name;
        r = f {"${t}" = v; };
      in lib.attrsets.recursiveUpdate acc { ${h} = r; };
    clean = if pkgs.stdenv.isDarwin then cleanMac else cleanLinux;
    cleanMac = str: if str == "^" then
      "\\\\^"
    else if str == "~" then
      "\\\\~"
    else
      str;
    cleanLinux = c:
      if c == "=" then "equal"
      else if c == "+" then "plus"
      else if c == "-" then "minus"
      else if c == "_" then "underscore"
      else if c == "<" then "less"
      else if c == ">" then "greater"
      else if c == "," then "comma"
      else if c == "." then "period"
      else if c == "/" then "slash"
      else if c == "\\" then "backslash"
      else if c == ":" then "colon"
      else if c == ";" then "semicolon"
      else if c == "'" then "apostrophe"
      else if c == "\"" then "quotedbl"
      else if c == "[" then "bracketleft"
      else if c == "]" then "bracketright"
      else if c == "{" then "braceleft"
      else if c == "}" then "braceright"
      else if c == "!" then "exclam"
      else if c == "?" then "question"
      else if c == "@" then "at"
      else if c == "#" then "numbersign"
      else if c == "$" then "dollar"
      else if c == "%" then "percent"
      else if c == "&" then "ampersand"
      else if c == "*" then "asterisk"
      else if c == "(" then "parenleft"
      else if c == ")" then "parenright"
      else if c == "~" then "asciitilde"
      else if c == "`" then "grave"
      else "${c}";

    toString = (key: attrs: lib.strings.concatStrings ["{\"${key}\" = " (foo 2 attrs) "}"]);
    toStringLinux = lib.attrsets.foldlAttrs (acc: name: value:
      strings.concatStrings [acc "<Multi_key> " (genKeyLinux name) ": \"${value}\"\n"]
    ) "";
    genKeyLinux = key: strings.concatStrings
      (map (c : "<${cleanLinux c}> ") (stringToCharacters key));
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
    home.file = if pkgs.stdenv.isDarwin then {
      "Library/KeyBindings/DefaultKeyBindingNix.dict" = {
        text = expand cfg.composeKey cfg.mapping;
        onChange = ''
          rm -f ${config.home.homeDirectory}/Library/KeyBindings/DefaultKeyBinding.dict
          cp ${config.home.homeDirectory}/Library/KeyBindings/DefaultKeyBindingNix.dict ${config.home.homeDirectory}/Library/KeyBindings/DefaultKeyBinding.dict
          chmod 764 ${config.home.homeDirectory}/Library/KeyBindings/DefaultKeyBinding.dict
        '';
      };
    } else {
      ".XCompose" = {
        text = expand cfg.composeKey cfg.mapping;
      };
    };
  };
}


