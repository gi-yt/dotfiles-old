{ pkgs, ... }:

{
  home.packages = [
    pkgs.firefox
    pkgs.pandoc
    pkgs.qutebrowser
    pkgs.lxsession
    pkgs.pavucontrol
    pkgs.starship
    pkgs.rofi
    pkgs.youtube-dl
    pkgs.qbittorrent
    pkgs.rclone
    pkgs.tdesktop
    pkgs.cmatrix
    pkgs.nodejs
    pkgs.gimp
    pkgs.lolcat
    pkgs.rnix-lsp
    pkgs.ncdu
    pkgs.font-awesome
    pkgs.trayer
    pkgs.fortune
  ];
  programs.emacs = {
    enable = true;
    extraPackages = epkgs: [
      epkgs.nix-mode
      epkgs.vterm
    ];
  };


  services.gpg-agent = {
    enable = true;
    defaultCacheTtl = 1800;
    enableSshSupport = true;
  };

  programs.home-manager = {
    enable = true;
    path = "…";
  };
    programs.git = {
    enable = true;
    userName  = "Arya Kiran";
    userEmail = "geniusinfomedia@protonmail.com";
    ignores = [ "*~" "*.swp" ];
};
    services.flameshot.enable = true;
    xsession.pointerCursor = {
      package = pkgs.bibata-cursors;
      name = "Bibata Original Ice";
    };
    xdg = {
      enable = true;
    };
}
