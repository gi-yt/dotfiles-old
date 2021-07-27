
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("88f59acbeacefb4998f45126d4d8ae8b2184f2a48753db362a349fd55321c7e1" "7a7b1d475b42c1a0b61f3b1d1225dd249ffa1abb1b7f726aec59ac7ca3bf4dae" "6c386d159853b0ee6695b45e64f598ed45bd67c47f671f69100817d7db64724d" default))
 '(highlight-indent-guides-character 124)
 '(keycast-log-mode nil)
 '(newsticker-url-list
   '(("GNEWSLINUX" "https://news.google.com/rss/topics/CAAqJggKIiBDQkFTRWdvSkwyMHZNR1p3ZW5wd0VnVmxiaTFIUWlnQVAB" nil nil nil)
     ("Hacker News" "https://hnrss.org/frontpage" nil nil nil)
     ("Tech Republic" "https://www.techrepublic.com/rssfeeds/articles/" nil nil nil)
     ("ZDNet" "https://www.zdnet.com/news/rss.xml" nil nil nil)
     ("TechRights" "http://techrights.org/feed/" nil nil nil)
     ("LWN" "https://lwn.net/headlines/rss" nil nil nil)
     ("TechRepublic Premium" "https://www.techrepublic.com/rssfeeds/premium/" nil nil nil)
     ("OpenSource.com" "https://opensource.com/feed" nil nil nil)
     ("FOSSMint" "https://www.fossmint.com/feed/" nil nil nil)
     ("Tecmint" "https://www.tecmint.com/feed/" nil nil nil)
     ("FOSSLinux" "https://www.fosslinux.com/feed" nil nil nil)
     ("NixCraft" "https://www.cyberciti.biz/atom/atom.xml" nil nil nil)
     ("ItsFoss" "https://itsfoss.com/feed" nil nil nil)
     ("Tech Crunch" "https://feeds.feedburner.com/TechCrunch/" nil nil nil)
     ("ArsTechnica" "http://feeds.arstechnica.com/arstechnica/technology-lab" nil nil nil)
     ("Luke Smooth" "https://lukesmith.xyz/rss.xml" nil nil nil)
     ("Bryan Lunduke" "https://www.lunduke.com/feed/" nil nil nil)
     ("dev.to" "https://dev.to/feed" nil nil nil)
     ("Linux.com" "https://linux.com/feed" nil nil nil)
     ("Linux Hint" "https://linuxhint.com/feed/" nil nil nil)
     ("DW Headlines" "https://distrowatch.com/news/news-headlines.xml" nil nil nil)
     ("OSTechnix" "https://ostechnix.com/feed/" nil nil nil)
     ("L4E" "https://feeds.fireside.fm/linuxforeveryone/rss" nil nil nil)
     ("Linux Journal" "https://www.linuxjournal.com/node/feed" nil nil nil)
     ("Techradar" "https://www.techradar.com/in/rss/news/computing" nil nil nil)
     ("Arya" "https://aryak.codeberg.page/index.xml" nil nil nil)
     ("CommandLine Heroes" "https://feeds.pacific-content.com/commandlineheroes" nil nil nil)))
 '(newsticker-url-list-defaults
   '(("The Register" "https://www.theregister.co.uk/headlines.rss")
     ("Wired News" "https://www.wired.com/feed/rss")))
 '(org-modules
   '(ol-bbdb ol-bibtex ol-docview ol-eww ol-gnus ol-info ol-irc ol-mhe ol-rmail ol-w3m org-toc))
 '(package-selected-packages
   '(scanner fzf org-roam-server ox-reveal pdf-continuous-scroll-mode quelpa multi-vterm diminish org-roam expand-region hungry-delete fancy-battery mu4e-alert dired-single daemons zerodark-theme spell-fu package-lint hackernews elfeed-goodies elfeed mu4e-dashboard org-mime mu4e-marker-icons company lsp-mode yasnippet pdf-tools lsp-rust cargo skewer-mode impatient-mode web-mode scheme-mode slime sly rust-mode lispyville lispy haskell-mode mu4e-views org-preview-html smex dired generic-x yasnippet-snippets which-key vterm use-package unicode-fonts undo-tree undo-fu-session toc-org telega super-save sudo-edit smartparens rainbow-mode rainbow-delimiters pyvenv python-mode py-autopep8 peep-dired pdf-view-restore org-bullets no-littering nix-mode multiple-cursors magit lsp-ui lsp-treemacs lsp-ivy keycast ivy-rich highlight-indent-guides hide-mode-line general flycheck evil-nerd-commenter evil-collection emojify doom-themes doom-modeline discover-my-major dired-open dired-hide-dotfiles dashboard counsel company-shell company-quickhelp company-nixos-options company-box centaur-tabs all-the-icons-ivy all-the-icons-dired alert aggressive-indent 0x0))
 '(sml/mode-width (if (eq (powerline-current-separator) 'arrow) 'right 'full))
 '(sml/pos-id-separator
   '(""
     (:propertize " " face powerline-active1)
     (:eval
      (propertize " " 'display
                  (funcall
                   (intern
                    (format "powerline-%s-%s"
                            (powerline-current-separator)
                            (car powerline-default-separator-dir)))
                   'powerline-active1 'powerline-active2)))
     (:propertize " " face powerline-active2)))
 '(sml/pos-minor-modes-separator
   '(""
     (:propertize " " face powerline-active1)
     (:eval
      (propertize " " 'display
                  (funcall
                   (intern
                    (format "powerline-%s-%s"
                            (powerline-current-separator)
                            (cdr powerline-default-separator-dir)))
                   'powerline-active1 'sml/global)))
     (:propertize " " face sml/global)))
 '(sml/pre-id-separator
   '(""
     (:propertize " " face sml/global)
     (:eval
      (propertize " " 'display
                  (funcall
                   (intern
                    (format "powerline-%s-%s"
                            (powerline-current-separator)
                            (car powerline-default-separator-dir)))
                   'sml/global 'powerline-active1)))
     (:propertize " " face powerline-active1)))
 '(sml/pre-minor-modes-separator
   '(""
     (:propertize " " face powerline-active2)
     (:eval
      (propertize " " 'display
                  (funcall
                   (intern
                    (format "powerline-%s-%s"
                            (powerline-current-separator)
                            (cdr powerline-default-separator-dir)))
                   'powerline-active2 'powerline-active1)))
     (:propertize " " face powerline-active1)))
 '(sml/pre-modes-separator (propertize " " 'face 'sml/modes))
 '(telega-use-images t)
 '(user-full-name "Arya Kiran")
 '(user-mail-address "aryakiran@zohomail.eu"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(org-babel-load-file
 (expand-file-name
  "emacs-config.org"
  user-emacs-directory))
