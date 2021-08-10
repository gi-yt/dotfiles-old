(defun remove-scratch-buffer ()
  (if (get-buffer "*straight-process*")
      (kill-buffer "*straight-process*")))
(add-hook 'after-change-major-mode-hook 'remove-scratch-buffer)

(use-package ivy
  :straight t
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("M-TAB" . ivy-immediate-done)
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))
    (use-package ivy-rich :after counsel
  :straight t
  :init
  (ivy-rich-mode 1))
(use-package counsel
  :straight t
  :bind (("M-x" . counsel-M-x)
         ("<menu>" . counsel-M-x)
         ("C-x b" . counsel-switch-buffer)
         ("C-x C-f" . counsel-find-file)
         :map minibuffer-local-map
   ("C-r" . 'counsel-minibuffer-history))
  :config
  (counsel-mode 1))

(use-package ivy-prescient :after counsel
  :custom
  (ivy-prescient-enable-filtering nil)
  :config
  (prescient-persist-mode 1)
  (ivy-prescient-mode t))

(global-set-key (kbd "s-S-r") 'reload-config)
(defun reload-config ()
  (interactive)
  (load-file (concat user-emacs-directory "init.el")))

;; Always replace encrypted text with plain text version
(setq epa-replace-original-text t)
;; Let Emacs query the passphrase through the minibuffer
(setq epg-pinentry-mode 'loopback)

(use-package goto-addr :straight t
  :hook ((org-mode compilation-mode prog-mode eshell-mode shell-mode) . goto-address-mode)
  :bind (:map goto-address-highlight-keymap
	 ("<RET>" . goto-address-at-point)
	 ("M-<RET>" . newline)))

;; highlight indentations in python
(use-package highlight-indent-guides
  :hook ((python-mode sass-mode yaml-mode nim-mode) . highlight-indent-guides-mode)
  :config
  ;; Don't highlight first level (that would be a line at column 1)
  (defun my-highlighter (level responsive display)
    (if (> 1 level) ; replace `1' with the number of guides you want to hide
        nil
      (highlight-indent-guides--highlighter-default level responsive display)))

  (setq highlight-indent-guides-highlighter-function 'my-highlighter)
  (setq highlight-indent-guides-method 'character)
  (setq highlight-indent-guides-character ?\|)
  (setq highlight-indent-guides-auto-odd-face-perc 15)
  (setq highlight-indent-guides-auto-even-face-perc 15)
  (setq highlight-indent-guides-auto-character-face-perc 20)

  (highlight-indent-guides-auto-set-faces))

(use-package aggressive-indent
  :hook ((emacs-lisp-mode lisp-mode hy-mode clojure-mode css js-mode) . aggressive-indent-mode)
  :config
  ;; Normally this functions from `indent.el' always displays an
  ;; annoying "reporter" message that it's indenting the current region.
  ;; This patch disables that message
  (defun indent-region-line-by-line (start end)
    (save-excursion
      (setq end (copy-marker end))
      (goto-char start)
      (while (< (point) end)
        (or (and (bolp) (eolp))
            (indent-according-to-mode))
        (forward-line 1))
      (move-marker end nil))))

(use-package smartparens
  :defer 1
  :hook ((
          emacs-lisp-mode lisp-mode lisp-data-mode clojure-mode cider-repl-mode hy-mode
          prolog-mode go-mode cc-mode python-mode
          typescript-mode json-mode javascript-mode java-mode
          ) . smartparens-strict-mode)
  ;; :hook (prog-mode . smartparens-strict-mode)
  :bind (:map smartparens-mode-map
         ;; This is the paredit mode map minus a few key bindings
         ;; that I use in other modes (e.g. M-?)
         ("C-M-f" . sp-forward-sexp) ;; navigation
         ("C-M-b" . sp-backward-sexp)
         ("C-M-u" . sp-backward-up-sexp)
         ("C-M-d" . sp-down-sexp)
         ("C-M-p" . sp-backward-down-sexp)
         ("C-M-n" . sp-up-sexp)
         ("C-w" . whole-line-or-region-sp-kill-region)
         ("M-s" . sp-splice-sexp) ;; depth-changing commands
         ("M-r" . sp-splice-sexp-killing-around)
         ("M-(" . sp-wrap-round)
         ("C-)" . sp-forward-slurp-sexp) ;; barf/slurp
         ("C-<right>" . sp-forward-slurp-sexp)
         ("C-}" . sp-forward-barf-sexp)
         ("C-<left>" . sp-forward-barf-sexp)
         ("C-(" . sp-backward-slurp-sexp)
         ("C-M-<left>" . sp-backward-slurp-sexp)
         ("C-{" . sp-backward-barf-sexp)
         ("C-M-<right>" . sp-backward-barf-sexp)
         ("M-S" . sp-split-sexp) ;; misc
         ("M-j" . sp-join-sexp))
  :config
  (require 'smartparens-config)
  (setq sp-base-key-bindings 'paredit)
  (setq sp-autoskip-closing-pair 'always)
  ;; Always highlight matching parens
  (show-smartparens-global-mode +1)
  (setq blink-matching-paren nil)  ;; Don't blink matching parens
  (defun whole-line-or-region-sp-kill-region (prefix)
    "Call `sp-kill-region' on region or PREFIX whole lines."
    (interactive "*p")
    (whole-line-or-region-wrap-beg-end 'sp-kill-region prefix))
  ;; Create keybindings to wrap symbol/region in pairs
  (defun prelude-wrap-with (s)
    "Create a wrapper function for smartparens using S."
    `(lambda (&optional arg)
       (interactive "P")
       (sp-wrap-with-pair ,s)))
  (define-key prog-mode-map (kbd "M-(") (prelude-wrap-with "("))
  (define-key prog-mode-map (kbd "M-[") (prelude-wrap-with "["))
  (define-key prog-mode-map (kbd "M-{") (prelude-wrap-with "{"))
  (define-key prog-mode-map (kbd "M-\"") (prelude-wrap-with "\""))
  (define-key prog-mode-map (kbd "M-'") (prelude-wrap-with "'"))
  (define-key prog-mode-map (kbd "M-`") (prelude-wrap-with "`"))
  ;; smart curly braces
  (sp-pair "{" nil :post-handlers
           '(((lambda (&rest _ignored)
                (crux-smart-open-line-above)) "RET")))
  (sp-pair "[" nil :post-handlers
           '(((lambda (&rest _ignored)
                (crux-smart-open-line-above)) "RET")))
  (sp-pair "(" nil :post-handlers
           '(((lambda (&rest _ignored)
                (crux-smart-open-line-above)) "RET")))
  ;; Don't include semicolon ; when slurping
  (add-to-list 'sp-sexp-suffix '(java-mode regexp ""))
  ;; use smartparens-mode everywhere
  (smartparens-global-mode))

(use-package expand-region
  :straight t
  :bind ("C-q" . er/expand-region)
:defer t)

(setq org-ellipsis "▾")
(defun ak-org-hooks ()
  (require 'org-tempo)
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("py" . "src python"))
  (add-to-list 'org-structure-template-alist '("sh" . "src bash"))
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (python . t)))
     (org-indent-mode 1)
     )
;; (add-hook 'org-mode-hook 'ak-org-hooks)
(use-package org
  :straight nil
  :hook (org-mode . ak-org-hooks))

(use-package org-bullets
:straight t
  :after org
  :hook (org-mode . org-bullets-mode))

(use-package which-key
  :init
  (setq which-key-side-window-location 'bottom
        which-key-sort-order #'which-key-key-order-alpha
        which-key-sort-uppercase-first nil
        which-key-add-column-padding 1
        which-key-max-display-columns nil
        which-key-min-display-lines 6
        which-key-side-window-slot -10
        which-key-side-window-max-height 0.25
        which-key-idle-delay 0.8
        which-key-max-description-length 25
        which-key-allow-imprecise-window-fit t
        which-key-separator " → " ))
(which-key-mode)
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(use-package rainbow-delimiters
:straight t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package all-the-icons :straight t :defer t)
(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :custom
  (doom-modeline-height 25)
  (doom-modeline-bar-width 1)
  (doom-modeline-icon t)
  (doom-modeline-major-mode-icon t)
  (doom-modeline-major-mode-color-icon t)
  (doom-modeline-buffer-file-name-style 'truncate-upto-project)
  (doom-modeline-buffer-state-icon t)
  (doom-modeline-buffer-modification-icon t)
  (doom-modeline-minor-modes nil)
  (doom-modeline-enable-word-count nil)
  (doom-modeline-buffer-encoding t)
  (doom-modeline-indent-info nil)
  (doom-modeline-checker-simple-format t)
  (doom-modeline-vcs-max-length 12)
  (doom-modeline-env-version t)
  (doom-modeline-irc-stylize 'identity)
  (doom-modeline-github-timer nil)
  (doom-modeline-gnus-timer nil))

;; (use-package doom-themes :straight t :init (load-theme 'doom-dracula))
(use-package atom-one-dark-theme :straight t :init (load-theme 'atom-one-dark))

(use-package magit :straight t :defer 0 :commands magit-status :custom  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package goggles)
(goggles-mode)

(use-package esup :defer t)

(defun xah-new-empty-buffer ()
      "Create a new empty buffer.
    New buffer will be named “untitled” or “untitled<2>”, “untitled<3>”, etc.

    It returns the buffer (for elisp programing).

    URL `http://ergoemacs.org/emacs/emacs_new_empty_buffer.html'
    Version 2017-11-01"
      (interactive)
      (let (($buf (generate-new-buffer "untitled")))
        (switch-to-buffer $buf)
        (funcall initial-major-mode)
        (setq buffer-offer-save t)
        $buf
        ))
(defun python-scratch () (interactive) (xah-new-empty-buffer)(python-mode)(company-mode))
(defun emacs-lisp-scratch () (interactive) (xah-new-empty-buffer)(emacs-lisp-mode)(company-mode))
(defun sh-scratch () (interactive) (xah-new-empty-buffer)(sh-mode)(company-mode))
(defun c-scratch () (interactive) (xah-new-empty-buffer)(c-mode)(company-mode))
(defun sh-scratch () (interactive) (xah-new-empty-buffer)(sh-mode)(company-mode))
(defun org-scratch () (interactive) (xah-new-empty-buffer)(org-mode))

(use-package hungry-delete
  :straight t
  :config (global-hungry-delete-mode))

(use-package org-roam ;; Package is on melpa
               :straight t
           :defer t
               :custom
             (make-directory "~/org-roam") ;; The dir all notes are gonna be stored
             (setq org-roam-directory (file-truename "~/org-roam"))
             :bind (("C-c n l" . org-roam-buffer-toggle) ;; Binds
                    ("C-c n f" . org-roam-node-find)
                    ("C-c n g" . org-roam-graph) ;; Graph i was talking about.
                    ("C-c n i" . org-roam-node-insert)
                    ("C-c n c" . org-roam-capture)
                    ;; Dailies
                    ("C-c n j" . org-roam-dailies-capture-today))
             :config
             ;; If using org-roam-protocol
             (require 'org-roam-protocol)
             ;; (use-package org-roam-server) ;; There is server but havent gotten it  to work yet
             ;; (setq org-roam-server-host "127.0.0.1"
             ;;       org-roam-server-port 8080
             ;;       org-roam-server-authenticate t
             ;;       org-roam-server-export-inline-images t
             ;;       org-roam-server-serve-files t
             ;;       org-roam-server-served-file-extensions '("pdf" "mp4" "ogv")
             ;;       org-roam-server-network-poll t
             ;;       org-roam-server-network-arrows nil
             ;;       org-roam-server-network-label-truncate t
             ;;       org-roam-server-network-label-truncate-length 60
             ;;       org-roam-server-network-label-wrap-length 20)
         (add-to-list 'display-buffer-alist
                      '("\\*org-roam\\*"
                        (display-buffer-in-direction)
                        (direction . right)
                        (window-width . 0.33)
                        (window-height . fit-window-to-buffer)))
         (setq org-roam-completion-everywhere t)
         (org-roam-setup))
(setq org-roam-v2-ack t)

(defun create-blog-post ()
  "Create an org file in ~/source/myblog/posts."
  (interactive)
  (let ((name (read-string "Filename: ")))
    (expand-file-name (format "%s.org" name) "~/fossnix/posts")))
(defun blog-publish ()
  (interactive)
  (cd "~/fossnix")
  (async-shell-command "make publish && git add -A && git commit -a -m New && git push"))
(defun blog-post ()
  (interactive)
  "Capture a TODO item"
  (org-capture nil "p"))

(use-package vterm :straight t :defer t)
      (setq vterm-eval-cmds '(("magit-status-setup-buffer" magit-status-setup-buffer)
                          ("find-file" find-file)
                          ("message" message)
                          ("vterm-clear-scrollback" vterm-clear-scrollback)))
;; (setq  vterm-always-compile-module nil)
(use-package multi-vterm :straight t :defer t
    :bind ("s-<return>" . multi-vterm))

;; Behave like vi's o command
(defun open-next-line (arg)
  "Move to the next line and then opens a line.
 See also `newline-and-indent'."
  (interactive "p")
  (end-of-line)
  (open-line arg)
  (forward-line 1)
  (when newline-and-indent
    (indent-according-to-mode)))
(global-set-key (kbd "C-o") 'open-next-line)
;; Behave like vi's O command
(defun open-previous-line (arg)
  "Open a new line before the current one.
 See also `newline-and-indent'."
  (interactive "p")
  (beginning-of-line)
  (open-line arg)
  (when newline-and-indent
    (indent-according-to-mode)))
    (global-set-key (kbd "M-o") 'open-previous-line)
(global-set-key (kbd "C-S-o") 'open-previous-line)
;; Autoindent open-*-lines
(defvar newline-and-indent t
  "Modify the behavior of the open-*-line functions to cause them to autoindent.")
    (global-set-key [S-return]   'open-next-line)
(global-set-key [C-S-return] 'open-previous-line)

(use-package general :straight t)
(global-unset-key (kbd "C-z"))
(general-define-key
 :prefix "C-z"
 "bb" 'ibuffer
 "bk" 'kill-current-buffer
 "bn" 'next-buffer
 "bp" 'previous-buffer
 "bB" 'ibuffer-list-buffers
 "bK" 'kill-buffer
 "eb" 'eval-buffer
 "ed" 'eval-defun
 "ee" 'eval-expression
 "el" 'eval-last-sexp
 "er" 'eval-region
 "ld" 'xref-find-definitions
 "lr" 'xref-find-references
 "ln" 'lsp-ui-find-next-reference
 "lp" 'lsp-ui-find-prev-reference
 "ls" 'counsel-imenu
 "le" 'lsp-ui-flycheck-list
 "lS" 'lsp-ui-sideline-mode
 "lX" 'lsp-execute-code-action
 "sp" 'python-scratch
 "sl" 'emacs-lisp-scratch
 "sc" 'c-scratch
 "so" 'org-scratch
 "ss" 'sh-scratch
 "ds" 'sudo-edit
 "dd" 'counsel-find-file
 "gf" 'epa-encrypt-file
 "gr" 'epa-encrypt-region
 "gme" 'epa-mail-encrypt
 "gmd" 'epa-mail-decrypt
 "gms" 'epa-mail-sign
 "gmv" 'epa-mail-verify
 "gki" 'epa-import-keys
 "gkd" 'epa-delete-keys
 "gkl" 'epa-list-keys
 "rt" 'newsticker-treeview
 "rs" 'newsticker-start
 "ra" 'newsticker-add-url
 "."     '(find-file :which-key "Find file")
 "d r"   '(counsel-recentf :which-key "Recent files")
 "d s"   '(save-buffer :which-key "Save file")
 "d c"   '(copy-file :which-key "Copy file")
 "d D"   '(delete-file :which-key "Delete file")
 "d r"   '(rename-file :which-key "Rename file")
 "f S"   '(write-file :which-key "Save file as...")
 "b n" 'blog-post
 "b p" 'blog-publish
 "SPC" 'counsel-M-x)
(use-package sudo-edit :straight t :defer t) ;; Utilities for opening files with sudo

(use-package undo-fu :straight t)
  (global-set-key (kbd "C-_")   'undo-fu-only-undo)
  (global-set-key (kbd "M-_") 'undo-fu-only-redo)
(use-package undo-fu-session
  :config
  (setq undo-fu-session-incompatible-files '("/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'")))

(global-undo-fu-session-mode)

(use-package evil-nerd-commenter
  :straight t
  :bind ("M-/" . evilnc-comment-or-uncomment-lines))

(use-package super-save
:straight t
  :diminish super-save-mode
  :config
  (super-save-mode +1)
  (setq super-save-auto-save-when-idle t))

(use-package emojify
  :straight t
  :commands emojify-mode)

(use-package 0x0 :straight t :defer t)

(use-package discover-my-major :straight t :defer t :bind (("C-h C-m" . discover-my-major)))

(use-package dired
   :straight nil
:bind (
   :map dired-mode-map
   ("h" . dired-single-buffer-up-directory)
   ("l" . dired-single-buffer)
   ("RET" . dired-single-buffer)
   ("S-RET" . dired-single-buffer)
   ("e" . dired-ediff-files)
   ))
 (setq dired-listing-switches "-agho --group-directories-first")
 (use-package dired-single :after dired)
   (setq wdired-allow-to-change-permissions t)
   (setq wdired-allow-to-redirect-links t)
   (setq wdired-use-interactive-rename nil)

(use-package kdeconnect
  :defer t)

(use-package gif-screencast :defer t :config
  (with-eval-after-load 'gif-screencast
    (define-key gif-screencast-mode-map (kbd "<f8>") 'gif-screencast-toggle-pause)
    (define-key gif-screencast-mode-map (kbd "<f9>") 'gif-screencast-stop))
(global-set-key (kbd "<f9>") 'gif-screencast-start-or-stop))

(use-package telega :defer t)

(use-package edwina
  :config
  (setq display-buffer-base-action '(display-buffer-below-selected))
  (edwina-setup-dwm-keys)
  (edwina-mode 1))

(defun efs/run-in-background (command)
  (let ((command-parts (split-string command "[ ]+")))
    (apply #'call-process `(,(car command-parts) nil 0 nil ,@(cdr command-parts)))))

(defun efs/exwm-init-hook ()
  ;; Make workspace 1 be the one where we land at startup
  (exwm-workspace-switch-create 1)

  ;; Open eshell by default
  ;;(eshell)

  ;; NOTE: The next two are disabled because we now use Polybar!

  ;; Show battery status in the mode line
  ;;(display-battery-mode 1)

  ;; Show the time and date in modeline
  ;;(setq display-time-day-and-date t)
  ;;(display-time-mode 1)
  ;; Also take a look at display-time-format and format-time-string

  ;; Start the Polybar panel
  ;; (efs/start-panel)

  ;; Launch apps that will run in the background
  (efs/run-in-background "dunst")
  (efs/run-in-background "flameshot")
  (efs/run-in-background "pasystray")
  (efs/run-in-background "blueman-applet"))

(defun efs/exwm-update-class ()
  (exwm-workspace-rename-buffer exwm-class-name))

(defun efs/exwm-update-title ()
  (pcase exwm-class-name
    ("Firefox" (exwm-workspace-rename-buffer (format "Firefox: %s" exwm-title)))))

;; This function isn't currently used, only serves as an example how to
;; position a window
(defun efs/position-window ()
  (let* ((pos (frame-position))
         (pos-x (car pos))
          (pos-y (cdr pos)))

    (exwm-floating-move (- pos-x) (- pos-y))))

(defun efs/configure-window-by-class ()
  (interactive)
  (pcase exwm-class-name
    ("Firefox" (exwm-workspace-move-window 2))
    ("Sol" (exwm-workspace-move-window 3))
    ("mpv" (exwm-floating-toggle-floating)
           (exwm-layout-toggle-mode-line))))

(use-package exwm
  :config
  ;; Set the default number of workspaces
  (setq exwm-workspace-number 5)

  ;; When window "class" updates, use it to set the buffer name
  (add-hook 'exwm-update-class-hook #'efs/exwm-update-class)

  ;; When window title updates, use it to set the buffer name
  (add-hook 'exwm-update-title-hook #'efs/exwm-update-title)

  ;; Configure windows as they're created
  (add-hook 'exwm-manage-finish-hook #'efs/configure-window-by-class)

  ;; When EXWM starts up, do some extra confifuration
  (add-hook 'exwm-init-hook #'efs/exwm-init-hook)

  ;; NOTE: Uncomment the following two options if you want window buffers
  ;;       to be available on all workspaces!

  ;; Automatically move EXWM buffer to current workspace when selected
  ;; (setq exwm-layout-show-all-buffers t)

  ;; Display all EXWM buffers in every workspace buffer list
  ;; (setq exwm-workspace-show-all-buffers t)

  ;; NOTE: Uncomment this option if you want to detach the minibuffer!
  ;; Detach the minibuffer (show it with exwm-workspace-toggle-minibuffer)
  ;;(setq exwm-workspace-minibuffer-position 'top)

  ;; This will need to be updated to the name of a display!  You can find
  ;; the names of your displays by looking at arandr or the output of xrandr
  ;; NOTE: Uncomment these lines after setting up autorandr!
  ;; React to display connectivity changes, do initial display update
  ;; (add-hook 'exwm-randr-screen-change-hook #'efs/update-displays)
  ;; (efs/update-displays)

  ;; NOTE: This is disabled because we now use Polybar!
  ;; Load the system tray before exwm-init
  ;; (require 'exwm-systemtray)
  ;; (setq exwm-systemtray-height 32)
  ;; (exwm-systemtray-enable)

  ;; Automatically send the mouse cursor to the selected workspace's display
  (setq exwm-workspace-warp-cursor t)

  ;; Window focus should follow the mouse pointer
  (setq mouse-autoselect-window t
        focus-follows-mouse t)

  ;; These keys should always pass through to Emacs
  (setq exwm-input-prefix-keys
    '(?\C-x
      ?\C-u
      ?\C-h
      ?\M-x
      ?\M-`
      ?\M-&
      ?\M-:
      ?\C-\M-j  ;; Buffer list
      ?\C-\ ))  ;; Ctrl+Space

  ;; Ctrl+Q will enable the next key to be sent directly
  (define-key exwm-mode-map [?\C-q] 'exwm-input-send-next-key)

  ;; Set up global key bindings.  These always work, no matter the input state!
  ;; Keep in mind that changing this list after EXWM initializes has no effect.
  (setq exwm-input-global-keys
        `(
          ;; Reset to line-mode (C-c C-k switches to char-mode via exwm-input-release-keyboard)
          ([?\s-r] . exwm-reset)

          ;; Move between windows
          ([s-left] . windmove-left)
          ([s-right] . windmove-right)
          ([s-up] . windmove-up)
          ([s-down] . windmove-down)

          ;; Launch applications via shell command
          ([?\s-&] . (lambda (command)
                       (interactive (list (read-shell-command "$ ")))
                       (start-process-shell-command command nil command)))

          ;; Switch workspace
          ([?\s-w] . exwm-workspace-switch)
          ([?\s-`] . (lambda () (interactive) (exwm-workspace-switch-create 0)))

          ;; 's-N': Switch to certain workspace with Super (Win) plus a number key (0 - 9)
          ,@(mapcar (lambda (i)
                      `(,(kbd (format "s-%d" i)) .
                        (lambda ()
                          (interactive)
                          (exwm-workspace-switch-create ,i))))
                    (number-sequence 0 9))))

  (exwm-input-set-key (kbd "s-SPC") 'counsel-linux-app)

  (exwm-enable))

(use-package desktop-environment
  :after exwm
  :config (desktop-environment-mode)
  :custom
  (desktop-environment-brightness-small-increment "2%+")
  (desktop-environment-brightness-small-decrement "2%-")
  (desktop-environment-brightness-normal-increment "5%+")
  (desktop-environment-brightness-normal-decrement "5%-"))

;; Make sure the server is started (better to do this in your main Emacs config!)
(server-start)

(defvar efs/polybar-process nil
  "Holds the process of the running Polybar instance, if any")

(defun efs/kill-panel ()
  (interactive)
  (when efs/polybar-process
    (ignore-errors
      (kill-process efs/polybar-process)))
  (setq efs/polybar-process nil))

(defun efs/start-panel ()
  (interactive)
  (efs/kill-panel)
  (setq efs/polybar-process (start-process-shell-command "polybar" nil "polybar panel")))

(defun efs/send-polybar-hook (module-name hook-index)
  (start-process-shell-command "polybar-msg" nil (format "polybar-msg hook %s %s" module-name hook-index)))

(defun efs/send-polybar-exwm-workspace ()
  (efs/send-polybar-hook "exwm-workspace" 1))

;; Update panel indicator when workspace changes
(add-hook 'exwm-workspace-switch-hook #'efs/send-polybar-exwm-workspace)

(use-package eaf
:straight nil
  :load-path "~/.emacs.d/site-lisp/emacs-application-framework" ; Set to "/usr/share/emacs/site-lisp/eaf" if installed from AUR
  :init
  (use-package epc :defer t :ensure t)
  (use-package ctable :defer t :ensure t)
  (use-package deferred :defer t :ensure t)
  (use-package s :defer t :ensure t)
  :custom
  (eaf-browser-continue-where-left-off t)
  :config
  (setq eaf-browser-enable-adblocker t)
  (eaf-bind-key scroll_up "C-n" eaf-pdf-viewer-keybinding)
  (eaf-bind-key scroll_down "C-p" eaf-pdf-viewer-keybinding)
  (eaf-bind-key take_photo "p" eaf-camera-keybinding)
  (eaf-bind-key nil "M-q" eaf-browser-keybinding)) ;; unbind, see more in the Wiki

(global-set-key (kbd "<f1>") (lambda() (interactive)(find-file "~/.emacs.d/config.org")))

(if (daemonp)
    (message "Loading in the daemon!")
  (message "Loading in regular Emacs!"))
(setq doom-modeline-icon t)

(use-package alert :defer t
  :config
  ;; send alerts by default to D-Bus
  (setq alert-default-style 'libnotify))
