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

(global-set-key (kbd "s-r") 'reload-config)
(defun reload-config ()
  (interactive)
  (load-file (concat user-emacs-directory "init.el")))

(use-package goto-addr :straight t
  :hook ((org-mode compilation-mode prog-mode eshell-mode shell-mode) . goto-address-mode)
  :bind (:map goto-address-highlight-keymap
	 ("<RET>" . goto-address-at-point)
	 ("M-<RET>" . newline)))

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
(setq org-hide-emphasis-markers t)
               (org-babel-do-load-languages
                'org-babel-load-languages
                '((emacs-lisp . t)
                  (python . t)))
                  (org-indent-mode 1)
                  )
        (defun up-n-fold ()
          (interactive)
           (progn
             (outline-previous-visible-heading 1)
             (org-cycle)))
             ;; (add-hook 'org-mode-hook 'ak-org-hooks)
             (use-package org
               :straight nil
               :bind (:map org-mode-map
   ("<C-tab>" . up-n-fold)
                      )
               :hook (org-mode . ak-org-hooks))

(use-package org-bullets
:straight t
  :after org
  :hook (org-mode . org-bullets-mode))

(use-package toc-org :defer t
:hook (org-mode . toc-org-mode)
    )

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

(if (display-graphic-p)
    (use-package atom-one-dark-theme :straight t :init (load-theme 'atom-one-dark))
  (load-theme 'tsdh-dark))

(use-package magit :straight t :defer t :commands magit-status :custom  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package goggles
:config
(setq-default goggles-pulse t)
(goggles-mode))

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
             (add-to-list 'display-buffer-alist
                      '("\\*org-roam\\*"
                        (display-buffer-in-direction)
                        (direction . right)
                        (window-width . 0.33)
                        (window-height . fit-window-to-buffer)))
         (setq org-roam-completion-everywhere t)
         (org-roam-setup))
(setq org-roam-v2-ack t)

(setq org-capture-templates
        '(("p" "Post" plain
                (file create-blog-post)
                (file "~/website/org-templates/post.orgcaptmpl"))))
    (defun create-blog-post ()
      "Create an org file in ~/source/myblog/posts."
      (interactive)
      (let ((name (read-string "Filename: ")))
        (expand-file-name (format "%s.org" name) "~/website/posts")))
    (defun blog-publish ()
      (interactive)
      (cd "~/website")
      (async-shell-command "make publish")
      (magit-status))
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

(use-package telega :defer t)

(use-package org-auto-tangle
  :defer t
  :hook (org-mode . org-auto-tangle-mode))

;; make cursor movement keys under right hand's home-row.
  (global-set-key (kbd "C-i ") 'previous-line)
  (global-set-key (kbd "C-j") 'backward-char)
  (global-set-key (kbd "C-k") 'next-line)
  (global-set-key (kbd "C-l") 'forward-char)

  (global-set-key (kbd "M-u") 'backward-word)
  (global-set-key (kbd "M-o") 'forward-word)



  (defun xah-cut-line-or-region ()
  "Cut current line, or text selection.
When `universal-argument' is called first, cut whole buffer (respects `narrow-to-region').

URL `http://ergoemacs.org/emacs/emacs_copy_cut_current_line.html'
Version 2015-06-10"
  (interactive)
  (if current-prefix-arg
      (progn ; not using kill-region because we don't want to include previous kill
        (kill-new (buffer-string))
        (delete-region (point-min) (point-max)))
    (progn (if (use-region-p)
               (kill-region (region-beginning) (region-end) t)
             (kill-region (line-beginning-position) (line-beginning-position 2))))))
(defun xah-copy-line-or-region ()
  "Copy current line, or text selection.
When called repeatedly, append copy subsequent lines.
When `universal-argument' is called first, copy whole buffer (respects `narrow-to-region').

URL `http://ergoemacs.org/emacs/emacs_copy_cut_current_line.html'
Version 2018-09-10"
  (interactive)
  (if current-prefix-arg
      (progn
        (copy-region-as-kill (point-min) (point-max)))
    (if (use-region-p)
        (progn
          (copy-region-as-kill (region-beginning) (region-end)))
      (if (eq last-command this-command)
          (if (eobp)
              (progn )
            (progn
              (kill-append "\n" nil)
              (kill-append
               (buffer-substring-no-properties (line-beginning-position) (line-end-position))
               nil)
              (progn
                (end-of-line)
                (forward-char))))
        (if (eobp)
            (if (eq (char-before) 10 )
                (progn )
              (progn
                (copy-region-as-kill (line-beginning-position) (line-end-position))
                (end-of-line)))
          (progn
            (copy-region-as-kill (line-beginning-position) (line-end-position))
            (end-of-line)
            (forward-char)))))))

(setq x-select-enable-clipboard t)
(defun xsel-cut-function (text &optional push)
  (with-temp-buffer
    (insert text)
    (call-process-region (point-min) (point-max) "xsel" nil 0 nil "--clipboard" "--input")))
(defun xsel-paste-function()

  (let ((xsel-output (shell-command-to-string "xsel --clipboard --output")))
    (unless (string= (car kill-ring) xsel-output)
      xsel-output )))
(setq interprogram-cut-function 'xsel-cut-function)
(setq interprogram-paste-function 'xsel-paste-function)

(global-set-key [(control ?h)] 'delete-backward-char)
(keyboard-translate ?\C-h ?\C-?)

(global-set-key (kbd "<f1>") (lambda() (interactive)(find-file "~/.emacs.d/config.org")))

(if (daemonp)
    (message "Loading in the daemon!")
  (message "Loading in regular Emacs!"))
(setq doom-modeline-icon t)

(use-package alert :defer t
  :config
  ;; send alerts by default to D-Bus
  (setq alert-default-style 'libnotify))
