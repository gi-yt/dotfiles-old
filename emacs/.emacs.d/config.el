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

(use-package org-auto-tangle
  :defer t
  :hook (org-mode . org-auto-tangle-mode))

(use-package ox-twbs :defer t
  :straight t)

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

;; (use-package spaceline
;;   :ensure t
;;   :config
;;   (require 'spaceline-config)
;;     (setq spaceline-buffer-encoding-abbrev-p nil)
;;     (setq spaceline-line-column-p nil)
;;     (setq spaceline-line-p nil)
;;     (setq powerline-default-separator (quote arrow))
;;     (spaceline-spacemacs-theme))
;; (use-package telephone-line
;;   :after winum
;;   :custom
;;   (telephone-line-primary-left-separator 'telephone-line-cubed-left)
;;   (telephone-line-secondary-left-separator 'telephone-line-cubed-hollow-left)
;;   (telephone-line-primary-right-separator 'telephone-line-cubed-right)
;;   (telephone-line-secondary-right-separator 'telephone-line-cubed-hollow-right)
;;   (telephone-line-height 24)
;;   (telephone-line-evil-use-short-tag t)
;;   :config
;;   (telephone-line-defsegment telephone-line-pdf-segment ()
;; 			     (if (eq major-mode 'pdf-view-mode)
;; 				 (propertize (pdf-view-page-number)
;; 					     'face '(:inherit)
;; 					     'display '(raise 0.0)
;; 					     'mouse-face '(:box 1)
;; 					     'local-map (make-mode-line-mouse-map
;; 							 'mouse-1 (lambda ()
;; 								    (interactive)
;; 								    (pdf-view-goto-page))))))
;;   (telephone-line-defsegment telephone-line-winum-segment ()
;; 			     (propertize winum--mode-line-segment
;; 					 'face '(:box (:line-width 2 :color "cyan" :style released-button))
;; 					 'display '(raise 0.0)
;; 					 'mouse-face '(:box 1)))
;;   (setq telephone-line-lhs '((accent . (telephone-line-winum-segment
;; 					telephone-line-pdf-segment
;; 					telephone-line-vc-segment
;; 					telephone-line-erc-modified-channels-segment
;; 					telephone-line-process-segment))
;; 			     (nil . (telephone-line-projectile-segment telephone-line-buffer-segment))))
;;   (telephone-line-mode t))

;; (use-package telephone-line
;;   :after winum
;;   :custom
;;   (telephone-line-primary-left-separator 'telephone-line-cubed-left)
;;   (telephone-line-secondary-left-separator 'telephone-line-cubed-hollow-left)
;;   (telephone-line-primary-right-separator 'telephone-line-cubed-right)
;;   (telephone-line-secondary-right-separator 'telephone-line-cubed-hollow-right)
;;   (telephone-line-height 24)
;;   (telephone-line-evil-use-short-tag t)
;;   :config
;;   (setq telephone-line-faces '((evil . telephone-line-modal-face)
;; 			       (modal . telephone-line-modal-face)
;; 			       (ryo . telephone-line-ryo-modal-face)
;; 			       (accent telephone-line-accent-active . telephone-line-accent-inactive)
;; 			       (nil mode-line . mode-line-inactive)
;; 			       (winum . (winum-face . winum-face))))
;;   (telephone-line-defsegment telephone-line-org-clock-segment ()
;;     (when (telephone-line-selected-window-active)
;;       (if (and (functionp 'org-clocking-p) (org-clocking-p))
;; 	  (org-clock-get-clock-string))))
;;   (telephone-line-defsegment telephone-line-pdf-segment ()
;;     (when (eq major-mode 'pdf-view-mode)
;;       (propertize (pdf-view-page-number)
;; 		  'face '(:inherit)
;; 		  'display '(raise 0.0)
;; 		  'mouse-face '(:box 1))))
;;   (telephone-line-defsegment telephone-line-winum-segment ()
;;     (propertize (eval (cadr winum--mode-line-segment))
;; 		'face '(:box (:line-width 2 :color "cyan" :style released-button))
;; 		'display '(raise 0.0)
;; 		'mouse-face '(:box 1)))
;;   (telephone-line-defsegment telephone-line-battery-segment ()
;;     (when (telephone-line-selected-window-active)
;;       (propertize battery-mode-line-string
;; 		  'mouse-face '(:box 1))))

;;   (setq telephone-line-lhs '((winum . (telephone-line-winum-segment))
;; 			     (accent . (telephone-line-pdf-segment
;; 					telephone-line-vc-segment
;; 					telephone-line-erc-modified-channels-segment
;; 					telephone-line-process-segment))
;; 			     (nil . (telephone-line-projectile-segment
;; 				     telephone-line-buffer-segment
;; 				     telephone-line-org-clock-segment
;; 				     ))))
;;   (setq telephone-line-center-rhs '((evil . (telephone-line-battery-segment))))
;;   (setq telephone-line-rhs '((nil . (telephone-line-flycheck-segment
;; 				     ))
;; 			     (accent . (telephone-line-major-mode-segment))
;; 			     (evil . (telephone-line-airline-position-segment))))
;;   (telephone-line-mode t))

(use-package moody
  :unless noninteractive
  :defer 1
  ;;:init
  ;;(set-background-color "black")
  ;;(set-foreground-color "white")
  ;; If you use the default Emacs black theme (no external theme loaded) you have to specify
  ;; a different color for mode-line-buffer-id or it will be the same as the background
  ;;(set-face-attribute 'mode-line-buffer-id nil :foreground "light sky blue" :weight 'bold)
  ;;(let ((line (face-attribute 'mode-line :underline)))
  ;;  (set-face-attribute 'mode-line nil :overline line)
  ;;  (set-face-attribute 'mode-line-inactive nil :overline line)
  ;;  (set-face-attribute 'mode-line-inactive nil :underline line)
  ;;  (set-face-attribute 'mode-line nil :box nil)
  ;;  (set-face-attribute 'mode-line-inactive nil :box nil))
  :config
  (setq x-underline-at-descent-line t)
  (setq moody-mode-line-height 20)
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode))

;; (use-package doom-themes :straight t :init (load-theme 'doom-dracula))

(if (display-graphic-p)
    ;;(use-package atom-one-dark-theme :straight t :init (load-theme 'atom-one-dark))
    (use-package zerodark-theme :straight t :init (load-theme 'zerodark))
  (load-theme 'tsdh-dark))

(use-package magit :straight t :defer t :commands magit-status :custom  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package goggles
:config
(setq-default goggles-pulse t)
(goggles-mode))

(use-package vterm :straight t :defer t)
      (setq vterm-eval-cmds '(("magit-status-setup-buffer" magit-status-setup-buffer)
                          ("find-file" find-file)
                          ("message" message)
                          ("vterm-clear-scrollback" vterm-clear-scrollback)))
;; (setq  vterm-always-compile-module nil)
(use-package multi-vterm :straight t :defer t
    :bind ("s-<return>" . multi-vterm))

(use-package with-editor :defer t)

(add-hook 'vterm-exec-hook  'with-editor-export-editor)

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

(use-package mark-multiple
  :straight t
:defer t
  :bind ("C-c q" . 'mark-next-like-this))

(defun daedreth/kill-inner-word ()
  "Kills the entire word your cursor is in. Equivalent to 'ciw' in vim."
  (interactive)
  (forward-char 1)
  (backward-word)
  (kill-word 1))
(global-set-key (kbd "C-c w k") 'daedreth/kill-inner-word)

(defun daedreth/copy-whole-word ()
  (interactive)
  (save-excursion
    (forward-char 1)
    (backward-word)
    (kill-word 1)
    (yank)))
(global-set-key (kbd "C-c w c") 'daedreth/copy-whole-word)

(defun daedreth/copy-whole-line ()
  "Copies a line without regard for cursor position."
  (interactive)
  (save-excursion
    (kill-new
     (buffer-substring
      (point-at-bol)
      (point-at-eol)))))
(global-set-key (kbd "C-c l c") 'daedreth/copy-whole-line)

(global-set-key (kbd "C-c l k") 'kill-whole-line)

(use-package diminish
  :ensure t
  :init
  (diminish 'which-key-mode)
  (diminish 'linum-relative-mode)
  (diminish 'hungry-delete-mode)
  (diminish 'visual-line-mode)
  (diminish 'subword-mode)
  (diminish 'beacon-mode)
  (diminish 'irony-mode)
  (diminish 'page-break-lines-mode)
  (diminish 'auto-revert-mode)
  (diminish 'rainbow-delimiters-mode)
  (diminish 'rainbow-mode)
  (diminish 'yas-minor-mode)
  (diminish 'flycheck-mode)
  (diminish 'ivy-mode))

(global-set-key (kbd "<f1>") (lambda() (interactive)(find-file "~/.emacs.d/config.org")))
