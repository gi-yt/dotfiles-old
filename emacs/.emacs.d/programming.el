(use-package haskell-mode
:mode (("\\.hs\\'" . haskell-mode))
      )

(defun efs/lsp-mode-setup ()
(setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
(lsp-headerline-breadcrumb-mode))

  (use-package lsp-mode
  :straight t
  :defer t
:after (company company-box)
:commands (lsp lsp-deferred)
:hook (lsp-mode . efs/lsp-mode-setup)
(lsp-mode . company-mode)
:init
(setq lsp-keymap-prefix "C-c l")  ;; Or 'C-l', 's-l'
:config
((let* (args)
   )lsp-enable-which-key-integration t))
  (use-package lsp-ui :after lsp-mode
  :straight t
:hook (lsp-mode . lsp-ui-mode)
:custom
(lsp-ui-doc-position 'bottom))

(use-package web-mode
  :mode "(\\.\\(html?\\|ejs\\|tsx\\|jsx\\)\\'"
  :hook (web-mode . lsp-deferred)
  :config
  (setq-default web-mode-code-indent-offset 2)
  (setq-default web-mode-markup-indent-offset 2)
  (setq-default web-mode-attribute-indent-offset 2))

(use-package impatient-mode :hook (web-mode . impatient-mode))

(use-package rust-mode
  :mode "\\.rs\\'"
:hook (rust-mode . lsp-deferred)
  :init (setq rust-format-on-save t))

(use-package cargo
  :defer t)

(add-hook 'c-mode-hook 'lsp-deferred)
(add-hook 'c++-mode-hook 'lsp-deferred)
  (defun auto-recompile-buffer ()
(interactive)
(if (member #'recompile after-save-hook)
    (remove-hook 'after-save-hook #'recompile t)
  (add-hook 'after-save-hook #'recompile nil t)))

(use-package flycheck :straight t :hook (prog-mode . flycheck-mode) :defer t)

(use-package markdown-mode
  :straight t
  :mode "\\.md\\'"
  :config
  (setq markdown-command "marked")
  (defun dw/set-markdown-header-font-sizes ()
    (dolist (face '((markdown-header-face-1 . 1.2)
                    (markdown-header-face-2 . 1.1)
                    (markdown-header-face-3 . 1.0)
                    (markdown-header-face-4 . 1.0)
                    (markdown-header-face-5 . 1.0)))
      (set-face-attribute (car face) nil :weight 'normal :height (cdr face))))

  (defun dw/markdown-mode-hook ()
    (dw/set-markdown-header-font-sizes))

  (add-hook 'markdown-mode-hook 'dw/markdown-mode-hook))

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

(use-package rainbow-delimiters
:straight t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package evil-nerd-commenter
  :straight t
  :bind ("M-/" . evilnc-comment-or-uncomment-lines))

(use-package rainbow-mode
:defer t
:straight t

  :init
    (add-hook 'prog-mode-hook 'rainbow-mode))

(use-package company
              :straight t
        :after (lsp-mode)
            :hook ((lsp-mode org-mode prog-mode) . company-mode)
            :bind (:map company-active-map
                   ("<tab>" . company-complete-selection))
            (:map lsp-mode-map
                  ("<tab>" . company-indent-or-complete-common))

            :custom
            (company-minimum-prefix-length 1)
            (company-idle-delay 0.0))

              (use-package company-box
              :straight t
            :hook (company-mode . company-box-mode))
        (use-package company-quickhelp :ensure t :after company :hook (company-mode . company-quickhelp-mode) :defer t)

(use-package python-mode
  :straight nil
  :mode ("\\.py\\'")
  :hook (python-mode . lsp-deferred))
        (use-package py-autopep8 :straight t :hook (python-mode . py-autopep8-enable-on-save))

(use-package company-shell :straight t
      :hook ((sh-mode shell-mode) . sh-mode-init)
      :config
      (defun sh-mode-init ()
        (setq-local company-backends '((company-shell
                        company-shell-env
                        company-files
                        company-dabbrev-code
                        company-capf
                        company-yasnippet)))))
(use-package sh-mode :straight nil
  :hook (sh-mode . lsp-deferred))
