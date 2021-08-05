(use-package haskell-mode
:mode (("\\.hs\\'" . haskell-mode))
      )

(defun efs/lsp-mode-setup ()
(setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
(lsp-headerline-breadcrumb-mode))

  (use-package lsp-mode
  :straight t
:after (company company-box)
:commands (lsp lsp-deferred)
:hook (lsp-mode . efs/lsp-mode-setup)
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
        (use-package company-quickhelp :ensure t :after company :hook (company-mode . company-quickhelp-mode))

(use-package python-mode
  :straight t
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

(use-package web-mode
  :mode "(\\.\\(html?\\|ejs\\|tsx\\|jsx\\)\\'"
  :config
  (setq-default web-mode-code-indent-offset 2)
  (setq-default web-mode-markup-indent-offset 2)
  (setq-default web-mode-attribute-indent-offset 2))

;; 1. Start the server with `httpd-start'
;; 2. Use `impatient-mode' on any buffer
(use-package impatient-mode :hook (web-mode . impatient-mode))

(use-package rust-mode
  :mode "\\.rs\\'"
  :init (setq rust-format-on-save t))

(use-package cargo
  :defer t)

(add-hook 'c-mode-hook 'lsp)
(add-hook 'c++-mode-hook 'lsp)
  (defun auto-recompile-buffer ()
(interactive)
(if (member #'recompile after-save-hook)
    (remove-hook 'after-save-hook #'recompile t)
  (add-hook 'after-save-hook #'recompile nil t)))

(use-package flycheck :straight t :hook (prog-mode . flycheck-mode))

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
