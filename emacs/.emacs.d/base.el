(setq inhibit-startup-screen t)
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t) ; Displays the File Size in the modeline
(setq auto-window-vscroll nil)
(fset 'yes-or-no-p 'y-or-n-p)
(global-hl-line-mode +1)

;; Silence compiler warnings as they can be pretty disruptive
;; (setq native-comp-async-report-warnings-errors nil)
;; Using garbage magic hack.
(use-package gcmh :straight t :config (gcmh-mode 1))
     ;; Silence compiler warnings as they can be pretty disruptive (setq comp-async-report-warnings-errors nil)
(delete-selection-mode t)
(load "~/.emacs.d/sensible-defaults.el")
(sensible-defaults/use-all-settings)
(sensible-defaults/use-all-keybindings)
(sensible-defaults/backup-to-temp-directory)
 '(org-modules
   '(ol-bbdb ol-bibtex ol-docview ol-eww ol-gnus ol-info ol-irc ol-mhe ol-rmail ol-w3m ol-man org-toc))
(use-package no-littering
  :demand t
  :config
  ;; /etc is version controlled and I want to store mc-lists in git
  (setq mc/list-file (no-littering-expand-etc-file-name "mc-list.el"))
  ;; Put the auto-save files in the var directory to the other data files
  (setq auto-save-file-name-transforms
        `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))))

(use-package custom
:straight nil
  :config
  ;; We don't use custom and don't have to set custom-file even
  ;; in the case when we "accidentally" click save in a custom buffer,
  ;; `init.el' would get modified which gets overwrite the next time
  ;; we run `make'.

  ;; Treat all themes as safe
  (setf custom-safe-themes t))
(setq user-full-name "Arya Kiran"
      user-mail-address "aryakiran@zohomail.eu")
(require 'generic-x)
(require 'display-line-numbers)
(defcustom display-line-numbers-exempt-modes
  '(vterm-mode eshell-mode shell-mode term-mode ansi-term-mode pdf-view-mode)
  "Major modes on which to disable line numbers."
  :group 'display-line-numbers
  :type 'list
  :version "green")
(defun display-line-numbers--turn-on ()
  "Turn on line numbers except for certain major modes.
      Exempt major modes are defined in `display-line-numbers-exempt-modes'."
  (unless (or (minibufferp)
              (member major-mode display-line-numbers-exempt-modes))
    (display-line-numbers-mode)))
(global-display-line-numbers-mode)
(global-visual-line-mode t)
;; Increase the amount of data which Emacs reads from the process
;; (Useful for LSP where the LSP responses are in the 800k - 3M range)
(setq read-process-output-max (* 1024 1024)) ;; 1mb
;; Don't compact font caches during GC as it doesn't play too nice
;; with org-superstar-mode and some of my large org files (e.g. this file).
;; This might enlarge the Emacs memory footprint but I don't mind if Emacs
;; uses more memory but rather prefer speed.
(setq inhibit-compacting-font-caches t)
;; Always just use left-to-right text
;; This makes Emacs a bit faster for very long lines
(setq-default bidi-paragraph-direction 'left-to-right)
(use-package saveplace :straight nil
  :unless noninteractive
  :config (save-place-mode))
(use-package savehist :straight nil
  :unless noninteractive
  :defer 1
  :config
  (setq savehist-additional-variables '(compile-command kill-ring regexp-search-ring))
  (savehist-mode 1))
(global-so-long-mode)
(setq-default indent-tabs-mode nil)   ; don't use tabs to indent
(setq-default tab-width 4)            ; but maintain correct appearance
;; smart tab behavior - indent or complete
(setq tab-always-indent 'complete)
;; Newline at end of file
(setq require-final-newline t)
;; Default to utf-8 unix encoding
(prefer-coding-system 'utf-8-unix)
;; Delete the selection with a keypress
(delete-selection-mode t)
;; Activate character folding in searches i.e. searching for 'a' matches 'ä' as well
(setq search-default-mode 'char-fold-to-regexp)
;; Paste with middle mouse button doesn't move the cursor
(setq mouse-yank-at-point t)
;; Save whatever’s in the current (system) clipboard before
;; replacing it with the Emacs’ text.
(setq save-interprogram-paste-before-kill t)
;; Accept 'UTF-8' (uppercase) as a valid encoding in the coding header
(define-coding-system-alias 'UTF-8 'utf-8)
;; Put authinfo.gpg first so new secrets will be stored there by default and not in plain text
(setq auth-sources '("~/.authinfo.gpg" "~/.authinfo" "~/.netrc"))
;; Don't ask to store credentials in .authinfo.gpg
(setq auth-source-save-behavior nil)
;; Silence ad-handle-definition about advised functions getting redefined
(setq ad-redefinition-action 'accept)
;; Use 'fancy' ellipses for truncated strings
(setq truncate-string-ellipsis  " ▾")
;; Increase the 'Limit on number of Lisp variable bindings and unwind-protects.'
;; mu4e seems to need more sometimes and it can be safely increased.
(setq max-specpdl-size 8192)
;; Increase the limit to catch infinite recursions.
;; Large scala files need sometimes more and this value can safely be increased.
(setq max-lisp-eval-depth 32768)
(setq-default
 indent-tabs-mode nil                             ; Prefers spaces over tabs
 load-prefer-newer t                              ; Prefers the newest version of a file
 mark-ring-max 128                                ; Maximum length of mark ring
 read-process-output-max (* 1024 1024)            ; Increase the amount of data reads from the process
 select-enable-clipboard t                        ; Merge system's and Emacs' clipboard
 tab-width 4                                      ; Set width for tabs
 view-read-only t)                                ; Always open read-only buffers in view-mode
(cd "~/")                                         ; Move to the user directory
(column-number-mode 1)                            ; Show the column number
(global-hl-line-mode)                             ; Hightlight current line
(set-default-coding-systems 'utf-8)               ; Default to utf-8 encoding
(show-paren-mode 1)                               ; Show the parent
(setq large-file-warning-threshold nil)
