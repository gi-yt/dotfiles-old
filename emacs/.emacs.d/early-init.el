(setq package-enable-at-startup nil)
(put 'inhibit-startup-echo-area-message 'saved-value t)
(setq inhibit-startup-message t)

(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room

(menu-bar-mode -1)            ; Disable the menu bar

;; Disable the visible bell
(setq visible-bell -1)

(column-number-mode)
(global-display-line-numbers-mode t)
