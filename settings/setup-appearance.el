(require 'cus-theme)

;; Turn off menubar/toolbar/scrollbars
(unless (memq window-system '(mac ns)) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'horizontal-scroll-bar-mode) (horizontal-scroll-bar-mode -1))

;; Turn off window bar
;; (add-to-list 'default-frame-alist '(undecorated . t))
;; (add-to-list 'default-frame-alist '(drag-internal-border . t))
;; (add-to-list 'default-frame-alist '(internal-border-width . 4))

;; Disable splash/startup screen
(setq inhibit-startup-screen t)
(setq inhibit-splash-screen t)

;; Disable sounds
(setq visible-bell nil)
(setq ring-bell-function 'ignore)

;; Highlight current line
(global-hl-line-mode 1)

;; Line/column in modeline
(setopt line-number-mode t)
(setopt column-number-mode t)

;; Prettier underlines
(setopt x-underline-at-descent-line nil)

;; Show buffer top and bottom in the margin
(setopt indicate-buffer-boundaries 'left)

;; Disable cursor blinking
(blink-cursor-mode -1)

;; No tooltips
(tooltip-mode -1)

;; Smooth scrolling
(pixel-scroll-precision-mode)

;; Resize the frame pixelwise, instead of by line-height
(setq frame-resize-pixelwise t)

;; Display line numbers in programming mode
(setopt display-line-numbers-width 3)

;; Nice line wrapping when working with text
(add-hook 'text-mode-hook 'visual-line-mode)

;; Disable loaded themes before loading a new theme
(defadvice load-theme (before theme-unload-existing activate)
  (mapc #'disable-theme custom-enabled-themes))

;; Save loaded themes to custom
(defadvice load-theme (after theme-save-to-custom activate)
  (custom-theme-save))

(use-package doom-themes
  :defer t)

(use-package ef-themes
  :defer t)

(use-package doom-modeline
  :ensure t
  :config
  (setq doom-modeline-icon nil)
  (doom-modeline-mode 1))

(use-package hl-todo
  :ensure t
  :config
  (global-hl-todo-mode 1))

(provide 'setup-appearance)
