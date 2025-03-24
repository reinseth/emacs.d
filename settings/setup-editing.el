;; Predictable undo/redo
(use-package undo-fu
  :ensure t
  :config
  ;; Source: https://github.com/magnars/emacsd-reboot/blob/main/packages/setup-undo-fu.el
  (setq undo-limit 400000               ; 400kb (default is 160kb)
        undo-strong-limit 3000000       ; 3mb   (default is 240kb)
        undo-outer-limit 48000000)      ; 48mb  (default is 24mb)

  :bind (("M-z" . undo-fu-only-undo)
         ("M-Z" . undo-fu-only-redo)
         ("C-M-z" . undo-fu-only-redo-all)))

;; Visualize undo tree
(use-package vundo
  :ensure t
  :config
  (setq vundo-glyph-alist vundo-unicode-symbols
        vundo-compact-display t)
  :bind (("s-z" . vundo)))

(use-package goto-chg
  :ensure t
  :bind ("M-`" . goto-last-change))

;; Avy: quickly jump to char sequence anywhere on the visible screen
(use-package avy
  :ensure t
  :bind (("C-c j" . avy-goto-line)
         ;; ("s-j"   . avy-goto-char-timer)
         ("M-j"   . avy-goto-char-timer)
         :map
         isearch-mode-map
         ;; ("s-j" . avy-isearch)
         ("M-j" . avy-isearch)))

(use-package expand-region
  :ensure t
  :bind (("C-;" . er/expand-region)
         ("C-:" . er/contract-region))
  :config
  (setq expand-region-contract-fast-key ":"))

(use-package multiple-cursors
  :ensure t
  :bind (("M-'" . mc/mark-all-dwim)
         ("C-'" . mc/mark-next-like-this)
         ("C-S-c C-S-c" . mc/edit-lines)
         :map mc/keymap
         ("C-'" . mc/mark-next-like-this)
         ("<return>" . nil)
         ("C-\"" . mc/skip-to-next-like-this))
  :config
  (multiple-cursors-mode 1)
  (setq mc/insert-numbers-default 1))

;; Enables copying/killing whole lines with C-w / M-w when no region is selected
(use-package whole-line-or-region
  :ensure t
  :config
  (whole-line-or-region-global-mode 1))

(use-package embrace
  :ensure t
  :bind (("s-\`" . embrace-commander)))

(provide 'setup-editing)
