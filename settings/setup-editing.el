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
  :bind (("M-S-<backspace>" . goto-last-change)
         ("M-`" . goto-last-change)))

;; Avy: quickly jump to char sequence anywhere on the visible screen
;; Especially useful for jumping to entries matched by C-s (isearch)
(use-package avy
  :ensure t
  :bind (("M-j"   . avy-goto-char-timer)
         :map
         isearch-mode-map
         ("M-j" . avy-isearch))
  :config
  (add-to-list 'avy-dispatch-alist '(?Y . my/avy-action-yank-line)))

(use-package expand-region
  :ensure t
  :bind (("C-j" . er/expand-region)
         ("C-S-j" . er/contract-region))
  :config
  (setq expand-region-fast-keys-enabled nil))

(use-package multiple-cursors
  :ensure t
  :bind (("s-j" . mc/mark-next-like-this)
         ("s-J" . mc/skip-to-next-like-this)
         ("M-s-j" . mc/mark-all-dwim)
         ("C-S-c C-S-c" . mc/edit-lines)
         :map
         mc/keymap
         ("M-<return>" . newline))
  :config
  (multiple-cursors-mode 1)
  (setq mc/insert-numbers-default 1))

;; Enables copying/killing whole lines with C-w / M-w when no region is selected
(use-package whole-line-or-region
  :ensure t
  :config
  (whole-line-or-region-global-mode 1))

(use-package surround
  :ensure t
  :bind-keymap ("s-\`" . surround-keymap))

(defun my/avy-action-yank-line (pt)
  (let ((avy-command 'avy-goto-line))
    (save-excursion
      (goto-char pt)
      (back-to-indentation)
      (avy-action-copy (point)))))

(provide 'setup-editing)
