(require-theme 'modus-themes)

(setopt modus-themes-common-palette-overrides
        `(;; Bordelesss modeline
          (border-mode-line-active bg-mode-line-active)
          (border-mode-line-inactive bg-mode-line-inactive)

          ;; Subtle line numbers
          (fg-line-number-inactive bg-inactive)
          (fg-line-number-active fg-main)
          (bg-line-number-inactive unspecified)
          (bg-line-number-active unspecified)

          ;; Subtle fringe
          (fringe unspecified)

          ;; Active region
          ;; (bg-region bg-lavender)
          ;; (fg-region unspecified)

          ;; Subtle comments
          (comment fg-dim)
          ))

(setopt modus-operandi-tinted-palette-overrides
        `(;; Bordelesss modeline
          (border-mode-line-active bg-mode-line-active)
          (border-mode-line-inactive bg-mode-line-inactive)

          ()

          ;; Cursor line
          (bg-mode-line-active bg-cyan-nuanced)
          (bg-hl-line bg-green-nuanced)

          ,@modus-themes-preset-overrides-faint))

(defun my/org-modus-customization ()
  (setq org-todo-keyword-faces
        `(("TODO" . (:foreground ,(modus-themes-get-color-value 'blue-warmer) :weight bold))
          ("DONE" . (:foreground ,(modus-themes-get-color-value 'border) :weight bold))))
  (set-face-foreground 'org-headline-done (modus-themes-get-color-value 'border))
  (when (derived-mode-p 'org-mode)
    (font-lock-fontify-buffer))
  )

(use-package modus-themes
  :hook (modus-themes-after-load-theme-hook . my/org-modus-customization)
  :config
  (setq modus-themes-to-toggle '(modus-operandi-tinted modus-vivendi-tinted)))

(provide 'setup-modus-themes)
