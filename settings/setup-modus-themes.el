(require-theme 'modus-themes)

(use-package modus-themes
  :hook (modus-themes-after-load-theme-hook . my/org-modus-customization)
  :config
  (setopt modus-themes-common-palette-overrides
        `(;; Bordelesss modeline
          (border-mode-line-active bg-mode-line-active)
          (border-mode-line-inactive bg-mode-line-inactive)
          (bg-mode-line-active bg-cyan-subtle)

          ;; Subtle line numbers
          (fg-line-number-inactive bg-inactive)
          (fg-line-number-active fg-main)
          (bg-line-number-inactive unspecified)
          (bg-line-number-active unspecified)

          ;; Subtle fringe
          (fringe unspecified)

          ;; Active region
          (bg-region bg-blue-intense)
          (fg-region unspecified)

          ;; Subtle comments
          (comment fg-dim)

          ;; Org agenda
          (prose-done fg-dim)
          ))

  (setopt modus-operandi-tinted-palette-overrides
          `(;; Cursor line
            (bg-hl-line bg-green-nuanced)

            ,@modus-themes-preset-overrides-warmer))

  (setopt modus-themes-to-toggle '(modus-operandi-tinted modus-vivendi-tinted)))

(provide 'setup-modus-themes)
