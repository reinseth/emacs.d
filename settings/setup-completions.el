(use-package yasnippet
  :defer t
  :diminish yas-minor-mode
  :config
  (yas-global-mode 1))

;; Vertico: vertical completion list in minibuffer
;; Note: use M-RET to accept literal input
(use-package vertico
  :ensure t
  :init
  (vertico-mode)
  :bind
  (:map vertico-map
        ("s-<return>" . embark-export)))

(use-package vertico-directory
  :after vertico
  :bind (:map vertico-map
              ("M-DEL" . vertico-directory-delete-word)))

;; Marginalia: annotations for completions in the minibuffer
(use-package marginalia
  :ensure t
  :config
  (marginalia-mode))

;; Orderless: completion style, enter tokens in any order, separated by space
(use-package orderless
  :ensure t
  :config
  (setq completion-styles '(orderless basic))
  ;; (setq completion-styles '(orderless partial-completion basic))
  ;;(setq orderless-matching-styles '(orderless-literal orderless-regexp my/orderless-camelcase))
  (setq orderless-matching-styles '(my/orderless-camelcase orderless-prefixes orderless-initialism)))

;; Corfu: completion-at-point ui
(use-package corfu
  :ensure t
  :init
  (global-corfu-mode)
  (corfu-popupinfo-mode)
  :config
  (setq corfu-auto-prefix 2)
  (setq corfu-auto nil)
  (setq corfu-on-exact-match 'show)
  (setq corfu-quit-at-boundary nil)
  :bind (:map corfu-map
              ("TAB" . corfu-complete)
              ("RET" . corfu-insert)
              ("M-j" . corfu-quick-jump)))

(use-package corfu-popupinfo
  :after corfu
  :hook (corfu-mode . corfu-popupinfo-mode)
  :custom
  (corfu-popupinfo-delay '(0.25 . 0.1))
  (corfu-popupinfo-hide nil)
  :config
  (corfu-popupinfo-mode))

;; Make corfu popup come up in terminal overlay
(use-package corfu-terminal
  :if (not (display-graphic-p))
  :ensure t
  :config
  (corfu-terminal-mode))

;; Cape: extra completions
(use-package cape
  :ensure t)

;; Pretty icons for corfu
(use-package kind-icon
  :if (display-graphic-p)
  :ensure t
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

;; Embark: take results from other commands run actions on them, e.g. export a list to a wgrep buffer
(use-package embark
  :ensure t
  :bind (("C-c a" . embark-act)))

;; Consult: completion provider for various actions in the minibuffer
(use-package consult
  :ensure t
  :bind (("C-x b"   . consult-buffer)
         ("C-x p b" . consult-project-buffer)
         ("C-x C-r" . consult-recent-file)
         ("M-y"     . consult-yank-pop)
         ("s-b"     . consult-bookmark)

         ;; Searching
         ;; Note that rg is bound to "M-s s"
         ("M-s r" . my/consult-ripgrep)
         ("M-s o" . my/occur-thing-at-point)
         ("M-s O" . consult-outline)
         ("M-s L" . consult-line-multi)
         ("M-s l" . consult-line)
         ("M-s S" . isearch)
         ("M-s k" . consult-keep-lines)
         ("M-s e" . consult-flymake)

         ;; Thing at point
         ("C-." . my/consult-line-thing-at-point)

         ;; Isearch integration
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)
         ("M-s e" . consult-isearch-history)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi))
  :config
  ;; Narrowing lets you restrict results to certain groups of candidates
  (setq consult-narrow-key "<")
  (setq xref-show-xrefs-function #'consult-xref)
  (setq xref-show-definitions-function #'consult-xref))

(use-package embark-consult
  :ensure t)

(defun my/orderless-camelcase (component)
  (orderless-regexp (s-join "[a-zA-Z0-9]*" (s-slice-at "[A-Z]" component))))

(defun my/occur-thing-at-point ()
  (interactive)
  (if (region-active-p)
      (occur (buffer-substring (region-beginning) (region-end)))
    (occur (thing-at-point 'symbol))))

(defun my/consult-line-thing-at-point ()
  (interactive)
  (if (region-active-p)
      (consult-line (buffer-substring (region-beginning) (region-end)))
    (consult-line (thing-at-point 'symbol))))

(defun my/consult-ripgrep (&optional dir given-initial)
  "Pass the region to consult-ripgrep if available.

DIR and GIVEN-INITIAL match the method signature of `consult-wrapper'.

Taken from https://github.com/minad/consult/wiki#start-consult-ripgrep-search-with-active-region"
  (interactive "P")
  (let ((initial
         (or given-initial
             (when (use-region-p)
               (buffer-substring-no-properties (region-beginning) (region-end))))))
    (consult-ripgrep dir initial)))

(provide 'setup-completions)
