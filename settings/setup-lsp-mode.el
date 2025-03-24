;; Config from https://github.com/minad/corfu/wiki#configuring-corfu-for-lsp-clients
(use-package lsp-mode
  ;; :ensure t
  :config
  (setq lsp-auto-execute-action nil)
  (setq lsp-completion-filter-on-incomplete t)
  (setq lsp-completion-provider :none) ; important! any setting here gets in the way of corfu
  (setq lsp-copilot-enabled nil)
  (setq lsp-diagnostics-provider :flycheck)
  (setq lsp-enable-folding nil)
  (setq lsp-enable-imenu t)
  (setq lsp-enable-indentation nil)
  (setq lsp-enable-on-type-formatting nil)
  (setq lsp-enable-snippet t)
  (setq lsp-enable-symbol-highlighting nil)
  (setq lsp-log-io nil)

  ;; Linting
  (setq lsp-eslint-enable t)

  ;; Headerline
  (setq lsp-headerline-breadcrumb-enable t)
  (setq lsp-headerline-breadcrumb-enable-diagnostics nil)
  (setq lsp-headerline-breadcrumb-enable-symbol-numbers nil)
  (setq lsp-headerline-breadcrumb-icons-enable nil)

  ;; Modeline
  (setq lsp-modeline-diagnostics-mode nil)
  (setq lsp-modeline-code-actions-mode nil)

  ;; Lens
  (setq lsp-lens-enable nil)

  ;; UI
  (setq lsp-ui-doc-position 'at-point)
  (setq lsp-ui-doc-use-webkit t)
  (setq lsp-ui-doc-show-with-mouse nil)
  (setq lsp-ui-imenu-window-fix-width t)
  (setq lsp-ui-sideline-enable nil)

  ;; Typescript
  (setq lsp-clients-typescript-prefer-use-project-ts-server t)

  ;; Actions - TODO do this only for typescript
  ;; Imports doesn't seem to be removed using the standard source.organizeImports action
  (lsp-make-interactive-code-action organize-imports "source.removeUnusedImports")

  ;; File watchers
  (add-to-list 'lsp-file-watch-ignored-directories "/build\\'")

  :init
  (setq lsp-use-plists t)
  ;; (advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command)

  ;; lsp-mode messes with the completion settings. See https://magnus.therning.org/2024-05-04-orderless-completion-in-lsp-mode.html
  (add-hook 'lsp-completion-mode-hook #'cleanup-lsp-completion-category-defaults)

  :bind ((:map lsp-mode-map
               ("C-s-o" . 'lsp-organize-imports)
               ("s-i" . 'lsp-ui-doc-glance)
               ("s-I" . 'lsp-describe-thing-at-point))))

;; (use-package lsp-ui
  ;; :defer t
  ;; :after lsp-mode)

;; (use-package lsp-eslint
;;   ;; :defer t
;;   :after lsp-mode)

(use-package lsp-tailwindcss
  ;; :ensure t
  :init
  (setq lsp-tailwindcss-add-on-mode t)
  (setq lsp-tailwindcss-major-modes
        '(typescript-ts-mode
          tsx-ts-mode
          html-mode
          shtml-mode
          css-mode
          css-ts-mode)))

(defun lsp-booster--advice-json-parse (old-fn &rest args)
  "Try to parse bytecode instead of json."
  (or
   (when (equal (following-char) ?#)
     (let ((bytecode (read (current-buffer))))
       (when (byte-code-function-p bytecode)
         (funcall bytecode))))
   (apply old-fn args)))

(advice-add (if (progn (require 'json)
                       (fboundp 'json-parse-buffer))
                'json-parse-buffer
              'json-read)
            :around
            #'lsp-booster--advice-json-parse)

(defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
  "Prepend emacs-lsp-booster command to lsp CMD."
  (let ((orig-result (funcall old-fn cmd test?)))
    (if (and (not test?) ;; for check lsp-server-present?
             (not (file-remote-p default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
             lsp-use-plists
             (not (functionp 'json-rpc-connection)) ;; native json-rpc
             (executable-find "emacs-lsp-booster"))
        (progn
          (when-let ((command-from-exec-path (executable-find (car orig-result)))) ;; resolve command from exec-path (in case not found in $PATH)
            (setcar orig-result command-from-exec-path))
          (message "Using emacs-lsp-booster for %s!" orig-result)
          (cons "emacs-lsp-booster" orig-result))
      orig-result)))

(defun cleanup-lsp-completion-category-defaults ()
  (setq-local completion-category-defaults
              (assoc-delete-all 'lsp-capf completion-category-defaults)))

(provide 'setup-lsp-mode)
