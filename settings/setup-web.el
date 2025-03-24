(use-package sgml-mode
  :hook (sgml-mode . my/setup-html-mode))

(use-package tagedit
  :ensure t
  :hook sgml-mode
  :config
  (tagedit-add-paredit-like-keybindings))

(use-package emmet-mode
  :ensure t
  :hook (sgml-mode css-mode tsx-ts-mode))

(use-package add-node-modules-path
  :ensure t
  :custom
  ;; Hack to get the top-level node_modules in a project, not the node_modules of a dependency inside node_modules.
  ;; Assumes the use of pnpm. Not sure what the npm solution for this would be.
  (add-node-modules-path-command '("cd $(pwd | awk -F 'node_modules' '{print $1}') && pnpm bin"))
  (add-node-modules-path-debug t))

(use-package flymake-eslint
  :ensure t
  :custom
  (flymake-eslint-defer-binary-check t)
  (flymake-eslint-prefer-json-diagnostics t))

(use-package jtsx
  :ensure t
  :config
  (add-hook 'tsx-ts-mode-hook #'my/setup-tsx-mode))

(defvar my/ts-other-file-alist
  '(("\\.test\\.ts$" (".ts" ".tsx"))
    ("\\.test\\.tsx$" (".tsx" ".ts"))
    ("\\.ts$" (".test.ts"))
    ("\\.tsx$" (".test.tsx" ".test.ts"))))

(defun my/setup-html-mode ()
  (sgml-electric-tag-pair-mode 1)
  (apheleia-mode 1))

(defun my/setup-json-mode ()
  (setq js-indent-level 2)
  (electric-indent-mode 1)
  (electric-pair-mode 1)
  (apheleia-mode 1))

(defun my/setup-css-mode ()
  (setq css-indent-offset 2)
  (electric-indent-mode 1)
  (apheleia-mode 1)
  (eglot-ensure))

(defun my/setup-ts-base ()
  (setq ff-other-file-alist #'my/ts-other-file-alist)
  (electric-pair-mode 1)
  (hs-minor-mode 1)
  (apheleia-mode 1)
  (breadcrumb-local-mode 1)
  (add-node-modules-path)
  (flymake-eslint-enable)
  (eglot-ensure)
  (eglot--code-action eglot-ts-code-action-organize-imports "source.removeUnusedImports"))

(defun my/setup-ts-mode ()
  (my/setup-ts-base)
  (setq-local electric-pair-pairs (append electric-pair-pairs '((?\< . ?\>)))))

(defun my/setup-tsx-mode ()
  (my/setup-ts-base)
  ;; Cherry-pick features from jtsx-mode
  ;; - autoedit matching tag pairs
  ;; - comment jsx dwim
  ;; - hs-mode extension
  (setq jtsx-enable-jsx-element-tags-auto-sync t)
  (setq-local forward-sexp-function 'jtsx-forward-sexp)
  (keymap-local-set "<remap> <comment-dwim>" 'jtsx-comment-dwim)
  (add-hook 'pre-command-hook 'jtsx-save-buffer-chars-modified-tick nil t)
  (add-hook 'post-command-hook 'jtsx-synchronize-jsx-element-tags -1 t)
  (add-to-list 'hs-special-modes-alist
               `(tsx-ts-mode
                 "{\\|(\\|[[]\\|\\(?:<>\\)\\|<[^/>][^>]*>"
                 "}\\|)\\|[]]\\|</[^>]*>"
                 ;; "/[*/]"
                 "\\({/[/*]\\)\\|\\(/[/*]\\)"
                 jtsx-forward-sexp
                 nil
                 jtsx-hs-find-block-beginning
                 nil
                 jtsx-hs-looking-at-block-start-p)))

(add-hook 'css-ts-mode-hook #'my/setup-css-mode)
(add-hook 'js-json-mode-hook #'my/setup-json-mode)
(add-hook 'js-ts-mode-hook #'my/setup-ts-mode)
(add-hook 'typescript-ts-mode-hook #'my/setup-ts-mode)

(provide 'setup-web)
