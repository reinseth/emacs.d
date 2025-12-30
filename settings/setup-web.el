(use-package sgml-mode
  :hook (sgml-mode . my/setup-html-mode))

(use-package tagedit
  :ensure t
  :hook sgml-mode
  :config
  (tagedit-add-paredit-like-keybindings))

(use-package emmet-mode
  :ensure t
  :bind (:map emmet-mode-keymap
              ("C-j" . nil))
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

(straight-use-package
 '(vite-test-mode
   :type git
   :host github
   :repo "chrishowejones/vite-test-mode"))

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

(defun my/setup-ts-base (mode-map)
  (with-significant-others f
    ("\\.test\\.ts$" (list
                      (s-replace ".test.ts" ".ts" f)
                      (s-replace ".test.ts" ".tsx" f)))
    ("\\.test\\.tsx$" (list
                       (s-replace ".test.tsx" ".tsx" f)
                       (s-replace ".test.tsx" ".ts" f)))
    ("\\.ts$" (list
               (s-replace ".ts" ".test.ts" f)
               (s-replace ".ts" ".test.tsx" f)))
    ("\\.tsx$" (list
                (s-replace ".tsx" ".test.tsx" f)
                (s-replace ".tsx" ".test.ts" f))))
  (electric-pair-mode 1)
  (hs-minor-mode 1)
  (apheleia-mode 1)
  (vite-test-mode 1)
  (breadcrumb-local-mode 1)
  (add-node-modules-path)
  (flymake-eslint-enable)
  (eglot--code-action eglot-ts-code-action-organize-imports "source.removeUnusedImports")
  (eglot-ensure)
  (keymap-set mode-map "<remap> <eglot-code-action-organize-imports>" 'eglot-ts-code-action-organize-imports)
  (keymap-set mode-map (kbd "\"") 'my/double-quotes)
  (keymap-set mode-map (kbd "'") 'my/single-quotes)
  (keymap-set mode-map (kbd "`") 'my/backtick-quotes)
  (keymap-set mode-map "C-c w" 'my/wrap-jsx-element))

(defun my/setup-js-mode ()
  (my/setup-ts-base js-ts-mode-map))

(defun my/setup-ts-mode ()
  (my/setup-ts-base typescript-ts-mode-map)
  (setq-local electric-pair-pairs (append electric-pair-pairs '((?\< . ?\>)))))

(defun my/setup-tsx-mode ()
  (my/setup-ts-base tsx-ts-mode-map)
  ;; Cherry-pick features from jtsx-mode
  ;; - autoedit matching tag pairs
  ;; - comment jsx dwim
  ;; - hs-mode extension
  (setq jtsx-enable-jsx-element-tags-auto-sync t)
  (setq-local forward-sexp-function 'jtsx-forward-sexp)
  (keymap-local-set "<remap> <comment-dwim>" 'jtsx-comment-dwim)
  (keymap-local-set "<remap> <comment-line>" 'jtsx-comment-line)
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
(add-hook 'js-ts-mode-hook #'my/setup-js-mode)
(add-hook 'typescript-ts-mode-hook #'my/setup-ts-mode)

(defun my/quoted-region-active-p (new-quote-char)
  (interactive)
  (when (use-region-p)
    (let* ((start (region-beginning))
           (end (region-end))
           (start-char (buffer-substring-no-properties start (+ start 1)))
           (end-char (buffer-substring-no-properties (- end 1) end)))
      (and (string= start-char end-char)
           (string-match-p "[\"'`]" start-char)
           (not (string= start-char new-quote-char))))))

(defun my/replace-region-quotes (char)
  (interactive)
  (if (my/quoted-region-active-p char)
      (save-excursion
        (goto-char (region-beginning))
        (delete-char 1)
        (insert char)
        (goto-char (- (region-end) 1))
        (delete-char 1)
        (insert char))
    (self-insert-command 1 (aref char 0))))

(defun my/double-quotes ()
  (interactive)
  (my/replace-region-quotes "\""))

(defun my/single-quotes ()
  (interactive)
  (my/replace-region-quotes "'"))

(defun my/backtick-quotes ()
  (interactive)
  (my/replace-region-quotes "`"))

(defun my/wrap-jsx-element ()
  (interactive)
  (jtsx-wrap-in-jsx-element ""))

(provide 'setup-web)
