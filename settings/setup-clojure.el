(use-package clojure-mode-extra-font-locking
  :ensure t)

(use-package cider
  :ensure t
  :config
  ;; When there's a cider error, show its buffer and switch to it
  (setq cider-show-error-buffer t)
  (setq cider-auto-select-error-buffer t)

  ;; Where to store the cider history.
  (setq cider-repl-history-file "~/.emacs.d/cider-history")

  ;; Wrap when navigating history.
  (setq cider-repl-wrap-history t)

  ;; Don't prompt for symbol
  (setq cider-prompt-for-symbol nil)

  ;; Autosave on load
  (setq cider-save-file-on-load t)

  ;; Eval top level forms inside comment blocks
  (setq clojure-toplevel-inside-comment-form t))

(use-package kaocha-runner
  :ensure t
  ;; TODO
  )

(use-package flymake-kondor
  :ensure t
  :hook (clojure-mode . flymake-kondor-setup))

(use-package clojure-mode
  :ensure t
  :mode (("\\.clj\\'" . clojure-mode)
         ("\\.cljc\\'" . clojure-mode)
         ("\\.edn\\'" . clojure-mode)
         ("\\.cljs\\'" . clojurescript-mode))
  :config
  (put-clojure-indent 'match 1))

(use-package clj-refactor
  :ensure t
  :hook (clojure-mode clojurescript-mode)
  :config
  (cljr-add-keybindings-with-prefix "C-c C-m")
  ;; Don't prompt to analyze when renaming symbol
  (setq cljr-warn-on-eval nil)

  ;; Formatting of ns form
  (setq cljr-insert-newline-after-require nil)
  )

(provide 'setup-clojure)
