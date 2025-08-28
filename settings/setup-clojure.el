(require 's)

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

  ;; Use LSP for xref
  (setq cider-use-xref nil)

  ;; Eval top level forms inside comment blocks
  (setq clojure-toplevel-inside-comment-form t))

;; Copied from https://github.com/magnars/emacsd-reboot/blob/main/packages/setup-kaocha-runner.el
(use-package kaocha-runner
  :after cider-mode
  :commands (kaocha-runner--run-tests)
  :hook (cider-file-loaded . kaocha-runner-run-relevant-tests)
  :bind (:map clojure-mode-map
              ("C-c k t" . kaocha-runner-run-test-at-point)
              ("C-c k r" . kaocha-runner-run-tests)
              ("C-c k a" . kaocha-runner-run-all-tests)
              ("C-c k w" . kaocha-runner-show-warnings)
              ("C-c k h" . kaocha-runner-hide-windows)))

(use-package flymake-kondor
  :ensure t
  :hook (clojure-mode . flymake-kondor-setup))

(use-package clojure-mode
  :ensure t
  :hook ((clojure-mode . my/setup-clojure-mode)
         (clojurec-mode . my/setup-clojure-mode)
         (clojurescript-mode . my/setup-clojure-mode))
  :config
  (put-clojure-indent 'match 1)
  (put-clojure-indent 'into :defn))

(use-package clj-refactor
  :ensure t
  :hook (clojure-mode clojurescript-mode)
  :config
  (cljr-add-keybindings-with-prefix "C-c C-m")
  ;; Don't prompt to analyze when renaming symbol
  (setq cljr-warn-on-eval nil)

  ;; Formatting of ns form
  (setq cljr-insert-newline-after-require nil))

(defun my/setup-clojure-mode ()
  (eglot-ensure)
  (with-significant-others f
    ("test/.*_test\\.clj" (list (s-with f
                           (s-replace "/test/" "/src/")
                           (s-replace "_test.clj" ".clj"))))
    ("src/.*\\.clj" (list (s-with f
                      (s-replace "/src/" "/test/")
                      (s-replace ".clj" "_test.clj"))))))

;; Kaocha config copied from https://github.com/magnars/emacsd-reboot/blob/main/packages/setup-kaocha-runner.el
(defun kaocha-runner--is-test? (s)
  (string-match-p "_test.clj" s))

(defun kaocha-runner--significant-other-find-existing-test ()
  (--first
   (and (file-exists-p it)
        (kaocha-runner--is-test? it))
   (funcall significant-other-find-fn)))

(defun kaocha-runner-run-relevant-tests ()
  (interactive)
  (when (cljr--project-depends-on-p "kaocha")
    (if (kaocha-runner--is-test? (buffer-file-name))
        (kaocha-runner--run-tests
         (kaocha-runner--testable-sym (cider-current-ns) nil nil)
         nil) t
      (let ((original-buffer (current-buffer)))
        (save-window-excursion
          (when-let ((file (kaocha-runner--significant-other-find-existing-test)))
            (find-file file)
            (kaocha-runner--run-tests
             (kaocha-runner--testable-sym (cider-current-ns) nil nil)
             nil t original-buffer)))))))

(provide 'setup-clojure)
