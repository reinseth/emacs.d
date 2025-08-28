(use-package diminish
  :ensure t)

(use-package which-key
  :ensure t
  :diminish which-key-mode
  :config
  (setq which-key-separator " ")
  (setq which-key-show-docstrings t)
  (setq which-key-max-description-length nil)
  (which-key-mode))

;; Reload buffers when files are changed from the outside
(use-package autorevert
  :config
  (global-auto-revert-mode 1))

;; Use user's path variable
(use-package exec-path-from-shell
  :ensure t
  :config
  (setq exec-path-from-shell-arguments nil)
  (exec-path-from-shell-initialize)

  ;; See https://emacs-lsp.github.io/lsp-mode/page/performance/#use-plists-for-deserialization
  (add-to-list 'exec-path-from-shell-variables "LSP_USE_PLISTS"))

;; Remove useless suspend frame bindings
(global-unset-key (kbd "C-x C-z"))
(global-unset-key (kbd "C-z"))

;; UTF-8 everywhere
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; Keep clipboard entries from external programs in the kill ring
(setq save-interprogram-paste-before-kill t)

;; No tabs
(setq-default indent-tabs-mode nil)

;; Backup directory
(unless backup-directory-alist
  (setq backup-directory-alist `(("." . ,(concat user-emacs-directory "backups")))))
;; Delete old backups
(setq delete-old-versions t)

;; Autosaves directory
(let ((my-autosave-dir (expand-file-name "autosave" user-emacs-directory)))
  (unless (file-exists-p my-autosave-dir)
    (make-directory my-autosave-dir))
  (setq auto-save-file-name-transforms
        `((".*" ,(file-name-as-directory my-autosave-dir) t))))


;; Changes all yes/no questions to y/n
(defalias 'yes-or-no-p 'y-or-n-p)

;; Don't create ~ files
(setq create-lockfiles nil)

;; Save history of minibuffer
(savehist-mode)

;; Fix archaic defaults
(setopt sentence-end-double-space nil)

;; Show keystrokes in progress
(setq echo-keystrokes 0.1)

;; Save recent files
(use-package recentf
  :config
  (setq recentf-max-menu-items 100)
  (setq recentf-max-saved-items 100)
  (recentf-mode 1))

;; Pop the mark repeatedly with C-u C-SPC C-SPC C-SPC ...
(setq set-mark-command-repeat-pop t)

;; Simple listing in dired by default
(add-hook 'dired-mode-hook (lambda () (dired-hide-details-mode)))

;; Replace selection with typed text
(delete-selection-mode 1)

;; Make switching buffers more consistent
(setopt switch-to-buffer-obey-display-actions t)

;; Mouse scrolling
(setopt mouse-wheel-tilt-scroll t)
(setopt mouse-wheel-flip-direction t)

;; Disable annoying mouse highlighting
(setopt mouse-highlight nil)

;; Use the minibuffer whilst in the minibuffer
(setopt enable-recursive-minibuffers t)

;; Use tab for indentation, not completion
(setopt tab-always-indent t)

;; TAB acts more like how it does in the shell
(keymap-set minibuffer-mode-map "TAB" 'minibuffer-complete)

;; Easier repetitions! E.g. navigating results with "M-g n" / "M-g p" can be done with "M-g n n n p n ..."
;; Good read on repeat-mode: https://karthinks.com/software/it-bears-repeating/
(repeat-mode 1)

;; Subword mode everywhere
(global-subword-mode 1)

;; No word-wrapping in programming modes
(add-hook 'prog-mode-hook (lambda () (setq truncate-lines t)))

;; Autospelling and fill mode in text modes
(add-hook 'text-mode-hook (lambda ()
                            (auto-fill-mode 1)
                            (flyspell-mode 1)))

;; Mac settings
(when (eq system-type 'darwin)
  (setq mac-option-modifier 'super)
  (setq mac-right-option-modifier nil)
  (setq mac-command-modifier 'meta))

(provide 'setup-defaults)
