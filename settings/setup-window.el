;; Keep splits balanced
(use-package balanced-windows
  :ensure t
  :config
  (balanced-windows-mode 1))

;; Enhanced window navigation
(use-package ace-window
  :ensure t
  :bind (("M-o" . ace-window)
         ("M-O" . aw-flip-window))
  :custom
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (aw-dispatch-always t)
  (aw-dispatch-alist
   '((?x aw-delete-window "Delete Window")
     (?m aw-swap-window "Swap Windows")
     (?M aw-move-window "Move Window")
     (?c aw-copy-window "Copy Window")
     (?n aw-flip-window)
     (?u aw-switch-buffer-other-window "Switch Buffer Other Window")
     (?e aw-execute-command-other-window "Execute Command Other Window")
     (?v aw-split-window-vert "Split Vert Window")
     (?b aw-split-window-horz "Split Horz Window")
     (?o delete-other-windows "Delete Other Windows")
     (?? aw-show-dispatch-help))))

;; Window navigation using super+arrow
(use-package windmove
  :config
  (windmove-default-keybindings 'super)
  (windmove-swap-states-default-keybindings))

;; Undo/redo window changes with C-c <left> / C-c <right>
(use-package winner
  :config
  (winner-mode 1))

;; Window workspaces like i3
(use-package eyebrowse
  :ensure t
  :bind (("s-1" . eyebrowse-switch-to-window-config-1)
         ("s-2" . eyebrowse-switch-to-window-config-2)
         ("s-3" . eyebrowse-switch-to-window-config-3)
         ("s-4" . eyebrowse-switch-to-window-config-4)
         ("s-5" . eyebrowse-switch-to-window-config-5)
         ("s-6" . eyebrowse-switch-to-window-config-6)
         ("s-7" . eyebrowse-switch-to-window-config-7)
         ("s-8" . eyebrowse-switch-to-window-config-8)
         ("s-9" . eyebrowse-switch-to-window-config-9))
  :config
  (add-hook 'eyebrowse-pre-window-switch-hook 'my/eyebrowse-pre-window-switch-hook)
  (setq eyebrowse-new-workspace 'my/open-workspace-with-current-buffer)
  (eyebrowse-mode 1))

(defvar my/eyebrowse-initiator-buffer nil)

(defun my/eyebrowse-pre-window-switch-hook ()
  (setq my/eyebrowse-initiator-buffer (buffer-name)))

(defun my/open-workspace-with-current-buffer ()
  (switch-to-buffer (get-buffer-create my/eyebrowse-initiator-buffer)))

(defun my/fit-window-or-max-half (window)
  (fit-window-to-buffer window (floor (frame-height) 2)))

;; Window layout configuration
(setq display-buffer-alist
      '(("\\*Help\\*\\|\\*Apropos\\*\\|\\*Warnings\\*\\|\\*eldoc\\*"
         (display-buffer-reuse-window
          display-buffer-below-selected)
         (window-height . my/fit-window-or-max-half)
         (body-function . select-window)
         )

        ((derived-mode . occur-mode)
         (display-buffer-reuse-window
          display-buffer-below-selected)
         (window-height . my/fit-window-or-max-half)
         (body-function . select-window))

        ((derived-mode . lsp-help-mode)
         (display-buffer-reuse-window
          display-buffer-below-selected)
         (window-height . my/fit-window-or-max-half)
         (body-function . select-window)))
      )

;; Fix annoying vertical window splitting.
;; https://lists.gnu.org/archive/html/help-gnu-emacs/2015-08/msg00339.html
(with-eval-after-load "window"
  (defcustom split-window-below nil
    "If non-nil, vertical splits produce new windows below."
    :group 'windows
    :type 'boolean)

  (defcustom split-window-right nil
    "If non-nil, horizontal splits produce new windows to the right."
    :group 'windows
    :type 'boolean)

  (fmakunbound #'split-window-sensibly)

  (defun split-window-sensibly
      (&optional window)
    (setq window (or window (selected-window)))
    (or (and (window-splittable-p window t)
             ;; Split window horizontally.
             (split-window window nil (if split-window-right 'left  'right)))
        (and (window-splittable-p window)
             ;; Split window vertically.
             (split-window window nil (if split-window-below 'above 'below)))
        (and (eq window (frame-root-window (window-frame window)))
             (not (window-minibuffer-p window))
             ;; If WINDOW is the only window on its frame and is not the
             ;; minibuffer window, try to split it horizontally disregarding the
             ;; value of `split-width-threshold'.
             (let ((split-width-threshold 0))
               (when (window-splittable-p window t)
                 (split-window window nil (if split-window-right
                                              'left
                                            'right))))))))

(setq-default split-height-threshold  4
              split-width-threshold   160) ; the reasonable limit for horizontal splits

(provide 'setup-window)
