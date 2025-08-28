(use-package crux
  :ensure t)

;; Paste rectangle as normal region, pushing the existing content instead of interleaving.
;; (as opposed to yank-rectangle, which is bound to C-x r y)
(global-set-key (kbd "C-x r C-y") #'my/insert-recangle-push-lines)

;; Kill word or selected region
(global-set-key (kbd "C-w") #'my/kill-region-or-backward-word)

;; Kill whole line
(global-set-key (kbd "C-S-k") #'crux-kill-whole-line)

;; Complement to transpose-sexps (C-M-t)
(global-set-key (kbd "C-M-y") #'my/reverse-transpose-sexps)

;; Contract selection marked with mark-sexp
(global-set-key (kbd "C-S-M-SPC") (lambda ()
                                    (interactive)
                                    (mark-sexp -1 t)))

;; Smarter move to beginning of line
(global-set-key (kbd "C-a") #'crux-move-beginning-of-line)

;; Move lines up and down
(global-set-key (kbd "C-S-<up>") #'my/move-line-up)
(global-set-key (kbd "C-S-p") #'my/move-line-up)
(global-set-key (kbd "C-S-<down>") #'my/move-line-down)
(global-set-key (kbd "C-S-n") #'my/move-line-down)

;; Open lines above and below
(global-set-key (kbd "C-o") #'crux-smart-open-line)
(global-set-key (kbd "C-S-o") #'crux-smart-open-line-above)

;; Duplicate line/region
(global-set-key (kbd "C-c d") #'crux-duplicate-current-line-or-region)
(global-set-key (kbd "C-c C-d") #'crux-duplicate-and-comment-current-line-or-region)

;; Join lines - M-^ is built in and joins with the previous line, the following joins the next line with this
(global-set-key (kbd "C-^") 'crux-top-join-line)
(global-set-key (kbd "M-J") 'crux-top-join-line)

;; Cleanup and reindent buffer
(global-set-key (kbd "C-c <tab>") #'my/cleanup-buffer)

;; Comments
(global-set-key (kbd "C-x ;") 'comment-dwim)

;; Hide/show element
(global-set-key (kbd "s-.") 'hs-toggle-hiding)
(global-set-key (kbd "s->") 'hs-show-all)
(global-set-key (kbd "s-<") 'hs-hide-all)

;; Add empty lines above/below (like vim-unimpared's "[-SPC" and "]-SPC")
(global-set-key (kbd "C-c C-SPC") #'my/insert-line-above)
(global-set-key (kbd "C-c SPC") #'my/insert-line-below)

;; Mark word using M-S-SPC, which is a lot easier thant M-@
;; (and mark-sexp is C-M-@ and C-M-SPC)
(global-set-key (kbd "M-S-SPC") 'mark-word)

;; Rebind cycle-spacing from M-SPC to s-SPC, leaving room for autcomplete
(global-set-key (kbd "s-SPC") 'cycle-spacing)

;; Autocomplete
(global-set-key (kbd "M-SPC") 'completion-at-point)
(global-set-key (kbd "M-/") 'hippie-expand-no-case-fold)
(global-set-key (kbd "C-M-/") 'hippie-expand-lines)

;; Reveal file in dired
(global-set-key (kbd "C-x C-j") 'dired-jump)

;; Find file at point
(global-set-key (kbd "C-c C-f") 'find-file-at-point)

;; Save project buffers
(global-set-key (kbd "C-x p s") (lambda ()
                                  (interactive)
                                  (save-some-buffers t 'save-some-buffers-root)))

;; Jump to related file, e.g. between src and test files.
;; The file pairings are configured per mode. See example in `setup-clojure'.
(global-set-key (kbd "s-t") 'significant-other-jump)

;; Toggle menu bar
(global-set-key (kbd "M-<f1>") 'menu-bar-mode)

;; Window splitting / sizing
(global-set-key (kbd "M-\\") 'my/split-window-right)
(global-set-key (kbd "s-\\") 'my/split-window-below)
(global-set-key (kbd "M-|") 'my/toggle-window-split)
(global-set-key (kbd "C-x C-\\") 'ace-swap-window)
(global-set-key (kbd "s-f") #'my/fullscreen)

;; Casing
(global-set-key (kbd "M-c") 'capitalize-dwim)
(global-set-key (kbd "M-l") 'downcase-dwim)
(global-set-key (kbd "M-u") 'upcase-dwim)

;; Selection / subword mode
(global-set-key (kbd "<f10>") 'global-subword-mode)
(global-set-key (kbd "<f11>") 'global-display-line-numbers-mode)

;; Documentation
(global-set-key (kbd "s-i") 'eldoc)
(global-set-key (kbd "M-i") 'eldoc-box-help-at-point)

;; Imenu replacement
(global-set-key (kbd "C-,") 'breadcrumb-jump)

;; Flymake errors
(global-set-key (kbd "s-{") 'flymake-goto-prev-error)
(global-set-key (kbd "s-}") 'flymake-goto-next-error)

;; Font size
(global-set-key (kbd "s-=") 'my/inc-font-size)
(global-set-key (kbd "s--") 'my/dec-font-size)
(global-set-key (kbd "s-0") 'my/reset-font-size)

;; Toggle theme
(global-set-key (kbd "<f12>") 'modus-themes-toggle)

;; Org
(global-set-key (kbd "s-c") 'org-capture)
(global-set-key (kbd "s-a") 'org-agenda)

;; Numbers
(global-set-key (kbd "C-c -") 'dec-number-at-point)
(global-set-key (kbd "C-c =") 'inc-number-at-point)

(defun my/change-font-size (step)
  (custom-set-faces `(default ((t (:height ,(+ (face-attribute 'default :height) step)))))))

(defun my/inc-font-size ()
  (interactive)
  (my/change-font-size 10))

(defun my/dec-font-size ()
  (interactive)
  (my/change-font-size -10))

(defun my/reset-font-size ()
  (interactive)
  (custom-set-faces '(default ((t (:height 150))))))

;; Borrowed from https://github.com/magnars/.emacs.d/
(defun my/kill-region-or-backward-word ()
  (interactive)
  (if (region-active-p)
      (kill-region (region-beginning) (region-end))
    (backward-kill-word 1)))

;; Lifted from https://emacs.stackexchange.com/questions/12799/move-form-up-and-down-on-paredit-mode
(defun my/reverse-transpose-sexps (arg)
  (interactive "*p")
  (transpose-sexps (- arg)))

(defun my/insert-line-above ()
  (interactive)
  (if (eq (point) (line-beginning-position))
      (insert "\n")
    (save-excursion
      (move-beginning-of-line nil)
      (insert "\n")))
  (scroll-up-line))

(defun my/insert-line-below ()
  (interactive)
  (save-excursion (crux-smart-open-line nil)))

(defun my/cleanup-buffer ()
  (interactive)
  (save-excursion (mark-whole-buffer)
                  (crux-cleanup-buffer-or-region)))

(defun my/move-line-up ()
  (interactive)
  (transpose-lines 1)
  (forward-line -2))

(defun my/move-line-down ()
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1))

;; Source: https://github.com/magnars/.emacs.d
(defun my/toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))

;; Source: https://emacs.stackexchange.com/questions/19461/insert-lines-when-yanking-rectangle-rather-than-inserting-among-following-lines
(defun my/insert-recangle-push-lines ()
  (interactive)
  (narrow-to-region (point) (mark))
  (yank-rectangle)
  (widen))

;; Source: https://github.com/magnars/emacsd-reboot
(defun my/split-window-right ()
  (interactive)
  (split-window-right)
  (windmove-right))

(defun my/split-window-below ()
  (interactive)
  (split-window-below)
  (windmove-down))

(defun my/fullscreen ()
  (interactive)
  (if-let ((r (get-register ?w)))
      (progn
        (set-register ?w nil)
        (set-window-configuration r))
    (progn
      (set-register ?w (current-window-configuration))
      (delete-other-windows))))

;; Source: https://github.com/magnars/emacsd-reboot
(defun incs (s &optional num)
  (let* ((inc (or num 1))
         (new-number (number-to-string (+ inc (string-to-number s))))
         (zero-padded? (s-starts-with? "0" s)))
    (if zero-padded?
        (s-pad-left (length s) "0" new-number)
      new-number)))

(defun goto-closest-number ()
  (interactive)
  (let ((closest-behind (save-excursion (search-backward-regexp "[0-9]" nil t)))
        (closest-ahead (save-excursion (search-forward-regexp "[0-9]" nil t))))
    (push-mark)
    (goto-char
     (cond
      ((and (not closest-ahead) (not closest-behind)) (error "No numbers in buffer"))
      ((and closest-ahead (not closest-behind)) closest-ahead)
      ((and closest-behind (not closest-ahead)) closest-behind)
      ((> (- closest-ahead (point)) (- (point) closest-behind)) closest-behind)
      ((> (- (point) closest-behind) (- closest-ahead (point))) closest-ahead)
      :else closest-ahead))))

(defun inc-number-at-point (arg)
  (interactive "p")
  (unless (or (looking-at "[0-9]")
              (looking-back "[0-9]"))
    (goto-closest-number))
  (save-excursion
    (while (looking-back "[0-9]")
      (forward-char -1))
    (re-search-forward "[0-9]+" nil)
    (replace-match (incs (match-string 0) arg) nil nil)))

(defun dec-number-at-point (arg)
  (interactive "p")
  (inc-number-at-point (- arg)))


(provide 'setup-keybindings)
