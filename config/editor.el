;; Enable line number
(when (version<= "26.0.50" emacs-version )
  (global-display-line-numbers-mode))

;; Show fill-column
(setq-default fill-column 80)
(global-display-fill-column-indicator-mode)

;; Turn-on auto-fill mode
(setq-default auto-fill-function 'do-auto-fill)

;; Multi-line edit keybindings
(global-set-key (kbd "C-c C-SPC") 'mulled/edit-leading-edges)
(global-set-key (kbd "C-c M-SPC") 'mulled/edit-trailing-edges)

;; Pulse highlight current line on switch window
(defun pulse-line (&rest _)
  "Pulse the current line."
  (pulse-momentary-highlight-one-line (point)))

(dolist (command '(scroll-up-command scroll-down-command
                                     recenter-top-bottom other-window))
  (advice-add command :after #'pulse-line))

;; Line comments
(global-set-key (kbd "s-/") 'comment-line)

;; Transparency
 (set-frame-parameter (selected-frame) 'alpha '(85 . 50))
 (add-to-list 'default-frame-alist '(alpha . (85 . 50)))
