;; Enable line number
(when (version<= "26.0.50" emacs-version )
  (global-display-line-numbers-mode))

;; MELPA config file
(setq-default custom-file (expand-file-name "config/custom.el"
                                            user-emacs-directory))

;; Show fill-column
(global-display-fill-column-indicator-mode)

;; Turn-on auto-fill mode
(setq-default auto-fill-function 'do-auto-fill)

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
(if (bound-and-true-p transparent-windows-mode)
    (progn
      (set-frame-parameter (selected-frame) 'alpha '(85 . 90))
      (add-to-list 'default-frame-alist '(alpha . (85 . 90)))))

;; whitespace cleanup on save
(add-hook 'before-save-hook 'whitespace-cleanup)

;; load the theme
(add-to-list 'custom-theme-load-path (expand-file-name "themes/nord-emacs/" user-emacs-directory))
(load-theme 'nord t)

;; Display README.org on start
(setq initial-buffer-choice (expand-file-name "README.org" user-emacs-directory))
