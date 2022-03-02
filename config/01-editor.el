;; Enable line number
(when (version<= "26.0.50" emacs-version )
  (global-display-line-numbers-mode))

;; Show fill-column
(setq-default fill-column 80)
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
      (set-frame-parameter (selected-frame) 'alpha '(85 . 50))
      (add-to-list 'default-frame-alist '(alpha . (85 . 50)))))

;; whitespace cleanup on save
(add-hook 'before-save-hook 'whitespace-cleanup)

;; minted for syntax highlighting on PDF exports
(setq org-latex-prefer-user-labels t)
(setq org-latex-listings
      'minted
      org-latex-packages-alist
      '(("" "minted"))
      org-latex-pdf-process
      '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

;; load the theme
(add-to-list 'custom-theme-load-path (expand-file-name "themes/nord-emacs/" user-emacs-directory))
(load-theme 'nord t)
