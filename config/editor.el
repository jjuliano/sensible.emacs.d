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
