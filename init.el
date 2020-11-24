;; pre-init
(setq pre-init-file
      (expand-file-name "pre-init.el" user-emacs-directory))
(load-file pre-init-file)

;; about-me
(setq about-me-file
      (expand-file-name "about-me.el" user-emacs-directory))
(load-file about-me-file)

;; load all the configs
(mapc 'load (file-expand-wildcards (expand-file-name "config/*.el" user-emacs-directory)))
