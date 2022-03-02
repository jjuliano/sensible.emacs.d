;; about-me
(setq user-full-name ""
      user-mail-address "")

;; org directory
(setq org-directory "~/Documents/Emacs/org")
(setq org-mobile-directory (expand-file-name "~/Dropbox/Apps/MobileOrg"))

;; config
(load (expand-file-name "config.el" user-emacs-directory))
