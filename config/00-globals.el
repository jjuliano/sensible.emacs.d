;; fill-column
(setq-default fill-column 80)

;; transparent-window mode
(setq-default transparent-windows-mode nil)

;; about-me
(setq-default user-full-name ""
              user-mail-address "")

;; org directory
(setq-default org-directory "~/Documents/Emacs/org")
(setq-default org-mobile-directory (expand-file-name "~/Dropbox/Apps/MobileOrg"))

;; zoom-mode
(setq-default use-zoom-mode t)

;; language-tool
(setq-default langtool-language-tool-jar (expand-file-name
                                          "data/languagetool-commandline.jar"
                                          user-emacs-directory))
(setq-default langtool-default-language "en-US")
