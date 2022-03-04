;; load all your pre-init personal configs
(mapc 'load (file-expand-wildcards (expand-file-name "personal/pre-*.el" user-emacs-directory)))

;; load all the configs
(mapc 'load (file-expand-wildcards (expand-file-name "config/*.el" user-emacs-directory)))

;; load all your post-init personal configs
(mapc 'load (file-expand-wildcards (expand-file-name "personal/*.el" user-emacs-directory)))
