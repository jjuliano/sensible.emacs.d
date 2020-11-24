;; load all the 3rd party packages
(let ((default-directory (expand-file-name "pkgs/" user-emacs-directory)))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))

;; no-littering settings to keep .emacs.d clean
(setq no-littering-etc-directory
      (expand-file-name "~/Documents/Emacs/config/"))
(setq no-littering-var-directory
      (expand-file-name "~/Documents/Emacs/data/"))
(setq auto-save-file-name-transforms
      `((".*" ,(expand-file-name "auto-save/" no-littering-var-directory) t)))
;; melpa/elpa config
(setq package-user-dir (expand-file-name "packages/" no-littering-var-directory))
(require 'recentf)
(add-to-list 'recentf-exclude no-littering-var-directory)
(add-to-list 'recentf-exclude no-littering-etc-directory)
(require 'no-littering)
(setq custom-file (expand-file-name "config/custom.el" user-emacs-directory))

;; exec-path-from-shell settings to load $PATH on run
(require 'exec-path-from-shell)
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

(require 'better-defaults)
(require 'backup-each-save)
(require 'multiple-line-edit)

;; zoom auto-resize window
(setq zoom-size '(0.618 . 0.618))
(require 'zoom)
(zoom-mode t)

;; workspaces management via perspective-el
(setq persp-state-default-file "~/Documents/Emacs/data/persp-state-file")
(require 'perspective)
(persp-mode)
(add-hook 'kill-emacs-hook #'persp-state-save)

(defvar persp-switch-prefix "C-M-%d")
(defvar persp-first-perspective "0")
(defvar persp-top-perspective "0")
(defvar persp-bottom-perspective "5")

(defun persp-setup ()
  (mapc (lambda (i)
          (persp-switch (int-to-string i))
          (global-set-key (kbd (format persp-switch-prefix i))
                          `(lambda ()
                             (interactive)
                             (persp-switch ,(int-to-string i)))))
        (number-sequence (string-to-number persp-top-perspective)
                         (string-to-number persp-bottom-perspective)))
  (persp-switch persp-first-perspective)
  (persp-kill "main"))

(add-hook 'persp-state-after-load-hook 'persp-setup)
(add-hook 'after-init-hook 'persp-setup)
