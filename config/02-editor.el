;; Enable line number
(when (version<= "26.0.50" emacs-version )
  (global-display-line-numbers-mode))

;; Show fill-column
(global-display-fill-column-indicator-mode t)

;; Default modes
(setq-default
 auto-fill-function 'do-auto-fill
 adaptive-wrap-prefix-mode 1
 visual-line-mode 1
 ;; visual-line-fill-column-mode 1
 ;; visual-fill-mode 1
 )

;; Pulse highlight current line on switch window
(defun pulse-line (&rest _)
  "Pulse the current line."
  (pulse-momentary-highlight-one-line (point)))

(dolist (command '(scroll-up-command scroll-down-command
                                     recenter-top-bottom other-window))
  (advice-add command :after #'pulse-line))

;; Line comments
(global-set-key (kbd "s-/") 'comment-line)

;; sort and remove duplicates keybinding
(global-set-key (kbd "s-[") 'sort-lines)
(global-set-key (kbd "s-]") 'delete-duplicate-lines)

;; Transparency
(if (bound-and-true-p transparent-windows-mode)
    (progn
      (set-frame-parameter (selected-frame) 'alpha '(85 . 90))
      (add-to-list 'default-frame-alist '(alpha . (85 . 90)))))

;; whitespace cleanup on save
(add-hook 'before-save-hook 'whitespace-cleanup)

;; Add prompt indicator to `completing-read-multiple'.
;; Alternatively try `consult-completing-read-multiple'.
(defun crm-indicator (args)
  (cons (concat "[CRM] " (car args)) (cdr args)))
(advice-add #'consult-completing-read-multiple :filter-args #'crm-indicator)

;; Do not allow the cursor in the minibuffer prompt
(setq minibuffer-prompt-properties
      '(read-only t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

;; Emacs 28: Hide commands in M-x which do not work in the current mode.
;; Vertico commands are hidden in normal buffers.
(setq read-extended-command-predicate #'command-completion-default-include-p)

;; Enable recursive minibuffers
(setq enable-recursive-minibuffers t)

;; load the themes
(let ((basedir (expand-file-name "themes/" user-emacs-directory)))
  (dolist (f (directory-files basedir))
    (if (and (not (or (equal f ".") (equal f "..")))
             (file-directory-p (concat basedir f)))
        (add-to-list 'custom-theme-load-path (concat basedir f)))))

(load-theme 'uwu t t)
(enable-theme 'uwu)

;; Display README.org on start
(setq initial-buffer-choice (expand-file-name "README.org" user-emacs-directory))

;; Guess the major mode from the file name
(setq-default major-mode
              (lambda () ; guess major mode from file name
                (unless buffer-file-name
                  (let ((buffer-file-name (buffer-name)))
                    (set-auto-mode)))))

;; Require a confirmation before closing Emacs
(setq confirm-kill-emacs #'yes-or-no-p)

;; Resize frame and window pixel-wise (instead of character-wise)
;; (setq window-resize-pixelwise t)
;; (setq frame-resize-pixelwise t)

;; Textsize
;; (setq textsize--point-size 16)
;; (global-set-key (kbd "C-=") 'textsize-increment)
;; (global-set-key (kbd "C--") 'textsize-decrement)
;; (global-set-key (kbd "C-0") 'textsize-reset)

;; Remember what files were last opened
(recentf-mode t)

;; Abbreviate all yes-or-no queries
(defalias 'yes-or-no #'y-or-n-p)

;; Disable splash screen
(setq inhibit-startup-screen t)

;; Enable line numbering by default
(global-display-line-numbers-mode t)

;; Automatically pair parentheses
(electric-pair-mode t)

;; Finally, re-require better-defaults to load it on top of our configurations
(cond ((locate-library "better-defaults")
       (require 'better-defaults)))

;; Need to define the custom config file path as better-defaults override it
(setq-default custom-file (expand-file-name "config/custom.el"
                                            user-emacs-directory))

;; Org inline images
(setq org-display-inline-images t)
(setq org-redisplay-inline-images t)
(setq org-startup-with-inline-images "inlineimages")

;; Disable mouse-wheel text zoom
(global-unset-key (kbd "<pinch>"))
(global-set-key (kbd "<C-wheel-up>") 'ignore)
(global-set-key (kbd "<C-wheel-down>") 'ignore)

;; Upgrade package default by default
(setq package-install-upgrade-built-in t)
