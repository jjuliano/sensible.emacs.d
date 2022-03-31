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
