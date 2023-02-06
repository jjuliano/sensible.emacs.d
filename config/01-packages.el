;; package cl is deprecated in Emacs-27
(setq byte-compile-warnings '(cl-functions))

;; load all the 3rd party packages
(let ((default-directory (expand-file-name "pkgs/" user-emacs-directory)))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))

(cond ((locate-library "no-littering")
       (progn
         ;; no-littering settings to keep .emacs.d clean
         (setq no-littering-etc-directory
               (expand-file-name "var/config/" user-emacs-directory))
         (setq no-littering-var-directory
               (expand-file-name "var/data/" user-emacs-directory))

         (setq auto-save-file-name-transforms
               `((".*" ,(expand-file-name "auto-save/" no-littering-var-directory) t)))

         ;; set recent files to no-littering dir
         (cond ((locate-library "recentf")
                (require 'recentf)
                (add-to-list 'recentf-exclude no-littering-var-directory)
                (add-to-list 'recentf-exclude no-littering-etc-directory)))

         ;; melpa/elpa config
         (setq package-user-dir (expand-file-name "pkgs/"
                                                  no-littering-var-directory))
         ;; workspaces management via perspective-el
         (cond ((locate-library "perspective")
                (setq persp-state-default-file (expand-file-name "persp-state-file"
                                                                 no-littering-var-directory))

                (require 'perspective)
                (global-set-key (kbd "C-x C-b") 'persp-list-buffers)
                (customize-set-variable 'persp-mode-prefix-key (kbd "C-c M-p"))
                (persp-mode)
                (add-hook 'kill-emacs-hook #'persp-state-save)

                ;; perspective keybinding
                (defvar persp-switch-prefix "C-M-%d")
                (defvar persp-first-perspective "0")
                (defvar persp-top-perspective "0")
                (defvar persp-bottom-perspective "5")

                ;; perspective workspaces setup
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
                (add-hook 'after-init-hook 'persp-setup)))
         (require 'no-littering))))

;; company-mode
(cond ((locate-library "company")
       (progn
         (require 'company)
         (setq company-minimum-prefix-length 3)
         (global-company-mode +1))))

;; exec-path-from-shell settings to load $PATH on run
(cond ((locate-library "exec-path-from-shell")
       (progn
         (require 'exec-path-from-shell)
         (when (memq window-system '(mac ns x))
           (exec-path-from-shell-initialize)))))

;; better-defaults
(cond ((locate-library "better-defaults")
       (require 'better-defaults)))

;; backup-each-save
(cond ((locate-library "backup-each-save")
       (require 'backup-each-save)))

;; multiple-line edit
(cond ((locate-library "multiple-line-edit")
       (progn
         (require 'multiple-line-edit)

         (global-set-key (kbd "C-c C-SPC") 'mulled/edit-leading-edges)
         (global-set-key (kbd "C-c M-SPC") 'mulled/edit-trailing-edges))))

;; zoom auto-resize window
(if (bound-and-true-p use-zoom-mode)
    (cond ((locate-library "zoom")
           (progn
             (setq zoom-size '(0.618 . 0.618))
             (require 'zoom)
             (zoom-mode t)))))

;; unicode and emoji support

;; font-utils
(cond ((locate-library "font-utils")
       (require 'font-utils)))

;; ucs-utils
(cond ((locate-library "ucs-utils")
       (require 'ucs-utils)))

;; list-utils
(cond ((locate-library "list-utils")
       (require 'list-utils)))

;; unicode-fonts
(cond ((locate-library "unicode-fonts")
       (progn
         (require 'unicode-fonts)
         (unicode-fonts-setup))))

;; typescript IDE support
(cond ((locate-library "dash")
       (require 'dash)))

(cond ((locate-library "s")
       (require 's)))

;; Go IDE support
(cond ((locate-library "go-mode")
       (progn
         (require 'go-mode)

         (defun go-mode-setup ()
           ;; Use goimports instead of go-fmt
           (setq gofmt-command "goimports")
           ;; Call Gofmt before saving
           (add-hook 'before-save-hook 'gofmt-before-save)

           ;; Godef jump key binding
           (local-set-key (kbd "M-.") 'godef-jump)
           (local-set-key (kbd "M-*") 'pop-tag-mark)

           ;; Customize compile command to run go build
           (if (not (string-match "go" compile-command))
               (set (make-local-variable 'compile-command)
                    "go build -v && go test -v && go vet"))

           ;; auto-complete mode for Go
           (with-eval-after-load 'go-mode
             (require 'go-autocomplete))
           (auto-complete-mode 1))

         (add-hook 'go-mode-hook 'go-mode-setup))))

;; auto-complete
(cond ((locate-library "auto-complete")
       (progn
         (require 'auto-complete)
         (require 'auto-complete-config)
         (ac-config-default)
         (global-auto-complete-mode t))))

;; auto-complete on org
(cond ((locate-library "org-ac")
       (progn
         (require 'auto-complete-pcmp)
         (require 'log4e)
         (require 'yaxception)
         (require 'org-ac)
         (org-ac/config-default))))

;; dumb-jump
(cond ((locate-library "dumb-jump")
       (progn
         (require 'dumb-jump)
         (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))))

;; js2-mode
(cond ((locate-library "js2-mode")
       (progn
         (require 'js2-mode)
         (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
         (add-to-list 'interpreter-mode-alist '("node" . js2-mode)))))

;; web-mode
(cond ((locate-library "web-mode")
       (progn
         (require 'web-mode)
         (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
         (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
         (add-to-list 'auto-mode-alist '("\\.blade\\.php\\'" . web-mode))
         (add-to-list 'auto-mode-alist '("\\.[gj]sp\\'" . web-mode))
         (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
         (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
         (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
         (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
         (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
         (add-to-list 'auto-mode-alist '("\\.jinja?\\'" . web-mode))
         (add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))
         (add-to-list 'auto-mode-alist '("\\.mako$" . web-mode))
         (add-to-list 'web-mode-engine-file-regexps '("django" . "\\.html"))
         (add-to-list 'web-mode-engine-file-regexps '("django" . "\\.jinja"))
         (setq web-mode-engines-alist
               '(("django"  . "\\.jinja\\'")
                 ("django"  . "\\.djhtml\\'")
                 ("django"  . "\\.html\\'")
                 ("erb"     . "\\.erb\\'")
                 ("erb"     . "\\.rhtml\\'")
                 ("erb"     . "\\.ejs\\'")
                 ("php"     . "\\.phtml\\'")
                 ("php"     . "\\.php\\'")
                 ("php"     . "\\.psp\\'")
                 ("php"     . "\\.ctp\\'")
                 ("jsp"     . "\\.jsp\\'")
                 ("jsp"     . "\\.gsp\\'")
                 ("asp"     . "\\.asp\\'")
                 ("aspx"    . "\\.aspx\\'")
                 ("aspx"    . "\\.ascx\\'")
                 ("closure" . "\\.soy\\'")
                 ("lsp"     . "\\.lsp\\'")
                 ("mako"    . "\\.mako\\'")
                 ("blade"   . "\\.blade\\.")))
         (setq web-mode-markup-indent-offset 2)
         (setq web-mode-css-indent-offset 2)
         (setq web-mode-code-indent-offset 2)
         (setq web-mode-style-padding 1)
         (setq web-mode-script-padding 1)
         (setq web-mode-block-padding 0)
         (setq web-mode-comment-style 2)
         (setq web-mode-enable-auto-pairing t)
         (setq web-mode-enable-css-colorization t)
         (setq web-mode-enable-block-face t)
         (setq web-mode-enable-part-face t)
         (setq web-mode-enable-comment-interpolation t)
         (setq web-mode-enable-heredoc-fontification t)
         (setq web-mode-enable-current-element-highlight t)
         (setq web-mode-enable-current-column-highlight t)
         (setq web-mode-ac-sources-alist
               '(("css" . (ac-source-css-property))
                 ("html" . (ac-source-words-in-buffer ac-source-abbrev)))))))

;; load node-modules/bin paths
(cond ((locate-library "add-node-modules-path")
       (progn
         (eval-after-load 'js-mode
           '(add-hook 'js-mode-hook #'add-node-modules-path))
         (eval-after-load 'web-mode
           '(add-hook 'web-mode-hook #'add-node-modules-path))
         (eval-after-load 'js2-mode
           '(add-hook 'js2-mode-hook #'add-node-modules-path)))))

;; prettier-rc
(cond ((locate-library "prettier-rc")
       (progn
         (require 'prettier-rc)

         (add-hook 'typescript-mode-hook 'prettier-rc-mode)
         (add-hook 'js2-mode-hook 'prettier-rc-mode)
         (add-hook 'web-mode-hook 'prettier-rc-mode))))

;; eslint-rc
(cond ((locate-library "eslint-rc")
       (progn
         (require 'eslint-rc)

         (add-hook 'typescript-mode-hook 'eslint-rc-mode)
         (add-hook 'js2-mode-hook 'eslint-rc-mode)
         (add-hook 'web-mode-hook 'eslint-rc-mode))))

;; langtool
(cond ((locate-library "langtool")
       (progn
         (require 'langtool)
         (global-set-key (kbd "C-x 4w") 'langtool-check)
         (global-set-key (kbd "C-x 4W") 'langtool-check-done)
         (global-set-key (kbd "C-x 4l") 'langtool-switch-default-language)
         (global-set-key (kbd "C-x 44") 'langtool-show-message-at-point)
         (global-set-key (kbd "C-x 4c") 'langtool-correct-buffer))))

;; flycheck
(cond ((locate-library "flycheck")
       (progn
         (require 'flycheck)
         (add-hook 'after-init-hook 'global-flycheck-mode))))

;; flycheck-color-mode-line
(cond ((locate-library "flycheck-color-mode-line")
       (progn
         (require 'flycheck-color-mode-line)
         (add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode))))

;; alternative workspace management via persp-mode.el
;;(cond ((locate-library "persp-mode")
;;       (require 'persp-mode)
;;       (persp-mode 1)))

;; textsize-mode
(cond ((locate-library "textsize")
       (progn
         (require 'textsize)
         (textsize-mode +1))))

;; flyspell spell-checking
(cond ((locate-library "flyspell")
       (progn
         (require 'flyspell)
         (add-hook 'text-mode-hook 'flyspell-mode)
         (add-hook 'prog-mode-hook 'flyspell-prog-mode))))

;; flyspell-popup to display popup on wrong spelling words
(cond ((locate-library "flyspell-popup")
       (progn
         (require 'flyspell-popup)
         (define-key flyspell-mode-map (kbd "C-;") #'flyspell-popup-correct))))

;; markdown-mode
(cond ((locate-library "markdown-mode")
       (progn
         (require 'markdown-mode)
         (add-to-list 'auto-mode-alist
                      '("\\.\\(?:md\\|markdown\\|mkd\\|mdown\\|mkdn\\|mdwn\\)\\'"
                        . markdown-mode))
         (add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))
         (setq markdown-command "multimarkdown"))))

;; rainbow-delimiters mode
(cond ((locate-library "rainbow-delimiters")
       (progn
         (require 'rainbow-delimiters)
         (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))))

;; consult
(cond ((locate-library "consult")
       (require 'consult)
       (progn
         (add-hook 'completion-list-mode-hook #'consult-preview-at-point-mode)
         (defun consult-find-file (arg)
           (interactive "P")
           (call-interactively (if arg 'consult-file-externally 'find-file)))

         (define-key consult-narrow-map (vconcat consult-narrow-key "?")
           #'consult-narrow-help)

         (global-set-key (kbd "C-x C-f") 'consult-find-file)

         (consult-customize consult-mark :preview-key 'any)
         (add-hook 'completion-list-mode-hook #'consult-preview-at-point-mode))))

;; which-key-mode
(cond ((locate-library "which-key")
       (progn
         (require 'which-key)
         (which-key-mode +1))))

;; vertico-mode
(cond ((locate-library "vertico")
       (progn
         (require 'vertico)
         (vertico-mode +1)
         (with-eval-after-load 'vertico
           (require 'vertico-directory)

           (define-key vertico-map "\r" #'vertico-directory-enter)
           (define-key vertico-map "\d" #'vertico-directory-delete-char)
           (define-key vertico-map "\M-\d" #'vertico-directory-delete-word)
           (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)

           (setq completion-in-region-function
                 (lambda (&rest args)
                   (apply (if vertico-mode
                              #'consult-completion-in-region
                            #'completion--in-region)
                          args)))))))

;; marginalia-mode
(cond ((locate-library "marginalia")
       (progn
         (require 'marginalia)
         (marginalia-mode +1))))

;; embark
(cond ((locate-library "embark")
       (progn
         (require 'embark)
         (with-eval-after-load 'consult
           (with-eval-after-load 'embark
             (require 'embark-consult)
             (add-hook 'embark-collect-mode-hook #'consult-preview-at-point-mode))))))

;; orderless-mode
(cond ((locate-library "orderless")
       (require 'orderless)
       (setq completion-styles '(orderless)
             completion-category-defaults nil
             completion-category-overrides '((file (styles partial-completion))))))

;; savehist-mode
(cond((locate-library "savehist")
      (progn
        (require 'savehist)
        (savehist-mode))))

;; ag.el
(cond ((locate-library "ag")
       (require 'ag)))

;; minted for syntax highlighting on PDF exports
(cond ((locate-library "minted")
       (progn
         (setq org-latex-prefer-user-labels t)
         (setq org-latex-listings
               'minted
               org-latex-packages-alist
               '(("" "minted"))
               org-latex-pdf-process
               '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
                 "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
                 "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f")))))

;; ternjs
(cond ((locate-library "tern")
       (require 'tern)
       (add-hook 'js-mode-hook (lambda () (tern-mode t)))
       (eval-after-load 'tern
         '(progn
            (require 'tern-auto-complete)
            (tern-ac-setup)))))

;; tide typescript IDE for Emacs
(cond ((locate-library "tide")
       (progn
         (require 'tide)

         (defun setup-tide-mode ()
           (interactive)
           (tide-setup)
           (flycheck-mode +1)
           (setq flycheck-check-syntax-automatically '(save mode-enabled))
           (eldoc-mode +1)
           (tide-hl-identifier-mode +1)

           ;; use prettier-rc if found
           (if (locate-library "prettier-rc")
               (prettier-rc-mode +1)
             ;; use default tide formatter on sav
             (add-hook 'before-save-hook 'tide-format-before-save)))

         (setq company-tooltip-align-annotations t)

         (add-hook 'typescript-mode-hook #'setup-tide-mode)

         ;; for handling TSX and JSX files
         (require 'web-mode)

         (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
         (add-hook 'web-mode-hook
                   (lambda ()
                     (when (string-equal "tsx" (file-name-extension buffer-file-name))
                       (setup-tide-mode))))
         ;; enable typescript-tslint checker
         (flycheck-add-mode 'typescript-tslint 'web-mode)

         (add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
         (add-hook 'web-mode-hook
                   (lambda ()
                     (when (string-equal "jsx" (file-name-extension buffer-file-name))
                       (setup-tide-mode))))
         ;; configure jsx-tide checker to run after your default jsx checker
         (flycheck-add-mode 'javascript-eslint 'web-mode)
         (flycheck-add-next-checker 'javascript-eslint 'jsx-tide 'append))))

;; Magit
(cond ((locate-library "magit")
       (progn
       ;; Compat
       (cond ((locate-library "compat")
              (require 'compat)
              (require 'magit))))))

;; improve vterm color mode
(cond ((locate-library "eterm-256color")
       (progn
         (require 'eterm-256color)
         (add-hook 'term-mode-hook #'eterm-256color-mode))))

;; dependency of eterm-256color
(cond ((locate-library "xterm-color")
       (require 'xterm-color)))

;; vterm
(cond ((locate-library "vterm")
       (progn
         (setq vterm-term-environment-variable "eterm-color")
         (add-hook 'vterm-mode-hook
                   (lambda ()
                     (set (make-local-variable 'buffer-face-mode-face) 'fixed-pitch)
                     (buffer-face-mode t)))
         (require 'vterm))))

;; org-tempo
(cond ((locate-library "org-tempo")
       (require 'org-tempo)))

;; projectile
(cond ((locate-library "projectile")
       (progn
         (require 'projectile)
         (projectile-global-mode)
         (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
         (setq projectile-indexing-method 'hybrid)
         (setq projectile-sort-order 'recentf)
         (setq projectile-tags-backend 'standard)
         (setq projectile-tags-command (concat (format "ctags --options=\"%s\""
                                                       (expand-file-name "data/ctags"
                                                                         user-emacs-directory))
                                               " -Re -f \"%s\" %s \"%s\""))

         (with-eval-after-load 'projectile
           (defun projectile-relevant-known-projects ()
             "Return a list of known projects except the current one (if present)."
             (if (projectile-project-p)
                 (->> projectile-known-projects
                      (--reduce-from
                       (if (-contains? (-map 's-downcase acc) (s-downcase it)) acc (cons it acc))
                       (list (abbreviate-file-name (projectile-project-root))))
                      (-sort 'string-lessp))
               projectile-known-projects)))

         (mapc (lambda (file)
                 (add-to-list 'projectile-globally-ignored-buffers file)
                 (add-to-list 'consult-buffer-filter file))
               '("TAGS" "GTAGS"))
         (mapc (lambda (dir)
                 (add-to-list 'projectile-globally-ignored-directories dir))
               '("vendor" "public" "out" "dist" "coverage" "node_modules")))))

;; consult-projectile
(cond ((locate-library "consult-projectile")
       (with-eval-after-load 'consult
         (with-eval-after-load 'projectile
           (setq projectile-find-dir-includes-top-level t)

           (require 'consult-projectile)

           (define-key projectile-command-map (kbd "0")
             'consult-projectile)
           (define-key projectile-command-map (kbd "f")
             'consult-projectile-find-file)
           (define-key projectile-command-map (kbd "D")
             'consult-projectile-find-dir)
           (define-key projectile-command-map (kbd "p")
             'consult-projectile-switch-project)
           (define-key projectile-command-map (kbd "e")
             'consult-projectile-recentf)))))

(cond ((locate-library "company-terraform")
       (with-eval-after-load 'company
         (require 'company-terraform)
         (with-eval-after-load 'company-terraform
           (require 'terraform-mode)
           (define-derived-mode auto-tf-mode hcl-mode "Terraform"
             (company-terraform-init))
           (add-to-list 'auto-mode-alist '("\\.tf\\(vars\\)?\\'" . auto-tf-mode))
           (add-hook 'auto-tf-mode-hook 'terraform-format-on-save-mode)))))

(cond ((locate-library "bicycle")
       (progn
         (require 'bicycle)
         (define-key outline-minor-mode-map (kbd "C-<tab>") 'bicycle-cycle)
         (define-key outline-minor-mode-map (kbd "S-<tab>") 'bicycle-cycle-global)
         (add-hook 'prog-mode-hook 'outline-minor-mode)
         (add-hook 'prog-mode-hook 'hs-minor-mode))))

(cond ((locate-library "yaml-mode")
       (progn
         (require 'yaml-mode)
         (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
         (add-hook 'yaml-mode-hook
                   '(lambda ()
                      (define-key yaml-mode-map "\C-m" 'newline-and-indent))))))

;; MELPA package variable initialization
(cond ((locate-library "package")
       (custom-set-variables
        '(org-export-backends '(ascii beamer html icalendar latex md odt))
        '(package-archives
          '(("melpa" . "http://melpa.org/packages/")
            ("gnu" . "https://elpa.gnu.org/packages/")
            ("org" . "https://orgmode.org/elpa/"))))

       ;; MELPA initialization
       (require 'package)
       (package-initialize)))

;; ChatGPT.el
(cond ((locate-library "chatgpt")
       (progn
         (setq python-interpreter "python")
         (setq chatgpt-repo-path (expand-file-name "pkgs/chatgpt-el/" user-emacs-directory))
         (require 'chatgpt)
         (global-set-key (kbd "C-c q") 'chatgpt-query))))
