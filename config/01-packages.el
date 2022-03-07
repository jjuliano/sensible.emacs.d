;; package cl is deprecated in Emacs-27
(setq byte-compile-warnings '(cl-functions))

;; load all the 3rd party packages
(let ((default-directory (expand-file-name "pkgs/" user-emacs-directory)))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))

;; no-littering settings to keep .emacs.d clean
(setq no-littering-etc-directory
      (expand-file-name "config/" org-directory))
(setq no-littering-var-directory
      (expand-file-name "data/" org-directory))
(setq auto-save-file-name-transforms
      `((".*" ,(expand-file-name "auto-save/" no-littering-var-directory) t)))

;; melpa/elpa config
(setq package-user-dir (expand-file-name "packages/"
                                         no-littering-var-directory))

;; set recent files to no-littering dir
(cond ((locate-library "recentf")
       (require 'recentf)
       (add-to-list 'recentf-exclude no-littering-var-directory)
       (add-to-list 'recentf-exclude no-littering-etc-directory)))

;; set custom.el to no-littering dir
(cond ((locate-library "no-littering")
       (require 'no-littering)))

;; exec-path-from-shell settings to load $PATH on run
(cond ((locate-library "exec-path-from-shell")
       (require 'exec-path-from-shell)
       (when (memq window-system '(mac ns x))
         (exec-path-from-shell-initialize))))

;; better-defaults
(cond ((locate-library "better-defaults")
       (require 'better-defaults)))

;; backup-each-save
(cond ((locate-library "backup-each-save")
       (require 'backup-each-save)))

(cond ((locate-library "multiple-line-edit")
       (require 'multiple-line-edit)

       ;; Multi-line edit keybinding
       (global-set-key (kbd "C-c C-SPC") 'mulled/edit-leading-edges)
       (global-set-key (kbd "C-c M-SPC") 'mulled/edit-trailing-edges)))

;; zoom auto-resize window
(if (bound-and-true-p use-zoom-mode)
    (progn
      (cond ((locate-library "zoom")
             (setq zoom-size '(0.618 . 0.618))
             (require 'zoom)
             (zoom-mode t)))))

;; workspaces management via perspective-el
(cond ((locate-library "perspective")
       (setq persp-state-default-file (expand-file-name "data/persp-state-file"
                                                        org-directory))

       (require 'perspective)
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
       (require 'unicode-fonts)
       (unicode-fonts-setup)))

;; typescript IDE support
(cond ((locate-library "dash")
       (require 'dash)))

(cond ((locate-library "s")
       (require 's)))

;; Go IDE support
(cond ((locate-library "go-mode")
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

       (add-hook 'go-mode-hook 'go-mode-setup)))


;; auto-complete
(cond ((locate-library "auto-complete")
       (require 'auto-complete)
       (require 'auto-complete-config)
       (ac-config-default)
       (global-auto-complete-mode t)))

;; auto-complete on org
(cond ((locate-library "org-ac")
       (require 'org-ac)
       (org-ac/config-default)))

;; dumb-jump
(cond ((locate-library "dumb-jump")
       (require 'dumb-jump)
       (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)))

;; js2-mode
(cond ((locate-library "js2-mode")
       (require 'js2-mode)
       (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
       (add-to-list 'interpreter-mode-alist '("node" . js2-mode))))

;; web-mode
(cond ((locate-library "web-mode")
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
               ("blade"   . "\\.blade\\."))
             )
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
               ("html" . (ac-source-words-in-buffer ac-source-abbrev))))
       ))

;; load node-modules/bin paths
(cond ((locate-library "add-node-modules-path")
       (eval-after-load 'js-mode
         (add-hook 'js-mode-hook #'add-node-modules-path))))

;; prettier-js
(cond ((locate-library "prettier-js")
       (require 'prettier-js)
       (add-hook 'js2-mode-hook 'prettier-js-mode)
       (add-hook 'web-mode-hook 'prettier-js-mode)))

;; langtool
(cond ((locate-library "langtool")
       (require 'langtool)
       (global-set-key (kbd "C-x 4w") 'langtool-check)
       (global-set-key (kbd "C-x 4W") 'langtool-check-done)
       (global-set-key (kbd "C-x 4l") 'langtool-switch-default-language)
       (global-set-key (kbd "C-x 44") 'langtool-show-message-at-point)
       (global-set-key (kbd "C-x 4c") 'langtool-correct-buffer)))

;; flycheck
(cond ((locate-library "flycheck")
       (require 'flycheck)
       (add-hook 'after-init-hook 'global-flycheck-mode)))

;; flycheck-color-mode-line
(cond ((locate-library "flycheck-color-mode-line")
       (require 'flycheck-color-mode-line)
       (add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode)))

;; alternative workspace management via persp-mode.el
;;(cond ((locate-library "persp-mode")
;;       (require 'persp-mode)
;;       (persp-mode 1)))

;; wucuo flyspell spell-checking
(if (locate-library "wucuo")
    (progn
      (require 'wucuo)
      (add-hook 'prog-mode-hook 'wucuo-start)
      (add-hook 'text-mode-hook 'wucuo-start))

  ;; start Flyspell if "wucuo" package is not found
  (progn
    (require 'flyspell)
    (add-hook 'text-mode-hook 'flyspell-mode)
    (add-hook 'prog-mode-hook 'flyspell-prog-mode)))

;; flyspell-popup to display popup on wrong spelling words
(cond ((locate-library "flyspell-popup")
       (require 'flyspell-popup)
       (define-key flyspell-mode-map (kbd "C-;") #'flyspell-popup-correct)))

;; markdown-mode
(cond ((locate-library "markdown-mode")
       (require 'markdown-mode)
       (add-to-list 'auto-mode-alist
                    '("\\.\\(?:md\\|markdown\\|mkd\\|mdown\\|mkdn\\|mdwn\\)\\'"
                      . markdown-mode))
       (add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))
       (setq markdown-command "multimarkdown")))

;; minted for syntax highlighting on PDF exports
(cond ((locate-library "minted")
       (setq org-latex-prefer-user-labels t)
       (setq org-latex-listings
             'minted
             org-latex-packages-alist
             '(("" "minted"))
             org-latex-pdf-process
             '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
               "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
               "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))))

;; tide typescript IDE for Emacs
(cond ((locate-library "tide")
       (require 'tide)

       (defun setup-tide-mode ()
         (interactive)
         (tide-setup)
         (flycheck-mode +1)
         (setq flycheck-check-syntax-automatically '(save mode-enabled))
         (eldoc-mode +1)
         (tide-hl-identifier-mode +1)

         (cond ((locate-library "company")
                (company-mode +1))))

       (setq company-tooltip-align-annotations t)

       (add-hook 'before-save-hook 'tide-format-before-save)
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
       (flycheck-add-next-checker 'javascript-eslint 'jsx-tide 'append)))

;; Magit
(cond ((locate-library "magit")
       (require 'magit)))

;; improve vterm color mode
(cond ((locate-library "eterm-256color")
       (require 'eterm-256color)
       (add-hook 'term-mode-hook #'eterm-256color-mode)))

;; dependency of eterm-256color
(cond ((locate-library "xterm-color")
       (require 'xterm-color)))

;; vterm
(cond ((locate-library "vterm")
       (setq vterm-term-environment-variable "eterm-color")
       (add-hook 'vterm-mode-hook
          (lambda ()
            (set (make-local-variable 'buffer-face-mode-face) 'fixed-pitch)
            (buffer-face-mode t)))
       (require 'vterm)))

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
