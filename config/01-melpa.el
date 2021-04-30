(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
;; (add-to-list 'package-archives '("melpa-stable"
;; . "https://stable.melpa.org/packages/") t)

(package-initialize)

;; MELPA/ELPA package configs

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

;; prettier-js
(cond ((locate-library "prettier-js")
       (require 'prettier-js)
       (add-hook 'js2-mode-hook 'prettier-js-mode)
       (add-hook 'web-mode-hook 'prettier-js-mode)))

;; langtool
(cond ((locate-library "langtool")
       (setq langtool-language-tool-jar "/opt/LanguageTool-5.1/languagetool-commandline.jar")
       (setq langtool-default-language "en-US")

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
       (add-hook 'typescript-mode-hook #'setup-tide-mode)))
