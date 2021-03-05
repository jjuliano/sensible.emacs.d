;; org directory
(setq org-directory "~/Documents/Emacs/org")
(setq org-latex-prefer-user-labels t)
(setq org-mobile-directory (expand-file-name "~/Dropbox/Apps/MobileOrg"))

;; minted for syntax highlighting on PDF exports
(setq org-latex-listings
      'minted
      org-latex-packages-alist
      '(("" "minted"))
      org-latex-pdf-process
      '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))
