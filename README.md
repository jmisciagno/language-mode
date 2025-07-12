# Requirements
Must have flyspell, langtools and vale modes installed in Emacs, The programs: aspell, language tool, and vale must be in $PATH.


# Quickstart

    (require 'language-mode)
    (add-hook 'org-mode-hook
	  (lambda ()
	    (define-key org-mode-map (kbd "C-c l") #'language-mode)))
    
