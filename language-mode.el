;;; language-mode. -*- lexical-binding:t -*-

;;; Flyspell for Spell Checking
(require 'flyspell)
(setq ispell-program-name "aspell")
(setq flyspell-mark-duplications-flag nil) ; disable duplicate word checking
(fset 'make-flyspell-overlay 'my-make-flyspell-overlay)

(defun my-make-flyspell-overlay (beg end face mouse-face)
  "Allocate an overlay to highlight an incorrect word.
BEG and END specify the range in the buffer of that word.
FACE and MOUSE-FACE specify the `face' and `mouse-face' properties
for the overlay."
  (let ((overlay (make-overlay beg end nil t nil)))
    (overlay-put overlay 'face face)
    ; (overlay-put overlay 'mouse-face mouse-face) ; disable word highlighting
    (overlay-put overlay 'flyspell-overlay t)
    (overlay-put overlay 'evaporate t)
    ;; (overlay-put overlay 'help-echo
    ;;              (lambda (window object pos)
    ;; 		   (let ((msg "Word not found in dictionary."))
    ;; 		     (minibuffer-message beg))))
    ;; 		     ;; (when (and (< beg (point)) (< (point end)))
    ;; 		     ;;   (minibuffer-message msg)))))
    (when (eq face 'flyspell-incorrect)
      (and (stringp flyspell-before-incorrect-word-string)
           (overlay-put overlay 'before-string
                        flyspell-before-incorrect-word-string))
      (and (stringp flyspell-after-incorrect-word-string)
           (overlay-put overlay 'after-string
                        flyspell-after-incorrect-word-string)))
    overlay))

(advice-add 'flyspell-mode  ; hide FLY in modeline
	       :before
	       (lambda (x)
		 (if (bound-and-true-p language-mode)
		     (setq flyspell-mode-line-string nil)
		     (setq flyspell-mode-line-string " Fly"))))

;;; Langtool for grammar check
(require 'langtool)
(setq langtool-bin "languagetool")
(setq langtool-default-language "en-US")
(setq langtool-disabled-rules (list "MORFOLOGIK_RULE_EN_US")) ; disable spell check
(set-face-attribute 'langtool-correction-face nil :background nil :foreground "black" :weight 'normal)
; (set-face-attribute 'langtool-errline nil :background "pale green")
(set-face-attribute 'langtool-errline nil :background nil :underline '(:color "green4" :style wave))
(add-hook 'after-save-hook (lambda () (interactive) (when (bound-and-true-p language-mode) (langtool-check-buffer-no-interactive)))) ; check on save
(add-hook 'langtool-noerror-hook (lambda () (interactive) (princ "No Grammar Errors.")))
(fset 'langtool-simple-error-message 'my-langtool-simple-error-message) ; simplify or hush messages

(defun my-langtool-simple-error-message (overlays)
  "Textify error messages as long as simple."
  (mapconcat
   (lambda (ov)
     (format
      "%s [%s]"
      (overlay-get ov 'langtool-simple-message)
      (overlay-get ov 'langtool-rule-id)))
   overlays "\n"))

(add-hook 'langtool-error-exists-hook #'langtool-maybe-do-interactive) ;; called when langtool-check-buffer is completed
(defun langtool-maybe-do-interactive ()
  (interactive)
  (when langtool-auto-check (langtool-interactive-correction))
  (princ ""))

(defun langtool-check-buffer-safe ()
  (interactive)
  (when (not langtool-buffer-process)
    (progn (princ "Checking grammar...")
	   (langtool-check-buffer)
	   (set-buffer-modified-p nil))))

(defun langtool-check-buffer-with-interactive ()
  (interactive)
  (if (buffer-modified-p)
      (progn
	(langtool-check-buffer-safe)
	(setq langtool-auto-check t)
	(princ "")
	)
    (progn
      (langtool-interactive-correction)
      (princ ""))))

(defun langtool-check-buffer-no-interactive ()
  (interactive)
  (setq langtool-auto-check nil)
  (langtool-check-buffer-safe)
  (princ ""))

(advice-add 'langtool-check-buffer
	       :before
	       (lambda ()
		 (if (bound-and-true-p language-mode)
		     (setq langtool-custom-mode-line-message "")
		     (setq langtool-custom-mode-line-message nil))))

;; ;;; Flycheck with Vale for Linting
(require 'vale-mode)
(require 'flycheck)
(setq flycheck-indication-mode nil) ; no red arrow
(setq flycheck-auto-display-errors-after-checking nil)
(fset 'flycheck-help-echo (lambda (_window object pos) "")) ;; Disable mouse-over message
(set-face-attribute 'flycheck-error nil :background nil :underline '(:color "blue" :style wave))

(flycheck-define-checker vale
  "A checker for prose"
  :command ("vale" "--output" "line"
            source)
  :standard-input nil
  :error-patterns
  ((error line-start (file-name) ":" line ":" column ":" (id (one-or-more (not (any ":")))) ":" (message) line-end))
  :modes (text-mode org-mode))

(add-to-list 'flycheck-checkers 'vale 'append)

; Hide FlyC in language mode, restore otherwise
(add-hook 'flycheck-mode-hook (lambda ()
				(if (bound-and-true-p language-mode)
				    (setq flycheck-mode-line '(:eval ""))
				    (setq flycheck-mode-line '(:eval (flycheck-mode-line-status-text))))))

(defvar language-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map flyspell-mode-map) ; set parent
    (define-key map (kbd "C-c 7") (lambda () (interactive) (if (region-active-p) (ispell-word) (ispell-buffer))))
    (define-key map (kbd "C-c 8") 'langtool-check-buffer-with-interactive)
    map)
  "Keymap for my extended minor mode.")

(define-minor-mode language-mode
  "Language mode documentation."
  :lighter " Language"
  :keymap language-mode-map)

(defun language-mode-toggle ()
  (if (derived-mode-p 'text-mode)
      (if language-mode
	  (progn
	    (setq inhibit-redisplay nil)
	    (unless flyspell-mode (flyspell-mode 1)) ; check spelling on open
	    (flyspell-buffer)
	    (langtool-check-buffer-no-interactive) ; check grammar on startup
	    (unless flycheck-mode (flycheck-mode 1)) ; check spelling on open
					;	(flycheck-buffer) 
	    )
	(progn
	  (flyspell-mode -1)
	  (langtool-check-done)
	  (flycheck-mode -1)))
      (progn (princ "Failed to launch Language mode.  Text mode must be active.")
	     (ding))))

(add-hook 'language-mode-hook #'language-mode-toggle) ; start or clean up

(provide 'language-mode)
