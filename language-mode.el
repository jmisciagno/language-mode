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
	       (lambda (n)
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

(defun flycheck-interactive ()
  (interactive)
  (flycheck-first-error)
  (flycheck--correction (flycheck-overlays-in (point-min) (point-max))))

(defun flycheck-word ()
  (interactive)
  (let ((v (flycheck-overlay-errors-at (point))))
    (if v
	(flycheck--correction-once v)
        (princ "No style errors at point."))))
	
(defun flycheck-error-message-at-point ()
  (flycheck-error-message (car (flycheck-overlay-errors-at (point)))))

(defun flycheck-overlay-at-point ()
  (car (flycheck-overlay-errors-at (point))))

(defun flycheck--erase-overlay (ov)
  (overlay-put ov 'face nil))

(defvar flycheck--correction-keys
  ;; (q)uit, (c)lear, (e)dit, (i)gnore
  [?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9
      ;; suggestions may over 10.
      ;; define rest of alphabet just in case.
      ?a ?b ?d ?f ?g ?h ?j ?k ?l ?m ?n
      ?o ?p ?r ?s ?t ?u ?v ?w ?x ?y ?z])

(defun flycheck--expire-buffer-overlays ()
  (mapc
   (lambda (o)
     (unless (overlay-get o 'face)
       (delete-overlay o)))
   (flycheck-overlays-in (point-min) (point-max))))

;;  Call this to enter the correction process
(defun flycheck--correction (overlays)
  (let ((conf (current-window-configuration)))
    (unwind-protect
        (let ((n 0)
	      (ovs overlays)
	      (rm-list nil))
          (while
	      (let* ((res (flycheck--correction-loop n ovs rm-list)))
		(progn
		  (setq n (nth 0 res))
		  (setq ovs (nth 1 res))
		  (setq rm-list (nth 2 res))
		  res))))
      (flycheck--expire-buffer-overlays)
      (set-window-configuration conf)
      (kill-buffer (flycheck--correction-buffer)))))

(defun flycheck--correction-once (overlays)
  (let ((conf (current-window-configuration)))
    (unwind-protect
      (save-excursion
	(flycheck--correction-loop 0 overlays nil)
	(flycheck--expire-buffer-overlays)
	(set-window-configuration conf)
	(kill-buffer (flycheck--correction-buffer))))))

(defun flycheck-previous-error-filter (n rm-list)
  (cond ((not (member (- n 1) rm-list))
	 (progn (flycheck-previous-error)
		  (- n 1)))
	(t (progn (flycheck-previous-error)
		  (flycheck-previous-error-filter (- n 1) rm-list)))))

(defun flycheck-next-error-filter (n rm-list)
  (cond ((not (member (+ n 1) rm-list))
	 (progn (flycheck-next-error)
		(+ n 1)))
	(t (progn (flycheck-next-error)
		  (flycheck-next-error-filter (+ n 1) rm-list)))))

(defun flycheck--correction-loop (n overlays rm-list)
  (flycheck--correction-popup (flycheck-error-message-at-point))
  (catch 'next
    (while (progn
             (let (message-log-max)
               (message (concat "C-h or ? for more options; "
                                "SPC to leave unchanged, "
                                "Digit to replace word")))
             (let* ((echo-keystrokes) ; suppress echoing
                    (c (downcase (read-char))))
               (cond
                ((memq c '(?q))
                 (keyboard-quit))
                ((memq c '(?c))
                 (flycheck--erase-overlay overlays)
                 nil)
                ((memq c '(?e))
                 (message (substitute-command-keys
                           "Type \\[exit-recursive-edit] to finish the edit."))
		 ;(recursive-edit)
                 (throw 'next (list n overlays rm-list)))
		((memq c '(?i))
		 (flycheck--erase-overlay (nth n overlays))
		 (let ((n0 (flycheck-next-error-filter n rm-list)))
		   (throw 'next (list n0 overlays (cons n rm-list)))))
	       ((memq c '(?\d))
                (throw 'next (let ((n0 (flycheck-previous-error-filter n rm-list)))
			       (list n0 overlays rm-list))))
               ((memq c '(?\C-h ?\?))
		(flycheck--correction-help)
		t)
               ((memq c '(?\s)) nil)
               (t t)))))
    ;; next item
    (let ((n0 (flycheck-next-error-filter n rm-list)))
      (list n0 overlays rm-list))))

(defun flycheck--correction-popup (msg)
  (let ((buf (flycheck--correction-buffer)))
    (delete-other-windows)
    (let ((win (progn (split-window) (selected-window))))
      (set-window-buffer win buf))
    (enlarge-window (- 1 (window-height (selected-window))))
    (other-window 1)
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert msg "\n\n")))))

(defun flycheck--correction-help ()
  (let ((help-1 "[q/Q]uit correction; [c/C]lear the colorized text; ")
        (help-2 "[e/E]dit the buffer manually")
        (help-3 "SPC skip; DEL move backward;"))
    (save-window-excursion
      (unwind-protect
          (let ((resize-mini-windows 'grow-only))
            (select-window (minibuffer-window))
            (erase-buffer)
            (message nil)
            ;;(set-minibuffer-window (selected-window))
            (enlarge-window 2)
            (insert (concat help-1 "\n" help-2 "\n" help-3))
            (sit-for 5))
        (erase-buffer)))))

(defun flycheck--correction-buffer ()
  (get-buffer-create "*Flycheck Correction*"))

;;; Define Language Mode

(defvar language-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map flyspell-mode-map) ; set parent
    (define-key map (kbd "C-c 7") (lambda () (interactive) (if (region-active-p) (ispell-word) (ispell-buffer))))
    (define-key map (kbd "C-c 8") 'langtool-check-buffer-with-interactive)
    (define-key map (kbd "C-c 9") (lambda () (interactive) (if (region-active-p) (flycheck-word) (flycheck-interactive))))
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
