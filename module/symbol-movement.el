;; some-procedure
;; ----            word
;;      ---------  word
;; --------------  symbol
;; forward-word, backward-word, forward-symbol, backword-symbol
;; These already exist, but I want them to work on snake_case,
;; camelCase, PascalCase, and kebab-case, etc

;; NSString
;; --       word
;;   ------ word
;; -------- symbol

;; applicationWillTerminate
;; -----------              word
;;            ----          word
;;                --------- word
;; ------------------------ symbol

;; test area:
;; serpent_case kebab-case camelCase PascalCase NSString GtkWindow

(global-subword-mode)

(defun e/forward-symbol (&optional arg)
  ""
  (interactive "^p")
  (let ((subword-mode-was-on
	 (if (and (boundp 'subword-mode) subword-mode) 1 -1))
	(superword-mode-was-on
	 (if (and (boundp 'superword-mode) superword-mode) 1 -1)))
    (superword-mode 1)
    (subword-forward arg)
    (subword-mode subword-mode-was-on)
    (superword-mode superword-mode-was-on)))

(defun e/backward-symbol (&optional arg)
  ""
  (interactive "^p")
  (let ((subword-mode-was-on
	 (if (and (boundp 'subword-mode) subword-mode) 1 -1))
	(superword-mode-was-on
	 (if (and (boundp 'superword-mode) superword-mode) 1 -1)))
    (superword-mode 1)
    (subword-backward arg)
    (subword-mode subword-mode-was-on)
    (superword-mode superword-mode-was-on)))

(defun e/mark-symbol (arg)
  ""
  (interactive "p")
  (let ((subword-mode-was-on
	 (if (and (boundp 'subword-mode) subword-mode) 1 -1))
	(superword-mode-was-on
	 (if (and (boundp 'superword-mode) superword-mode) 1 -1)))
    (superword-mode 1)
    (subword-mark arg)
    (subword-mode subword-mode-was-on)
    (superword-mode superword-mode-was-on)))

(defun e/kill-symbol (arg)
  ""
  (interactive "p")
  (let ((subword-mode-was-on
	 (if (and (boundp 'subword-mode) subword-mode) 1 -1))
	(superword-mode-was-on
	 (if (and (boundp 'superword-mode) superword-mode) 1 -1)))
    (superword-mode 1)
    (subword-kill arg)
    (subword-mode subword-mode-was-on)
    (superword-mode superword-mode-was-on)))

(defun e/backward-kill-symbol (arg)
  ""
  (interactive "p")
  (let ((subword-mode-was-on
	 (if (and (boundp 'subword-mode) subword-mode) 1 -1))
	(superword-mode-was-on
	 (if (and (boundp 'superword-mode) superword-mode) 1 -1)))
    (superword-mode 1)
    (subword-backward-kill arg)
    (subword-mode subword-mode-was-on)
    (superword-mode superword-mode-was-on)))

(defun e/transpose-symbols (arg)
  ""
  (interactive "*p")
  (let ((subword-mode-was-on
	 (if (and (boundp 'subword-mode) subword-mode) 1 -1))
	(superword-mode-was-on
	 (if (and (boundp 'superword-mode) superword-mode) 1 -1)))
    (superword-mode 1)
    (subword-transpose arg)
    (subword-mode subword-mode-was-on)
    (superword-mode superword-mode-was-on)))

(defun e/capitalize-symbol (arg)
  ""
  (interactive "p")
  (let ((subword-mode-was-on
	 (if (and (boundp 'subword-mode) subword-mode) 1 -1))
	(superword-mode-was-on
	 (if (and (boundp 'superword-mode) superword-mode) 1 -1)))
    (superword-mode 1)
    (subword-capitalize arg)
    (subword-mode subword-mode-was-on)
    (superword-mode superword-mode-was-on)))

(defun e/upcase-symbol (arg)
  ""
  (interactive "p")
  (let ((subword-mode-was-on
	 (if (and (boundp 'subword-mode) subword-mode) 1 -1))
	(superword-mode-was-on
	 (if (and (boundp 'superword-mode) superword-mode) 1 -1)))
    (superword-mode 1)
    (subword-upcase arg)
    (subword-mode subword-mode-was-on)
    (superword-mode superword-mode-was-on)))

(defun e/downcase-symbol (arg)
  ""
  (interactive "p")
  (let ((subword-mode-was-on
	 (if (and (boundp 'subword-mode) subword-mode) 1 -1))
	(superword-mode-was-on
	 (if (and (boundp 'superword-mode) superword-mode) 1 -1)))
    (superword-mode 1)
    (subword-downcase arg)
    (subword-mode subword-mode-was-on)
    (superword-mode superword-mode-was-on)))

(setq mac-command-modifier 'super)

(global-set-key (kbd "s-f") 'e/forward-symbol)
(global-set-key (kbd "s-b") 'e/backward-symbol)
(global-set-key (kbd "s-@") 'e/mark-symbol)
(global-set-key (kbd "s-d") 'e/kill-symbol)
(global-set-key (kbd "s-<backspace>") 'e/backward-kill-symbol)
(global-set-key (kbd "s-t") 'e/transpose-symbols)
(global-set-key (kbd "s-c") 'e/capitalize-symbol)
(global-set-key (kbd "s-u") 'e/upcase-symbol)
(global-set-key (kbd "s-l") 'e/downcase-symbol)
