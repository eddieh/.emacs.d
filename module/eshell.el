;;; eshell

(require 'eshell)

;; display a normal UNIX prompt
(setq eshell-prompt-function
      (lambda ()
	; # for root, $ for a regular user
	(if (= (user-uid) 0) "# " "$ ")))
(setq eshell-prompt-regexp "^[^#$\n]*[#$] ")

(defalias 'f 'find-file)
(defalias 'o 'find-file-other-window)
(defalias 'd 'dired)
(defalias 'l 'dired-other-window)

;; Don't display the default banner message. The
;; ~/.emacs.d/eshell/login file has been customized to show a message.
(setq eshell-banner-message "")

;; Use the values for HISTFILE and HISTSIZE.
(if (package-installed-p 'exec-path-from-shell)
    (setq eshell-history-file-name nil
	eshell-history-size nil)
  (setq eshell-history-file-name "~/.bash_history"
	eshell-history-size 64000))

;; match key bindings for Terminal.app
;;   jump to previous mark ⌘↑
;;   jump to next mark ⌘↓
;;   clear to start ⌘K (clear scrollback & clear screen)
(add-hook 'eshell-mode-hook
	  (lambda ()
	    (local-set-key (kbd "<s-up>") 'eshell-previous-prompt)
	    (local-set-key (kbd "<s-down>") 'eshell-next-prompt)
	    (local-set-key (kbd "s-k") (lambda ()
					 (interactive)
					 (eshell/clear t)
					 (eshell-reset)))))

;; don't change the directory without cd
(setq eshell-cd-on-directory nil)

(defun e/eshell-name-buffer-with-dir ()
  (let* ((curdir (eshell/pwd))
	 (name (car (last (split-string curdir "/" t)))))
      (rename-buffer (concat "  " name "") t)))

(add-hook 'eshell-mode-hook
	  'e/eshell-name-buffer-with-dir)

(add-hook 'eshell-directory-change-hook
	    'e/eshell-name-buffer-with-dir)

;; Must return nil to indicate that we are not intercepting the
;; command
(add-hook 'eshell-named-command-hook
	  (lambda (cmd args)
	    (rename-buffer
	     (concat " ️" cmd "") t) nil))

(add-hook 'eshell-post-command-hook
	  'e/eshell-name-buffer-with-dir)

;; http://www.howardism.org/Technical/Emacs/eshell-fun.html
(defun e/eshell-for-current-dir ()
  "Opens up a new shell in the directory associated with the
current buffer's file. The eshell is renamed to match that
directory to make multiple eshell windows easier."
  (interactive)
  (let* ((parent (if (buffer-file-name)
                     (file-name-directory (buffer-file-name))
                   default-directory))
         (height (/ (window-total-height) 3))
         (name   (car (last (split-string parent "/" t)))))
    (split-window-vertically (- height))
    (other-window 1)
    (eshell "new")
    (rename-buffer (concat "*eshell: " name "*"))

    (insert (concat "ls"))
    (eshell-send-input)))

;; http://www.howardism.org/Technical/Emacs/eshell-fun.html
(defun eshell/x ()
  "Type x to kill current eshell session and window."
  (insert "exit")
  (eshell-send-input)
  (delete-window))

(defun e/recent-eshell ()
  (seq-some (lambda (buf)
	      (if (equal
		   (with-current-buffer buf major-mode)
		   'eshell-mode)
		  buf
		nil))
	    (buffer-list)))

(defun e/eshell-recent-or-new ()
  (interactive)
  (let ((buf (get-buffer-create (or (e/recent-eshell)
				    eshell-buffer-name))))
    (cl-assert (and buf (buffer-live-p buf)))
    (pop-to-buffer-same-window buf)
    (unless (derived-mode-p 'eshell-mode)
      (eshell-mode))
    buf))

(defun e/eshell-new ()
  (interactive)
  (eshell 'N))

;; (defun e/eshell-recent-or-new-cur-dir ()
;;   (interactive))

;; (defun e/eshell-new-cur-dir ()
;;   (interactive))

;; quick access
(global-set-key (kbd "C-c e e") 'e/eshell-recent-or-new)
(global-set-key (kbd "C-c e E") 'e/eshell-new)
;; (global-set-key (kbd "C-c e d") 'e/eshell-recent-or-new-cur-dir)
;; (global-set-key (kbd "C-c e D") 'e/eshell-new-cur-dir)


(setq eshell-visual-subcommands '())
(setq eshell-visual-options '())

;;(add-to-list 'eshell-visual-commands "brew")
(add-to-list 'eshell-visual-subcommands '("brew" "install"))
(add-to-list 'eshell-visual-subcommands '("git" "diff" "log"))
(add-to-list 'eshell-visual-options '("curl" "-O"))

(setq eshell-visual-subcommands nil)

;; remove first
;; (setq eshell-visual-commands
;;       (cdr eshell-visual-commands))
