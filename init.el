;; Eddie's Initialization File
(message "Loading Eddie's initialization…")


;; Path config
(message "Loading Eddie's configuration paths…")

(add-to-list 'load-path "~/.emacs.d/site-lisp")
(add-to-list 'load-path "/opt/local/share/emacs/site-lisp")

(setq exec-path (append exec-path '("/opt/local/bin")))
(setq exec-path (append exec-path '("/usr/local/bin")))

(setenv "PATH"
        (concat (getenv "PATH")
                ":/opt/local/bin"
                ":/opt/local/sbin"
                ":/Users/eddie/bin"))


;; Package config
(message "Loading Eddie's configuration package…")

(require 'package)
(package-initialize)
(add-to-list 'package-archives
             '("elpa" . "http://tromey.com/elpa/"))
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/"))

;; install use-package if necessary
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)

;; make it easy to debug
(setq use-package-verbose t)

;; install packages on-demand
(setq use-package-always-ensure t)


;; Theme config
(message "Loading Eddie's configuration theme…")

(use-package zenburn-theme
             :init
             (add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
             :config
             (load-theme 'zenburn t))


;; Appearance config
(message "Loading Eddie's configuration appearance…")

;; Kill the splash screen (death to all splash screens)
(setq inhibit-splash-screen t)

; Never use the menu in a terminal
(unless window-system
  (menu-bar-mode -1))

; Don't display the fringe
(if window-system
    (fringe-mode 0))

; Don't display the scroll-bar
(if window-system
    (scroll-bar-mode -1))

; Don't display the pointless toolbar
(if window-system
    (tool-bar-mode -1))

; Set the font
;; (set-frame-font "DejaVu Sans Mono 15")
;; (set-default-font (if (eq system-type 'windows-nt)
;;                       "DejaVu Sans Mono 11"
;;                     "DejaVu Sans Mono 15"))

(setq eddie/default-frame-style
      `((vertical-scroll-bars)
        (right-fringe . 0)
        (left-fringe . 0)
        (font . ,(if (eq system-type 'windows-nt)
                     "DejaVu Sans Mono 11"
                   "DejaVu Sans Mono 15"))
        ;; (menu-bar-lines . 0) ;; causes the menu-bar in MacOS X to
        ;; disappear
        (tool-bar-lines . 0)
        (width . 80)))

(setq initial-frame-alist eddie/default-frame-style)
(setq default-frame-alist eddie/default-frame-style)

;; Show Paren
;; Found this at: http://www.emacsblog.org/
(setq show-paren-style 'parenthesis)
(show-paren-mode t)

;; Stretch cursor on Tabs
(setq x-stretch-cursor t)

;; show column mode
(setq column-number-mode t)

;; prevent mouse wheel scrolling
(if window-system
    (mouse-wheel-mode -1))

;; Show directory names
(require 'uniquify nil 'noerror)
(setq uniquify-buffer-name-style 'forward)

;; ido mode config
(message "Loading Eddie's configuration ido…")

;; ido mode (vertical)
(use-package ido-vertical-mode
  :config
  (ido-vertical-mode 1)
  (setq ido-vertical-define-keys 'C-n-and-C-p-only))

;; ido mode
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(setq ido-default-file-method 'selected-window)
(setq ido-default-buffer-method 'selected-window)
; don't look for a match outside of the current directory
(setq ido-auto-merge-work-directories-length -1)
(ido-mode 1)

(add-hook 'ido-setup-hook
 (lambda ()
   ;; Go straight home
   (define-key ido-file-completion-map
     (kbd "~")
     (lambda ()
       (interactive)
       (if (looking-back "/")
           (insert "~/")
         (call-interactively 'self-insert-command))))))


;; Save config
(message "Loading Eddie's configuration saving…")

;; delete trailing whitespace before every save
(add-hook 'before-save-hook (lambda () (delete-trailing-whitespace)))

;; don't save backup files
(setq make-backup-files nil)

;; save backups and auto saves in temp directory
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; new line at end of file
(setq require-final-newline t)

;; Column Selection
; don't change the behavior of  C-z, C-x, C-c, C-v
(setq cua-enable-cua-keys nil)
(cua-mode)

;; Org mode
(setq org-CUA-compatible t)
(setq org-use-extra-keys t)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(global-set-key "\C-cl" 'org-store-link)
(setq org-log-done nil)

(add-hook 'org-mode-hook
            (lambda ()
              (auto-fill-mode)))

(require 'ob-sh)
(require 'ob-js)
(require 'ob-python)
(require 'ob-C)
(org-babel-do-load-languages 'org-bable-load-languages
			     '((emacs-lisp . t)
			       (sh . t)
			       (C . t)
			       (cpp . t)
			       (css . t)
			       (gnuplot . t)
			       (haskell . t)
			       (java . t)
			       (js . t)
			       (latex . t)
			       (lisp . t)
			       (makefile . t)
			       (org . t)
			       (perl . t)
			       (python . t)
			       (R . t)
			       (ruby . t)
			       (scala . t)
			       (scheme . t)
			       (sed . t)
			       (sql . t)
			       (sqlite . t)))


;; Java config
(message "Loading Eddie's configuration for Java…")

(setq c-default-style
      '((java-mode . "bsd")
        (awk-mode . "awk")
        (other . "k&r")))

(add-hook 'java-mode-hook
	  (lambda ()
	    (setq tab-width 4
		  c-basic-offset 4
		  indent-tabs-mode nil)))

;; C config
(message "Loading Eddie's configuration C…")

(setq c-basic-offset 4
      tab-width 4
      indent-tabs-mode nil)

;; WebKit config
(message "Loading Eddie's configuration WebKit…")

;; https://chromium.googlesource.com/chromium/src/+/master/docs/emacs.md
(use-package google-c-style
  :config
  (c-add-style "Google" google-c-style)
  (c-add-style "WebKit" '("Google"
                          (c-basic-offset . 4)
                          (c-offsets-alist . ((innamespace . 0)
                                              (access-label . -)
                                              (case-label . 0)
                                              (member-init-intro . +)
                                              (topmost-intro . 0)
                                              (arglist-cont-nonempty . +))))))

;; https://bugs.webkit.org/show_bug.cgi?id=72483
(defun webkit-c++-mode ()
  "C++ mode with adjusted defaults for use with WebKit."
  (interactive)
  (c++-mode)
  (c-set-style "WebKit")
  (setq indent-tabs-mode nil)
  (setq c-basic-offset 4))

(setq auto-mode-alist (cons '("/WebKit/.*\\.[ch|cpp]*$" . webkit-c++-mode)
                            auto-mode-alist))
(setq auto-mode-alist (cons '("/JavaScriptCore/.*\\.[ch|cpp]*$" . webkit-c++-mode)
                            auto-mode-alist))


;; ADC config
(message "Loading Eddie's configuration ADC3…")

(defun adc3-c++-mode ()
  "C++ mode with adjusted defaults for use with ADC3."
  (interactive)
  (c++-mode)
  (c-set-style "bsd")
  (setq indent-tabs-mode nil)
  (setq c-basic-offset 2))

(setq auto-mode-alist (cons '("/adc3-sdk/.*\\.[ch|cpp]*$" . adc3-c++-mode)
                            auto-mode-alist))
(setq auto-mode-alist (cons '("/adc3-jsc/.*\\.[ch|cpp]*$" . adc3-c++-mode)
                            auto-mode-alist))


;; Go mode config
(use-package go-mode)

;; Markdown mode config
(use-package markdown-mode)

;; Emacs Lisp (Elisp)
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (setq indent-tabs-mode t)
            (setq tab-width 8)))

;; eshell
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

; suppress the follow warning
;   ls does not support --dired; see `dired-use-ls-dired' for more details.
(setq ls-lisp-use-insert-directory-program nil)
(require 'ls-lisp)


;;; Stuff for speaking on OS X.
(defun speak (str)
  "Speak the string."
  (shell-command (concat "say " (shell-quote-argument str))))

(defun speak-word ()
  "Speak the current word at point."
  (interactive)
  (speak (thing-at-point 'word)))

(defun speak-line ()
  "Speak the current line at point."
  (interactive)
  (speak (thing-at-point 'line)))

(defun speak-sentence ()
  "Speak the current sentence at point."
  (interactive)
  (speak (thing-at-point 'sentence)))

(defun speak-paragraph ()
  "Speak the current paragraph at point."
  (interactive)
  (speak (thing-at-point 'paragraph)))

(defun speak-buffer ()
  "Speak the entire buffer."
  (interactive)
  (speak (buffer-string)))

(defun speak-region ()
  "Speak region."
  (interactive)
  (speak (buffer-substring (region-beginning) (region-end))))

;; Need to implement
;; some-procedure
;; ----            word
;;      ---------  word
;; --------------  symbol
;; forward-word, backward-word, forward-symbol, backword-symbol
