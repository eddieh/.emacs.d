;; Eddie's Initialization File
(message "Loading Eddie's initialization…")

;; Moving all the appearance stuff to the top

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

;; Enable emoji
(if window-system
    (set-fontset-font "fontset-default" 'unicode "Apple Color Emoji" nil 'prepend))

;(add-hook 'after-init-hook #'global-emojify-mode)

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

;; (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
;; (add-to-list 'default-frame-alist '(ns-appearance . dark))


;; Path config
(message "Loading Eddie's configuration paths…")

(defconst eddie/default-emacs-dir "~/.emacs.d/"
  "My default emacs director.")

(add-to-list 'load-path "~/site-lisp/plist-mode")
(add-to-list 'load-path "~/.emacs.d/site-lisp")
(add-to-list 'load-path "/opt/local/share/emacs/site-lisp")

(setq exec-path (append exec-path '("/Library/TeX/texbin")))
(setq exec-path (append exec-path '("/opt/local/bin")))
(setq exec-path (append exec-path '("/usr/local/bin")))

(setenv "PATH"
        (concat (getenv "PATH")
		":/Library/TeX/texbin"
                ":/opt/local/bin"
                ":/opt/local/sbin"
		":/usr/local/bin"
                ":/Users/eddie/bin"))

;(setenv "DICTIONARY" "en_US")
;; (setq ispell-program-name "hunspell")
;; (setq ispell-really-hunspell t)
(setq ispell-program-name "aspell")

(defun string-from-file (path)
  "Get the content of the file at PATH as a string"
  (with-temp-buffer
    (insert-file-contents path)
    (buffer-string)))

(defun eddie/html-style-tag-with-file (file)
  (concat
   "<style>"
   (string-from-file (concat eddie/default-emacs-dir file))
   "</style>"))

;; keep customization in a separate file
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

;; Package config
(message "Loading Eddie's configuration package…")

(require 'package)
;; (setq package-enable-at-startup nil)
(add-to-list 'package-archives
             '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(add-to-list 'package-archives
             '("elpa" . "http://tromey.com/elpa/"))
;; (add-to-list 'package-archives
;; 	     '("melpa-stable" . "http://melpa-stable.milkbox.net/packages/") t)
;; (add-to-list 'package-archives
;;              '("marmalade" . "http://marmalade-repo.org/packages/"))
;; (add-to-list 'package-archives
;;              '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

;; install use-package if necessary
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
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

(use-package project-explorer)

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

;;; Org mode
(setq org-CUA-compatible t)
(setq org-use-extra-keys t)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(global-set-key "\C-cl" 'org-store-link)
(setq org-log-done nil)

;; org html export
(setq org-html-doctype "html5")
(setq org-html-head-include-default-style nil)
(setq org-html-head
      (concat
       (eddie/html-style-tag-with-file "css/reset.css")
       (eddie/html-style-tag-with-file "css/style.css")))
(setq org-html-head-include-scripts nil)
(setq org-html-preamble nil)
(setq org-html-preamble-format
      '(("en"
	 "<h1 class=\"title\">%t</h1>
<p class=\"date\">%d</p>
<p class=\"author\">by %a</p>")))
(setq org-html-postamble t)
(setq org-html-postamble-format
      '(("en" "<p class=\"author\">Author: %a</p>
<p class=\"date\">Date: %d</p>
<p class=\"modified\">Modified: %C</p>")))
(setq org-html-htmlize-output-type nil)

;; org notes & capture
(setq org-default-notes-file "~/CloudStation/notes.org")
(global-set-key "\C-cc" 'org-capture)
(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/CloudStation/plan/inbox.org" "Tasks")
	 "* TODO %?\n %i\n %a")))

(add-hook 'org-mode-hook (lambda () (auto-fill-mode)))

;; Org babel
;; (require 'ob-sh)
(require 'ob-js)
(require 'ob-python)
(require 'ob-C)
(org-babel-do-load-languages
 'org-bable-load-languages
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


;; default indent style
(setq tab-width 4)
(setq-default indent-tabs-mode nil)

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

(add-hook 'c-mode-common-hook
	  (lambda ()
	    (setq c-basic-offset 4
		  tab-width 4
		  indent-tabs-mode nil)))


;; WebKit config
(message "Loading Eddie's configuration WebKit…")

;; https://chromium.googlesource.com/chromium/src/+/master/docs/emacs.md
(require 'google-c-style)
(c-add-style "Google" google-c-style)
(c-add-style "WebKit" '("Google"
                        (c-basic-offset . 4)
                        (c-offsets-alist . ((innamespace . 0)
                                            (access-label . -)
                                            (case-label . 0)
                                            (member-init-intro . +)
                                            (topmost-intro . 0)
                                            (arglist-cont-nonempty . +)))))

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


;; ADC JavaScript
(add-hook 'js-mode-hook
	  (lambda ()
	    (let ((filename (buffer-file-name)))
	      (when (and filename
			 (string-match "adc3-sdk" filename))
		(message "ADC js-mode engage!")
		(setq tab-width 2
		      js-indent-level 2
		      indent-tabs-mode nil)))))

;; JS Modules
(add-to-list 'auto-mode-alist '("\\.mjs\\'" . js-mode))

;; ActionScript
(add-to-list 'auto-mode-alist '("\\.as\\'" . actionscript-mode))

(add-to-list 'auto-mode-alist '("\\.mm\\'" . objc-mode))

;; Go mode config
(use-package go-mode)

;; Markdown mode config
(use-package markdown-mode)

;; Groovy mode config
(use-package groovy-mode)

;; Gradle mode config
(use-package gradle-mode)

;; CMake mode config
(use-package cmake-mode)

;; gitignroe mode config
(use-package gitignore-mode)

(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

;; Projectile
(use-package projectile
  :ensure t
  :init
  (setq projectile-require-project-root nil)
  :config
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "C-c C-p") 'projectile-command-map))

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

;; (require 'dired-x)
;; (setq-default dired-omit-files-p t)
;; (setq dired-omit-files
;;       (concat
;;        dired-omit-files ; default omits '.', '..', auto-save files and lock files
;;        "\\|^\\.DS_Store$"		  ; macOS noise
;;        "\\|^\\.SynologyWorkingDirectory$" ; CloudStation noise
;;        "\\|^Icon$"))			  ; CloudStation noise (wish
;; 					  ; there was a way to omit
;; 				          ; it as 'parent/Icon')

;; Language Server Protocol (LSP) mode
;; (use-package lsp-mode
;;   :ensure t
;;   :config

;;   ;; make sure we have lsp-imenu everywhere we have LSP
;;   (require 'lsp-imenu)
;;   (add-hook 'lsp-after-open-hook 'lsp-enable-imenu)
;;   ;; get lsp-python-enable defined
;;   ;; NB: use either projectile-project-root or ffip-get-project-root-directory
;;   ;;     or any other function that can be used to find the root directory of a project
;;   (lsp-define-stdio-client lsp-python "python"
;;                            #'projectile-project-root
;;                            '("pyls"))

;;   ;; make sure this is activated when python-mode is activated
;;   ;; lsp-python-enable is created by macro above
;;   (add-hook 'python-mode-hook
;;             (lambda ()
;;               (lsp-python-enable)))

;;   ;; lsp extras
;;   ;; (use-package lsp-ui
;;   ;;   :ensure t
;;   ;;   :config
;;   ;;   (setq lsp-ui-sideline-ignore-duplicate t)
;;   ;;   (add-hook 'lsp-mode-hook 'lsp-ui-mode))

;;   (use-package company-lsp
;;     :config
;;     (push 'company-lsp company-backends))

;;   ;; NB: only required if you prefer flake8 instead of the default
;;   ;; send pyls config via lsp-after-initialize-hook -- harmless for
;;   ;; other servers due to pyls key, but would prefer only sending this
;;   ;; when pyls gets initialised (:initialize function in
;;   ;; lsp-define-stdio-client is invoked too early (before server
;;   ;; start)) -- cpbotha
;;   (defun lsp-set-cfg ()
;;     (let ((lsp-cfg `(:pyls (:configurationSources ("flake8")))))
;;       ;; TODO: check lsp--cur-workspace here to decide per server / project
;;       (lsp--set-configuration lsp-cfg)))

;;   (add-hook 'lsp-after-initialize-hook 'lsp-set-cfg))

(use-package magit
  :init
  (global-set-key (kbd "C-c m") 'magit))

;;; Stuff for speaking on OS X.
(defun speak (str)
  "Speak the string."
  (with-temp-buffer
    (call-process-shell-command
     (concat "say " (shell-quote-argument str)) nil t nil)))

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

(defun eddie/forward-symbol (&optional arg)
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

(defun eddie/backward-symbol (&optional arg)
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

(defun eddie/mark-symbol (arg)
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

(defun eddie/kill-symbol (arg)
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

(defun eddie/backward-kill-symbol (arg)
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

(defun eddie/transpose-symbols (arg)
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

(defun eddie/capitalize-symbol (arg)
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

(defun eddie/upcase-symbol (arg)
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

(defun eddie/downcase-symbol (arg)
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

(global-set-key (kbd "s-f") 'eddie/forward-symbol)
(global-set-key (kbd "s-b") 'eddie/backward-symbol)
(global-set-key (kbd "s-@") 'eddie/mark-symbol)
(global-set-key (kbd "s-d") 'eddie/kill-symbol)
(global-set-key (kbd "s-<backspace>") 'eddie/backward-kill-symbol)
(global-set-key (kbd "s-t") 'eddie/transpose-symbols)
(global-set-key (kbd "s-c") 'eddie/capitalize-symbol)
(global-set-key (kbd "s-u") 'eddie/upcase-symbol)
(global-set-key (kbd "s-l") 'eddie/downcase-symbol)


;;; Define word at point

(require 'shr)

(defcustom def-width fill-column
  "Frame width to use for rendering.
May either be an integer specifying a fixed width in characters,
or nil, meaning that the full width of the window should be
used."
  :type '(choice (integer :tag "Fixed width in characters")
		 (const   :tag "Use the width of the window" nil))
  :group 'def)

(defvar def-external-rendering-functions nil)

(defun def-transform-dom (dom)
  (let ((result (list (pop dom))))
    (dolist (arg (pop dom))
      (push (cons (intern (concat ":" (symbol-name (car arg))) obarray)
		  (cdr arg))
	    result))
    (dolist (sub dom)
      (if (stringp sub)
	  (push (cons 'text sub) result)
	(push (def-transform-dom sub) result)))
    (nreverse result)))

(defun def-remove-trailing-whitespace (start end))

(defun def-insert-document (dom)
  "Render the parsed document DOM into the current buffer.
DOM should be a parse tree as generated by
`libxml-parse-html-region' or similar."
  (setq def-content-cache nil)
  (let ((start (point))
	(def-state nil)
	(def-start nil)
	(def-base nil)
	(def-width (or def-width (1- (window-width)))))
    (def-descend (def-transform-dom dom))
    ;;(prin1 (def-transform-dom dom))
    (def-remove-trailing-whitespace start (point))))

(defun def-descend (dom)
  (let ((function
	 (or
	  ;; Allow other packages to override (or provide) rendering
	  ;; of elements.
	  (cdr (assq (car dom) def-external-rendering-functions))
	  (intern (concat "def-tag-" (symbol-name (car dom))) obarray)))
	(style (cdr (assq :style (cdr dom))))
	(start  (point)))
    (if (fboundp function)
	(funcall function (cdr dom))
      (def-generic (cdr dom)))))

(defsubst def-generic (cont)
  (dolist (sub cont)
    (cond
     ((eq (car sub) 'text)
      (shr-insert (cdr sub)))
     ((listp (cdr sub))
      (def-descend sub)))))

(defun def-tag-body (cont)
  (def-generic cont))

(defun def-tag-span (cont)
  (let ((class (cdr (assq :class (cdr sub)))))
    ;; insert newlines before
    (cond
     ((string-match "t_first" class)
      (insert "\n\n"))
     ((string-match "t_subsense" class)
      (insert "\n"))
     ((string-match "t_derivatives" class)
      (insert "\n"))
     ((string= class "etym")
      (insert "\n"))
     ((string= class "note")
      (iqnsert "\n")))
    ;; insert the body
    (def-generic cont)
    ;; insert newlines after
    (cond
     ((string= class "hg")
      (insert "\n"))
     ((string= class "posg")
      (insert ""))
     ((string-match "t_derivatives" class)
      (insert "\n"))
     ((string= class "etym")
      (insert "\n"))
     ((string= class "note")
      (insert "\n")))))

(defun define-word--format-definition ()
  "Formats a buffer containing a definition from the Dictionary
command."
  (let ((body (libxml-parse-html-region (point-min) (point-max))))
    (erase-buffer)
    (def-insert-document body)))

(defun define-word--internal (word)
  "Puts the definition for WORD in a temporary buffer."
  (with-temp-buffer-window
   "*Definition*" nil nil
   (with-current-buffer standard-output
     (call-process-shell-command
      (concat "Dictionary --html " (shell-quote-argument word)) nil t nil)
     (define-word--format-definition))))

(defun define-word ()
  "Display the definition of word at point."
  (interactive)
  (define-word--internal (thing-at-point 'word)))

(global-set-key (kbd "M-#") 'define-word)

;;; Stefan Monnier <foo at acm.org>. It is the opposite of fill-paragraph
(defun unfill-paragraph (&optional region)
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max))
        ;; This would override `fill-column' if it's an integer.
        (emacs-lisp-docstring-fill-column t))
    (fill-paragraph nil region)))

;; Handy key definition
(define-key global-map "\M-Q" 'unfill-paragraph)

(require 'plist-mode)

;; exec-path-from-shel
;; Emacs library to ensure environment variables inside Emacs look the
;; same as in the user's shell.
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;;; Frequent files

;; Open Emacs init file
(global-set-key (kbd "C-c i")
		(lambda ()
		  (interactive)
		  (find-file "~/.emacs.d/init.el")))

;; Open .profile file
(global-set-key (kbd "C-c p")
		(lambda ()
		  (interactive)
		  (find-file "~/.profile")))

;; No need for copy & paste
(global-set-key (kbd "C-c f") 'find-file-at-point)
(global-set-key (kbd "C-c w") 'browse-url-at-point)
