;; Eddie's Initialization File
(message "Loading Eddie's initialization…")

;; Shut your yap about "Package cl is deprecated"
(setq byte-compile-warnings '(cl-functions))

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

(setq-default frame-title-format
	      '(buffer-file-name "%f"
                (dired-directory dired-directory "%b")))
;; icon-title-format


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
		     "DejaVuSansMono Nerd Font Book 11"
		   "DejaVuSansMono Nerd Font Book 15"))
                     ;;"DejaVu Sans Mono Book 11"
                   ;;"DejaVu Sans Mono Book 15"))
        ;; (menu-bar-lines . 0) ;; causes the menu-bar in MacOS X to
        ;; disappear
        (tool-bar-lines . 0)
        (width . 80)))

(setq initial-frame-alist eddie/default-frame-style)
(setq default-frame-alist eddie/default-frame-style)

;; (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
;; (add-to-list 'default-frame-alist '(ns-appearance . dark))

;; (setq header-line-format nil)
;; (setq mode-line-format nil)

;; (use-package simple-modeline
;;   :hook (after-init . simple-modeline-mode))

(use-package mood-line
  :hook (after-init . mood-line-mode))

;; Path config
(message "Loading Eddie's configuration paths…")

;; Looks like we don't need exec-path-from-shell with Yamamoto
;; Mitsuharu's Mac port of GNU Emacs

;; Ensure environment variables from the shell are in sync with Emacs
;;(use-package exec-path-from-shell)

;; exec-path-from-shel
;; Emacs library to ensure environment variables inside Emacs look the
;; same as in the user's shell.
;;(when (memq window-system '(mac ns x))
;;  (exec-path-from-shell-initialize))

;; Don't need to manually set the path since we get it from the shell
;; (see above exec-path-from-shell).
;; (setenv "PATH"
;;         (concat (getenv "PATH")
;; 		":/Library/TeX/texbin"
;;                 ":/opt/local/bin"
;;                 ":/opt/local/sbin"
;; 		":/usr/local/bin"
;;                 ":/Users/eddie/bin"))

(defconst eddie/default-emacs-dir "~/.emacs.d/"
  "My default emacs director.")

(add-to-list 'load-path "~/site-lisp/plist-mode")
(add-to-list 'load-path "~/.emacs.d/site-lisp")
;;(add-to-list 'load-path "/opt/local/share/emacs/site-lisp")
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu/mu4e/")

;;(setq exec-path (append exec-path '("/Library/TeX/texbin")))
;;(setq exec-path (append exec-path '("/opt/local/bin")))
;;(setq exec-path (append exec-path '("/usr/local/bin")))

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
;;              '("melpa" . "http://melpa.org/packages/"))

;; Defunct package repos:
;; (add-to-list 'package-archives
;; 	     '("melpa-stable" . "http://melpa-stable.milkbox.net/packages/") t)
;; (add-to-list 'package-archives
;;              '("marmalade" . "http://marmalade-repo.org/packages/"))

(package-initialize)

;; install use-package if necessary
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; package syntactic sugar
(require 'use-package)
(setq use-package-verbose t) ;; make it easy to debug
(setq use-package-always-ensure t) ;; install packages on-demand

(require 'subr-x)
(setq eddie/xcode-developer-dir
      (string-trim (shell-command-to-string "xcode-select -p")))

;; (find-file eddie/xcode-developer-dir)
;; (setq eddie/xcode-sdk-headers-dir "")


;; Theme config
(message "Loading Eddie's configuration theme…")

;; Don't scale any zenburn faces!
;;
;; From the font face info pages:
;;
;;    The height of the font. In the simplest case, this is an integer
;;    in units of 1/10 point.
;;
;;    The value can also be floating point or a function, which
;;    specifies the height relative to an “underlying face”
;;
;; NOTE: The value must be set to a float "1.0" or it will think 1
;; isn't a scale of default face size.
(setq zenburn-height-minus-1 1.0
      zenburn-height-plus-1 1.0
      zenburn-height-plus-2 1.0
      zenburn-height-plus-3 1.0
      zenburn-height-plus-4 1.0)

(use-package zenburn-theme
  :init
  (add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
  :config
  (load-theme 'zenburn t))

(use-package rainbow-mode)

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

(use-package htmlize)

;;; Org mode

(require 'org-tempo)

(setq org-CUA-compatible t)
(setq org-use-extra-keys t)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(global-set-key "\C-cl" 'org-store-link)
(setq org-log-done nil)

;; org html export
(require 'ox)

(org-export-define-derived-backend 'html-styles 'html
  :menu-entry
  '(?h 1
       ((?D "As HTML (Default)" eddie/org-html-export-to-html-with-default-style)
	(?d "As HTML (Default) File"
	    (lambda (a s v b)
	      (if a (eddie/org-html-export-to-html-with-default-style t s v b)
		(org-open-file
		 (eddie/org-html-export-to-html-with-default-style nil s v b)))))
	(?E "As HTML (Empty)" eddie/org-html-export-to-html-with-empty-style)
	(?e "As HTML (Empty) File"
	    (lambda (a s v b)
	      (if a (eddie/org-html-export-to-html-with-empty-style t s v b)
		(org-open-file
		 (eddie/org-html-export-to-html-with-empty-style nil s v b)))))
	(?F "As HTML (Foo)" eddie/org-html-export-to-html-with-foo-style)
	(?f "As HTML (Foo) File"
	    (lambda (a s v b)
	      (if a (eddie/org-html-export-to-html-with-foo-style t s v b)
		(org-open-file
		 (eddie/org-html-export-to-html-with-foo-style nil s v b))))))))

(defun eddie/org-html-export-style-default ()
  (setq-local org-html-doctype "html5")
  (setq-local org-html-html5-fancy t)
  (setq-local org-html-head-include-default-style t)
  (setq-local org-html-head "")
  (setq-local org-html-head-include-scripts t)
  (setq-local org-html-preamble t)
  (setq-local org-html-preamble-format '(("en" "")))
  (setq-local org-html-postamble t)
  (setq-local org-html-postamble-format
	'(("en" "<p class=\"author\">Author: %a (%e)</p>
<p class=\"date\">Date: %d</p>
<p class=\"creator\">%c</p>")))
  (setq-local org-html-htmlize-output-type 'css)
  (setq-local org-html-with-latex 'mathjax)
  (setq-local org-html-self-link-headlines nil)
  (setq-local org-html-toplevel-hlevel 2))

(defun eddie/org-html-export-style-empty ()
  (setq-local org-html-doctype "html5")
  (setq-local org-html-html5-fancy t)
  (setq-local org-html-viewport
      '((width "device-width")
	(initial-scale "1")
	(minimum-scale "")
	(maximum-scale "1")
	(user-scalable "")))
  (setq-local org-html-head-include-default-style nil)
  (setq-local org-html-head "")
  (setq-local org-html-head-include-scripts nil)
  (setq-local org-html-preamble t)
  (setq-local org-html-preamble-format '(("en" "")))
  (setq-local org-html-postamble t)
  (setq-local org-html-postamble-format
	'(("en" "<p class=\"author\">Author: %a</p>
<p class=\"date\">Date: %d</p>")))
  (setq-local org-html-htmlize-output-type 'css)
  (setq-local org-html-with-latex 'verbatim)
  (setq-local org-html-self-link-headlines nil)
  (setq-local org-html-toplevel-hlevel 1))

(defun eddie/org-html-export-style-foo ()
  (setq-local org-html-doctype "html5")
  (setq-local org-html-html5-fancy t)
  (setq-local org-html-head-include-default-style nil)
  (setq-local org-html-head
    (concat
     ;; (eddie/html-style-tag-with-file "css/reset.css")
     ;; (eddie/html-style-tag-with-file "css/style.css")
     (string-from-file (concat eddie/default-emacs-dir "html-resources/remote-orgcss.html"))
     (string-from-file (concat eddie/default-emacs-dir "html-resources/remote-katex.html"))))
  (setq-local org-html-head-include-scripts nil)
  (setq-local org-html-preamble t)
  (setq-local org-html-preamble-format '(("en" "")))
  (setq-local org-html-postamble t)
  (setq-local org-html-postamble-format
	'(("en" "<p class=\"author\">Author: %a</p>
<p class=\"date\">Date: %d</p>")))
  (setq-local org-html-htmlize-output-type 'css)
  (setq-local org-html-with-latex 'verbatim)
  (setq-local org-html-self-link-headlines nil)
  (setq-local org-html-toplevel-hlevel 2))

(defun eddie/org-html-export-to-html-with-default-style
    (&optional async subtreep visible-only body-only ext-plist)
  (eddie/org-html-export-style-default)
  (org-html-export-to-html async subtreep visible-only body-only ext-plist))

(defun eddie/org-html-export-to-html-with-empty-style
    (&optional async subtreep visible-only body-only ext-plist)
  (eddie/org-html-export-style-empty)
  (org-html-export-to-html async subtreep visible-only body-only ext-plist))

(defun eddie/org-html-export-to-html-with-foo-style
    (&optional async subtreep visible-only body-only ext-plist)
  (eddie/org-html-export-style-foo)
  (org-html-export-to-html async subtreep visible-only body-only ext-plist))

;; org notes & capture
(setq org-default-notes-file "~/CloudStation/notes.org")
(global-set-key "\C-cc" 'org-capture)
(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/CloudStation/plan/inbox.org" "Tasks")
	 "* TODO %?\n %i\n %a")))

(add-to-list 'org-structure-template-alist '("el" . "src elisp"))
(add-to-list 'org-structure-template-alist '("sh" . "src sh"))
; org-tempo-keywords-alist

(define-skeleton eddie/org-doc-info
  "Insert common document info comments."
  "Title: "
"#+TITLE: " str \n
"#+DATE: " (format-time-string "%F") \n
"#+STARTUP: showall" \n "" \n "" \n)
(define-abbrev org-mode-abbrev-table "di" "" 'eddie/org-doc-info)

(add-hook 'org-mode-hook (lambda () (auto-fill-mode)))

;; Org babel
;; (require 'ob-sh)
(require 'ob-ditaa)
(require 'ob-js)
(require 'ob-python)
(require 'ob-C)
(require 'ob-shell)
(org-babel-do-load-languages
 'org-bable-load-languages
 '((emacs-lisp . t)
   (sh . t)
   (C . t)
   (cpp . t)
   (css . t)
   (ditaa . t)
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
(setq org-confirm-babel-evaluate nil)

(setq org-ditaa-jar-path "/usr/local/Cellar/ditaa/0.11.0/libexec/ditaa-0.11.0-standalone.jar")

(use-package ox-hugo
  :ensure t
  :after ox)

(require 'ox-org)

(use-package ox-json
  :ensure t
  :after ox)

(require 'ox-json)

;; default indent style
(setq tab-width 4)
(setq-default indent-tabs-mode nil)

(use-package lorem-ipsum
  :load-path "~/src/emacs-lorem-ipsum"
  :config
  (global-set-key (kbd "C-c C-l s") 'lorem-ipsum-insert-sentences)
  (global-set-key (kbd "C-c C-l p") 'lorem-ipsum-insert-paragraphs)
  (global-set-key (kbd "C-c C-l l") 'lorem-ipsum-insert-list))

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

;; TODO: Xcode-style header/implementation switching
;; Projectile `C-c C-p a' works

(define-skeleton eddie/c-protect-include
  "Insert ifndef multi-include protection."
  "Define: "
"#ifndef " str \n
"#define " str \n "" \n "" _ "" \n "" \n
"#endif /* " str " */" \n)
(define-abbrev c-mode-abbrev-table "cpi" "" 'eddie/c-protect-include)


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

;; JS Modules
(add-to-list 'auto-mode-alist '("\\.mjs\\'" . js-mode))

;; ActionScript
(add-to-list 'auto-mode-alist '("\\.as\\'" . actionscript-mode))

;; Obj-C, Objective-C, objc, Cocoa
(add-to-list 'auto-mode-alist '("\\.mm\\'" . objc-mode))

(defun eddie/objc-headerp ()
  (and buffer-file-name
       (string= (file-name-extension buffer-file-name) "h")
       (re-search-forward "@\\<interface\\>"
			  magic-mode-regexp-match-limit t)))

(add-to-list 'magic-mode-alist '(eddie/objc-headerp . objc-mode))

;; Web mode
(use-package web-mode)

;; Swift mode config
(use-package swift-mode)

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

(use-package yaml-mode)
(use-package dockerfile-mode)
(use-package foreman-mode)
(use-package dotenv-mode)

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

(defun eddie/eshell-name-buffer-with-dir ()
  (let* ((curdir (eshell/pwd))
	 (name (car (last (split-string curdir "/" t)))))
      (rename-buffer (concat "  " name "") t)))

(add-hook 'eshell-mode-hook
	  'eddie/eshell-name-buffer-with-dir)

(add-hook 'eshell-directory-change-hook
	    'eddie/eshell-name-buffer-with-dir)

;; Must return nil to indicate that we are not intercepting the
;; command
(add-hook 'eshell-named-command-hook
	  (lambda (cmd args)
	    (rename-buffer
	     (concat " ️" cmd "") t) nil))

(add-hook 'eshell-post-command-hook
	  'eddie/eshell-name-buffer-with-dir)

;; http://www.howardism.org/Technical/Emacs/eshell-fun.html
(defun eddie/eshell-for-current-dir ()
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

(defun eddie/recent-eshell ()
  (seq-some (lambda (buf)
	      (if (equal
		   (with-current-buffer buf major-mode)
		   'eshell-mode)
		  buf
		nil))
	    (buffer-list)))

(defun eddie/eshell-recent-or-new ()
  (interactive)
  (let ((buf (get-buffer-create (or (eddie/recent-eshell)
				    eshell-buffer-name))))
    (cl-assert (and buf (buffer-live-p buf)))
    (pop-to-buffer-same-window buf)
    (unless (derived-mode-p 'eshell-mode)
      (eshell-mode))
    buf))

(defun eddie/eshell-new ()
  (interactive)
  (eshell 'N))

;; (defun eddie/eshell-recent-or-new-cur-dir ()
;;   (interactive))

;; (defun eddie/eshell-new-cur-dir ()
;;   (interactive))

;; quick access
(global-set-key (kbd "C-c e e") 'eddie/eshell-recent-or-new)
(global-set-key (kbd "C-c e E") 'eddie/eshell-new)
;; (global-set-key (kbd "C-c e d") 'eddie/eshell-recent-or-new-cur-dir)
;; (global-set-key (kbd "C-c e D") 'eddie/eshell-new-cur-dir)


(setq eshell-visual-subcommands '())
(setq eshell-visual-options '())

;;(add-to-list 'eshell-visual-commands "brew")
(add-to-list 'eshell-visual-subcommands '("brew" "install"))
(add-to-list 'eshell-visual-options '("curl" "-O"))

;; remove first
;; (setq eshell-visual-commands
;;       (cdr eshell-visual-commands))

;; Tramp - vagrant

(use-package vagrant-tramp
  :after dash
  :load-path "~/site-lisp/vagrant-tramp")

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

;;; YASnippet
(use-package yasnippet)
(use-package yasnippet-snippets
  :after yasnippet)
(yas-global-mode 1)

;;; Magit

(use-package magit
  :init
  (global-set-key (kbd "C-x g") 'magit-status))

(defun eddie/magit-status-bufferp ()
  (string= "magit-status-mode" major-mode))

;;; Company mode

;; use tab to indent or complete
(defun tab-indent-or-complete ()
  (interactive)
  (if (minibufferp)
      (minibuffer-complete)
    (if (eddie/magit-status-bufferp)
	(magit-section-toggle (magit-current-section))
      (if (looking-at "\\_>")
	  (company-complete-common)
	(indent-for-tab-command)))))

(use-package company
  :ensure t
  :config

  ;; turn off auto-complete
  (setq company-idle-delay nil)
  (global-set-key (kbd "<tab>") 'tab-indent-or-complete)

  :bind (:map company-active-map
	      ("C-p" . company-select-previous)
	      ("C-n" . company-select-next)))

;; magit-mode-map

(add-hook 'after-init-hook 'global-company-mode)

;;; Language Server Protocol (LSP) mode

(use-package lsp-mode
  :config
  (setq lsp-enable-snippet t)

  (setq lsp-enable-text-document-color nil)
  (setq lsp-log-io t)

  (setq read-process-output-max (* 1024 1024))
  (setq gc-cons-threshold 100000000)
  (setq lsp-clients-clangd-executable "/usr/local/opt/llvm/bin/clangd"))


(use-package lsp-sourcekit
  :after lsp-mode
  :load-path "~/src/lsp-sourcekit"
  :config
  (setq lsp-sourcekit-executable "/Applications/Xcode11.6.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/sourcekit-lsp"))

(add-hook 'lsp-managed-mode-hook
	  (lambda ()
	    (setq-local company-backends '(company-capf))))

(add-hook 'c-mode-hook (lambda () (lsp)))
(add-hook 'objc-mode-hook (lambda () (lsp)))
(add-hook 'swift-mode-hook (lambda () (lsp)))

;; Ruby LSP
;;(setq lsp-solargraph-diagnostics nil)
(add-hook 'ruby-mode-hook #'lsp)

;; Go LSP
;;(setq lsp-clients-go-diagnostics-enabled nil)
(add-hook 'go-mode-hook #'lsp)

;;(lsp-diagnostics-mode -1)

;; Disable diagnostics globally
;; comming soon? lsp-diagnostics-disabled-modes
(setq lsp-diagnostics-provider :none)
(setq lsp-modeline-diagnostics-enable nil)

(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs                 (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay      0.5
          treemacs-directory-name-transformer    #'identity
          treemacs-display-in-side-window        t
          treemacs-eldoc-display                 t
          treemacs-file-event-delay              5000
          treemacs-file-extension-regex          treemacs-last-period-regex-value
          treemacs-file-follow-delay             0.2
          treemacs-file-name-transformer         #'identity
          treemacs-follow-after-init             t
          treemacs-git-command-pipe              ""
          treemacs-goto-tag-strategy             'refetch-index
          treemacs-indentation                   2
          treemacs-indentation-string            " "
          treemacs-is-never-other-window         nil
          treemacs-max-git-entries               5000
          treemacs-missing-project-action        'ask
          treemacs-move-forward-on-expand        nil
          treemacs-no-png-images                 nil
          treemacs-no-delete-other-windows       t
          treemacs-project-follow-cleanup        nil
          treemacs-persist-file                  (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                      'left
          treemacs-read-string-input             'from-child-frame
          treemacs-recenter-distance             0.1
          treemacs-recenter-after-file-follow    nil
          treemacs-recenter-after-tag-follow     nil
          treemacs-recenter-after-project-jump   'always
          treemacs-recenter-after-project-expand 'on-distance
          treemacs-show-cursor                   nil
          treemacs-show-hidden-files             t
          treemacs-silent-filewatch              nil
          treemacs-silent-refresh                nil
          treemacs-sorting                       'alphabetic-asc
          treemacs-space-between-root-nodes      t
          treemacs-tag-follow-cleanup            t
          treemacs-tag-follow-delay              1.5
          treemacs-user-mode-line-format         nil
          treemacs-user-header-line-format       nil
          treemacs-width                         35
          treemacs-workspace-switch-cleanup      nil)

    (treemacs-resize-icons 20)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode 'always)
    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple))))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-projectile
  :after (treemacs projectile)
  :ensure t)

(use-package treemacs-icons-dired
  :after (treemacs dired)
  :ensure t
  :config (treemacs-icons-dired-mode))

(use-package treemacs-magit
  :after (treemacs magit)
  :ensure t)

(require 'treemacs-themes)
(require 'treemacs-icons)
(treemacs-create-theme "Normal"
  :icon-directory (concat eddie/default-emacs-dir "/icons")
  :extends "Default"
  :config
  (progn
    ;; directory and generic icon
    (treemacs-create-icon
     :file "folder-closed.png"
     :extensions (root)
     :fallback "")
    (treemacs-create-icon
     :file "folder-closed.png"
     :extensions (dir-closed)
     :fallback (propertize "+ " 'face 'treemacs-term-node-face))
    (treemacs-create-icon
     :file "folder-closed.png"
     :extensions (dir-open)
     :fallback (propertize "- " 'face 'treemacs-term-node-face))
    (treemacs-create-icon
     :file "file.png"
     :extensions (fallback))

    ;; file types
    (treemacs-create-icon
     :file "PBX-applescript-Icon.tiff"
     :extensions ("applescript"))
    ;; (treemacs-create-icon
    ;;  :file "asciidoc.png"
    ;;  :extensions ("adoc" "asciidoc"))
    ;; (treemacs-create-icon
    ;;  :file "asm.png"
    ;;  :extensions ("asm" "arm"))
    ;; (treemacs-create-icon
    ;;  :file "audio.png"
    ;;  :extensions ("mp3" "ogg" "oga" "wav" "flac"))
    ;; (treemacs-create-icon
    ;;  :file "babel.png"
    ;;  :extensions ("babel"))
    ;; (treemacs-create-icon
    ;;  :file "babel.png"
    ;;  :extensions ("babelrc" "babelignore" "babelrc.js"
    ;; 		  "babelrc.json" "babel.config.js"))
    (treemacs-create-icon
     :file "ExecutableRef.tiff"
     :extensions ("exe" "dll" "obj" "so" "o"))
    (treemacs-create-icon
     :file "PBX-c-Icon.tiff"
     :extensions ("c"))
    (treemacs-create-icon
     :file "PBX-h-Icon.tiff"
     :extensions ("h"))
    ;; (treemacs-create-icon
    ;;  :file "cert.png"
    ;;  :extensions ("csr" "crt" "cer" "der" "pfx" "p12"
    ;; 		  "p7b" "p7r" "src" "crl" "sst" "stl"))
    ;; (treemacs-create-icon
    ;;  :file "cmake.png"
    ;;  :extensions ("cmake" "cmake-cache"))
    ;; (treemacs-create-icon
    ;;  :file "conf.png"
    ;;  :extensions ("properties" "conf" "config" "cfg"
    ;; 		  "ini" "xdefaults" "xresources"
    ;; 		  "terminalrc" "ledgerrc"))
    (treemacs-create-icon
     :file "PBX-cpp-Icon.tiff"
     :extensions ("cpp" "cxx" "tpp" "cc"))
    (treemacs-create-icon
     :file "PBX-hpp-Icon.tiff"
     :extensions ("hpp" "hh"))
    (treemacs-create-icon
     :file "css.png"
     :extensions ("css"))
    ;; (treemacs-create-icon
    ;;  :file "diff.png"
    ;;  :extensions ("diff"))
    ;; (treemacs-create-icon
    ;;  :file "direnv.png"
    ;;  :extensions ("envrc"))
    ;; (treemacs-create-icon
    ;;  :file "docker.png"
    ;;  :extensions ("dockerfile" "docker-compose.yml"))
    ;; (treemacs-create-icon
    ;;  :file "editorcfg.png"
    ;;  :extensions ("editorconfig"))
    (treemacs-create-icon
     :file "elisp.png"
     :extensions ("el"))
    (treemacs-create-icon
     :file "elisp-bytecode.png"
     :extensions ("elc"))
    (treemacs-create-icon
     :file "elisp-gzip.png"
     :extensions ("el.gz"))
    ;; (treemacs-create-icon
    ;;  :file "erb.png"
    ;;  :extensions ("erb"))
    ;; (treemacs-create-icon
    ;;  :file "erlang.png"
    ;;  :extensions ("erl" "hrl"))
    ;; (treemacs-create-icon
    ;;  :file "eslint.png"
    ;;  :extensions ("eslintrc" "eslintignore" "eslintcache"))
    (treemacs-create-icon
     :file "font.png"
     :extensions ("woff" "woff2" "ttf" "otf"
		  "eot" "pfa" "pfb" "sfd"))
    ;; (treemacs-create-icon
    ;;  :file "git.png"
    ;;  :extensions ("git" "gitignore" "gitconfig"
    ;; 		  "gitmodules" "gitattributes"))
    (treemacs-create-icon
     :file "go-doc.png"
     :extensions ("go"))
    (treemacs-create-icon
     :file "markup.png"
     :extensions ("html" "htm"))
    (treemacs-create-icon
     :file "image.png"
     :extensions ("jpg" "jpeg" "bmp" "svg" "png"
		  "xpm" "gif" "tiff" "tga" "heic"
		  "heif"))
    ;; (treemacs-create-icon
    ;;  :file "jar.png"
    ;;  :extensions ("jar"))
    ;; (treemacs-create-icon
    ;;  :file "java.png"
    ;;  :extensions ("java"))
    ;; (treemacs-create-icon
    ;;  :file "jinja2.png"
    ;;  :extensions ("j2" "jinja2"))
    (treemacs-create-icon
     :file "js.png"
     :extensions ("js" "jsx"))
    ;; (treemacs-create-icon
    ;;  :file "json.png"
    ;;  :extensions ("json"))
    ;; (treemacs-create-icon
    ;;  :file "key.png"
    ;;  :extensions ("key" "pem"))
    ;; (treemacs-create-icon
    ;;  :file "license.png"
    ;;  :extensions ("license"))
    ;; (treemacs-create-icon
    ;;  :file "lisp.png"
    ;;  :extensions ("lisp"))
    ;; (treemacs-create-icon
    ;;  :file "log.png"
    ;;  :extensions ("log"))
    ;; (treemacs-create-icon
    ;;  :file "lua.png"
    ;;  :extensions ("lua"))
    (treemacs-create-icon
     :file "makefile.png"
     :extensions ("makefile"))
    ;; (treemacs-create-icon
    ;;  :file "manifest.png"
    ;;  :extensions ("manifest"))
    ;; (treemacs-create-icon
    ;;  :file "markdown.png"
    ;;  :extensions ("md"))
    ;; (treemacs-create-icon
    ;;  :file "nginx.png"
    ;;  :extensions ("nginx.conf" "nginx"))
    ;; (treemacs-create-icon
    ;;  :file "npm.png"
    ;;  :extensions ("npmignore" "npmrc" "package.json"
    ;; 		  "package-lock.json" "npm-shrinwrap.json"))
    ;; (treemacs-create-icon
    ;;  :file "org.png"
    ;;  :extensions ("org"))
    ;; (treemacs-create-icon
    ;;  :file "patch.png"
    ;;  :extensions ("patch"))
    ;; (treemacs-create-icon
    ;;  :file "pdf.png"
    ;;  :extensions ("pdf"))
    ;; (treemacs-create-icon
    ;;  :file "perl.png"
    ;;  :extensions ("pl" "pm" "perl"))
    ;; (treemacs-create-icon
    ;;  :file "perl6.png"
    ;;  :extensions ("perl6"))
    ;; (treemacs-create-icon
    ;;  :file "pgsql.png"
    ;;  :extensions ("pgsql"))
    ;; (treemacs-create-icon
    ;;  :file "php.png"
    ;;  :extensions ("php"))
    ;; (treemacs-create-icon
    ;;  :file "phpunit.png"
    ;;  :extensions ("phpunit" "phpunit.xml"))
    ;; (treemacs-create-icon
    ;;  :file "pip.png"
    ;;  :extensions ("pipfile" "pipfile.lock"
    ;; 		  "pip-requirements"))
    (treemacs-create-icon
     :file "Package.tiff"
     :extensions ("pkg"))
    ;; (treemacs-create-icon
    ;;  :file "plsql.png"
    ;;  :extensions ("plsql" "orcale"))
    ;; (treemacs-create-icon
    ;;  :file "project.png"
    ;;  :extensions ("project"))
    ;; (treemacs-create-icon
    ;;  :file "puppet.png"
    ;;  :extensions ("pp"))
    (treemacs-create-icon
     :file "python-doc.png"
     :extensions ("py" "pyc"))
    ;; (treemacs-create-icon
    ;;  :file "r.png"
    ;;  :extensions ("r"))
    ;; (treemacs-create-icon
    ;;  :file "rake.png"
    ;;  :extensions ("rake" "rakefile"))
    (treemacs-create-icon
     :file "ruby-doc.png"
     :extensions ("rb"))
    ;; (treemacs-create-icon
    ;;  :file "rust.png"
    ;;  :extensions ("rs"))
    ;; (treemacs-create-icon
    ;;  :file "scons.png"
    ;;  :extensions ("sconstruct" "sconstript"))
    ;; (treemacs-create-icon
    ;;  :file "scss.png"
    ;;  :extensions ("scss"))
    (treemacs-create-icon
     :file "ExecutableRef.tiff"
     :extensions ("sh" "zsh" "fish"))
    ;; (treemacs-create-icon
    ;;  :file "sql.png"
    ;;  :extensions ("sql"))
    ;; (treemacs-create-icon
    ;;  :file "sqlite.png"
    ;;  :extensions ("sqlite" "db3" "sqlite3"))
    ;; (treemacs-create-icon
    ;;  :file "swift.png"
    ;;  :extensions ("swift"))
    ;; (treemacs-create-icon
    ;;  :file "tex.png"
    ;;  :extensions ("tex"))
    ;; (treemacs-create-icon
    ;;  :file "toml.png"
    ;;  :extensions ("toml"))
    ;; (treemacs-create-icon
    ;;  :file "vagrant.png"
    ;;  :extensions ("vagrantfile"))
    ;; (treemacs-create-icon
    ;;  :file "video.png"
    ;;  :extensions ("webm" "mp4" "avi" "mkv" "flv"
    ;; 		  "mov" "wmv" "mpg" "mpeg" "mpv"))
    ;; (treemacs-create-icon
    ;;  :file "xml.png"
    ;;  :extensions ("xml" "xsl"))
    ;; (treemacs-create-icon
    ;;  :file "yaml.png"
    ;;  :extensions ("yml" "yaml" "travis.yml"))
    ;; (treemacs-create-icon
    ;;  :file "yarn.png"
    ;;  :extensions ("yarn.lock" "yarnrc" "yarnclean"
    ;; 		  "yarn-integrity" "yarn-metadata.json"
    ;; 		  "yarnignore"))
    ;; (treemacs-create-icon
    ;;  :file "zip.png"
    ;;  :extensions ("zip" "7z" "tar" "gz"
    ;; 		  "rar" "tar.gz"))))
    ))

(treemacs-load-theme "Normal")

(require 'treemacs-icons-dired)

;; don't underline non-breaking space
(set-face-attribute 'nobreak-space nil
		    :underline nil)

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
      (concat "Dictionary --html "
	      (shell-quote-argument word)) nil t nil)
     (define-word--format-definition))))

(defun define-word ()
  "Display the definition of word at point."
  (interactive)
  (define-word--internal (thing-at-point 'word)))

(global-set-key (kbd "M-#") 'define-word)


;;;  Pargraphs

;;; Stefan Monnier <foo at acm.org>. It is the opposite of
;;; fill-paragraph
(defun unfill-paragraph (&optional region)
  "Takes a multi-line paragraph and makes it into a single line
of text."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max))
        ;; This would override `fill-column' if it's an integer.
        (emacs-lisp-docstring-fill-column t))
    (fill-paragraph nil region)))

;; Handy key definition
(define-key global-map "\M-Q" 'unfill-paragraph)

;; Emacs showing its age by thinking sentences are delimited by a
;; double-space. Not on my watch!
(setq sentence-end-double-space nil)


(require 'plist-mode)
(add-to-list 'auto-mode-alist '("\\.pbfilespec$" . plist-mode))
(add-to-list 'auto-mode-alist '("\\.pbcompspec$" . plist-mode))
(add-to-list 'auto-mode-alist '("\\.xcspec$" . plist-mode))

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


;;; Calc

(setq math-additional-units
      '((bit   nil	      "Bit")	 (B   "8 * bit"	   "Byte")

	;; SI prefix
	(kbit  "1000 * bit"   "Kilobit") (kB  "1000 * B"   "Kilobyte")
	(Mbit  "1000 * kbit"  "Megabit") (MB  "1000 * kB"  "Megabyte")
	(Gbit  "1000 * Mbit"  "Gigabit") (GB  "1000 * MB"  "Gigabyte")
	(Tbit  "1000 * Gbit"  "Terabit") (TB  "1000 * GB"  "Terabyte")

	;; ISO/IEC 80000
	(Kibit "1024 * bit"   "Kibibit") (KiB "1024 * B"   "Kibibyte")
	(Mibit "1024 * Kibit" "Mebibit") (MiB "1024 * KiB" "Mebibyte")
	(Gibit "1024 * Mibit" "Gibibit") (GiB "1024 * MiB" "Gibibyte")
	(Tibit "1024 * Gibit" "Tebibit") (TiB "1024 * GiB" "Tebibyte")

	;; lazy programmer notation
	(b "B" "Byte") ;; (byte "B" "Byte") (Byte "B" "Byte")
	(kb "KiB" "Kibibyte") (mb "MiB" "Mebibyte")
	(gb "GiB" "Gibibyte") (tb "TiB" "Tebibyte"))
      math-units-table nil)


;;; smgl-mode, html-mode, etc

;; Don't underline HTML title or headings h1-h6
(require 'sgml-mode)
(add-to-list 'html-tag-face-alist '("title" bold))
(add-to-list 'html-tag-face-alist '("h1" bold))
(add-to-list 'html-tag-face-alist '("h2" bold-italic))
(add-to-list 'html-tag-face-alist '("h3" italic))
(add-to-list 'html-tag-face-alist '("h4" . default))
(add-to-list 'html-tag-face-alist '("h5" . default))
(add-to-list 'html-tag-face-alist '("h6" . default))


;;; Date helpers

;; ISO 8601 date or 'merican style with prefix argument
(defun eddie/insert-date (arg)
  (interactive "P")
  (insert (format-time-string (if arg "%D" "%F"))))

(defun eddie/insert-time ()
   (interactive)
   (insert (format-time-string "%-l:%M %p")))

(defun eddie/insert-datetime ()
  (interactive)
  (insert (format-time-string "%F %-l:%M %p")))

;; Full ISO 8601 formatted timestamp (2020-01-19T17:01:54-0800)
(defun eddie/insert-timestamp ()
  (interactive)
  (insert (format-time-string "%FT%T%z")))

(global-set-key (kbd "C-c d") 'eddie/insert-date)


;;; Image mode

(defun eddie/edit-image-in-acorn ()
  (interactive)
  (shell-command (format "open -a Acorn \"%s\"" (buffer-file-name))))

(add-hook 'image-mode-hook
	  (lambda ()
	    (local-set-key (kbd "e") 'eddie/edit-image-in-acorn)))


;;; Wrapping

(use-package visual-fill-column)

;; turn wrapping off (no wrap)
(global-set-key (kbd "C-c v n")
		(lambda ()
		  (interactive)
		  (visual-fill-column-mode -1)
		  (visual-line-mode -1)
		  (toggle-word-wrap -1)))


;; wrap to frame
(global-set-key (kbd "C-c v w")
		(lambda ()
		  (interactive)
		  (visual-fill-column-mode -1)
		  (visual-line-mode 1)))

;; wrap to fill column
(global-set-key (kbd "C-c v W")
		(lambda ()
		  (interactive)
		  (visual-fill-column-mode 1)
		  (visual-line-mode 1)))

(use-package fill-column-indicator)
(setq fci-rule-color "#6F6F6F")


;; Email & name

(defun eddie/full-name ()
  (or (getenv "FULL_NAME") "FULL_NAME.not.set"))

(defun eddie/email-primary ()
  (or (getenv "EMAIL_PRIMARY") "EMAIL_PRIMARY@not.set"))

(defun eddie/email-corp ()
  (or (getenv "EMAIL_CORP") "EMAIL_CORP@not.set"))

(defun eddie/email-blog ()
  (or (getenv "EMAIL_BLOG") "EMAIL_BLOG@not.set"))


;;; Gnus

;; Don't show the splash screen
(setq gnus-inhibit-startup-message t)

;; Keep gnus.el in the .emacs.d directory
(setq gnus-init-file "~/.emacs.d/gnus.el")

;; Quick access key
;; (global-set-key (kbd "C-c g") 'gnus)


;;; mu4e

(require 'mu4e)

;; use mu4e for e-mail in emacs
(setq mail-user-agent 'mu4e-user-agent)

;; avoids mbsync errors such as
;; "Maildir error: UID 26088 is beyond highest assigned UID 1672."
(setq mu4e-change-filenames-when-moving t)

;; Send mail with msmtp; these are the same that `gnus' uses.
(setq message-send-mail-function 'message-send-mail-with-sendmail
      sendmail-program "/usr/local/bin/msmtp")

;; By default show the senders email address
(setq mu4e-view-show-addresses t)

;; Always prefere plain text
(setq mu4e-view-html-plaintext-ratio-heuristic most-positive-fixnum)

; mu4e-compose-format-flowed ?

;; (setq mu4e-view-mode-hook nil)

;; The following doesn't work because the hook is called before
;; mu4e~view-message is set.

;; (add-hook 'mu4e-view-mode-hook
;;   (lambda ()
;;     (with-current-buffer mu4e~view-buffer-name
;;       (when (mu4e-message-has-field mu4e~view-message :body-txt)
;; 	(progn
;; 	  (visual-fill-column-mode 1)
;; 	  (visual-line-mode 1))))))

(defun eddie/toggle-visual-fill-column-mode ()
  "Toggle visual fill column based editing in the current
buffer."
  (interactive)
  (if visual-fill-column-mode
      (progn
	(visual-fill-column-mode -1)
	(visual-line-mode -1))
    (progn
      (visual-fill-column-mode 1)
      (visual-line-mode 1))))

(define-key mu4e-view-mode-map (kbd "W")
  'eddie/toggle-visual-fill-column-mode)

(define-key-after mu4e-view-mode-map
  [menu-bar headers wrap-column]
  '("Toggle wrap to fill column" . eddie/toggle-visual-fill-column-mode)
  'wrap-lines)

(defun eddie/mu4e-search-for-sender (msg)
  "Search for messages sent by the sender of the message at point."
  (mu4e-headers-search
    (concat "from:" (cdar (mu4e-message-field msg :from)))))

;; define 'x' as the shortcut
(add-to-list 'mu4e-view-actions
    '("xsearch for sender" . eddie/mu4e-search-for-sender) t)

;; define 'b' as the shortcut
(add-to-list 'mu4e-view-actions
    '("bview message in browser" . mu4e-action-view-in-browser) t)

;; Functions such as `match-func' are passed a complete message s-expression
;; https://www.djcbsoftware.nl/code/mu/mu4e/The-message-s_002dexpression.html

(setq mu4e-contexts `(
  ,(make-mu4e-context
    :name "Primary" ; iCloud
    :enter-func (lambda () (mu4e-message "→ Primary ctx"))
    :leave-func (lambda () (mu4e-message "⤺ Primary ctx"))
    :match-func
    (lambda (msg)
      (when msg
        (string-match-p "^/primary" (mu4e-message-field msg :maildir))))
    :vars
    `((user-mail-address . ,(eddie/email-primary))
      (user-full-name . ,(eddie/full-name))
      (mu4e-compose-signature . nil)
      (mu4e-maildir . "~/Maildir/primary")
      (mu4e-mu-home . "~/.mu/primary")
      (mu4e-sent-folder . "/Sent Messages")
      (mu4e-drafts-folder . "/Drafts")
      (mu4e-trash-folder . "/Deleted Messages")
      (mu4e-refile-folder . "/Archive")
      (mu4e-get-mail-command . "mbsync primary")))
  ,(make-mu4e-context
    :name "Corp" ; FastMail
    :enter-func (lambda () (mu4e-message "→ Corp ctx"))
    :leave-func (lambda () (mu4e-message "⤺ Corp ctx"))
    :match-func
    (lambda (msg)
      (when msg
        (string-match-p "^/corp" (mu4e-message-field msg :maildir))))
    :vars
    `((user-mail-address . ,(eddie/email-corp))
      (user-full-name . ,(eddie/full-name))
      (mu4e-maildir . "~/Maildir/corp")
      (mu4e-mu-home . "~/.mu/corp")
      (mu4e-compose-signature . nil)
      (mu4e-sent-folder . "/Sent")
      (mu4e-drafts-folder . "/Drafts")
      (mu4e-trash-folder . "/Trash")
      (mu4e-refile-folder . "/Archive")
      (mu4e-get-mail-command . "mbsync corp")))
  ,(make-mu4e-context
    :name "Blog" ; Gmail
    :enter-func (lambda () (mu4e-message "→ Blog ctx"))
    :leave-func (lambda () (mu4e-message "⤺ Blog ctx"))
    :match-func
    (lambda (msg)
      (when msg
        (string-match-p "^/blog" (mu4e-message-field msg :maildir))))
    :vars
    `((user-mail-address . ,(eddie/email-blog))
      (user-full-name . ,(eddie/full-name))
      (mu4e-compose-signature . nil)
      (mu4e-maildir . "~/Maildir/blog")
      (mu4e-mu-home . "~/.mu/blog")
      (mu4e-sent-folder . "/[Gmail]/Sent Mail")
      (mu4e-drafts-folder . "/[Gmail]/Drafts")
      (mu4e-trash-folder . "/[Gmail]/Trash")
      (mu4e-refile-folder . "/[Gmail]/All Mail")
      (mu4e-get-mail-command . "mbsync blog")))))

;; Gmail may need this:
;; (setq mu4e-sent-messages-behavior 'delete)
;; (setq mu4e-sent-messages-behavior (lambda ()
;;     (if (string= (message-sendmail-envelope-from) (eddie/email-blog))
;; 	'delete 'sent)))

;; More Gmail garbage:
;; (add-hook 'mu4e-mark-execute-pre-hook
;;     (lambda (mark msg)
;;       (cond ((member mark '(refile trash)) (mu4e-action-retag-message msg "-\\Inbox"))
;; 	    ((equal mark 'flag) (mu4e-action-retag-message msg "\\Starred"))
;; 	    ((equal mark 'unflag) (mu4e-action-retag-message msg "-\\Starred")))))

;; If your main Maildir is not configured as mu4e-maildir you'll get
;; this error: 'mu4e~start: Args out of range: "", 0, 1'. This is the
;; fix:
(setq mu4e-context-policy 'ask-if-none)

(eval-when-compile (require 'cl)) ;; lexical-let

(setq mu4e-confirm-quit nil)

(defun eddie/mu4e-is-current-account (account)
  (string= (mu4e-context-name (mu4e-context-current)) account))

(defun eddie/mu4e-switch-account (account)
  (if (and (mu4e~proc-running-p)
	   (not (eddie/mu4e-is-current-account account)))
      (lexical-let ((original-sentinel (process-sentinel mu4e~proc-process))
		    (original-account account))
	(set-process-sentinel mu4e~proc-process
			      (lambda (proc event)
				(funcall original-sentinel proc event)
				(mu4e-context-switch t original-account)
				(mu4e)))
	(mu4e-quit))
    (progn
      (mu4e-context-switch t account)
      (mu4e))))

(defun eddie/mu4e-primary ()
  "Start mu4e with primary account."
  (interactive)
  (eddie/mu4e-switch-account "Primary"))

(defun eddie/mu4e-corp ()
  "Start mu4e with corp account."
  (interactive)
  (eddie/mu4e-switch-account "Corp"))

(defun eddie/mu4e-blog ()
  "Start mu4e with blog account."
  (interactive)
  (eddie/mu4e-switch-account "Blog"))

(global-set-key (kbd "C-c m p") 'eddie/mu4e-primary)
(global-set-key (kbd "C-c m c") 'eddie/mu4e-corp)
(global-set-key (kbd "C-c m b") 'eddie/mu4e-blog)
(put 'upcase-region 'disabled nil)
