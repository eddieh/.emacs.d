;; Eddie's Initialization File
(message "Loading Eddie's initialization…")

;; Shut your yap about "Package cl is deprecated"
(setq byte-compile-warnings '(cl-functions))

;; Moving all the appearance stuff to the top

;; Appearance config
(message "Loading Eddie's configuration appearance…")

;; Kill the splash screen (death to all splash screens)
(setq inhibit-splash-screen t)
;;(defun use-fancy-splash-screens-p() nil)

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

;; ;; Enable emoji
;; (if window-system
;;     (set-fontset-font "fontset-default" 'unicode "Apple Color Emoji" nil 'prepend))

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
        (font . ,(if (memq system-type '(bsd gnu/linux windows-nt))
		     "DejaVuSansMono Nerd Font 11"
		   "DejaVuSansMono Nerd Font 15"))
		   ;;   "DejaVuSansMono Nerd Font Book 11"
		   ;; "DejaVuSansMono Nerd Font Book 15"))
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

;; keep customization in a separate file
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

;; Package config
(message "Loading Eddie's configuration package…")

(require 'package)
;; (setq package-enable-at-startup nil)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives
             '("elpa" . "https://tromey.com/elpa/"))
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

(add-to-list 'load-path "~/.emacs.d/site-lisp")
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu/mu4e/")

;;(setq exec-path (append exec-path '("/Library/TeX/texbin")))
;;(setq exec-path (append exec-path '("/opt/local/bin")))
;;(setq exec-path (append exec-path '("/usr/local/bin")))

;(setenv "DICTIONARY" "en_US")
;; (setq ispell-program-name "hunspell")
;; (setq ispell-really-hunspell t)
(setq ispell-program-name "aspell")

(require 'subr-x)
(setq eddie/xcode-developer-dir
      (string-trim (shell-command-to-string "xcode-select -p")))
(setq eddie/xcode-sdk-headers-dir
      (concat eddie/xcode-developer-dir
	      "/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/include"))
(setq eddie/xcode-sdk-frameworks-dir
      (concat eddie/xcode-developer-dir
	      "/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/System/Library/Frameworks"))
(setq eddie/xcode-toolchain-dir
      (concat eddie/xcode-developer-dir
	      "/Toolchains/XcodeDefault.xctoolchain"))
(setq eddie/xcode-toolchain-clangd
      (concat eddie/xcode-toolchain-dir
	      "/usr/bin/clangd"))

(defun eddie/find-xcode-developer-dir ()
  (interactive)
  (find-file eddie/xcode-developer-dir))

(defun eddie/find-xcode-sdk-headers-dir ()
  (interactive)
  (find-file eddie/xcode-sdk-headers-dir))

(defun eddie/find-xcode-sdk-frameworks-dir ()
  (interactive)
  (find-file eddie/xcode-sdk-frameworks-dir))

(global-set-key (kbd "C-c x d") 'eddie/find-xcode-developer-dir)
(global-set-key (kbd "C-c x i") 'eddie/find-xcode-sdk-headers-dir)
(global-set-key (kbd "C-c x f") 'eddie/find-xcode-sdk-frameworks-dir)

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

;; (use-package simple-modeline
;;   :hook (after-init . simple-modeline-mode))

(use-package mood-line
  :hook (after-init . mood-line-mode))

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
;; disable stupid outline style indentation
(setq org-adapt-indentation nil)
(setq org-src-preserve-indentation nil)
(setq org-edit-src-content-indentation 0)

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

(setq org-babel-python-command "python3")

(use-package ox-hugo
  :ensure t
  :after ox)

(require 'ox-org)

(use-package ox-json
  :ensure t
  :after ox)

(require 'ox-json)

(use-package ox-groff
  :load-path "~/elisp/org-contrib/lisp")

(require 'ox-latex)
(setq org-latex-default-packages-alist
  '(("AUTO" "inputenc"  t ("pdflatex"))
    ("T1"   "fontenc"   t ("pdflatex"))
    (""     "graphicx"  t)
    (""     "longtable" nil)
    (""     "wrapfig"   nil)
    (""     "rotating"  nil)
    ("normalem" "ulem"  t)
    (""     "amsmath"   t)
    (""     "amssymb"   t)
    (""     "capt-of"   nil)
    ("hidelinks"     "hyperref"  nil)))

(add-to-list 'org-latex-packages-alist '("" "listings"))
(add-to-list 'org-latex-packages-alist '("" "color"))
;; (setq org-latex-listings t)
(setq org-latex-listings-options
    '(("basicstyle" "\\tiny")
      ("keywordstyle" "\\color{black}\\bfseries")
      ("showstringspaces" "false")))
(add-to-list 'org-latex-listings-langs '(org "Org"))

(setq org-latex-listings 'minted)
(add-to-list 'org-latex-packages-alist '("newfloat" "minted"))
(setq org-latex-minted-options
      '(("fontsize" "\\small")
	("frame" "leftline")))

(setq org-latex-pdf-process
  (if (executable-find "latexmk")
      '("latexmk -f -pdflua -%latex=\"lualatex --shell-escape\" -interaction=nonstopmode -output-directory=%o %f")
    '("%latex -interaction nonstopmode -output-directory %o %f"
      "%latex -interaction nonstopmode -output-directory %o %f"
      "%latex -interaction nonstopmode -output-directory %o %f")))

(defun eddie/org-latex-format-headline-function
    (todo _todo-type priority text tags _info)
  "Make TODO style headlines standout, otherwise this is the same
as the default. See `org-latex-format-headline-function' for
details."
  (concat
   (and todo
	(format
	 "\\texorpdfstring{\\bfseries%s }{}{ %s }"
	 (pcase _todo-type
	   ('todo "\\BTTodoColor\\HollowBox")
	   ('done "\\BTDoneColor\\Checkedbox"))
	 (pcase _todo-type ('todo "☐") ('done "☑︎"))))
   (and priority (format "\\framebox{\\#%c} " priority))
   text
   (and tags
	(format "\\hfill{}\\textsc{%s}"
		(mapconcat #'org-latex--protect-text tags ":")))))

(setq org-latex-format-headline-function
      'eddie/org-latex-format-headline-function)


;; (setq org-latex-pdf-process
;;       '("latexmk -lualatex=\"lualatex --shell-escape\" -f -pdf -interaction=nonstopmode -output-directory=%o %f"))

;; (setq org-latex-pdf-process
;;       (lambda (x)
;; 	(if (executable-find "latexmk")
;; 	    '("latexmk -lualatex=\"lualatex --shell-escape\" -f -pdf -interaction=nonstopmode -output-directory=%o %f")
;; 	  '("lualatex"))))


(add-to-list 'org-latex-classes '("memoir"
     "\\documentclass[12pt]{memoir}"
     ("\\chapter{%s}" . "\\chapter*{%s}")
     ("\\section{%s}" . "\\section*{%s}")
     ("\\subsection{%s}" . "\\subsection*{%s}")
     ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))

(setq org-latex-remove-logfiles nil)

;; default indent style
(setq tab-width 4)
(setq-default indent-tabs-mode nil)

(use-package lorem-ipsum
  :load-path "~/elisp/emacs-lorem-ipsum"
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


(defun discombobulated-c-mode ()
  "C mode for some darwin kernel projects, system projects and
cctools, etc. that can not pick either tabs or spaces."
  (interactive)
  (c-mode)
  (c-set-style "linux")
  (setq tab-width 8)
  (setq indent-tabs-mode t)
  (setq c-basic-offset 8)
  (electric-indent-mode 0))

(setq auto-mode-alist (cons '("/cctools/.*\\.[ch]*$" . discombobulated-c-mode)
			    auto-mode-alist))


(defconst align-this-c-style
  '((c-offsets-alist . ((arglist-cont-nonempty . +)
			(arglist-close . 0))))
  "C style that performs no 'smart' alignment.")
(c-add-style "Align this." align-this-c-style)
(defun align-this-c-mode ()
  "C mode that turns off all 'smart' alignment. Say 'align this'
like Trinity says 'Dodge this.'"
  (interactive)
  (c-mode)
  (c-set-style "Align this.")
  (setq tab-width 4
	indent-tabs-mode nil
	c-basic-offset 4))


;; JS Modules
(add-to-list 'auto-mode-alist '("\\.mjs\\'" . js-mode))

;; ActionScript
(add-to-list 'auto-mode-alist '("\\.as\\'" . actionscript-mode))

;; Pascal
(add-to-list 'auto-mode-alist '("\\.pp$" . opascal-mode))

(use-package ada-mode)

;; https://www.gnu.org/software/emacs/manual/html_mono/ccmode.html#Objective_002dC-Method-Symbols

(defun eddie/c-lineup-ObjC-method-call-not-stupid (langelem)
  "Simple +1 c-basic-offset indention from the start of the
statements indention. The anti-Xcode & anti-Elisp style.

    value = [NSSomeClass someObjectWithString:@\"arg1\"
        secondString:@\"arg2\"
        thridObject:NULL
    ];
            ^
            |__________ anchor pos [12]
        ^
        |______________ target column (start of stm + c-basic-offset) [8]
    ^
    |__________________ start of statement indention [4]


    ret = desired offset realative to anchor
    anchor = anchor pos
    stm-indent = start of statement indention

    ret = stm-indent + c-basic-offset - anchor

Works with: objc-method-call-cont."
  (save-excursion
    (back-to-indentation)
    (let* ((anchor (c-langelem-pos langelem))
	   (paren-state (c-parse-state))
	   (containing-sexp (c-most-enclosing-brace paren-state))
	   (stm-indent (progn
			 (goto-char containing-sexp)
			 (setq placeholder (c-point 'boi))
			 (if (and (c-safe (backward-up-list 1) t)
				  (>= (point) placeholder))
			     (progn
			       (forward-char)
			       (skip-chars-forward " \t"))
			   (goto-char placeholder))
			 (point))))
      (- (+ stm-indent c-basic-offset) anchor))))

(defun eddie/c-lineup-ObjC-arglist (langelem)
  "Line up block arguments.")

(defconst not-stupid-objc-style
  '((c-offsets-alist . ((objc-method-call-cont
			 .
			 eddie/c-lineup-ObjC-method-call-not-stupid)
			(arglist-cont-nonempty . +)
			(arglist-close . 0)
			(objc-method-args-cont . +))))
    "Objective-C style that is not stupid!")

(c-add-style "Objective-C" not-stupid-objc-style)

(defun not-stupid-objc-mode ()
  (interactive)
  (font-lock-add-keywords
   'objc-mode
   '(("@property" . font-lock-keyword-face)
     ("@available" . font-lock-keyword-face)
     ("@autoreleasepool" . font-lock-keyword-face)
     ;; ("@optional" . font-lock-builtin-face)
     ;; ("@required" . font-lock-builtin-face)
     ("instancetype" . font-lock-type-face)
     ("__weak" . font-lock-type-face)
     ("__strong" . font-lock-type-face)))
  (objc-mode)
  (c-set-style "Objective-C")
  (c-toggle-comment-style 1)
  (setq tab-width 4
	indent-tabs-mode nil))

;; Obj-C, Objective-C, objc, Cocoa
(add-to-list 'auto-mode-alist '("\\.m$" . not-stupid-objc-mode))
(add-to-list 'auto-mode-alist '("\\.mm$" . not-stupid-objc-mode))

(defun eddie/objc-headerp ()
  (and buffer-file-name
       (string= (file-name-extension buffer-file-name) "h")
       (re-search-forward "@\\<interface\\>"
			  magic-mode-regexp-match-limit t)))

(add-to-list 'magic-mode-alist '(eddie/objc-headerp . not-stupid-objc-mode))

;; Web mode
(use-package web-mode)

;; Polymode
(use-package polymode
  :ensure t
  :pin melpa-stable)
(use-package poly-erb
  :ensure t
  :after polymode
  :pin melpa-stable)
(use-package poly-markdown
  :ensure t
  :after polymode
  :pin melpa-stable)
(use-package poly-erb
  :ensure t
  :after polymode
  :pin melpa-stable)
;; (use-package poly-org
;;   :ensure t
;;   :after polymode
;;   :pin melpa-stable)
(use-package poly-ruby
  :ensure t
  :after polymode
  :pin melpa-stable)

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

;;(add-to-list 'auto-mode-alist '("meson.build$" . python-mode))
(use-package meson-mode)

(use-package ninja-mode)

;; use makefile-mode for files named Makefile.something
(add-to-list 'auto-mode-alist '("Makefile.*$" . makefile-mode))
;(setq auto-mode-alist (cdr auto-mode-alist))

;; gitignroe mode and others
(use-package git-modes)
(use-package gitignore-templates)

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

;; SLIME
(use-package slime
  :ensure t
  :pin melpa-stable
  :config
  (setq inferior-lisp-program "~/src/ccl-dev/dx86cl64"))

;; with-editor to use the current editor as $EDITOR
(use-package with-editor
  :ensure t
  :init
  (progn
    (add-hook 'shell-mode-hook  'with-editor-export-editor)
    (add-hook 'eshell-mode-hook 'with-editor-export-editor)))

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
(add-to-list 'eshell-visual-subcommands '("git" "diff" "log"))
(add-to-list 'eshell-visual-options '("curl" "-O"))

(setq eshell-visual-subcommands nil)

;; remove first
;; (setq eshell-visual-commands
;;       (cdr eshell-visual-commands))

;;; pkgmgr selectfile-mode
(use-package selectfile-mode
     :load-path "~/pm/pkgmgr/tools/emacs")

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
  ;;(global-set-key (kbd "<tab>") 'tab-indent-or-complete)
  (global-set-key (kbd "C-/") 'company-complete)

  :bind (:map company-active-map
	      ("C-p" . company-select-previous)
	      ("C-n" . company-select-next)))

;; magit-mode-map

(add-hook 'after-init-hook 'global-company-mode)

(use-package eglot
  :config
  (add-to-list 'eglot-server-programs
	       '(c-mode .
			("/Applications/Xcode12.4.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/clangd")))
  (add-to-list 'eglot-stay-out-of "flymake")
  (add-to-list 'eglot-stay-out-of "eldoc"))

(use-package realgud)
(use-package realgud-lldb)
(use-package dap-mode)


;; don't underline non-breaking space
(set-face-attribute 'nobreak-space nil
		    :underline nil)

;; Increment number under point (cursor)
;; https://www.emacswiki.org/emacs/IncrementNumber
(defun increment-number-at-point ()
  (interactive)
  (skip-chars-backward "0-9")
  (or (looking-at "[0-9]+")
      (error "No number at point"))
  (replace-match (number-to-string
		  (1+ (string-to-number (match-string 0))))))

(global-set-key (kbd "C-c +") 'increment-number-at-point)

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

;;(global-set-key (kbd "M-#") 'define-word)

(use-package osx-dictionary
  :config
  (global-set-key (kbd "M-#") 'osx-dictionary-search-word-at-point))


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

;; Plist Mode
(use-package plist-mode
  :load-path "~/elisp/plist-mode"
  :config
  (add-to-list 'auto-mode-alist '("\\.pbfilespec$" . plist-mode))
  (add-to-list 'auto-mode-alist '("\\.pbcompspec$" . plist-mode))
  (add-to-list 'auto-mode-alist '("\\.xcspec$" . plist-mode)))

;; XML mode
(add-hook 'nxml-mode-hook
	  (lambda ()
	    (setq nxml-child-indent 4
		  nxml-attribute-indent 6)))

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

(add-to-list 'auto-mode-alist '("\\.mbox$" . vm-mode))

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
