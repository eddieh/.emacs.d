;; init.el

(defconst e/config-dir
  (cond ((boundp 'user-emacs-directory)
         user-emacs-directory)
        ((boundp 'user-init-directory)
         user-init-directory)
        (t "~/.emacs.d/"))
  "The user's emacs directory.")

(defun e/config-path (subpath)
  "The path of some file or directory in user's emacs directory."
  (interactive "f")
  (concat e/config-dir subpath))

(defconst e/custom-file (e/config-path "custom.el"))
(defconst e/themes-dir (e/config-path "themes"))

(defun e/load-init-module (file)
  "Load an init module from the user's emacs directory."
  (load-file (expand-file-name file e/config-dir)))

                               ;; look for 'file' in dir:
                               ;; (e/config-path "module"))))

(setq byte-compile-warnings '(cl-functions))
(setq inhibit-splash-screen t)

(unless window-system
  (menu-bar-mode -1))
(if window-system
    (progn
      (fringe-mode 0)
      (scroll-bar-mode -1)
      (tool-bar-mode -1)))

;; ;; eval in *scratch* to get a list of available fonts
;; (dolist (font (x-list-fonts "*"))
;;   (insert (format "%s\n" font)))
;; (defun e/font ()
;;   "Logic for selecting font and size."
;;   (if (memq system-type '(darwin))
;;       "DejaVuSansM Nerd Font Mono-14"
;;     "DejaVuSansM Nerd Font Mono-10"))
(defun e/font ()
  "Logic for selecting font and size."
  (cond ((memq system-type '(darwin))
	 "DejaVuSansM Nerd Font Mono-14")
	((memq system-type '(windows-nt))
	 "DejaVuSansM Nerd Font Mono-13")
	(t "DejaVuSansM Nerd Font Mono-10")))

(defun e/frame-height ()
  "Logic for setting the frame height."
  (if (memq system-type '(darwin))
      70 ;87
    44))

;; (display-monitor-attributes-list)
;; (display-pixel-width)
;; (frame-pixel-width)
(defun e/frame-left ()
  "Logic for setting the frame height."
  (if (memq system-type '(darwin))
      1916
    634))

(setq e/frame-style
      `((vertical-scroll-bars)
        (right-fringe . 0)
        (left-fringe . 0)
        (tool-bar-lines . 0)
        ;; (top . 0)
        ;; (left . ,(e/frame-left))
        (height . ,(e/frame-height))
        (width . 80)
        (font . ,(e/font))))

(setq initial-frame-alist e/frame-style)
(setq default-frame-alist e/frame-style)

(setq custom-file e/custom-file)
(load custom-file 'noerror)

(require 'package)
(add-to-list
 'package-archives
 '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list
 'package-archives
 '("melpa" . "https://melpa.org/packages/") t)
(add-to-list
 'package-archives
 '("elpa" . "https://tromey.com/elpa/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-verbose t
      use-package-always-ensure t)

(setq zenburn-height-minus-1 1.0
      zenburn-height-plus-1 1.0
      zenburn-height-plus-2 1.0
      zenburn-height-plus-3 1.0
      zenburn-height-plus-4 1.0)
(use-package zenburn-theme
  :init
  (add-to-list 'custom-theme-load-path e/themes-dir)
  :config
  (load-theme 'zenburn t))

(use-package mood-line
  :hook (after-init . mood-line-mode))

(use-package rainbow-mode)

;; Because Homebrew doesn't package dictionaries for Hunspell.
;;
;; Homebrew: hunspell caveats
;;
;;   Dictionary files (*.aff and *.dic) should be placed in
;;   ~/Library/Spelling/ or /Library/Spelling/.  Homebrew itself
;;   provides no dictionaries for Hunspell, but you can download
;;   compatible dictionaries from other sources, such as
;;   https://cgit.freedesktop.org/libreoffice/dictionaries/tree/ .

;; Aspell Personal and Replacement Dictionaries locations:
;;
;;   ~/.aspell.en.pws
;;   ~/.aspell.en.prepl

(if (memq system-type '(darwin))
    (setq ispell-program-name "aspell")
  (setq ispell-program-name "hunspell"
        ispell-really-hunspell t))

;; Hunspell installed on Windows with
;;
;;   winget install --id=FSFhu.Hunspell  -e
;;

(if (memq system-type '(windows-nt))
    (progn
      (setq ispell-hunspell-dict-paths-alist
	    `(("en_US" ,(expand-file-name "~/Dictionary/en_US.aff")))) ; "C:/path/to/en_US.aff"
      (setq ispell-local-dictionary "en_US")
      (setq ispell-local-dictionary-alist
	    '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_US") nil utf-8)))
      (setenv "LANG" "en_US.UTF-8")
      (setenv "DICPATH" (expand-file-name "~/Dictionary"))))

(setq show-paren-style 'parenthesis)
(show-paren-mode t)

(require 'uniquify nil 'noerror)
(setq uniquify-buffer-name-style 'forward)

(fido-mode)
(fido-vertical-mode)
(setq icomplete-tidy-shadowed-file-names t)

;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Face-Attributes.html
(set-face-attribute 'icomplete-selected-match nil :weight 'bold :underline t)

(add-hook 'before-save-hook (lambda () (delete-trailing-whitespace)))
(setq make-backup-files nil
      require-final-newline t)

(setq tab-width 4)
(setq indent-tabs-mode nil)

(setq x-stretch-cursor t)

;; load server specific init files
(if (string= (daemonp) "browser")
    (load (expand-file-name
           (concat e/config-dir "init-browser"))))

(setq ring-bell-function (lambda ()))

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
		  (find-file "~/.zshrc")))

;; No need for copy & paste
(global-set-key (kbd "C-c f") 'find-file-at-point)
(global-set-key (kbd "C-c w") 'browse-url-at-point)

(e/load-init-module "module/symbol-movement.el")
(e/load-init-module "module/asm.el")
(e/load-init-module "module/eshell.el")
(e/load-init-module "module/elisp.el")
(e/load-init-module "module/xcode.el")
(e/load-init-module "module/make.el")
(e/load-init-module "module/netbsd-knf-style.el")
(e/load-init-module "module/c.el")
(e/load-init-module "module/cpp.el")
(e/load-init-module "module/objc.el")
(e/load-init-module "module/objc++-mode.el")
(e/load-init-module "module/packages.el")
(e/load-init-module "module/projectile.el")
(e/load-init-module "module/magit.el")
(e/load-init-module "module/eglot.el")
(e/load-init-module "module/speak.el")
(e/load-init-module "module/kbd-macros.el")

;; (eval-after-load 'egot
;;   (progn
;;     (add-to-list 'eglot-server-programs
;; 		 '(objc++-mode . ("xcrun" "sourcekit-lsp")))
;;     (add-to-list 'eglot-server-programs
;; 		 '(objc-mode . ("xcrun" "sourcekit-lsp")))))

;; (with-eval-after-load 'speedbar
;;   (speedbar-add-supported-extension ".lua"))

(put 'upcase-region 'disabled nil)

(setq ls-lisp-use-insert-directory-program nil)
(require 'ls-lisp)

(global-unset-key (kbd "C-<wheel-up>"))
(global-unset-key (kbd "C-<wheel-down>"))
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))

(delete-selection-mode 1)

(defun unfill-paragraph ()
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))

(defun unfill-region ()
  (interactive)
  (let ((fill-column (point-max)))
    (fill-region (region-beginning) (region-end) nil)))
