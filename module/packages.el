;; load packages that don't have significant configurations or
;; modifications

(use-package git-modes)
(use-package gitignore-templates)

(use-package yaml-mode)

(use-package editorconfig
  :ensure nil
  :config
  (editorconfig-mode 1)
  (add-to-list 'editorconfig-indentation-alist '(js-json-mode js-indent-level)))
;;  (add-to-list 'editorconfig-indentation-alist '(conf-toml-mode  js-indent-level)))

;; (use-package applescript-mode)

(use-package cmake-mode)
(use-package lua-mode
  :config
  (setq lua-indent-nested-block-content-align nil))

(use-package swift-mode
  :after (eglot)
  :config
  (setq swift-mode:parenthesized-expression-offset 4
	swift-mode:multiline-statement-offset 4)
  (add-to-list 'eglot-server-programs
	       '(swift-mode . ("xcrun" "sourcekit-lsp"))))

;; (use-package company
;;   :config
;;   (add-hook 'after-init-hook 'global-company-mode))

(use-package multiple-cursors
  :config
  (global-set-key (kbd "C-S-c .") 'mc/edit-lines)
  (global-set-key (kbd "C-S-c >") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-S-c <") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-S-c C-.") 'mc/mark-all-like-this))

(use-package elpher)

(use-package markdown-mode)

(use-package elfeed)

(use-package typescript-mode)

(use-package groovy-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.gradle\\'" . groovy-mode)))

(use-package docstr
  :config
  (add-hook 'objc-mode-hook (lambda () (docstr-mode 1)))
  ;; (defun e/docstr-trigger-objc (&rest _)
  ;;   "Trigger document string in ObjC mode."
  ;;   (message "docstr ObjC")
  ;;   (when (and (memq major-mode '(objc-mode))
  ;;            (docstr--doc-valid-p)
  ;;            (docstr-util-looking-back "/**" 3))
  ;;   (save-excursion
  ;;     ;; (insert " <summary>\n")
  ;;     ;; (insert "/// \n")
  ;;     ;; (insert "/// </summary>"))
  ;;     (insert "\n")
  ;;     (insert "* @abstract\n")
  ;;     (insert "* @discussion\n"))
  ;;   (forward-line 1)
  ;;   (end-of-line)
  ;;   (docstr--insert-doc-string (docstr--c-style-search-string 2))))
  ;; (add-to-list 'docstr-trigger-alist '("*" . e/docstr-trigger-objc)))
  )

(use-package yasnippet)

(use-package yasnippet-snippets
  :after (yasnippet))

(use-package languagetool
  :ensure t
  :defer t
  :commands (languagetool-check
             languagetool-clear-suggestions
             languagetool-correct-at-point
             languagetool-correct-buffer
             languagetool-set-language
             languagetool-server-mode
             languagetool-server-start
             languagetool-server-stop)
  :config
  ;; languagetool assumes these are jar files and not wrappers or
  ;; executables

  ;(setq languagetool-console-command "/opt/homebrew/opt/languagetool/bin/languagetool"
  ;      languagetool-server-command "/opt/homebrew/opt/languagetool/bin/languagetool-server"))

  (setq languagetool-console-command
	"/opt/homebrew/Cellar/languagetool/6.5/libexec/languagetool-commandline.jar"
	languagetool-server-command
	"/opt/homebrew/Cellar/languagetool/6.5/libexec/languagetool-server.jar"))

;; This isn't toml-mode ><
;; (use-package toml
;;   :config
;;   (add-to-list 'auto-mode-alist '("\\.wara\\'" . toml)))

(add-to-list 'auto-mode-alist '("\\.wara\\'" . conf-toml-mode))


(use-package auctex)

;; load packages that don't have significant configurations or
;; modifications

(use-package git-modes)
(use-package gitignore-templates)

(use-package yaml-mode)

;; (use-package applescript-mode)

(use-package cmake-mode)
(use-package lua-mode
  :config
  (setq lua-indent-nested-block-content-align nil))

(use-package swift-mode
  :after (eglot)
  :config
  (setq swift-mode:parenthesized-expression-offset 4
	swift-mode:multiline-statement-offset 4)
  (add-to-list 'eglot-server-programs
	       '(swift-mode . ("xcrun" "sourcekit-lsp"))))

;; (use-package company
;;   :config
;;   (add-hook 'after-init-hook 'global-company-mode))

(use-package multiple-cursors
  :config
  (global-set-key (kbd "C-S-c .") 'mc/edit-lines)
  (global-set-key (kbd "C-S-c >") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-S-c <") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-S-c C-.") 'mc/mark-all-like-this))

(use-package elpher)

(use-package markdown-mode)

(use-package elfeed)

(use-package typescript-mode)

(use-package groovy-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.gradle\\'" . groovy-mode)))

(use-package docstr
  :config
  (add-hook 'objc-mode-hook (lambda () (docstr-mode 1)))
  ;; (defun e/docstr-trigger-objc (&rest _)
  ;;   "Trigger document string in ObjC mode."
  ;;   (message "docstr ObjC")
  ;;   (when (and (memq major-mode '(objc-mode))
  ;;            (docstr--doc-valid-p)
  ;;            (docstr-util-looking-back "/**" 3))
  ;;   (save-excursion
  ;;     ;; (insert " <summary>\n")
  ;;     ;; (insert "/// \n")
  ;;     ;; (insert "/// </summary>"))
  ;;     (insert "\n")
  ;;     (insert "* @abstract\n")
  ;;     (insert "* @discussion\n"))
  ;;   (forward-line 1)
  ;;   (end-of-line)
  ;;   (docstr--insert-doc-string (docstr--c-style-search-string 2))))
  ;; (add-to-list 'docstr-trigger-alist '("*" . e/docstr-trigger-objc)))
  )

(use-package yasnippet
  :config
  (yas-global-mode 1))

(use-package yasnippet-snippets
  :after (yasnippet))

(use-package languagetool
  :ensure t
  :defer t
  :commands (languagetool-check
             languagetool-clear-suggestions
             languagetool-correct-at-point
             languagetool-correct-buffer
             languagetool-set-language
             languagetool-server-mode
             languagetool-server-start
             languagetool-server-stop)
  :config
  ;; languagetool assumes these are jar files and not wrappers or
  ;; executables

  ;(setq languagetool-console-command "/opt/homebrew/opt/languagetool/bin/languagetool"
  ;      languagetool-server-command "/opt/homebrew/opt/languagetool/bin/languagetool-server"))

  (setq languagetool-console-command
	"/opt/homebrew/Cellar/languagetool/6.5/libexec/languagetool-commandline.jar"
	languagetool-server-command
	"/opt/homebrew/Cellar/languagetool/6.5/libexec/languagetool-server.jar"))

;; This isn't toml-mode ><
;; (use-package toml
;;   :config
;;   (add-to-list 'auto-mode-alist '("\\.wara\\'" . toml)))

(add-to-list 'auto-mode-alist '("\\.wara\\'" . conf-toml-mode))
(add-to-list 'auto-mode-alist '("\\.inf\\'" . conf-windows-mode))
(add-to-list 'auto-mode-alist '("\\.inx\\'" . conf-windows-mode))

(use-package auctex)

(use-package powershell)
(use-package ob-powershell)

(use-package clang-format
  :config
  (global-set-key (kbd "C-c TAB") 'clang-format-region))

;; https://github.com/eddieh/plist-mode
(use-package plist-mode
  ;;:load-path "~/elisp/plist-modeq"
  :quelpa (plist-mode :fetcher github :repo "eddieh/plist-mode")
  :config
  (add-to-list 'auto-mode-alist '("\\.pbxproj\\'" . plist-mode)))

(when nil
;; https://github.com/sensorflo/font-lock-ext
(use-package font-lock-ext
  ;;:load-path "~/elisp/font-lock-ext"
  :quelpa (font-lock-ext :fetcher github :repo "sensorflo/font-lock-ext"))

;; https://github.com/sensorflo/sln-mode
(use-package sln-mode
  ;;:load-path "~/elisp/sln-mode"
  :quelpa (sln-mode :fetcher github :repo "sensorflo/sln-mode")
  :after (font-lock-ext)
  :config
  (add-to-list 'auto-mode-alist '("\\.sln\\'" . sln-mode)))
)

(use-package web-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.jsx?\\'" . web-mode))
  (setq web-mode-comment-formats
	'(("java"       . "/*")
	  ("javascript" . "//")
	  ("typescript" . "//")
	  ("php"        . "/*")
	  ("css"        . "/*")))
  (setq web-mode-indentation-params
	'(("lineup-args"       . nil)
	  ("lineup-calls"      . t)
	  ("lineup-concats"    . t)
	  ("lineup-quotes"     . t)
	  ("lineup-ternary"    . nil)
	  ("case-extra-offset" . nil))))

(defun e/modify-gyp-syntax ()
  "Generate Your Projects (GYP) files .gyp & .gypi files look like JSON,
but can have # comments and single quotes are permitted as string
delimiters. It also permits trailing commas at the end of list and
dictionary content."
  (if (and (stringp buffer-file-name)
	   (string-match "\\.gypi?\\'" buffer-file-name))
      (let ((gyp-syntax-table (copy-syntax-table js-mode-syntax-table)))
	(modify-syntax-entry ?# "<" gyp-syntax-table)
	(modify-syntax-entry ?\n ">" gyp-syntax-table)
	(modify-syntax-entry ?\' "\"" gyp-syntax-table)
	(set-syntax-table gyp-syntax-table))))

(add-to-list 'auto-mode-alist '("\\.gypi?\\'" . js-json-mode))

(add-hook 'js-json-mode-hook (lambda ()
			       (e/modify-gyp-syntax)
			       (editorconfig-apply)))

(use-package objc++-mode
  :load-path "~/elisp/objc++-mode"
  :config
  (add-hook 'c-mode-common-hook (lambda () (message "hello from c-mode-common-hook")))
  (add-hook 'objc++-mode-hook (lambda () (message "hello from objc++-mode-hook"))))

(use-package dart-mode)

;;; TOO annoying!
;; (use-package paredit
;;   :config
;;   (add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode))

(use-package helpful)
(use-package emr
  :config
  (define-key prog-mode-map (kbd "M-RET") 'emr-show-refactor-menu))

;; this might be causing some issues
(use-package osx-clipboard)

(use-package osx-dictionary
  :bind (("M-#" . osx-dictionary-search-word-at-point)))

;;; THIS BREAKS EVERYTHING!!!
;; (use-package mise
;;   :config
;;   (add-hook 'after-init-hook #'global-mise-mode))

(use-package devdocs)
