;; load packages that don't have significant configurations or
;; modifications

(use-package git-modes)
(use-package gitignore-templates)

(use-package yaml-mode)

(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

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
