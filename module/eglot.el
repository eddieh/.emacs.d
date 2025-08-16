;; (use-package eglot
;;   :config
;;   (add-to-list 'eglot-server-programs
;; 	       `((c-mode c-ts-mode objc-mode) . ,(e/xcode-clangd-path)))
;;   (add-to-list 'eglot-stay-out-of "flymake")
;;   (add-to-list 'eglot-stay-out-of "eldoc")
;;   (add-to-list 'eglot-ignored-server-capabilities :inlayHintProvider))


(use-package eglot
  :ensure nil
  :config
  (add-to-list 'eglot-server-programs
	       '((objc++-mode) . ("clangd" "-log=verbose")))
  (add-to-list 'eglot-server-programs
	       '((web-mode) . ("typescript-language-server" "--stdio")))
  (add-hook 'c-mode-hook 'eglot-ensure)
  (add-hook 'c++-mode-hook 'eglot-ensure)
  (add-hook 'objc-mode-hook 'eglot-ensure)
  (add-hook 'objc++-mode-hook 'eglot-ensure)
  (add-hook 'web-mode-hook 'eglot-ensure)
  (add-hook 'js-mode-hook 'eglot-ensure)
  (add-hook 'typescript-mode-hook 'eglot-ensure)
  ;; (add-to-list 'eglot-stay-out-of "flymake")
  ;; (add-to-list 'eglot-stay-out-of "eldoc")
  (add-to-list 'eglot-ignored-server-capabilities :inlayHintProvider)
  (add-to-list 'eglot-ignored-server-capabilities :documentFormatting)
  (add-to-list 'eglot-ignored-server-capabilities :documentRangeFormatting)
  (add-to-list 'eglot-ignored-server-capabilities :documentOnTypeFormattingProvider))


(with-eval-after-load 'eglot
  (setq completion-category-defaults nil))

;; Corfu
(use-package corfu
  :ensure t

  ;; :straight (corfu :files (:defaults "extensions/*")
  ;;                  :includes corfu-popupinfo)

  :custom
  (corfu-auto t)
  (corfu-auto-prefix 1)
  (corfu-auto-delay 1)
  (corfu-quit-no-match 'separator)

  :config
  (setq completion-cycle-threshold 3)
  (setq tab-always-indent 'complete)
  (global-corfu-mode)
  (corfu-popupinfo-mode))

;; Cape
(use-package cape
  ;; :straight t
  :ensure t
  :config
  (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster))

(use-package kind-icon
  ;; :straight t
  :ensure t
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

;; orderless
(use-package orderless
  :ensure t
  ;; :straight t
  :init
  ;; Tune the global completion style settings to your liking!
  ;; This affects the minibuffer and non-lsp completion at point.
  (setq completion-styles '(orderless partial-completion basic)
        completion-category-defaults nil
        completion-category-overrides nil))
