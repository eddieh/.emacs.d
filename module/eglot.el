;; (use-package eglot
;;   :config
;;   (add-to-list 'eglot-server-programs
;; 	       `((c-mode c-ts-mode objc-mode) . ,(e/xcode-clangd-path)))
;;   (add-to-list 'eglot-stay-out-of "flymake")
;;   (add-to-list 'eglot-stay-out-of "eldoc")
;;   (add-to-list 'eglot-ignored-server-capabilities :inlayHintProvider))
