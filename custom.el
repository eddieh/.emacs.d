(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("985e4428a49e4f9ec5c02f981ca47d9825168ca4ed06aa0bf58e9e500ea3168b"
     default))
 '(elfeed-feeds
   '("https://developer.apple.com/design/downloads/sketch-macOS.rss"))
 '(ignored-local-variable-values
   '((cmake-tab-width . 2)
     (eval unless (featurep 'llbuild-project-settings)
	   (message "loading 'llbuild-project-settings")
	   (add-to-list 'load-path
			(concat
			 (let
			     ((dlff
			       (dir-locals-find-file default-directory)))
			   (if (listp dlff) (car dlff)
			     (file-name-directory dlff)))
			 "utils/emacs")
			:append)
	   (require 'llbuild-project-settings))))
 '(package-selected-packages
   '(applescript-mode auctex cape cmake-mode corfu dart-mode devdocs
		      docstr editorconfig elfeed elpher emr git-modes
		      gitignore-templates groovy-mode helpful
		      kind-icon languagetool lua-mode magit
		      markdown-mode mise mood-line multiple-cursors
		      ob-powershell orderless osx-clipboard
		      osx-dictionary plist-mode powershell
		      quelpa-use-package rainbow-mode swift-mode
		      typescript-mode web-mode yaml-mode
		      yasnippet-snippets zenburn-theme))
 '(safe-local-variable-values
   '((eval if (assoc "llbuild" c-style-alist) (c-set-style "llbuild"))
     (web-mode-indent-style . 2) (web-mode-block-padding . 4)
     (web-mode-script-padding . 4) (web-mode-style-padding . 4)
     (eval c-add-style "fontforge"
	   '("stroustrup" (indent-tabs-mode . t) (tab-width . 8)
	     (c-offsets-alist (case-label . *)))))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
