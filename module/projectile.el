;; Projectile
(use-package projectile
  :ensure t
  :init
  (setq projectile-require-project-root nil)
  :config
  (projectile-mode +1)
  ;;(add-to-list 'projoectile-globally-ignored-directories "^\\.build$")
  (add-to-list 'grep-find-ignored-directories ".build")
  ;;(add-to-list 'projectile-other-file-alist '("cpp" "iig"))
  (add-to-list 'projectile-other-file-alist '("cpp" "h" "hpp" "ipp" "igg"))
  (add-to-list 'projectile-other-file-alist '("iig" "cpp"))
  (define-key projectile-mode-map (kbd "C-c C-p") 'projectile-command-map))
