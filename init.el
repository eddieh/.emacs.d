;; Eddie's Initialization File

(message "Loading Eddie's initialization…")

(setq eddie/dir
      (file-name-directory (or load-file-name (buffer-file-name))))

(defun eddie/load-file (file-name)
  "Load a .org file."
  (org-babel-load-file (expand-file-name (concat file-name ".org") eddie/dir)))

(eddie/load-file "README")

