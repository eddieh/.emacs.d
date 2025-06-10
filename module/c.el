;; (setq c-basic-offset 4
;;       tab-width 4
;;       indent-tabs-mode nil)

;; (add-hook 'c-mode-common-hook
;; 	  (lambda ()
;; 	    (setq c-basic-offset 4
;; 		  tab-width 4
;; 		  indent-tabs-mode nil)))

(defconst align-this-c-style
  '((indent-tabs-mode . nil)
    (c-basic-offset  . 4)
    (c-offsets-alist . ((arglist-cont-nonempty . +)
			(arglist-close . 0)
			(substatement-open . 0)
			(label . 0))))
  "C style that performs no 'smart' alignment.")

(c-add-style "Align this." align-this-c-style)
(c-add-style "Align this [t4]." '("Align this."
				  (indent-tabs-mode . t)
				  (c-basic-offset . 4)))
(c-add-style "Align this [t8]." '("Align this."
				  (indent-tabs-mode . t)
				  (c-basic-offset . 8)))

(add-to-list 'c-default-style
	     '(c-mode . "Align this."))

(add-hook 'c-mode-hook
	  (lambda ()
	    (let ((path (buffer-file-name)))
	      (cond
	       ((string-match "/gxemul.*/.*\\.[ch|cpp]*$" path)
		(netbsd-knf-c-mode-hook))
	       (t
		(c-set-style "Align this."))))))

(add-hook 'c-mode-common-hook
	  (lambda ()
	    (let ((path (buffer-file-name)))
	      (cond
	       ((string-match ".*VirtualController.*/.*\\.[c|h|cpp|hpp|iig]*$" path)
		(default-c++-style-mode))
	       ((string-match ".*irrlicht.*/.*\\.[c|h|cpp]*$" path)
		(default-c++-style-mode))))))
