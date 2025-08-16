(defun e/objc-headerp ()
  (and buffer-file-name
       (string= (file-name-extension buffer-file-name) "h")
       (re-search-forward "@\\<interface\\>"
			  magic-mode-regexp-match-limit t)))

;; (add-to-list 'magic-mode-alist
;;              '(e/objc-headerp . not-stupid-objc-mode))
;; https://www.gnu.org/software/emacs/manual/html_mono/ccmode.html#Objective_002dC-Method-Symbols

(defun e/c-lineup-ObjC-method-call-not-stupid (langelem)
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

(defun e/c-lineup-ObjC-arglist (langelem)
  "Line up block arguments.")

(defconst not-stupid-objc-style
  '((c-offsets-alist . ((objc-method-call-cont
			 .
			 e/c-lineup-ObjC-method-call-not-stupid)
			(arglist-cont-nonempty . +)
			(arglist-close . 0)
			(objc-method-args-cont . +))))
    "Objective-C style that is not stupid!")

(c-add-style "!Stupid Objective-C" not-stupid-objc-style)

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
  (c-set-style "!Stupid Objective-C")
  (c-toggle-comment-style 1)
  (setq tab-width 4
	indent-tabs-mode nil))

;; Obj-C, Objective-C, objc, Cocoa
(add-to-list 'auto-mode-alist '("\\.m$" . not-stupid-objc-mode))

(eval-and-compile
  (unless (or (stringp c-default-style)
              (assoc 'objc-mode c-default-style))
    (setq c-default-style
          (cons '(objc-mode . "!Stupid Objective-C") c-default-style))))


(defun e/objc-headerp ()
  (and buffer-file-name
       (string= (file-name-extension buffer-file-name) "h")
       (re-search-forward "@\\<interface\\>"
			  magic-mode-regexp-match-limit t)))

(add-to-list 'magic-mode-alist '(e/objc-headerp . not-stupid-objc-mode))
