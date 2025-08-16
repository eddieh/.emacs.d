(require 'cc-mode)

(eval-when-compile
  (require 'cl)
  (require 'cc-langs)
  (require 'cc-fonts)
  (require 'cc-menus))

(eval-and-compile
  ;; fall back on c-mode
  (c-add-language 'objc++-mode 'c++-mode))

(c-lang-defconst c-identifier-syntax-modifications
  objc++ (append
	  '((?@ . "w"))
	  (c-lang-const c-identifier-syntax-modifications)))

(c-lang-defconst c-symbol-start
  objc++ (concat "[" c-alpha "_@]"))

(c-lang-defconst c-symbol-chars
  objc++ (concat c-alnum "_$@"))

(c-lang-defconst c-cpp-include-directives
  objc++ (append
	  '("import")
	  (c-lang-const c-cpp-include-directives)))

;; Not sure show to properly modify this one:
;; (c-lang-defconst c-operators
;; (prefix "++" "--" "+" "-" "!" "~"
;; 	      ,@(when (c-major-mode-is 'objc++-mode)
;; 		  '("@selector" "@protocol" "@encode"))))

(c-lang-defconst c-other-op-syntax-tokens
  objc++ (append '("+" "-")
               (c-lang-const c-other-op-syntax-tokens)))

(c-lang-defconst c-primitive-type-kwds
  objc++ (append
	  '("id" "Class" "SEL" "IMP" "BOOL")
	  (c-lang-const c-primitive-type-kwds)))

(c-lang-defconst c-class-decl-kwds
  objc++ (append
	  '("@interface" "@implementation" "@protocol")
	  (c-lang-const c-class-decl-kwds)))

(c-lang-defconst c-modifier-kwds
  objc++ (append
	  '("auto" "bycopy" "byref" "extern" "in"
	    "inout" "oneway" "out" "static")
	  (c-lang-const c-modifier-kwds)))

(c-lang-defconst c-other-decl-kwds
  objc++ (append
	  '("@import @class" "@defs" "@end" "@property" "@dynamic"
	    "@synthesize" "@compatibility_alias")
	  (c-lang-const c-other-decl-kwds)))

(c-lang-defconst c-protection-kwds
  objc++ (append
	  '("@private" "@protected" "@package" "@public"
	    "@required" "@optional")
	  (c-lang-const c-protection-kwds)))

(c-lang-defconst c-type-list-kwds
  objc++ (append
	  '("@class")
	  (c-lang-const c-type-list-kwds)))

(c-lang-defconst c-paren-type-kwds
  objc++ (append
	  '("@defs")
	  (c-lang-const c-paren-type-kwds)))

(c-lang-defconst c-<>-type-kwds
  objc++ (append
	  '("id")
	  (c-lang-const c-<>-type-kwds)))

(c-lang-defconst c-block-stmt-1-kwds
  objc++ (append
	  '("@finally" "@try" "@autoreleasepool")
	  (c-lang-const c-block-stmt-1-kwds)))

(c-lang-defconst c-block-stmt-2-kwds
  objc++ (append
	  '("@catch" "@synchronized")
	  (c-lang-const c-block-stmt-2-kwds)))

(c-lang-defconst c-simple-stmt-kwds
  objc++ (append
	  '("@throw")
	  (c-lang-const c-simple-stmt-kwds)))

(c-lang-defconst c-constant-kwds
  objc++ (append
	  '("nil" "Nil" "YES" "NO" "IBAction" "IBOutlet"
	    "NS_DURING" "NS_HANDLER" "NS_ENDHANDLER")
	  (c-lang-const c-constant-kwds c)))

(c-lang-defconst c-primary-expr-kwds
  objc++ (append
	  '("super" "self")
	  (c-lang-const c-primary-expr-kwds)))

(c-lang-defconst c-opt-method-key
  ;; Special regexp to match the start of Objective-C methods.  The
  ;; first submatch is assumed to end after the + or - key.
  t    nil
  objc++ (concat
	;; TODO: Ought to use a better method than anchoring on bol.
	"^\\s *"
	"\\([+-]\\)"
	(c-lang-const c-simple-ws) "*"
	(concat "\\("			; Return type.
		"([^)]*)"
		(c-lang-const c-simple-ws) "*"
		"\\)?")
	"\\(" (c-lang-const c-symbol-key) "\\)"))
(c-lang-defvar c-opt-method-key (c-lang-const c-opt-method-key))

(c-lang-defconst c-type-decl-end-used
  ;; Must be set in buffers where the `c-type' text property might be
  ;; used with the value `c-decl-end'.
  ;;
  ;; `c-decl-end' is used to mark the ends of labels and access keys
  ;; to make interactive refontification work better.
  t (or (c-lang-const c-recognize-colon-labels)
	(and (c-lang-const c-label-kwds) t))
  ;; `c-decl-end' is used to mark the end of the @-style directives in
  ;; Objective-C.
  objc++ t)
(c-lang-defvar c-type-decl-end-used (c-lang-const c-type-decl-end-used))

;; (defcustom objc++-font-lock-extra-types
;;   (list objc++--regex-type-name)
;;   (c-make-font-lock-extra-types-blurb "ObjC++" "objc++-mode" (concat))
;;   :type 'c-extra-types-widget
;;   :group 'c)

(defconst objc++-font-lock-keywords-1 (c-lang-const c-matchers-1 objc++)
  "Minimal font locking for ObjC++ mode.")

(defconst objc++-font-lock-keywords-2 (c-lang-const c-matchers-2 objc++)
  "Fast normal font locking for ObjC++ mode.")

(defconst objc++-font-lock-keywords-3 (c-lang-const c-matchers-3 objc++)
  "Accurate normal font locking for ObjC++ mode.")

(defvar objc++-font-lock-keywords objc++-font-lock-keywords-3
  "Default expressions to highlight in ObjC++ mode.")

(defun objc++-font-lock-keywords-2 ()
  (c-compose-keywords-list objc++-font-lock-keywords-2))
(defun objc++-font-lock-keywords-3 ()
  (c-compose-keywords-list objc++-font-lock-keywords-3))
(defun objc++-font-lock-keywords ()
  (c-compose-keywords-list objc++-font-lock-keywords))


(defconst objc++-syle
  '((c-basic-offset . 4)
    (c-backslash-column . 78)
    (c-backslash-max-column . 78)
    (c-cleanup-list . (brace-else-brace
		       brace-elseif-brace
		       brace-catch-brace
                       empty-defun-braces
		       defun-close-semi
		       list-close-comma
		       scope-operator))
    ;; do not indent lines containing only start-of-comment more than default
    (c-comment-only-line-offset . 0)
    ;; start new lines after braces
    ;; default is: before and after (for all other cases)
    (c-hanging-braces-alist . ((defun-open . (before after))
			       (defun-close . (before after))
			       (block-open . (after))
			       (block-close . c-snug-do-while)
			       (substatement-open . after)
			       (statement-case-open . nil)
			       (brace-list-open . after)
			       (brace-list-close . nil)
			       (class-open after)
                               (class-close before after)
                               (inexpr-class-open after)
                               (inexpr-class-close before)
                               (namespace-open after)
                               (inline-open after)
                               (inline-close before after)
			       (extern-lang-open after)
                               (extern-lang-close after)))
    ;; where to put newlines around colons
    (c-hanging-colons-alist . ((case-label)
                               (label after)
                               (access-label after)
                               (member-init-intro before)
                               (inher-intro)))
    (c-indent-comments-syntactically-p . t)
    ;; no spaces needed before a label
    ;; (c-label-minimum-indentation . 0)
    ;; define offsets for some code parts
    (c-offsets-alist . ((innamespace           . 0)
			(inextern-lang         . 0)
			(access-label          . -)
			(case-label            . 0)
			(member-init-intro     . +)
			(topmost-intro         . 0)
			(arglist-cont-nonempty . +)
			(block-open            . 0)
			(brace-list-open       . +)
			(brace-list-intro      . +)
			(brace-list-entry      . 0)
			(brace-list-close      . 0)
			(label                 . -)
			(statement-cont        . 4)
			(substatement-open     . 0)
			(case-label            . 0)
			(inher-intro           . ++)
			(inline-open           . 0)))
    ;; indent line when pressing tab, instead of a plain tab character
    (c-tab-always-indent . t)
    (indent-tabs-mode . nil)
    (tab-width . 4))
  "ObjC++ Style")

(c-add-style "objc++" objc++-syle)

;; (c-add-style "special-objc++"
;;              '("objc++"
;;                (c-basic-offset . 4)
;;                (c-comment-only-line-offset . (0 . 0))
;;                (c-offsets-alist . ((inline-open           . 0)
;;                                    (arglist-intro         . +)
;;                                    (arglist-close         . 0)
;;                                    (inexpr-class          . 0)
;;                                    (case-label            . +)
;;                                    (cpp-macro             . c-lineup-dont-change)
;;                                    (substatement-open     . 0)))))

(eval-and-compile
  (unless (or (stringp c-default-style)
              (assoc 'objc++-mode c-default-style))
    (setq c-default-style
          (cons '(objc++-mode . "objc++")
                c-default-style))))

(defvar objc++-mode-syntax-table
  (funcall (c-lang-const c-make-mode-syntax-table objc++))
  "Syntax table used in `objc++-mode' buffers.")

(defvar objc++-mode-map
  (let ((map (c-make-inherited-keymap)))
    map)
  "Keymap used in `objc++-mode' buffers.")

(easy-menu-define objc++-mode-menu objc++-mode-map "ObjC++ Mode Commands."
  (cons "ObjC++" (c-lang-const c-mode-menu objc++)))

;;;###autoload
(define-derived-mode objc++-mode prog-mode "ObjC++"
  "Major mode for editing ObjC++ code.

Key bindings:
\\{objc++-mode-map}"
  :after-hook (c-update-modeline)
  (c-initialize-cc-mode t)
  (c-init-language-vars objc++-mode)
  (c-common-init 'objc++-mode)
  ;;(setq-local c-doc-comment-style '((objc++-mode . ???)))
  (run-mode-hooks 'c-mode-common-hook))


;; (defun objc++-headerp ()
;;   (and buffer-file-name
;;        (string= (file-name-extension buffer-file-name) "h")
;;        (re-search-forward "@\\<interface\\>"
;; 			  magic-mode-regexp-match-limit t)))
;; (add-to-list 'magic-mode-alist '(objc++-headerp . objc++-mode))

;; (defun objc++-guess-basic-syntax ()
;;   "Return the syntactic context of the current line."
;;   (save-excursion
;;     (beginning-of-line)
;;     (c-save-buffer-state
;; 	(save-match-data
;; 	  (message c-syntactic-context)))))
;; (advice-add 'c-guess-basic-syntax :after #'objc++-guess-basic-syntax)

;; (advice-mapc (lambda (fn prop)
;; 	       (progn
;; 		 (message "%s" fn)
;; 		 (message "%s" prop))) 'c-guess-basic-syntax)
;; (advice-remove 'c-guess-basic-syntax #'objc++-guess-basic-syntax)

(defun c-indent-objc++-hook ()
  (interactive)
  (if (c-major-mode-is 'objc++-mode)
      (let ((context c-syntactic-context)
	    (col (current-indentation))
	    (pos (point)))
	(message "context %s" context)
	(message "col %s" col)
	(message "pos %s" pos)
	(when (assq 'func-decl-cont context)
	  (save-excursion
	    (beginning-of-line)
	    (c-backward-syntactic-ws)
	    (let* ((langelms (c-guess-basic-syntax))
		   (target-col (c-get-syntactic-indentation langelms)))
	      (message "langelms %s" langelms)
	      (message "target-col %s" target-col)
	      (goto-char pos)
	      (indent-line-to target-col)))))
    (message "not objc++-mode")))

(add-hook 'c-special-indent-hook 'c-indent-objc++-hook)

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.mm\\'" . objc++-mode))

(provide 'objc++-mode)
