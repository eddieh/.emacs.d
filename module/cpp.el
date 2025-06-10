(defconst default-c++-style
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
    ;; use block comments
    (c-block-comment-is-default . t)
    (c-block-comment-flag . t)
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
    (indent-tabs-mode . t)
    (tab-width . 4))
  "Default C++ Style")

(defun default-c++-style-mode ()
   "Set the current buffer's c-style to Default C++ Style.
Meant to be added to `c-mode-common-hook'."
   (interactive)
   (c-add-style "default-c++" default-c++-style t))


;; IOKit interface generator (.iig) header file
(add-to-list 'auto-mode-alist '("\\.iig\\'" . c++-mode))
