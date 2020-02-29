;; Helpful websites:
;; http://www.cataclysmicmutation.com/2010/11/multiple-gmail-accounts-in-gnus/
;; https://github.com/redguardtoo/mastering-emacs-in-one-year-guide/blob/master/gnus-guide-en.org
;; https://www.gnu.org/software/emacs/manual/html_node/gnus/Group-Parameters.html

;; This needs to be something, nil doesn't cut it. If we set it to a
;; mail method it will make further configuration confusing because
;; mailbox names will not be qualified names.
(setq gnus-select-method '(nntp "news.gwene.org"))

;; Use secondary select methods for mail so that mailboxes have
;; qualified names of the form method+identifier:MAILBOX.
(add-to-list 'gnus-secondary-select-methods
	     '(nnmaildir "primary"
			 (directory "~/.maildir/primary")
			 (expire-age 'never)))

;; Always show topics
(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)

;; Show all groups (mailboxes)
(setq gnus-permanently-visible-groups ".*")

;; Group format
(setq gnus-group-line-format "%M%S%p%P%5y:%B%(%g%)\n")


;; Group parameters can be set via the gnus-parameters variable too.
;; But some variables, such as visible, have no effect (For this case
;; see gnus-permanently-visible-groups as an alternative.).

;; (setq gnus-parameters
;;            '(("mail\\..*"
;;               (gnus-show-threads nil)
;;               (gnus-use-scoring nil)
;;               (gnus-summary-line-format
;;                "%U%R%z%I%(%[%d:%ub%-23,23f%]%) %s\n")
;;               (gcc-self . t)
;;               (display . all))
;;              ("^nnimap:\\(foo.bar\\)$"
;;               (to-group . "\\1"))
;;              ("mail\\.me"
;;               (gnus-use-scoring t))
;;              ("list\\..*"
;;               (total-expire . t)
;;               (broken-reply-to . t))))

;; Here is an example to sort an NNTP group by reverse date to see the
;; latest news at the top and an RSS group by subject. In this
;; example, the first group is the Debian daily news group
;; gmane.linux.debian.user.news from news.gmane.org. The RSS group
;; corresponds to the Debian weekly news RSS feed
;; http://packages.debian.org/unstable/newpkg_main.en.rdf

;; (setq gnus-parameters
;;       '(("nntp.*gmane\\.debian\\.user\\.news"
;;          (gnus-show-threads nil)
;;          (gnus-article-sort-functions '((not gnus-article-sort-by-date)))
;;          (gnus-use-adaptive-scoring nil)
;;          (gnus-use-scoring nil))
;;         ("nnrss.*debian"
;;          (gnus-show-threads nil)
;;          (gnus-article-sort-functions 'gnus-article-sort-by-subject)
;;          (gnus-use-adaptive-scoring nil)
;;          (gnus-use-scoring t)
;;          (gnus-score-find-score-files-function 'gnus-score-find-single)
;;          (gnus-summary-line-format "%U%R%z%d %I%(%[ %s %]%)\n"))))

(setq gnus-parameters
      '(("nnmaildir\\+primary:INBOX"
	 (display . all)
	 (posting-style
	  (name (or (getenv "FULL_NAME") "FULL_NAME not set"))
	  (address (or (getenv "EMAIL_PRIMARY") "EMAIL_PRIMARY not set")))
	 (expiry-target . delete))))

;;(setq message-send-mail-function 'message-send-mail-with-sendmail)
;;(setq sendmail-program "/usr/local/bin/msmtp")


;; Threads
;; (setq gnus-summary-line-format "%*%U%R%z %(%&user-date; %-15,15f  %B%s%)\n"
;;       gnus-summary-thread-gathering-function 'gnus-gather-threads-by-references
;;       gnus-thread-sort-functions '(gnus-thread-sort-by-score)
;;       gnus-sum-thread-tree-false-root ""
;;       gnus-sum-thread-tree-indent " "
;;       gnus-sum-thread-tree-leaf-with-other "├► "
;;       gnus-sum-thread-tree-root ""
;;       gnus-sum-thread-tree-single-leaf "╰► "
;;       gnus-sum-thread-tree-vertical "│"
;;       gnus-user-date-format-alist '((t . "%d %b %Y %H:%M")))

;; Discourage showing HTML email.
(setq mm-discouraged-alternatives '("text/html" "text/richtext")
      mm-automatic-display (remove "text/html" mm-automatic-display))
