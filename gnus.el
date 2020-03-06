;;; Name ideas and prefixes

;; Threaded Messages
;;   threadmsg/ thmsg/ tmsg/

;; Threaded Communication (Thread Com)
;;   threadcom/ thcom/ thcm/ thc/

;; Threaded Communicator (ThreadCom)
;;   threadcom/ thcom/ thcm/ thc/



;; Helpful websites:
;; http://www.cataclysmicmutation.com/2010/11/multiple-gmail-accounts-in-gnus/
;; https://github.com/redguardtoo/mastering-emacs-in-one-year-guide/blob/master/gnus-guide-en.org
;; https://www.gnu.org/software/emacs/manual/html_node/gnus/Group-Parameters.html
;; http://laltromondo.amusewiki.org/informatica/soft/mail_with_gnus/
;; http://doc.rix.si/cce/cce-gnus.html
;; https://stackoverflow.com/questions/22150745/changing-the-display-name-of-a-gnus-group


;;; Accounts

;; This needs to be something, nil doesn't cut it. If we set it to a
;; mail method it will make further configuration confusing because
;; mailbox names will not be qualified names.
;;
;; Oh, the way to set it to nil is to use 'nnnil!
(setq gnus-select-method '(nnnil "not-a-group"))

;; Use secondary select methods for mail so that mailboxes have
;; qualified names of the form method+identifier:MAILBOX.
(add-to-list 'gnus-secondary-select-methods
	     '(nnmaildir "primary"
			 (directory "~/.maildir/primary")
			 (expire-age 'never)))


;;; Group/Mailbox & Article/Message list formats

;; Always show topics
(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)

;; Show all groups (mailboxes)
(setq gnus-permanently-visible-groups ".*")


;; Formats

;; ‘M-x gnus-update-format’ will ‘eval’ the current form near the
;; point, update the spec in question and pop you to a buffer where
;; you can examine the resulting Lisp code to be run to generate the
;; line.

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

;; group list and summary list should behave like dired

;; n - next unread
;; p - previous unread

;; N - next
;; P - previous

;; RET - view
;; m - mark


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


;;; Viewing

;; Discourage showing HTML email
(setq mm-discouraged-alternatives '("text/html" "text/richtext")
      mm-automatic-display (remove "text/html" mm-automatic-display))


;;; Sending

;; Send mail with msmtp
(setq message-send-mail-function 'message-send-mail-with-sendmail
      sendmail-program "/usr/local/bin/msmtp")

(defun thc/message-sent-group-for-group (group)
  (cond
   ((string-match-p (regexp-quote "nnmaildir+primary:") group)
    "nnmaildir+primary:Sent Messages")
   (t '((format-time-string "sent.%Y-%m")))))

;; Copy to sent mailbox and mark the copy read
(setq gnus-message-archive-group 'thc/message-sent-group-for-group
      gnus-gcc-mark-as-read t)


;;; Sane commands and key bindings

;; Compose new mail "c"
(defun thc/compose-mail ())
(defun thc/compose-mail-other-frame ())
(defun thc/compose-mail-other-window ())

(define-key gnus-group-mode-map (kbd "c") 'gnus-group-mail)
;(define-key gnus-topic-mode-map (kbd "c") 'gnus-group-mail)
(define-key gnus-summary-mode-map (kbd "c")
  'gnus-summary-mail-other-window)

;; Reply to message "r"
;; • With prefix don't quote original
;; • With active region, quote region
(define-key gnus-summary-mode-map (kbd "r")
  'gnus-summary-reply-with-original)

;; DOC: Start composing a reply mail to the current message.
;; The text in the region will be yanked.  If the region isn’t active,
;; the entire article will be yanked.
(define-key gnus-article-mode-map (kbd "r")
  'gnus-article-reply-with-original)

;; Reply all "R"
(define-key gnus-summary-mode-map (kbd "R")
  'gnus-summary-wide-reply-with-original)
(define-key gnus-article-mode-map (kbd "R")
  'gnus-article-wide-reply-with-original)

;; Reply to list "l"
;; TODO:
;; gnus-summary-reply-to-list-with-original

;; Forward "f"
;; TODO:
;; gnus-summary-mail-forward

;; Marks

(defconst thc/gnus-unread-mark ?•
  "Mark used to indicate a message is unread.")

(defconst thc/gnus-read-mark ?		; whitespace
  "Mark used to indicate a message has been read.")

(defconst thc/gnus-archive-mark ?a
  "Mark used for messages to be archived.")

(defconst thc/gnus-delete-mark ?d
  "Mark used for messages to be deleted.")

(defconst thc/gnus-junk-mark ?$
  "Mark used for messages that are junk or spam.")

(defconst thc/gnus-summary-marks
  (list thc/gnus-read-mark
    thc/gnus-archive-mark
    thc/gnus-delete-mark
    thc/gnus-junk-mark)
  "The list of marks used in ThreadCom.")

(defun thc/gnus-unread-mark-p (mark)
  "Says whether MARK is the unread mark."
  (= mark thc/gnus-unread-mark))

(defun thc/gnus-read-mark-p (mark)
  "Says whether MARK is the read mark."
  (= mark thc/gnus-read-mark))

(defun thc/gnus-archive-mark-p (mark)
  "Says whether MARK is the archive mark."
  (= mark thc/gnus-archive-mark))

(defun thc/gnus-delete-mark-p (mark)
  "Says whether MARK is the delete mark."
  (= mark thc/gnus-delete-mark))

(defun thc/gnus-junk-mark-p (mark)
  "Says whether MARK is the archive mark."
  (= mark thc/gnus-junk-mark))

;; Archiving "a"
(defun thc/message-archive-group-for-group (group)
  (cond
   ((string-match-p (regexp-quote "nnmaildir+primary:") group)
    "nnmaildir+primary:Archive")
   (t nil)))

(defun thc/gnus-summary-archive-article-forward (n)
  "Archive N articles forwards.
If N is negative, archive backwards instead.
The difference between N and the number of articles archived is returned."
  (interactive "p")
  (gnus-summary-mark-forward n thc/gnus-archive-mark))

;; function that can possibly be used to archive mail
;; gnus-summary-copy-article

;; (defun thc/gnus-summary-archive-article ()
;;    "Copy current article to a suitable nnml archive group.
;;  The copied article will be marked as \"ancient\", the original
;;  will retain its current marks."
;;    (interactive)
;;    (let* ((group-name (if (string-match "^nnml:" gnus-newsgroup-name)
;; 			  (substring gnus-newsgroup-name 5 nil)
;; 		        gnus-newsgroup-name))
;; 	  (archive-name (concat "nnml:archive." group-name))
;; 	  (orig-mark (gnus-summary-article-mark)))
;;      (gnus-summary-mark-article nil gnus-ancient-mark t)
;;      (gnus-summary-copy-article 1 archive-name)
;;      (gnus-summary-mark-article nil orig-mark t)))

(define-key gnus-summary-mode-map (kbd "a")
  'thc/gnus-summary-archive-article-forward)

;; Deleting "d"

(defun thc/gnus-summary-delete-article-forward (n)
  "Delete N articles forwards.
If N is negative, delete backwards instead.
The difference between N and the number of articles deleted is returned."
  (interactive "p")
  (gnus-summary-mark-forward n thc/gnus-delete-mark))

(define-key gnus-summary-mode-map (kbd "d")
  'thc/gnus-summary-delete-article-forward)

;; gnus-summary-expire-articles
;; gnus-summary-delete-article

;; Unread/read ";"
(defun thc/gnus-summary-mark-as-read-forward (n)
  "Archive N articles forwards.
If N is negative, archive backwards instead.
the difference between N and the number of articles archived is returned."
  (interactive "p")
  (gnus-summary-mark-forward n thc/gnus-read-mark))

(define-key gnus-summary-mode-map (kbd ";")
  'thc/gnus-summary-mark-as-read-forward)

;; Junk/spam "j"
(define-key gnus-summary-mode-map (kbd "j")
  'gnus-summary-mark-as-spam)

;; unmark "u", unmark all "U"
(define-key gnus-summary-mode-map (kbd "u")
  'gnus-summary-clear-mark-forward)

(defun thc/gnus-move-article (article orig-group dest-group)
  ;; Remove this article from future suppression.
  (gnus-dup-unsuppress-article article)
  (let* ((from-method (gnus-find-method-for-group orig-group))
	 (to-method (gnus-find-method-for-group dest-group))
	 (move-is-internal (gnus-server-equal from-method to-method)))
    (gnus-request-move-article
     article
     orig-group
     (nth 1 from-method)
     (list
      'gnus-request-accept-article dest-group to-method nil t)
     nil
     (and move-is-internal
	  dest-group
	  (gnus-group-real-name dest-group)))))

;; Execute marks (deletes, unread, junk, etc) "x"
(defun thc/gnus-summary-execute-marks ()
  (interactive)
  (message "Executing marks in %s" gnus-newsgroup-name)
  (let ((data gnus-newsgroup-data)
	(group gnus-newsgroup-name)
	(marks thc/gnus-summary-marks)
	article id mark)
    (while data
      (when (memq (gnus-data-mark (car data)) marks)
	(setq article (car data))
	(setq id (gnus-data-number article))
	(setq mark (gnus-data-mark article))
	(message "article => %s %s %s" id mark
		 (mail-header-subject (gnus-data-header article)))
	(cond
	 ((thc/gnus-read-mark-p mark)
	  (message "read"))
	 ((thc/gnus-archive-mark-p mark)
	  (progn
	    (message "archive")
	    (let ((archive-group (thc/message-archive-group-for-group group)))
	      (gnus-summary-mark-article id gnus-ancient-mark t)
	      (thc/gnus-move-article article group archive-group))))
	  ((thc/gnus-delete-mark-p mark)
	   (message "delete"))
	  ((thc/gnus-junk-mark-p mark)
	   (message "junk"))
	  (t (message "unmarked")))
	(message "next..."))
      (setq data (cdr data)))))

(define-key gnus-summary-mode-map (kbd "x")
  'thc/gnus-summary-execute-marks)



;;; Sorting


;;; Searching
