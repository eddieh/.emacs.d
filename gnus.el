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
			 (directory "~/Maildir/primary")
			 (expire-age 'never)))


;;; Group/Mailbox & Article/Message list formats

;; Always show topics
(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)

;; Show all groups (mailboxes)
(setq gnus-permanently-visible-groups ".*")
