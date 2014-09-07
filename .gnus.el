;; -*-Emacs-Lisp-*-

(load "starttls")
(load-library "smtpmail")

(setq user-mail-address "pa2311@gmail.com")
(setq user-full-name "Artem Petrov")

;; incoming mail (IMAP)
(setq gnus-select-method '(nnimap "gmail"
                                  (nnimap-address "imap.gmail.com")
                                  (nnimap-server-port 993)
                                  (nnimap-stream ssl)))

;; outbound mail (SMTP)
(setq smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-default-smtp-server "smtp.gmail.com"
      send-mail-function 'smtpmail-send-it
      message-send-mail-function 'smtpmail-send-it
      smtpmail-smtp-service 587
      smtpmail-auth-credentials '(("smtp.gmail.com" 587 "pa2311@gmail.com" nil))
      smtpmail-debug-info t
      smtpmail-debug-verb t
      )
(setq smtpmail-local-domain nil)
(setq gnus-permanently-visible-groups "gmail")
(executable-find starttls-program)

;;
(setq gnus-thread-sort-functions
      '((not gnus-thread-sort-by-date)
        (not gnus-thread-sort-by-number)))
