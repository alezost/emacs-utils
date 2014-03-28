;;; utl-erc.el --- Additional functionality for ERC

;; Author: Alex Kost <alezost@gmail.com>
;; Created: 29 Jul 2013

;;; Code:

(require 'erc)

(defun utl-erc-server-buffer-name ()
  "Return a name of buffer with default server."
  (concat (erc-compute-server) ":"
           (number-to-string (erc-compute-port))))

(defun utl-erc-server-buffer-rename ()
  "Rename current server buffer (make a general name)."
  ;; sometimes we need to modify names like "irc.freenode.net:7000<2>"
  (interactive)
  (let ((old-name (buffer-name))
        (new-name (utl-erc-server-buffer-name)))
    (when (string-match (concat (erc-compute-server) ":.*")
                        old-name)
      (rename-buffer new-name)
      (message "Current buffer was renamed from '%s' to '%s'."
               old-name new-name))))

(defun utl-erc-switch-to-server-buffer ()
  "Switch to ERC buffer with server."
  (interactive)
  (switch-to-buffer (utl-erc-server-buffer-name)))

;;;###autoload
(defun utl-erc-ido-switch-buffer ()
  "Switch to ERC buffer, or start ERC if not already started.
ERC buffer is selected using IDO."
  (interactive)
  (let ((erc-bufs (mapcar (lambda (b) (buffer-name b))
                          (erc-buffer-list))))
    (if erc-bufs
     	(switch-to-buffer (ido-completing-read "ERC buffer: " erc-bufs))
      (erc))))

;;;###autoload
(defun utl-erc-track-switch-buffer (arg)
  "Same as `erc-track-switch-buffer', but start ERC if not already started."
  (interactive "p")
  (let ((server-buffer (utl-erc-server-buffer-name)))
    (or (and (get-buffer server-buffer)
             ;; (with-current-buffer server-buffer
             ;;   (erc-server-process-alive))
             (progn (erc-track-switch-buffer arg) t))
        (let ((erc-join-buffer 'bury))
          (erc)))))

(defun utl-erc-get-channel-buffer-list ()
  "Return a list of the ERC-channel-buffers."
  (erc-buffer-filter
   '(lambda () (string-match "^#.*" (buffer-name (current-buffer))))))

;;;###autoload
(defun utl-erc-cycle ()
  "Switch to ERC channel buffer, or run `erc-select'.
When called repeatedly, cycle through the buffers."
  (interactive)
  (let ((buffers (utl-erc-get-channel-buffer-list)))
    (if buffers
        (progn (when (eq (current-buffer) (car buffers))
                 (bury-buffer)
                 (setq buffers (cdr buffers)))
               (and buffers
                    (switch-to-buffer (car buffers))))
      (call-interactively 'erc-select))))

(defvar utl-erc-channel-list '("#emacs" "#erc" "#gnus")
  "A list of channels used in `utl-erc-join-channel'.")

(defun utl-erc-join-channel (channel &optional key)
  "Join CHANNEL.
Similar to `erc-join-channel', but use IDO with `utl-erc-channel-list'."
  (interactive
   (list
    (let* ((cur-sexp (thing-at-point 'sexp))
           (chn (if (and cur-sexp
                         (eq 0 (string-match-p "#" cur-sexp)))
                    cur-sexp
                  ;; "#"
                  )))
      (ido-completing-read "Join channel: " utl-erc-channel-list nil nil chn))
    (when (or current-prefix-arg erc-prompt-for-channel-key)
      (read-from-minibuffer "Channel key (RET for none): " nil))))
  (with-current-buffer
      (utl-erc-server-buffer-name)
    (erc-cmd-JOIN channel (when (>= (length key) 1) key))))

(defun utl-erc-quit-server (reason)
  "Disconnect from current server.
Similar to `erc-quit-server', but without prompting for REASON."
  (interactive (list ""))
  (with-current-buffer
      (utl-erc-server-buffer-name)
    (erc-cmd-QUIT reason)))

(defun utl-erc-ghost-maybe (server nick)
  "Send GHOST message to NickServ if NICK ends with `erc-nick-uniquifier'.
The function is suitable for `erc-after-connect'."
  (when (string-match (format "\\(.*?\\)%s+$" erc-nick-uniquifier) nick)
    (let ((nick-orig (match-string 1 nick))
          (password erc-session-password))
      (erc-message "PRIVMSG" (format "NickServ GHOST %s %s"
                                     nick-orig password))
      (erc-cmd-NICK nick-orig)
      (erc-message "PRIVMSG" (format "NickServ IDENTIFY %s %s"
                                     nick-orig password)))))


;;; Away

(defvar utl-erc-away-msg-list '("just away" "learning emacs" "sleeping")
  "A list of away messages for `utl-erc-away'.")

(defun utl-erc-away (&optional reason)
  "Mark the user as being away.
Interactively prompt for reason; with prefix mark as unaway.
Reasons are taken from `utl-erc-away-msg-list'."
  (interactive
   (list (if current-prefix-arg
             ""
           (ido-completing-read "Reason for AWAY: "
                                utl-erc-away-msg-list))))
  (with-current-buffer
      (utl-erc-server-buffer-name)
    (erc-cmd-AWAY (or reason ""))))

(defun utl-erc-away-time ()
  "Return non-nil if the current ERC process is set away.
Similar to `erc-away-time', but no need to be in ERC buffer."
  (with-current-buffer
      (utl-erc-server-buffer-name)
    (erc-away-time)))


;;; CTCP info

(defun utl-erc-ctcp-query-FINGER (proc nick login host to msg)
  "Respond to a CTCP FINGER query."
  (unless erc-disable-ctcp-replies
    (erc-send-ctcp-notice nick "FINGER Keep your FINGER out of me."))
  nil)

(defun utl-erc-ctcp-query-ECHO (proc nick login host to msg)
  "Respond to a CTCP ECHO query."
  (when (string-match "^ECHO\\s-+\\(.*\\)\\s-*$" msg)
    (let (;;(str (rot13 (match-string 1 msg)))
          (str (apply 'string(reverse (string-to-list (match-string 1 msg))))))
      (unless erc-disable-ctcp-replies
	(erc-send-ctcp-notice nick (format "ECHO Did you mean '%s'?" str)))))
  nil)

(defun utl-erc-ctcp-query-TIME (proc nick login host to msg)
  "Respond to a CTCP TIME query."
  (unless erc-disable-ctcp-replies
    (let* ((hour (nth 2 (decode-time (current-time))))
           (str (cond ((utl-erc-away-time) "time to be away")
                      ((>= hour 18) "almost night")
                      ((>= hour 12) (format-time-string "%A"))
                      ((>= hour 6)  "always morning")
                      (t            "time to sleep"))))
      (erc-send-ctcp-notice nick (format "TIME %s." str))))
  nil)

(defun utl-erc-ctcp-query-VERSION (proc nick login host to msg)
  "Respond to a CTCP VERSION query."
  (unless erc-disable-ctcp-replies
    (erc-send-ctcp-notice
     nick
     (format "VERSION %s." (erc-version))))
  nil)


;;; Log

(defun utl-erc-view-log-file ()
  "Visit a log file for current ERC buffer."
  (interactive)
  (view-file (erc-current-logfile)))

(defun utl-erc-log-file-name-network-channel (buffer target nick server port)
  "Return erc log-file name of network (or server) and channel names.
The result file name is in the form \"network_channel.txt\".
This function is suitable for `erc-generate-log-file-name-function'."
  (with-current-buffer buffer
    (let* ((target (erc-default-target)) ; nil for server buffer
           (file (concat (or (erc-network-name) server)
                         (and target (concat "_" target))
                         ".txt")))
      ;; we need a make-safe-file-name function.
      (convert-standard-filename file))))

;; If you want to exclude a particular channel "#foochannel" and
;; channels that have "beard" in their names, use the following:
;;
;; (setq utl-erc-log-excluded-regexps '("\\`#foochannel" "beard"))
;; (setq erc-enable-logging 'utl-erc-log-all-but-some-buffers)
;;
;; Note: channel buffers may have names like "#foobar<2>", so too strict
;; regexps like "\\`#foochannel\\'" may not be not good.

(defvar utl-erc-log-excluded-regexps nil
  "List of regexps for erc buffer names that will not be logged.")

(defun utl-erc-log-all-but-some-buffers (buffer)
  "Return t if logging should be enabled for BUFFER.
Use `utl-erc-log-excluded-regexps' to check if BUFFER should be
logged or not.
The function is intended to be used for `erc-enable-logging'."
  (cl-notany (lambda (re)
               (string-match-p re (buffer-name buffer)))
             utl-erc-log-excluded-regexps))

(provide 'utl-erc)

;;; utl-erc.el ends here
