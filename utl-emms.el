;;; utl-emms.el --- Additional functionality for EMMS

;; Author: Alex Kost <alezost@gmail.com>
;; Created: 2 May 2013

;;; Code:

(require 'emms)

(defvar utl-emms-seek-seconds 60
  "The number of seconds to seek forward or backward for
\"custom\" seeking.")

(defun utl-emms-seek-forward (&optional seconds)
  "Seek by SECONDS forward.
If SECONDS is nil, use `emms-big-seek-seconds'.
Interactively with prefix, prompt for SECONDS."
  (interactive
   (list (if current-prefix-arg
             (read-number "Seek forward by (seconds): ")
           nil)))
  (when emms-player-playing-p
    (emms-player-seek (or seconds utl-emms-seek-seconds))))

(defun utl-emms-seek-backward (&optional seconds)
  "Seek by SECONDS backward.
See `utl-emms-seek-forward' for details."
  (interactive
   (list (if current-prefix-arg
             (read-number "Seek backward by (seconds): ")
           nil)))
  (utl-emms-seek-forward (- (or seconds utl-emms-seek-seconds))))

(defun utl-emms-seek-to (seconds)
  "Seek the current player to SECONDS.
Interactively, prompt for the number of minutes.
With prefix, prompt for the number of seconds."
  (interactive
   (list (if current-prefix-arg
             (read-number "Seconds to seek to: ")
           (* 60 (read-number "Minutes to seek to: ")))))
  (emms-seek-to seconds))

;;;###autoload
(defun utl-emms-add-url (url)
  "Add url or a list of urls to emms playlist."
  (with-current-emms-playlist
    ;; use `emms-sourse-add' instead of `emms-add-url' because
    ;; the last one runs `emms-play-url' if prefix is specified
    (if (listp url)
        (dolist (u url)
          (emms-source-add 'emms-source-url u))
      (emms-source-add 'emms-source-url url))))

;;;###autoload
(defun utl-emms-add-and-play-url (url)
  "Add url or a list of urls to emms playlist and play the first added track."
  (with-current-emms-playlist
    (let ((first-new-track (point-max)))
      (utl-emms-add-url url)
      (emms-playlist-select first-new-track)))
  (emms-stop)
  (emms-start))

(provide 'utl-emms)

;;; utl-emms.el ends here
