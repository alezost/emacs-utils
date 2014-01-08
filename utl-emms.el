;;; utl-emms.el --- Additional functionality for EMMS

;; Author: Alex Kost <alezost@gmail.com>
;; Created: 2 May 2013

;;; Code:

(require 'emms)

(defcustom emms-big-seek-seconds 60
  "The number of seconds to seek forward or backward for
\"custom\" seeking with `emms-seek'."
  :group 'emms
  :type 'number)

(defun emms-big-seek-forward ()
  "Seek `emms-big-seek-seconds' forward."
  (interactive)
  (when emms-player-playing-p
    (emms-player-seek emms-big-seek-seconds)))

(defun emms-big-seek-backward ()
  "Seek `emms-big-seek-seconds' backward."
  (interactive)
  (when emms-player-playing-p
    (emms-player-seek (- emms-big-seek-seconds))))

(defun emms-seek-to-minute (minutes)
  "Seek the current player to MINUTES minutes."
  (interactive "nMinutes to seek to: ")
  (emms-seek-to (* 60 minutes)))

;;;###autoload
(defun utl-emms-add-url (url)
  "Add url or a list of urls to emms playlist."
  (with-current-emms-playlist
    ;; use `emms-sourse-add' instead of `emms-add-url' because
    ;; the last one run `emms-play-url' if prefix is specified
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
