;;; utl-emms.el --- Additional functionality for EMMS

;; Author: Alex Kost <alezost@gmail.com>
;; Created: 2 May 2013

;;; Code:

(require 'emms)

(defvar emms-playing-time-string)

(defvar utl-emms-seek-seconds 60
  "The number of seconds to seek forward or backward.
Used as a default value by `utl-emms-seek-forward' and
`utl-emms-seek-backward'.")

(defun utl-emms-seek-forward (&optional seconds)
  "Seek by SECONDS forward.
If SECONDS is nil, use `utl-emms-seek-seconds'.
Interactively, define SECONDS with a numeric prefix."
  (interactive "p")
  (when emms-player-playing-p
    (emms-player-seek (or seconds utl-emms-seek-seconds))))

(defun utl-emms-seek-backward (&optional seconds)
  "Seek by SECONDS backward.
See `utl-emms-seek-forward' for details."
  (interactive "p")
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

(defun utl-emms-source-add-and-play (source &rest args)
  "Add the tracks of SOURCE to EMMS playlist and play the first one."
  (with-current-emms-playlist
    (goto-char (point-max))
    (let ((first-new-track (point)))
      (apply #'emms-playlist-insert-source source args)
      (emms-playlist-select first-new-track)))
  (emms-stop)
  (emms-start))

(defun utl-emms-first ()
  "Start playing the first track in the EMMS playlist."
  (interactive)
  (when emms-player-playing-p
    (emms-stop))
  (emms-playlist-current-select-first)
  (emms-start))

(defun utl-emms-playing-time-string (track)
  "Return playing time of TRACK."
  (let ((time (emms-track-get track 'info-playing-time)))
    (if time
        (format "%d:%02d" (/ time 60) (% time 60))
      "")))


;;; Track description

(defun utl-emms-full-track-description (track)
  "Return a full description of TRACK.
Intended to be used for `emms-track-description-function'."
  (let ((artist   (emms-track-get track 'info-artist))
        (title    (emms-track-get track 'info-title))
        (tracknum (emms-track-get track 'info-tracknumber))
        (album    (emms-track-get track 'info-album))
        (year     (emms-track-get track 'info-year)))
    (let ((name (cond
                 ((and artist title) (concat artist " – " title))
                 (title title))))
      (if (null name)
          (emms-track-simple-description track)
        (when tracknum
          (setq name (format "%02d. %s" (string-to-number tracknum) name)))
        (cond
         ((and album year)
          (setq name (format "%s [%s – %s]" name year album)))
         (year
          (setq name (format "%s [%s]" name year)))
         (album
          (setq name (format "%s [%s]" name album))))
        name))))

(defun utl-emms-short-track-description (track)
  "Return a short description of TRACK suitable for mode-line."
  (let ((title  (emms-track-get track 'info-title)))
    (if title
        title
      (let ((type (emms-track-type track)))
        (cond ((eq 'file type)
               (file-name-nondirectory (emms-track-name track)))
              ((eq 'url type)
               (url-file-nondirectory (emms-format-url-track-name
                                       (emms-track-name track))))
              (t (concat (symbol-name type)
                         ": " (emms-track-name track))))))))


;;; Notifications

(require 'notifications)
(require 'xml)

(defvar utl-emms-notification-artist-format "%s")
(defvar utl-emms-notification-title-format "%s")
(defvar utl-emms-notification-album-format "%s")
(defvar utl-emms-notification-year-format "%s")

(defun utl-emms-notification-track-property (track property
                                             &optional format-str)
  "Return TRACK PROPERTY formatted with FORMAT-STR."
  (let* ((val (emms-track-get track property))
         (val (and (stringp val)
                   (xml-escape-string val))))
    (and val
         (if format-str
             (format format-str val)
           val))))

(defun utl-emms-notification-track-description (track)
  "Return description of TRACK suitable for (dunst) notifications."
  (let ((artist   (utl-emms-notification-track-property
                   track 'info-artist
                   utl-emms-notification-artist-format))
        (title    (utl-emms-notification-track-property
                   track 'info-title
                   utl-emms-notification-title-format))
        (tracknum (utl-emms-notification-track-property
                   track 'info-tracknumber))
        (album    (utl-emms-notification-track-property
                   track 'info-album
                   utl-emms-notification-album-format))
        (year     (utl-emms-notification-track-property
                   track 'info-year
                   utl-emms-notification-year-format)))
    (let* ((title (or title
                      (emms-track-simple-description track)))
           (title (if tracknum
                      (format "%02d. %s"
                              (string-to-number tracknum) title)
                    title))
           (album (cond ((and album year)
                         (format "%s – %s" year album))
                        (year  (format "%s" year))
                        (album (format "%s" album)))))
      (mapconcat #'identity
                 (delq nil (list artist title album))
                 "\n"))))

;;;###autoload
(defun utl-emms-notify ()
  "Notify about the current track using `notifications-notify'."
  (interactive)
  (let ((track (emms-playlist-current-selected-track)))
    (when track
      (let ((status (if emms-player-playing-p
                        (if emms-player-paused-p "⏸" "⏵")
                      "⏹"))
            (time (if (string= "" emms-playing-time-string)
                      (utl-emms-playing-time-string track)
                    emms-playing-time-string)))
        (notifications-notify
         :app-name "emms"
         :title (format "%s %s" status time)
         :body (utl-emms-notification-track-description track))))))

;;;###autoload
(define-minor-mode utl-emms-notification-mode
  "Minor mode for EMMS notifications."
  :global t
  :init-value nil
  (if utl-emms-notification-mode
      (add-hook 'emms-player-started-hook 'utl-emms-notify)
    (remove-hook 'emms-player-started-hook 'utl-emms-notify)))


;;; Mode line

(require 'emms-mode-line)
(require 'emms-playing-time)

(defvar utl-emms-state-mode-line-string
  '(emms-player-playing-p (emms-player-paused-p "⏸" "⏵") "⏹")
  "Mode line string displaying the state of the current EMMS process.")

(defvar utl-emms-mode-line-string
  '(" " utl-emms-state-mode-line-string
    (:propertize emms-playing-time-string face font-lock-constant-face)
    emms-mode-line-string)
  "Mode line string with the EMMS info.")
(put 'utl-emms-mode-line-string 'risky-local-variable t)

;;;###autoload
(defun utl-emms-mode-line (arg)
  "Turn on EMMS mode-line string if ARG is positive, off otherwise.

This is a substitution for `emms-mode-line' and `emms-playing-time'.

Use a single variable `utl-emms-mode-line-string' instead of
adding `emms-mode-line-string' and `emms-playing-time-string' to
the `global-mode-string'."
  (interactive "p")
  (or global-mode-string (setq global-mode-string '("")))
  (let (hook-action activep)
    (if (and arg (> arg 0))
        (progn
          (setq hook-action 'add-hook
                activep t)
          (when (and emms-mode-line-mode-line-function
                     (not (memq 'utl-emms-mode-line-string global-mode-string)))
            (setq global-mode-string
                  (append global-mode-string
                          '(utl-emms-mode-line-string))))
          (when emms-player-playing-p (emms-mode-line-alter)))
      (setq hook-action 'remove-hook
            activep nil)
      (emms-mode-line-restore-titlebar)
      (emms-playing-time-stop)
      (utl-emms-mode-line-restore-mode-line))
    (setq emms-mode-line-active-p activep
          emms-playing-time-p activep
          emms-playing-time-display-p activep)
    (funcall hook-action 'emms-track-updated-functions   'emms-mode-line-alter)
    (funcall hook-action 'emms-player-started-hook       'emms-mode-line-alter)
    (funcall hook-action 'emms-player-finished-hook      'emms-mode-line-blank)
    (funcall hook-action 'emms-player-stopped-hook       'emms-mode-line-blank)
    (funcall hook-action 'emms-player-started-hook       'emms-playing-time-start)
    (funcall hook-action 'emms-player-stopped-hook       'emms-playing-time-stop)
    (funcall hook-action 'emms-player-finished-hook      'emms-playing-time-stop)
    (funcall hook-action 'emms-player-paused-hook        'emms-playing-time-pause)
    (funcall hook-action 'emms-player-seeked-functions   'emms-playing-time-seek)
    (funcall hook-action 'emms-player-time-set-functions 'emms-playing-time-set)))

(defun utl-emms-mode-line-restore-mode-line ()
  "Restore the mode-line."
  (setq emms-mode-line-string nil
        emms-playing-time-string nil))

(defvar utl-emms-mode-line-song-function
  'utl-emms-short-track-description
  "Default function used in `utl-emms-mode-line-song-string'.")

(defun utl-emms-mode-line-song-string ()
  "Format the currently playing song.
Intended to be used for `emms-mode-line-mode-line-function'."
  (format emms-mode-line-format
          (funcall utl-emms-mode-line-song-function
                   (emms-playlist-current-selected-track))))

(provide 'utl-emms)

;;; utl-emms.el ends here
