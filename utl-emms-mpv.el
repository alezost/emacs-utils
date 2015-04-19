;;; utl-emms-mpv.el --- Additional functionality for using EMMS with mpv

;; Author: Alex Kost <alezost@gmail.com>
;; Created: 17 Apr 2015

;;; Code:

(require 'emms-player-simple-mpv)

(defun utl-emms-mpv-run-command (command)
  "Run mpv COMMAND for the current EMMS mpv process.
COMMAND is what may be put in mpv conf-file, e.g.: 'cycle mute',
'show_text ${playback-time}', etc."
  (interactive "sRun mpv command: ")
  (when (emms-player-simple-mpv-playing-p)
    (tq-enqueue emms-player-simple-mpv--tq
                (concat command "\n")   ; newline is vital
                "" nil #'ignore)))

(defun utl-emms-mpv-show-progress ()
  "Show progress in the OSD of the current video."
  (interactive)
  (utl-emms-mpv-run-command "show_progress"))

(defun utl-emms-mpv-toggle-fullscreen ()
  "Toggle fullscreen."
  (interactive)
  (utl-emms-mpv-run-command "cycle fullscreen"))

(provide 'utl-emms-mpv)

;;; utl-emms-mpv.el ends here
