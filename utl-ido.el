;;; utl-ido.el --- Additional functionality for ido-mode

;; Author: Alex Kost <alezost@gmail.com>
;; Created: 18 May 2013

;;; Code:

(require 'ido)


;;; Ido completion maps

;; The problem with ido completion maps is that they are defvar-ed as
;; nils (not keymaps), so a usual

;; (eval-after-load 'ido '(define-key ido-common-completion-map " " nil))

;; will not work.  Those maps are set with `ido-init-completion-maps'.
;; So there are the following ways to define bindings in a completion
;; map (like the above "release" of SPC):

;; 1. Make a function with bindings and add it to `ido-setup-hook'.
;; 2. Make an after/around advice for `ido-init-completion-maps' with bindings.
;; 3. Make an empty around advice and bind keys as usual.

;; I prefer the 3rd variant as it prevents a stupid recreating bindings
;; each time `ido-init-completion-maps' is called.  So i add the
;; following to my .emacs:

;; (require 'utl-ido)
;; (ido-init-completion-maps)  ; call one time to generate bindings
;; (ad-activate 'ido-init-completion-maps)
;; (define-key ido-common-completion-map " " nil)  ; use SPC as a normal char

(defadvice ido-init-completion-maps (around utl-no-actions)
  "Do nothing because the function is always run for any ido-thing.
Such behaviour prevents to modify key bindings.
It is run by `ido-common-initialization', which is run by `ido-mode'.")


;;; Use ido everywhere

;; Got from <http://www.emacswiki.org/emacs/InteractivelyDoThings#toc15>.
;; To activate the following code, add to .emacs:
;;   (ad-activate 'completing-read)
;;   (ad-activate 'read-file-name-default)

(defvar utl-ido-enable-replace-completing-read t
  "If non-nil, use `ido-completing-read' instead of `completing-read'.

Set it to nil using let in around-advice for functions where the
original completing-read is required.  For example, if a function
foo absolutely must use the original completing-read, define some
advice like this:

  (defadvice foo (around original-completing-read activate)
    (let (utl-ido-enable-replace-completing-read) ad-do-it))")

(defadvice completing-read (around utl-use-ido-completing-read)
  "Replace `completing-read' with `ido-completing-read' anywhere.
See also `utl-ido-enable-replace-completing-read'."
  (if (or (null utl-ido-enable-replace-completing-read)
          (and (boundp 'ido-cur-list)
               ido-cur-list)) ; Avoid infinite loop from ido calling this
      ad-do-it
    (let ((allcomp (all-completions "" collection predicate)))
      (if allcomp
          (setq ad-return-value
                ;; match is never required because with requiring it's
                ;; not possible to select "#XXXXXX" with `read-color'
                (ido-completing-read
                 prompt allcomp nil nil initial-input hist def))
        ad-do-it))))

(defadvice read-file-name-default (around utl-original-completing-read)
  "Roll back from `ido-completing-read' to `completing-read'."
  (let (utl-ido-enable-replace-completing-read)
    ad-do-it))



;;;###autoload
(defun utl-ido-home-work-directory ()
  "Change to the home working directory."
  (interactive)
  (ido-set-current-directory "~")
  (setq ido-exit 'refresh)
  (setq ido-text-init ido-text)
  (setq ido-rotate-temp t)
  (exit-minibuffer))

(provide 'utl-ido)

;;; utl-ido.el ends here
