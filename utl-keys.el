;;; utl-keys.el --- Additional functionality for key bindings

;; Author: Alex Kost <alezost@gmail.com>
;; Created: 4 Jun 2014

;;; Code:

(require 'bind-key)

(defvar utl-keys-default-variables nil
  "Default list of variables used by `utl-bind-keys'.")

;;;###autoload
(defun utl-bind-keys-from-vars (maps &optional vars no-default)
  "Bind all keys from variables VARS in all MAPS.

MAPS is a keymap or a list of keymaps.

VARS is a variable or a list of variables with bindings.  Each
variable should contain a list of key bindings specifications.
Each spec should be either a cons of a key string and a function,
or a key string (the bound function is nil in the latter case).

Variables from `utl-keys-default-variables' are also used for
binding, unless NO-DEFAULT is non-nil.  The bindings from VARS
have a priority over the bindings from these variables."
  (or maps (error "Maps should be specified"))
  (when (symbolp maps)
    (setq maps (list maps)))
  ;; Join `utl-keys-default-variables' and VARS into one list
  (unless (or no-default (null utl-keys-default-variables))
    (setq vars (append utl-keys-default-variables
                       (cond ((null vars) nil)
                             ((symbolp vars) (list vars))
                             ((listp vars) vars)
                             (t (error "Wrong value of VARS"))))))
  (if (null vars)
      (error "Variables are not specified and `utl-keys-default-variables' is nil")
    (when (symbolp vars)
      (setq vars (list vars)))
    (let ((keys (utl-specs-from-vars vars)))
      ;; FIXME Is it possible to avoid `eval'?  Simply converting
      ;; `utl-bind-keys-from-vars' to a macro wouldn't work as the
      ;; variables with keys (from VARS) are not known at expansion time
      ;; (see also
      ;; <http://stackoverflow.com/questions/2571401/why-exactly-is-eval-evil/4052618#4052618>)
      (mapc (lambda (spec)
              (eval `(utl-bind-key-in-maps
                      ,(car spec) ,(cdr spec) ,maps)))
            keys))))

(defun utl-specs-from-vars (vars)
  "Return list of key binding specifications from variables VARS.
For the meaning of values of VARS, see `utl-bind-keys-from-vars'.

Returning value is alist of keys and functions with removed key
duplicates (rightmost values retain)."
  (let* ((keys-raw (apply 'append
                          (mapcar (lambda (var) (symbol-value var))
                                  vars)))
         (keys (mapcar (lambda (spec)
                         (if (stringp spec) (list spec) spec))
                       keys-raw)))
    (cl-remove-duplicates
     keys :test (lambda (obj1 obj2)
                  (string= (car obj1) (car obj2))))))

(defmacro utl-bind-key-in-maps (key fun maps)
  "Bind KEY to a function FUN in all MAPS.
If FUN is nil, unbind the KEY only if it is already bound in a
particular map.

Example:
  (utl-bind-key-in-maps \"C-j\" newline (ielm-map lisp-mode-map))"
  `(progn
     ,@(mapcar (lambda (map)
                 (if fun
                     `(bind-key ,key ',fun ,map)
                   `(and (lookup-key ,map (read-kbd-macro ,key))
                         (bind-key ,key nil ,map))))
               maps)))


;;; Binding buffer local keys

;; Idea from <http://www.emacswiki.org/emacs/BufferLocalKeys>

(defvar-local utl-local-map nil
  "Local keymap used by `utl-bind-local-keys'.")

;;;###autoload
(defun utl-bind-local-keys (&rest vars)
  "Bind all keys from variables VARS locally in the current buffer.
VARS are variables with bindings supported by
`utl-bind-keys-from-vars'."
  (setq utl-local-map (copy-keymap (current-local-map)))
  (use-local-map utl-local-map)
  (utl-bind-keys-from-vars 'utl-local-map vars t))

(provide 'utl-keys)

;;; utl-keys.el ends here
