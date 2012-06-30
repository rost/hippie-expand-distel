;;; hippie-expand-distel.el --- Hook Distel's completion into hippie-expand

;; Author: Robin Österberg
;; URL: https://github.com/rost/hippie-expand-distel
;; Version:

;;; Commentary:

;; This is a very crude version with a bunch of duplicated code. Still
;; haven't figured out how hippie-expand try functions work. In need
;; of some major refactoring. Parts of this should probably be ripped
;; out to a separate library for reuse by auto-complete-distel.el

;;; Bugs:

;; This should not work for completing imported functions. Maybe not
;; even locally defined functions in a file. It's late, ok. I
;; primarily wanted something for the shell here.

;; Will not cycle complete list of a module's functions, you have to
;; know the first letter for now.

;; Usage:

;; (add-to-list 'load-path "~/path/to/hippie-expand-distel")
;; (require 'hippie-expand-distel)
;; (add-hook 'erlang-mode-hook 'hippie-expand-distel-setup)
;; (add-hook 'erlang-shell-mode-hook 'hippie-expand-distel-setup)

;;; Code:

(require 'distel)


;;; hippie-expand try function

(defun try-expand-erl-complete (old)
  "Complete Erlang module and function names using Distel as backend."
  (unless old
    (let ((end (point))
          (beg (save-excursion
                 (skip-syntax-backward "w_")
                 (when (eql (char-before) ?:)
                   (backward-sexp 1))
                 (point))))
      (he-init-string beg end)
      (destructuring-bind (mod func beg)
          (erl-destructure-module-function he-search-string beg)
        (setq erl-complete-module-name (if (string= mod "")
                                           mod
                                         (format "%s:" mod)))
        (setq he-expand-list
              (all-completions
               func
               (try-erl-complete he-search-string beg))))))
  (while (and he-expand-list
              (he-string-member (car he-expand-list) he-tried-table))
    (setq he-expand-list (cdr he-expand-list)))
  (if (null he-expand-list)
      (progn
        (when old (he-reset-string))
        ())
    (he-substitute-string (concat erl-complete-module-name (car he-expand-list)))
    (setq he-tried-table (cons (car he-expand-list) (cdr he-tried-table)))
    (setq he-expand-list (cdr he-expand-list))
    (setq he-num 0)
    t))


;;; Global variables needed for now
(defvar erl-complete-module-name ""
  "When completing a function name in a module we store the
  module name here for the next cycle of executing
  hippie-expand. We only save the function names in
  hippie-expand's internal variables, and the module name is lost
  during the first round unless we store it here.")

(defvar try-erl-complete-candidates-cache nil
  "Completions from Distel are put here by
  `&erl-complete-receuve-completions' and then returned to the
  hippie-expand try function'")


;;; Main

(defun try-erl-complete (search-string beg)
  "Return a list of completions matching `search-string'."
  (erl-complete-distel search-string beg)
  (sleep-for 0.1)
  try-erl-complete-candidates-cache)


;;; Determine type and send request

(defun erl-complete-distel (search-string beg)
  "Complete the module or remote function name at point."
  (let ((node erl-nodename-cache))
    (when beg
      (let* ((str search-string)
             (buf (current-buffer)))
        (if (erl-is-module-function search-string)
            (destructuring-bind (module function beg)
                (erl-destructure-module-function search-string beg)
              (erl-complete-function module function buf))
          (erl-complete-module search-string buf))))))


;;; Helpers

(defun erl-is-module-function (str)
  "Determine wether `str' is a call on the form
'module:function()' or 'function()'."
  (if (string-match "^\\(.*\\):\\(.*\\)$" str)
      t
    nil))

(defun erl-destructure-module-function (str beg)
  "If `str' is a call on the form 'module:function()',
destructure it and return 'module' and 'function' separately."
  (if (string-match "^\\(.*\\):\\(.*\\)$" str)
      (let ((module (intern (match-string 1 str)))
            (function (match-string 2 str))
            (beg (+ beg (match-beginning 2))))
        (list module function beg))
    (list "" str beg)))


;;; Sending and receiving messages

(defun erl-complete-module (module buf)
  "Request a list of module names matching `module'."
  (erl-spawn
    (erl-send-rpc node 'distel 'modules (list module))
    (&erl-complete-receive-completions
     "module" module buf)))

(defun erl-complete-function (module function buf)
  "Request a list of function names matching `function' for
module `module'."
  (erl-spawn
    (erl-send-rpc node 'distel 'functions (list module function))
    (&erl-complete-receive-completions
     "function" function buf)))

(defun &erl-complete-receive-completions (what prefix buf)
  "Receive completion result from distel and store in
`try-erl-complete-candidates-cache'."
  (let ((state (erl-async-state buf)))
    (erl-receive (what state prefix buf)
        ((['rex ['ok completions]]
          (setq try-erl-complete-candidates-cache completions))
         (['rex ['error reason]]
          (message "Error: %s" reason))
         (other
          (message "Unexpected reply: %S" other))))))


;;; Setup

;;;###autoload
(defun hippie-expand-distel-setup ()
  "Add try-expand-erl-complete to `hippie-expand-try-functions-list'"
  (interactive)
  (set (make-local-variable 'hippie-expand-try-functions-list)
       hippie-expand-try-functions-list)
  (add-to-list 'hippie-expand-try-functions-list 'try-expand-erl-complete))


(provide 'hippie-expand-distel)

;;; hippie-expand-distel.el ends here
