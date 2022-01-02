;;; INCREMENTAL-READING.EL-AUTOLOADS --- 
;;
;; Author: c <c@MacBook-Pro.local>
;; Copyright © 2022, c, all rights reserved.
;; Created:  2 January 2022
;;
;;; Commentary:
;;
;;  
;;
;;; Code:



;;; incremental-reading.el-autoloads.el ends here

;;;### (autoloads nil "incremental-reading" "incremental-reading.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from incremental-reading.el

(autoload 'incremental-reading-parse-cards "incremental-reading" "\
Parse all the Anki special-blocks in the current buffer and
send them to Anki through http to the anki-connect addon." t nil)

(autoload 'incremental-reading-extract-basic "incremental-reading" "\
Extract current region into a basic anki card." t nil)

(autoload 'incremental-reading-extract-basic-no-back "incremental-reading" "\
Extract current region into a basic anki card without the back
field." t nil)

(autoload 'incremental-reading-extract-cloze "incremental-reading" "\
Extract current region into a cloze anki card." t nil)

(autoload 'incremental-reading-extract-cloze-no-back "incremental-reading" "\
Extract current region into a cloze anki card without the back
field." t nil)

(autoload 'incremental-reading-hide-properties "incremental-reading" "\
Hide all mess properties." t nil)

(autoload 'incremental-reading-show-properties "incremental-reading" "\
Show all properties." t nil)

(autoload 'incremental-reading-toggle-properties-display "incremental-reading" "\
Toggle properties hidden/shown state." t nil)

(autoload 'incremental-reading-mode "incremental-reading" "\
incremental-reading-mode

If called interactively, enable Incremental-Reading mode if ARG
is positive, and disable it if ARG is zero or negative.  If
called from Lisp, also enable the mode if ARG is omitted or nil,
and toggle it if ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "incremental-reading" '("incremental-reading")))

;;;***
