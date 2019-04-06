(defcustom org-dot-image-dir-name "dotImg"
  "Default directory name for org dot image."
  :type 'string
  )

(defcustom org-screenshot-image-dir-name "screenshotImg"
  "Default directory name for org dot image."
  :type 'string
  )

(defun org-dot-image-dir ()
  (or org-dot-image-dir-name "."))

(defun org-screenshot-image-dir ()
  (or org-screenshot-image-dir-name "."))

(defun select-or-enter-file-name (img-dir)
  (ivy-read
   "please selete or enter a image name (Ctrl-n for next item, Ctrl-p for previous item)"
            (delete ".."
                    (delete "."
                            (directory-files img-dir)))))

(defun trim-space-in-string (string)
  (replace-regexp-in-string "[\t\n ]+" "" string))

(defun replace-in-the-entire-buffer (query replace subexp)
  "replace query string with the replace string in the entire buffer..
query: the string will be replaced.
replace: the string used to replace.
subexp:
  1. used in function 'replace-match'.
  2. represent 'replace' argument will be implemented in which one match group."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward query nil t)
      (replace-match replace t nil nil subexp))))

(defun jump-to-penultimate-line ()
  (delete-blank-lines)
  (save-current-buffer)
  (goto-char (point-max))
  (newline-and-indent)
  (delete-blank-lines)
  (previous-line 1))

(defun tobase64 (file-full-path)
  (base64-encode-string
   (with-temp-buffer
     (insert-file-contents file-full-path)
     (buffer-string))))

(defun image-to-base64-handler (file-full-path)
  "Encode the image file to base64 string.
file-full-path: the full path of image which will be converted."
  (progn
    (setq result (trim-space-in-string (format "data:image/png;base64,%s"
                                               (tobase64 file-full-path))))
    (jump-to-penultimate-line)
    (insert (concat "[" (file-relative-name file-full-path default-directory) "]:" result ))
    (newline-and-indent)))

(defun image-to-base64-converter (file-full-path)
  (interactive)
  (image-to-base64-handler file-full-path))

(defun org-image-to-base64-converter ()
  (interactive)
  (let* ((img-dir (if (file-directory-p org-screenshot-image-dir-name)
                      (org-screenshot-image-dir)
                    (org-dot-image-dir))))
    (progn
      (setq temp-name (select-or-enter-file-name img-dir))
      (setq full-file-path (concat default-directory img-dir "/" temp-name))
      (image-to-base64-handler full-file-path))))

(defun org-download-images-to-base64-converter ()
  (interactive)
  (cl-loop for image-file in (delete ".." (delete "." (directory-files (concat default-directory "screenshotImg"))))
           do (progn
                (setq full-file-path (concat default-directory "screenshotImg" "/" image-file))
                (image-to-base64-handler full-file-path))))

(defun org-screenshot ()
  "Take a screenshot into a user specified file in the current
buffer file directory and insert a link to this file."
  (interactive)
  (let* ((img-dir org-screenshot-image-dir-name))
    (progn
      (if (file-exists-p img-dir)
          (print (format "Screnshot image directory: '%s' already exists." img-dir))
        (mkdir img-dir))
      ;; 统一将截图和下载图片存放的文件夹, 为以文件的同一目录下的'screenshotImg'文件夹.
      ;; (setq absolute-img-dir (concat default-directory img-dir))
      ;; (let ((temp-name (select-or-enter-file-name absolute-img-dir)))
      (let ((temp-name (select-or-enter-file-name img-dir)))
        (setq name-base (file-name-base temp-name))
        (setq file-name (concat name-base ".png"))
        ;; (setq absolute-full-file-path (concat absolute-img-dir "/" file-name))
        (setq full-file-path (concat img-dir "/" file-name))
        (call-process-shell-command "screencapture" nil nil nil nil "-i"
                                    (concat "\"" full-file-path "\"" ))
        (insert (concat "[[file:" full-file-path "]]"))
        (image-to-base64-converter full-file-path)))))

(defun find-org-link-begin-and-end (plist string)
  "Find link from plist whose link is equal to string, return a
list just like '((name begin-position end-position))'"
  (let ((return-list '()))
    (progn
      (while plist
        (progn
          (if (string-equal (car (car plist))
                            string)
              (add-to-list 'return-list (cdr (car plist))))
          (setq plist (cdr plist))))
      return-list)))

(defun do-delete-link-function (be-list)
  "goto the begining of link and delete it, be-list is a list
just like '((name begin-position end-position))'"
  (while be-list
    (progn
      (goto-char (car (car be-list)))
      (delete-char (- (car (cdr (car be-list)))
                      (car (car be-list))))
      (setq be-list (cdr be-list)))))

(defun delete-image-file-and-link (img-dir)
  (interactive)
  (let* ((link-list (org-element-map (org-element-parse-buffer) 'link
                      (lambda (link)
                        (when (string= (org-element-property :type link) "file")
                          (list (org-element-property :path link)
                                (org-element-property :begin link)
                                (org-element-property :end link))))))
         (absolute-img-dir (concat default-directory img-dir))
         (temp-name (ivy-read "please selete a image name you want to delete"
                              (delete ".."
                                      (delete "." (directory-files img-dir)))))
         (file-full-path (concat absolute-img-dir "/" temp-name))
         (begin-end-list (find-org-link-begin-and-end link-list file-full-path)))
    (progn
      (if (yes-or-no-p "Do you want to delete the image links?")
        (do-delete-link-function begin-end-list))
      (if (yes-or-no-p
           "Do you really want to delete the image file? This can't be revert!!")
        (progn
          (delete-file file-full-path)
          (replace-in-the-entire-buffer
            (concat "\\[" file-full-path "\\]\.*") "" nil))))))

(defun org-delete-screenshot-image-file-and-link ()
  (interactive)
  (delete-image-file-and-link org-screenshot-image-dir-name))

(defun org-delete-image-file-and-link ()
  (interactive)
  (delete-image-file-and-link (or org-screenshot-image-dir-name org-dot-image-dir-name)))

(defun replace-symbols-dollar-and-times ()
  "Used in markdown file , replace unexpected symbols:
           '\(' -> '$'
           '\)' -> '$'
       '\times' -> '×'
 '![imag](...)' -> '![img][...]\r' <- '\r' : new line
   '```comment' -> ''
           '\`' -> ''
"
  (interactive)
  (replace-in-the-entire-buffer "\\\\(" " $" nil)
  (replace-in-the-entire-buffer "\\\\)" "$ " nil)
  (replace-in-the-entire-buffer "\\\\times" "×" nil)
  (replace-in-the-entire-buffer "!\\[img\\]\\((\\)\.*" "[" 1)
  (replace-in-the-entire-buffer "!\\[img\\]\.*?\\()\\)\.*?" "]\r" 1)
  (replace-in-the-entire-buffer "```comment" "" nil)
  (replace-in-the-entire-buffer "\\\\`" "`" nil))

(defun create-graphviz ()
  (interactive)
  (let* ((img-dir org-dot-image-dir-name))
    (setq absolute-img-dir (concat default-directory img-dir))
    (progn
      (if (file-exists-p img-dir)
          (print (format "Org dot image directory: '%s' already exists." img-dir))
        (mkdir img-dir))
      (setq temp-name (select-or-enter-file-name absolute-img-dir))
      (setq name-base (file-name-base temp-name))
      (setq file-name (concat name-base ".png"))
      (setq file-full-path (concat absolute-img-dir "/" file-name))
      (setq graph-name name-base)
      (insert (format "#+name: %s-node-table\n" graph-name))
      (insert "| *node*     | *label*        | *shape* | *fillcolor* | *fontcolor* | *fontsize(数字必须是字符串格式)* |
|------------+----------------+---------+-------------|-------------|------------|\n")
      (newline-and-indent)
      (insert (format "#+name: %s-graph\n" graph-name))
      (insert "| *from*       | *to*       | *label* | *fontcolor* |
|------------+------------+-------|-------------|\n")
      (newline-and-indent)
      (insert (format "#+name: create-%s-from-tables\n" graph-name))
      (insert (format "#+HEADER: :var nodes=%s-node-table graph=%s-graph\n"
                      graph-name graph-name))
      (insert "#+BEGIN_SRC emacs-lisp :results output :exports none\n")
      (insert "#+END_SRC\n")
      (newline-and-indent)
      (insert (format "#+BEGIN_SRC dot :file %s :var input=create-%s-from-tables :exports results\n"
                      file-full-path graph-name))
      (insert "digraph {
  $input
}\n")
      (insert "#+END_SRC\n")
      (previous-line 10)
      (org-edit-src-code)
      (insert "(concat
    \"//rankdir=LR;\\n\" ;; remove comment characters '//' for horizontal layout; add for vertical layout
    (mapconcat
      (lambda (x)
        (princ (replace-regexp-in-string \"\\\\\\\\vert\" \"|\" (format \"%s [label=\\\"%s\\\" shape=%s style=\\\"filled\\\" fillcolor=\\\"%s\\\" fontcolor=\\\"%s\\\" fontsize=\\\"%s\\\"];\\n\"
                          (car x)
                          (nth 1 x)
                          (if (string= \"\" (nth 2 x)) \"box\" (nth 2 x))
                          (if (string= \"\" (nth 3 x)) \"none\" (nth 3 x))
                          (if (string= \"\" (nth 4 x)) \"black\" (nth 4 x))
                          (if (string= \"\" (nth 5 x)) \"18\" (nth 5 x))
                          )))) nodes \"\\n\")
    \"\\n\"
    (mapconcat
    (lambda (x)
      (princ (replace-regexp-in-string \"\\\\\\\\vert\" \"|\" (format \"%s -> %s [taillabel=\\\"%s\\\" fontcolor=\\\"%s\\\"];\\n\"
              (car x) (nth 1 x) (nth 2 x) (nth 3 x))))) graph \"\\n\"))")
      (org-edit-src-exit)
  )))

(defun czqhurricane/org-insert-src-block (src-code-type)
  "Insert a 'SRC-CODE-TYPE' type source code block in org-mode."
  (interactive
    (let ((src-code-types
          '("ipython" "example" "value" "org-download" "emacs-lisp" "python" "comment" "C" "sh" "java" "js" "clojure" "C++" "css"
            "calc" "asymptote" "dot" "gnuplot" "ledger" "lilypond" "mscgen"
            "octave" "oz" "plantuml" "R" "sass" "screen" "sql" "awk" "ditaa"
            "haskell" "latex" "lisp" "matlab" "ocaml" "perl" "ruby"
            "scheme" "sqlite" "graphviz")))
      (list (ido-completing-read "Source code type: " src-code-types))))
  (catch 'return-catch
  (progn
    (setq region-active-flag nil)
    (if (region-active-p)
        (progn
          (clipboard-kill-region (region-beginning) (region-end))
          (setq region-active-flag t)))
    (newline-and-indent)
      (cond ((equal src-code-type "ipython")
              (insert (format "#+BEGIN_SRC %s :preamble # -*- coding: utf-8 -*- :results raw drawer output list :exports both :session\n" src-code-type)))
            ((equal src-code-type "example")
              (insert "#+BEGIN_SRC python :preamble # -*- coding: utf-8 -*- :results raw drawer output list :exports both :session example\n"))
            ((equal src-code-type "value")
              (insert "#+BEGIN_SRC python :preamble # -*- coding: utf-8 -*- :results raw drawer values list :exports both :session\n"))
            ((equal src-code-type "js")
             (insert (format "#+BEGIN_SRC %s :results values list :exports both\n" src-code-type)))
            ((equal src-code-type "C")
              (insert "#+header: :cmdline :includes <stdio.h> \"/Users/c/Unix/error_function.c\" \"/Users/c/Unix/get_num.c\"\n")
              (insert (format "#+BEGIN_SRC %s :results output list :exports both\n" src-code-type)))
            ((equal src-code-type "sql")
              (insert (format "#+BEGIN_SRC %s :results value table :exports both\n" src-code-type)))
            ((equal src-code-type "dot")
              (insert (format "#+BEGIN_SRC %s :file /Users/c/dotimg/example.png :cmdline -Kdot -Tpng\n" src-code-type)))
            ((equal src-code-type "org-download")
             (progn
               (goto-char (point-min))
               (insert "# -*- eval: (setq org-download-image-dir (concat default-directory \"/screenshotImg\")); -*-\n")
               (save-buffer)
               (spacemacs/kill-this-buffer)
               (reopen-killed-file))
             (throw 'return-catch "I will not going any where else."))
            ((equal src-code-type "graphviz")
              (create-graphviz)
            (throw 'return-catch "I will not going any where else."))
            (t (insert (format "#+BEGIN_SRC %s :results valuse list :exports both\n" src-code-type))))
    (newline-and-indent)
    (insert "#+END_SRC\n")
    (previous-line 2)
    (org-edit-src-code)
    (if region-active-flag
        (clipboard-yank)))))

(defun org-gfm-export-to-markdown-filter ()
  (interactive)
  (progn
    (org-open-file (org-gfm-export-to-markdown))
    (end-of-buffer)
    (previous-line 1)
    (kill-line)
    (replace-symbols-dollar-and-times)
    (save-buffer)
    (kill-buffer-and-window))
)

(defun save-buffer-filter ()
  "Replace the expected charaters except 'funcs.el<czqhurricane-org>' file."
  (interactive)
  (save-buffer)
  (and (not (string-equal (buffer-file-name) "funcs.el<czqhurricane-org>"))
      (progn
          (replace-in-the-entire-buffer "，" "," nil)
          (replace-in-the-entire-buffer "。" "." nil)
          (replace-in-the-entire-buffer "（" "(" nil)
          (replace-in-the-entire-buffer "）" ")" nil))))

(defun is-useless-buffer (buffer-to-be-inspected useless-buffer-name)
  "Check is the buffer useless one.
'(= ?* (aref name 0))' <- check whether the first character of string name
is '*'or not?
if yes, then compara the 'useless-buffer-name' with the name of current buffer."
  (let ((name (buffer-name buffer-to-be-inspected)))
    (and (= ?* (aref name 0))
         (string-equal useless-buffer-name name))))

(defun kill-buffer-without-confirmation (buffer)
  "'kill-buffer' references the varibale 'kill-buffer-query-functions',
remove the expected function from the relevant varibale."
  (interactive)
  (let ((buffer-modified-p nil))
    (setq kill-buffer-query-functions
          (delq 'process-kill-buffer-query-function
                kill-buffer-query-functions))
    (kill-buffer buffer)))

(defun kill-useless-buffer (useless-buffer-name)
  "'(require 'cl)' brings in Emacs's Common Lisp Package,
which is where the 'loop' macro lives."
  (require 'cl)
  (interactive)
  (loop for buffer being the buffers
        do (and (is-useless-buffer buffer useless-buffer-name)
                (kill-buffer-without-confirmation buffer))))

(defun org-venv-workon ()
  "Kill the existing Python buffer,so make the new virtual envirnment take effect,
should only be used in org-mode."
  (interactive)
  (ignore-errors
    (progn
      (kill-useless-buffer "*Python*")
      (kill-useless-buffer "*ob-ipython-out*")
      (kill-useless-buffer "*ob-ipython-kernel-default*")))
  (venv-workon))

(defun dos2unix ()
  "Replace DOS eolns CR LF with Unix eolns CR"
  (interactive)
  (goto-char (point-min))
  (while (search-forward "\r" nil t) (replace-match "")))

(defvar killed-file-list nil
  "List of recently killed files.")

(defun add-file-to-killed-file-list ()
  "If buffer is associated with a file name, add that file to the
'killed-file-list' when killing the buffer."
  (when buffer-file-name
    (push buffer-file-name killed-file-list)))

(add-hook 'kill-buffer-hook #'add-file-to-killed-file-list)

(defun reopen-killed-file ()
  "Reopen the most recently killed file, if one exists."
  (when killed-file-list
    (find-file (pop killed-file-list))))

(defun czqhurricane/filter-by-tags ()
  (let ((head-tags (org-get-tags-at)))
    (member current-tag head-tags)))

(defun czqhurricane/org-clock-sum-today-by-tags (timerange &optional tstart tend noinsert)
  (interactive "P")
  (let* ((timerange-numeric-value (prefix-numeric-value timerange))
         (files (org-add-archive-files (org-agenda-files)))
         (include-tags '("TODO" "NEXT" "DONE" "MEETING" "WAITING"
                         ))
         (tags-time-alist (mapcar (lambda (tag) `(,tag . 0)) include-tags))
         (output-string "")
         (tstart (or tstart
                     (and timerange (equal timerange-numeric-value 4) (- (org-time-today) 86400))
                     (and timerange (equal timerange-numeric-value 16) (org-read-date nil nil nil "Start Date/Time:"))
                     (org-time-today)))
         (tend (or tend
                   (and timerange (equal timerange-numeric-value 16) (org-read-date nil nil nil "End Date/Time:"))
                   (+ tstart 86400)))
         h m file item prompt donesomething)
    (while (setq file (pop files))
      (setq org-agenda-buffer (if (file-exists-p file)
                                  (org-get-agenda-file-buffer file)
                                (error "No such file %s" file)))
      (with-current-buffer org-agenda-buffer
        (dolist (current-tag include-tags)
          (org-clock-sum tstart tend 'czqhurricane/filter-by-tags)
          (setcdr (assoc current-tag tags-time-alist)
                  (+ org-clock-file-total-minutes (cdr (assoc current-tag tags-time-alist)))))))
    (while (setq item (pop tags-time-alist))
      (unless (equal (cdr item) 0)
        (setq donesomething t)
        (setq h (/ (cdr item) 60)
              m (- (cdr item) (* 60 h)))
        (setq output-string (concat output-string (format "[-%s-] %.2d:%.2d\n" (car item) h m)))))
    (unless donesomething
      (setq output-string (concat output-string "[-Nothing-] Done nothing!!!\n")))
    (unless noinsert
      (insert output-string))
    output-string))

(defun czqhurricane/notify-osx (title message)
  (call-process "terminal-notifier"
                nil 0 nil
                "-group" "Emacs"
                "-title" title
                "-sender" "org.gnu.Emacs"
                "-message" message
                "-activate" "oeg.gnu.Emacs"
                "-sound" "default"))

(defun image-save-to-file (file-name)
  "Save the image under point."
  (let ((image (get-text-property (point) 'display)))
    (when (or (not (consp image))
              (not (eq (car image) 'image)))
      (error "No image under point"))
    (with-temp-buffer
      (let ((file (plist-get (cdr image) :file)))
        (if file
            (if (not (file-exists-p file))
                (error "File %s no longer exists" file)
              (insert-file-contents-literally file))
          (insert (plist-get (cdr image) :data))))
      (write-region (point-min) (point-max)
                    file-name))))

;; {{
;; @see: https://github.com/JamieMason/ImageOptim-CLI#installation
;; $ brew update
;; $ brew install imageoptim-cli
;; @see: https://imageoptim.com/mac
;; Download & install
(defun org-image-save ()
  (interactive)
  (let* ((img-dir org-screenshot-image-dir-name))
    (progn
      (if (file-exists-p img-dir)
          (print (format "Screnshot image directory: '%s' already exists." img-dir))
        (mkdir img-dir))
      (let ((temp-name (select-or-enter-file-name img-dir)))
        (setq absolute-img-dir (concat default-directory img-dir))
        (setq name-base (file-name-base temp-name))
        (setq file-name (concat name-base ".png"))
        (setq full-file-path (concat img-dir "/" file-name))
        (image-save-to-file full-file-path)
        (setq absolute-full-file-path (concat absolute-img-dir "/" file-name))
        (defun callback-imageoptim()
          (let* ((cmd (format "imageoptim --imagealpha %s" absolute-full-file-path)))
            (do-applescript
             (format
              "
  tell application \"iTerm2\"
       activate
       set _session to current session of current window
       tell _session
            set command to get the clipboard
            write text \"%s\"
       end tell
  end tell
  tell application \"emacs\"
       activate
  end tell
  " cmd))))
        (install-monitor-file-exists absolute-full-file-path 1 #'callback-imageoptim)
        (insert (concat "[[file:" full-file-path "]]"))
        ))))
;; }}
