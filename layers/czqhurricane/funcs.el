(defun occur-dwin ()
  "Call 'occur' with a same default."
  (interactive)
  (push (if (region-active-p)
            (buffer-substring-no-properties
             (region-beginning)
             (region-end))
          (let ((sym (thing-at-point 'symbol)))
            (when (stringp sym)
              (regexp-quote sym))))
        regexp-history)
  (call-interactively 'occur))

(defun czqhurricane/vcs-project-root ()
  "Return the project root for current buffer."
  (let ((directory default-directory))
    (locate-dominating-file directory ".git")))

(defun czqhurricane/open-file-with-projectile-or-counsel-git ()
  "vcs: version control system."
  (interactive)
  (if (czqhurricane/vcs-project-root)
      (counsel-git)
    (if (projectile-project-p)
        (projectile-find-file)
      (ido-find-file))))

(defcustom org-screenshot-image-dir-name  "screenshotImg"
  "Default directory name for org screenshot image."
  :type 'string
  )

(defcustom org-dot-image-dir-name "dotImg"
  "Default directory name for org dot image." 
  :type 'string
  )

(defun tobase64 (file-full-path)
  (base64-encode-string
   (with-temp-buffer
     (insert-file-contents file-full-path)
     (buffer-string))))

(defun trim-space-in-string (string)
  (replace-regexp-in-string "[\t\n ]+" "" string))

(defun replace-in-the-entire-buffer (query replace subexp)
  "query: 所需要被替换的字符串
  replace: 用来替换的字符串
  subexp:
          1. 在函数'replace-match'中使用
          2. 用来表示'replace'参数被应用在第几个匹配到的模式组"
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward query nil t)
      (replace-match replace nil nil nil subexp))))

(defun jump-to-penultimate-line ()
  (delete-blank-lines)
  (save-current-buffer)
  (goto-char (point-max))
  (newline-and-indent)
  (delete-blank-lines)
  (previous-line 1))

(defun image-to-base64-converter (file-full-path)
  "file-full-path: the full path of image which will be converted."
  (interactive)
  (progn
    (setq result (trim-space-in-string (format "data:image/png;base64,%s" (tobase64 file-full-path))))
    (jump-to-penultimate-line) 
    (insert (concat "[" file-full-path "]:" result ))
    (newline-and-indent)))

(defun select-or-enter-file-name (img-dir)
  (ivy-read "please selete or enter a image name (Ctrl-n for next item, Ctrl-p for previous item)"
            (delete ".."
                    (delete "."
                            (directory-files img-dir)))))

(defun org-dot-image-to-base64-converter ()
  (interactive)
  (let* ((img-dir org-dot-image-dir-name))
    (progn
    (setq temp-name (file-name-base (select-or-enter-file-name img-dir)))
    (setq full-file-path (concat default-directory img-dir "/" temp-name ".png"))
    (image-to-base64-converter full-file-path))))

(defun org-screenshot ()
  "Take a screenshot into a user specified file in the current
  buffer file directory and insert a link to this file."
  (interactive)
  (let* ((img-dir org-screenshot-image-dir-name))
    (progn
      (if (file-exists-p img-dir)
          (print (format "yes directory: '%s' exists" img-dir))
        (mkdir img-dir))
      (setq absolute-img-dir (concat default-directory img-dir))
      (let ((temp-name (select-or-enter-file-name absolute-img-dir)))
        (setq name-base (file-name-base temp-name))
        (setq file-name (concat name-base ".png"))
        (setq file-full-path (concat absolute-img-dir "/" file-name))
        (call-process-shell-command "screencapture" nil nil nil nil "-i" (concat
                                                                          "\"" file-full-path "\"" ))
        (insert (concat "[[" file-full-path "]]"))
        (image-to-base64-converter file-full-path)))))

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
                              (delete ".." (delete "." (directory-files img-dir)))))
         (file-full-path (concat absolute-img-dir "/" temp-name))
         (begin-end-list (find-org-link-begin-and-end link-list file-full-path)))
    (progn
      (if (yes-or-no-p "Do you want to delete the image links?")
          (do-delete-link-function begin-end-list))
      (if (yes-or-no-p "Do you really want to delete the image file? This can't be revert!!")
          (progn 
                 (delete-file file-full-path)
                 (replace-in-the-entire-buffer (concat "\\[" file-full-path "\\]\.*") "" nil))))))

(defun org-delete-screenshot-image-file-and-link ()
  (interactive)
  (delete-image-file-and-link org-screenshot-image-dir-name))

(defun org-delete-dot-image-file-and-link ()
  (interactive)
  (delete-image-file-and-link org-dot-image-dir-name))

(defun replace-symbols-dollar-and-times ()
  "Used in markdown file:
                         '\(' -> '$'
                         '\)' -> '$'
                         '\times' -> '×'
                         '![imag](...)' -> '![img][...]\r' <- '\r':new line
                         `'''comment` -> ``
                         `'''` ->``
  "
  (interactive)
  (replace-in-the-entire-buffer "\\\\(" " $" nil)
  (replace-in-the-entire-buffer "\\\\)" "$ " nil)
  (replace-in-the-entire-buffer "\\\\times" "×" nil)
  (replace-in-the-entire-buffer "!\\[img\\]\\((\\)\.*" "[" 1)
  (replace-in-the-entire-buffer "!\\[img\\]\.*?\\()\\)\.*?" "]\r" 1)
  (replace-in-the-entire-buffer "```comment" "" nil))

(defun create-graphviz ()
  (interactive)
  (let* ((img-dir org-dot-image-dir-name))
    (setq absolute-img-dir (concat default-directory img-dir))
    (progn
      (if (file-exists-p img-dir)
          (print (format "yes directory: '%s' exists" img-dir))
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
      (insert (format "#+BEGIN_SRC dot :file %s :var input=create-%s-from-tables :exports results\n" file-full-path graph-name))
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

(defun org-insert-src-block (src-code-type)
  "Insert a 'SRC-CODE-TYPE' type source code block in org-mode."
  (interactive
    (let ((src-code-types
          '("ipython" "emacs-lisp" "python" "comment" "C" "sh" "java" "js" "clojure" "C++" "css"
            "calc" "asymptote" "dot" "gnuplot" "ledger" "lilypond" "mscgen"
            "octave" "oz" "plantuml" "R" "sass" "screen" "sql" "awk" "ditaa"
            "haskell" "latex" "lisp" "matlab" "ocaml" "org" "perl" "ruby"
            "scheme" "sqlite" "graphviz")))
      (list (ido-completing-read "Source code type: " src-code-types))))
  (catch 'return-catch
  (progn
    (newline-and-indent)
      (cond ((equal src-code-type "ipython")
              (insert (format "#+BEGIN_SRC %s :preamble # -*- coding: utf-8 -*- :results raw drawer output list :exports both :session\n" src-code-type)))
            ((equal src-code-type "example")
              (insert "#+BEGIN_SRC ipython :preamble # -*- coding: utf-8 -*- :results raw drawer output list :exports both :session example\n"))
            ((equal src-code-type "value")
              (insert "#+BEGIN_SRC ipython :preamble # -*- coding: utf-8 -*- :results raw drawer value list :exports both :session\n" src-code-type))
            ((equal src-code-type "js")
             (insert (format "#+BEGIN_SRC %s :results values list :exports both\n" src-code-type)))
            ((equal src-code-type "C")
              (insert "#+header: :cmdline :includes <stdio.h> \"/Users/c/Unix/error_function.c\" \"/Users/c/Unix/get_num.c\"\n")
              (insert (format "#+BEGIN_SRC %s :results output list :exports both\n" src-code-type)))
            ((equal src-code-type "sql")
              (insert (format "#+BEGIN_SRC %s :results value table :exports both\n" src-code-type)))
            ((equal src-code-type "dot")
              (insert (format "#+BEGIN_SRC %s :file /Users/c/dotimg/example.png :cmdline -Kdot -Tpng\n" src-code-type)))
            ((equal src-code-type "graphviz")
              (create-graphviz)
            (throw 'return-catch "I will not going any where else"))
            (t (insert (format "#+BEGIN_SRC %s\n" src-code-type))))
    (newline-and-indent)
    (insert "#+END_SRC\n")
    (previous-line 2)
    (org-edit-src-code))))

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
  "Replace the expected charaters except 'funcs.el' file."
  (interactive)
  (save-buffer)
  (and (not (string-equal (buffer-name) "funcs.el"))
    (progn
      (replace-in-the-entire-buffer "，" "," nil)
      (replace-in-the-entire-buffer "。" "." nil)
      (replace-in-the-entire-buffer "（" "(" nil)
      (replace-in-the-entire-buffer "）" ")" nil))))

(defun is-useless-buffer (buffer-to-be-inspected useless-buffer-name)
  "Check is the buffer useless one.
  '(= ?* (aref name 0))' <- check whether the first character of string name is '*'
   or not? if yes, then compara the 'useless-buffer-name' with the name of current buffer.
  "
  (let ((name (buffer-name buffer-to-be-inspected)))
    (and (= ?* (aref name 0))
         (string-equal useless-buffer-name name))))

(defun kill-buffer-without-confirmation (buffer)
  "'kill-buffer' references the varibale 'kill-buffer-query-functions',
  Remove the expected function form the relevant varibale."
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
        do (and (is-useless-buffer buffer useless-buffer-name) (kill-buffer-without-confirmation buffer))))

(defun org-venv-workon ()
  "Kill the existing Python buffer,so make the new virtual envirnment take effect,
  should only be used in org-mode."
  (interactive)
  (ignore-errors
   (kill-useless-buffer "*Python*"))
  (venv-workon))
