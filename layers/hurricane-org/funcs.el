(defvar killed-file-list nil
  "List of recently killed files.")

(defcustom org-dot-image-dir-name "dotImg"
  "Default directory name for org dot image."
  :type 'string)

(defcustom org-screenshot-image-dir-name "./static"
  "Default directory name for org dot image."
  :type 'string)

(setq buffer-replace-string-rule-lists '(("，" . ",")
                                         ("。" . "\\.")
                                         ("：" . ":")
                                         ("！" . "!")
                                         ("？" . "?")
                                         ("【" . "\\[")
                                         ("】" . "\\]")
                                         ("（" . "(")
                                         ("）" . ")")
                                         ("％" . "%")
                                         ("＃" . "#")
                                         ("＠" . "@")
                                         ("＆" . "&")
                                         ;; ("１" . "1")
                                         ;; ("２" . "2")
                                         ;; ("３" . "3")
                                         ;; ("４" . "4")
                                         ;; ("５" . "5")
                                         ;; ("６" . "6")
                                         ;; ("７" . "7")
                                         ;; ("８" . "8")
                                         ;; ("９" . "9")
                                         ;; ("０" . "0")
                                         ;; ;; ("、" . ",")
                                         ;; ("；" . ",")
                                         ("“" . "\"")
                                         ;; ("”" . "\"")
                                         ("：" . ":")
                                         ("“" . "`")
                                         ("；" . ";")))

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

(defun replace-region-or-buffer (query replace subexp)
  "Replace query string with the replace string in the region or entire buffer.
'QUERY': the string will be replaced.
'REPLACE': the string used to replace.
'SUBEXP':
1. Used in function `replace-match'.
2. Represent 'replace' argument will be implemented in which one match group."
  (progn
    (setq start (point-min))
    (setq end (point-max))

    (when (use-region-p)
      (setq start (region-beginning))
      (setq end (region-end)))

    (save-excursion
      (goto-char start)
      (while (re-search-forward query end t)
      (replace-match replace t nil nil subexp)))))

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
`file-full-path': the full path of image which will be converted."
  (progn
    (setq result (trim-space-in-string (format "data:image/png;base64,%s"
                                               (tobase64 file-full-path))))
    (jump-to-penultimate-line)
    (insert (concat "[" (file-relative-name file-full-path default-directory) "]:" result ))
    (newline-and-indent)))

(defun image-to-base64-converter (file-full-path)
  (interactive)
  (image-to-base64-handler file-full-path))

(defun hurricane/org-image-to-base64-converter ()
  (interactive)
  (let* ((img-dir (if (file-directory-p org-screenshot-image-dir-name)
                      (org-screenshot-image-dir)
                    (org-dot-image-dir))))
    (progn
      (setq temp-name (select-or-enter-file-name img-dir))
      (setq full-file-path (concat default-directory img-dir "/" temp-name))
      (image-to-base64-handler full-file-path))))

(defun hurricane/org-download-images-to-base64-converter ()
  (interactive)
  (cl-loop for image-file in (delete ".." (delete "." (directory-files (concat default-directory "./static"))))
           do (progn
                (setq full-file-path (concat default-directory "./static" "/" image-file))
                (image-to-base64-handler full-file-path))))

(defun hurricane/org-screenshot ()
  "Take a screenshot into a user specified file in the current
buffer file directory and insert a link to this file."
  (interactive)
  (let* ((img-dir org-screenshot-image-dir-name))
    (progn
      (if (file-exists-p img-dir)
          (print (format "Screenshot image directory: '%s' already exists." img-dir))
        (mkdir img-dir))
      ;; 统一将截图和下载图片存放的文件夹, 为以文件的同一目录下的'./static'文件夹.
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

(defun hurricane//find-org-link-begin-and-end (plist string)
  "Find link from plist whose link is equal to STRING, return a
list just like `((name begin-position end-position))'"
  (let ((return-list '()))
    (progn
      (while plist
        (progn
          (if (string-equal (car (car plist))
                            string)
              (add-to-list 'return-list (cdr (car plist))))
          (setq plist (cdr plist))))
      return-list)))

(defun hurricane//do-delete-link-function (be-list)
  "Goto the begining of link and delete it, BE-LIST is a list
just like `((name begin-position end-position))'"
  (while be-list
    (progn
      (goto-char (car (car be-list)))
      (delete-char (- (car (cdr (car be-list)))
                      (car (car be-list))))
      (setq be-list (cdr be-list)))))

(defun hurricane//delete-image-file-and-link (img-dir)
  (let* ((relative-img-dir (concat img-dir "/" (file-name-sans-extension (buffer-name)) "/"))
         (link-list (org-element-map (org-element-parse-buffer) 'link
                      (lambda (link)
                        (when (string= (org-element-property :type link) "file")
                          (list (org-element-property :path link)
                                (org-element-property :begin link)
                                (org-element-property :end link))))))
         (absolute-img-dir (concat default-directory relative-img-dir "/"))
         (temp-name (ivy-read "Please select an image you want to delete."
                              (delete ".."
                                      (delete "." (directory-files relative-img-dir)))))
         (file-full-path (concat absolute-img-dir temp-name))
         (begin-end-list (hurricane//find-org-link-begin-and-end link-list (concat relative-img-dir temp-name))))
    (progn
      (if (yes-or-no-p "Do you want to delete the image link?")
        (hurricane//do-delete-link-function begin-end-list))
      (if (yes-or-no-p
           "Do you really want to delete the image file? This can't be revert!")
        (progn
          (delete-file file-full-path)
          )))))

(defun hurricane/org-delete-screenshot-image-file-and-link ()
  (interactive)
  (hurricane//delete-image-file-and-link org-screenshot-image-dir-name))

(defun hurricane/org-delete-image-file-and-link ()
  (interactive)
  (hurricane//delete-image-file-and-link (or org-screenshot-image-dir-name org-dot-image-dir-name)))

(defun hurricane/create-graphviz ()
  (interactive)
  (let* ((img-dir org-dot-image-dir-name))
    (setq absolute-img-dir (concat default-directory org-screenshot-image-dir-name "/" (file-name-base (spacemacs--file-path)) "/" img-dir "/"))
    (progn
      (if (file-exists-p absolute-img-dir)
          (message (format "Org dot image directory: '%s' already exists." img-dir))
        (mkdir absolute-img-dir))
      (setq temp-name (select-or-enter-file-name absolute-img-dir))
      (setq name-base (file-name-base temp-name))
      (setq file-name (concat name-base ".png"))
      (setq file-full-path (concat absolute-img-dir file-name))
      (setq graph-name name-base)
      (insert "#+LATEX: \\resizebox{\\textwidth}{!}{\n")
      (newline-and-indent)
      (insert (format "#+name: %s-subgraph-table\n" graph-name))
      (insert "| *cluster(必填)* | *label* | *style(默认\"none\")* | *color(默认\"black\")* | *nodestyle(默认\"none\")* | *nodecolor(默认\"black\")* | *nodeflow(必填, 以\";\"分隔)* | *kwargs(以\";\"结尾)* |
|-----------------+---------+---------------------+----------------------+-------------------------+--------------------------+-----------------------+---------------------|\n")
      (insert "#+LATEX: }")
      (newline-and-indent)
      (insert "#+LATEX: \\resizebox{\\textwidth}{!}{\n")
      (insert (format "#+name: %s-node-table\n" graph-name))
      (insert "| *node(必填)* | *label* | *shape(默认\"box\")* | *style(可选\"filled\")* | *fillcolor(默认\"none\")* | *fontcolor(默认\"black\")* | *fontsize(默认\"18\",数字必须是字符串格式)* | *kwargs(以\",\"结尾)* |
|--------------+---------+--------------------+-----------------------+-------------------------+--------------------------+-------------------------------------------+---------------------------|\n")
      (insert "#+LATEX: }")
      (newline-and-indent)
      (insert "#+LATEX: \\resizebox{\\textwidth}{!}{\n")
      (insert (format "#+name: %s-graph-table\n" graph-name))
      (insert "| *from* | *to* | *label* | *style(默认\"bold\",可选\"dotted\") | *color(默认\"black\")* | *fontcolor* | *tailport(可选\"n\",\"ne\",\"e\",\"se\",\"sw\",\"w\",\"nw\")* | *lhead(为子图的名称即 cluster 列的值)* | *ltail(为子图的名称即 cluster 列的值)* | *kwargs(以\" \"结尾)* |
|--------+------+---------+---------------------------------+----------------------+-------------+-------------------------------------------------+----------------------------------------+----------------------------------------+---------------------|\n")
      (insert "#+LATEX: }")
      (newline-and-indent)
      (insert (format "#+name: create-%s-from-tables\n" graph-name))
      (insert (format "#+HEADER: :var subgraph=%s-subgraph-table nodes=%s-node-table graph=%s-graph-table\n"
                      graph-name graph-name graph-name))
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
    (princ \"//rankdir=LR;\\n\") ;; remove comment characters '//' for horizontal layout; add for vertical layout
    (princ \"compound=true;\\n\")
    (mapconcat
      (lambda (x)
        (princ (replace-regexp-in-string \"\\\\\\\\vert\" \"|\" (format \"subgraph %s {label=\\\"%s\\\"; style=%s; color=%s; node [style=%s, color=%s]; %s %s}\n\"
                          (car x)
                          (nth 1 x)
                          (if (string= \"\" (nth 2 x)) \"none\" (nth 2 x))
                          (if (string= \"\" (nth 3 x)) \"black\" (nth 3 x))
                          (if (string= \"\" (nth 4 x)) \"none\" (nth 4 x))
                          (if (string= \"\" (nth 5 x)) \"black\" (nth 5 x))
                          (nth 6 x)
                          (nth 7 x)
                          )))) subgraph \"\n\")
    \"\\n\"
    (mapconcat
      (lambda (x)
        (princ (replace-regexp-in-string \"\\\\\\\\vert\" \"|\" (format \"%s[label=\\\"%s\\\", shape=%s, style=\\\"%s\\\", fillcolor=\\\"%s\\\", fontcolor=\\\"%s\\\", fontsize=\\\"%s\\\", %s];\\n\"
                          (car x)
                          (nth 1 x)
                          (if (string= \"\" (nth 2 x)) \"box\" (nth 2 x))
                          (if (string= \"\" (nth 3 x)) \"none\" (nth 3 x))
                          (if (string= \"\" (nth 4 x)) \"none\" (nth 4 x))
                          (if (string= \"\" (nth 5 x)) \"black\" (nth 5 x))
                          (if (string= \"\" (nth 6 x)) \"18\" (nth 6 x))
                          (nth 7 x)
                          )))) nodes \"\n\")
    \"\\n\"
    (mapconcat
    (lambda (x)
      (princ (replace-regexp-in-string \"\\\\\\\\vert\" \"|\" (format \"%s -> %s [label=\\\"%s\\\" style=%s color=%s fontcolor=\\\"%s\\\" tailport=%s lhead=%s ltail=%s %s];\\n\"
              (car x)
              (nth 1 x)
              (nth 2 x)
              (if (string= \"\" (nth 3 x)) \"bold\" (nth 3 x))
              (if (string= \"\" (nth 4 x)) \"black\" (nth 4 x))
              (if (string= \"\" (nth 5 x)) \"black\" (nth 5 x))
              (if (string= \"\" (nth 6 x)) \"none\" (nth 5 x))
              (if (string= \"\" (nth 7 x)) \"none\" (nth 6 x))
              (if (string= \"\" (nth 8 x)) \"none\" (nth 7 x))
              (nth 9 x)
              )))) graph \"\\n\"))")
      (org-edit-src-exit)
  )))

(defun hurricane/org-insert-src-block (src-code-type)
  "Insert a `src-code-type' type source code block in org-mode."
  (interactive
    (let ((src-code-types
          '("ipython" "example" "value" "emacs-lisp" "python" "comment" "C" "sh" "java" "js" "clojure" "C++" "css"
            "calc" "asymptote" "dot" "gnuplot" "ledger" "lilypond" "mscgen"
            "octave" "oz" "plantuml" "R" "sass" "screen" "sql" "awk" "ditaa"
            "haskell" "latex" "lisp" "matlab" "ocaml" "perl" "ruby"
            "scheme" "sqlite" "graphviz" "html" "call")))
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
              (insert (format "#+BEGIN_SRC %s :preamble # -*- coding: utf-8 -*- :results raw drawer output list :exports no-eval :session\n" src-code-type)))
            ((equal src-code-type "example")
              (insert "#+BEGIN_SRC python :preamble # -*- coding: utf-8 -*- :results raw drawer output list :exports no-eval :session example\n"))
            ((equal src-code-type "value")
              (insert "#+BEGIN_SRC python :preamble # -*- coding: utf-8 -*- :results raw drawer values list :exports no-eval :session\n"))
            ((equal src-code-type "js")
             (insert (format "#+BEGIN_SRC %s :results values list :exports no-eval\n" src-code-type)))
            ((equal src-code-type "C")
              (insert "#+header: :cmdline :includes <stdio.h> \"/Users/c/Unix/error_function.c\" \"/Users/c/Unix/get_num.c\"\n")
              (insert (format "#+BEGIN_SRC %s :results output list :exports no-eval\n" src-code-type)))
            ((equal src-code-type "sql")
              (insert (format "#+BEGIN_SRC %s :results value table :exports no-eval\n" src-code-type)))
            ((equal src-code-type "dot")
              (insert (format "#+BEGIN_SRC %s :file /Users/c/dotimg/example.png :cmdline -Kdot -Tpng\n" src-code-type)))
            ((equal src-code-type "call")
             (insert "#+CALL: createTree(toInclude=\"*.*\", toExclude=\"\", directory=\"\", createLink=\"true\")"))
            ((equal src-code-type "graphviz")
              (hurricane/create-graphviz)
            (throw 'return-catch "I will not going any where else."))
            (t (insert (format "#+BEGIN_SRC %s :results raw drawer values list :exports no-eval\n" src-code-type))))
    (newline-and-indent)
    (insert "#+END_SRC\n")
    (previous-line 2)
    (org-edit-src-code)
    (if region-active-flag
        (clipboard-yank)))))

(defun save-buffer-filter ()
  "Replace the expected charaters except `funcs.el<hurricane-org>' file."
  (interactive)
  (save-buffer)
  (and (not (string-equal (buffer-file-name) "/Users/c/.spacemacs.d/layers/hurricane-org/funcs.el"))
      (progn
        (dolist (replace-string-rule buffer-replace-string-rule-lists)
          (replace-region-or-buffer (cdr replace-string-rule) (car replace-string-rule) nil)))))

(defun is-useless-buffer (buffer-to-be-inspected useless-buffer-name)
  "Check is the buffer useless one.
`(= ?* (aref name 0))' <- check whether the first character of string name is '*'or not?
If yes, then compara the 'useless-buffer-name' with the name of current buffer."
  (let ((name (buffer-name buffer-to-be-inspected)))
    (and (= ?* (aref name 0))
         (string-equal useless-buffer-name name))))

(defun kill-buffer-without-confirmation (buffer)
  "`kill-buffer' references the varibale `kill-buffer-query-functions',
remove the expected function from the relevant varibale."
  (interactive)
  (let ((buffer-modified-p nil))
    (setq kill-buffer-query-functions
          (delq 'process-kill-buffer-query-function
                kill-buffer-query-functions))
    (kill-buffer buffer)))

(defun kill-useless-buffer (useless-buffer-name)
  "`(require 'cl)' brings in Emacs's Common Lisp Package,
which is where the `loop' macro lives."
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

(defun add-file-to-killed-file-list ()
  "If buffer is associated with a file name, add that file to the
`killed-file-list' when killing the buffer."
  (when buffer-file-name
    (push buffer-file-name killed-file-list)))

(add-hook 'kill-buffer-hook #'add-file-to-killed-file-list)

(defun reopen-killed-file ()
  "Reopen the most recently killed file, if one exists."
  (when killed-file-list
    (find-file (pop killed-file-list))))

(defun hurricane//filter-by-tags ()
  (let ((head-tags (org-get-tags-at)))
    (member current-tag head-tags)))

(defun hurricane/org-clock-sum-today-by-tags (timerange &optional tstart tend noinsert)
  (interactive "P")
  (let* ((timerange-numeric-value (prefix-numeric-value timerange))
         (files (org-add-archive-files (org-agenda-files)))
         (include-tags '("TODO" "NEXT" "DONE" "MEETING" "WAITING"
                         ))
         (tags-time-alist (mapcar (lambda (tag) `(,tag . 0)) include-tags))
         (output-string "")
         (tstart (or tstart
                     (and timerange (equal timerange-numeric-value 4) (- (hurricane//org-time-today) 86400))
                     (and timerange (equal timerange-numeric-value 16) (org-read-date nil nil nil "Start Date/Time:"))
                     (hurricane//org-time-today)))
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
          (org-clock-sum tstart tend 'hurricane//filter-by-tags)
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

(defun hurricane//notify-osx (title message)
  (call-process "terminal-notifier"
                nil 0 nil
                "-group" "Emacs"
                "-title" title
                "-message" message
                "-activate" "oeg.gnu.Emacs"
                "-sound" "default"))

(defun hurricane//image-save-to-file (file-name)
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
;; @see: https://pngmini.com/
;; Download & install.
(defun hurricane/org-image-save ()
  (interactive)
  (let* ((relative-img-dir (concat org-screenshot-image-dir-name "/" (file-name-sans-extension (buffer-name)) "/")))
    (progn
      (if (file-exists-p relative-img-dir)
          (print (format "Screnshot image directory: '%s' already exists." relative-img-dir))
        (mkdir relative-img-dir))
      (let ((temp-name (select-or-enter-file-name relative-img-dir)))
        (setq absolute-img-dir (concat default-directory relative-img-dir "/"))
        (setq name-base (file-name-base temp-name))
        (setq file-name (concat name-base ".png"))
        (setq full-file-path (concat relative-img-dir file-name))
        (hurricane//image-save-to-file full-file-path)
        (setq absolute-full-file-path (concat absolute-img-dir file-name))
        (defun callback-imageoptim()
          (let* ((cmd (format "imageoptim --imagealpha %s" absolute-full-file-path)))
            (eshell-command cmd)))
        ;; (install-monitor-file-exists absolute-full-file-path 1 #'callback-imageoptim)
        (insert (concat "[[file:" full-file-path "]]"))
        (evil-normal-state)))))
;; }}

(defun hurricane/org-screenshot-and-ocr ()
  "Take a screenshot into a user specified file in the current buffer file directory
and insert a link to this file."
  (interactive)
  (let* ((img-dir org-screenshot-image-dir-name))
    (progn
      (if (file-exists-p img-dir)
          (print (format "Screnshot image directory: '%s' already exists." img-dir))
        (mkdir img-dir))
      ;; 统一将截图和下载图片存放的文件夹，为以文件的同一目录下的 `./static' 文件夹。
      (setq absolute-img-dir (concat default-directory img-dir))
      ;; (let ((temp-name (select-or-enter-file-name absolute-img-dir)))
      (let ((temp-name (select-or-enter-file-name img-dir)))
        (setq name-base (file-name-base temp-name))
        (setq file-name (concat name-base ".png"))
        (setq absolute-full-file-path (concat absolute-img-dir "/" file-name))
        (setq full-file-path (concat img-dir "/" file-name))
        (call-process-shell-command "screencapture" nil nil nil nil "-i"
                                    (concat "\"" full-file-path "\"" ))
        (defun callback-BaiduOcr()
          (let* ((cmd (format "python %s \"%s\"" Baidu-OCR-Python-file absolute-full-file-path)))
            (eshell-command "workon ipy3")
            (eshell-command cmd)))
        (install-monitor-file-exists absolute-full-file-path 1 #'callback-BaiduOcr)))))

(defun hurricane/org-insert-caption-and-target ()
  (interactive)
  (let* ((current-symbol (hurricane//pointer-string))
         (input-string
          (string-trim
           (read-string
            (format "Org-insert-caption-and-target Search (%s): " current-symbol)
            nil
            ))))
    (when (string-blank-p input-string)
      (setq input-string current-symbol))
    (insert (concat "#+CAPTION: " input-string))
    (newline-and-indent)
    (insert (format "<<%s>>" input-string))))

(defun hurricane/save-and-publish-website()
  "Save all buffers and publish."
  (interactive)
  (when (yes-or-no-p "Really save and publish current project?")
    (save-some-buffers t)
    (org-publish-project "website" t)
    (message "Site published done.")))

(defun hurricane/save-and-publish-statics ()
  "Just copy statics like js, css, and image file .etc."
  (interactive)
  (org-publish-project "statics" t)
  (message "Copy statics done."))

(defun hurricane/save-and-publish-file ()
  "Save current buffer and publish."
  (interactive)
  (save-buffer t)
  ;; (blog-site-project-setup)
  (org-publish-current-file t))

(defun hurricane/delete-org-and-html ()
  "Delete current org and the relative html when it exists."
  (interactive)
  (when (yes-or-no-p "Really delete current org and the relative html?")
    (let ((fileurl (concat "~/site/public/" (file-name-base (buffer-name)) ".html")))
      (if (file-exists-p fileurl)
          (delete-file fileurl))
      (delete-file (buffer-file-name))
      (kill-this-buffer)
      (message "Delete org and the relative html done."))))

(defun hurricane/just-delete-relative-html ()
  "Just delete the relative html when it exists."
  (interactive)
  (when (yes-or-no-p "Really delete the relative html?")
    (let ((fileurl (concat "~/site/public/" (file-name-base (buffer-name)) ".html")))
      (if (file-exists-p fileurl)
          (progn
            (delete-file fileurl)
            (message "Delete the relative html done.")
            )
        (message "None relative html.")))))

(defun hurricane/preview-current-buffer-in-browser ()
  "Open current buffer as html."
  (interactive)
  (let ((fileurl (concat "http://127.0.0.1:8080/" (file-relative-name (file-name-directory buffer-file-name) (concat deft-dir "notes/")) (file-name-base (buffer-file-name)) ".html")))
    (hurricane/save-and-publish-file)
    (unless (httpd-running-p) (httpd-start))
    (browse-url fileurl)))

(defun hurricane/blog-site-project-setup ()
  (interactive)
  (set (make-local-variable 'org-publish-project-alist)
       `(("orgfiles"
         ;; Sources and destinations for files.
         ;; local org files directory.
         :base-directory ,(concat deft-dir (file-name-as-directory "notes"))
         :publishing-directory ,(concat deft-dir (file-name-as-directory "public"))
         ;; :preparation-function
         ;; :complete-function

         ;; Selecting files.
         :base-extension "org"
         ;; :exclude "PrivatePage.org"
         ;; :include
         :recursive t

         ;; Publishing action.
         :publishing-function org-html-publish-to-html
         :htmlized-source nil

         ;; Options for the exporters.

         ;; {{
         ;; Generic properties.
         ;; :archived-trees org-export-with-archived-trees
         ;; :exclude-tags org-export-exclude-tags
         ;; org-export-headline-levels.
         :headline-levels 4
         ;; :language org-export-default-language
         ;; :preserve-breaks org-export-preserve-breaks
         ;; org-export-with-section-numbers.
         :section-numbers nil
         ;; :select-tags org-export-select-tags
         ;; org-export-with-author.
         ;; :with-author "Hurricane Chen"
         ;; :with-broken-links org-export-with-broken-links
         ;; org-export-with-clocks.
         ;; :with-clocks t
         ;; org-export-with-creator.
         ;; :with-creator nil
         ;; :with-date org-export-with-date
         ;; :with-drawers org-export-with-drawers
         ;; :with-email org-export-with-email
         ;; :with-emphasize org-export-with-emphasize
         ;; :with-fixed-width org-export-with-fixed-width
         :with-footnotes org-export-with-footnotes
         ;; :with-latex org-export-with-latex
         ;; :with-planning org-export-with-planning
         ;; org-export-with-priority.
         :with-priority t
         ;; :with-properties org-export-with-properties
         ;; :with-special-strings org-export-with-special-strings
         ;; :with-sub-superscript org-export-with-sub-superscripts
         ;; :with-tables org-export-with-tables
         ;; :with-tags org-export-with-tags
         ;; :with-tasks org-export-with-tasks
         ;; :with-timestamps org-export-with-timestamps
         ;; :with-title org-export-with-title
         ;; org-export-with-toc.
         :with-toc t
         ;; :with-todo-keywords org-export-with-todo-keywords
         ;; }}

         ;; {{
         ;;  HTML specific properties
         ;; :html-allow-name-attribute-in-anchors org-html-allow-name-attribute-in-anchors
         ;; :html-checkbox-type org-html-checkbox-type
         :html-container "section"
         ;; :html-divs org-html-divs
         ;; org-html-doctype.
         :html-doctype "html5"
         ;; :html-extension org-html-extension
         ;; org-html-footnote-format.
         ;; :html-footnote-format nil
         ;; :html-footnote-separator org-html-footnote-separator
         ;; :html-footnotes-section org-html-footnotes-section
         ;; :html-format-drawer-function org-html-format-drawer-function
         ;; :html-format-headline-function org-html-format-headline-function
         ;; :html-format-inlinetask-function org-html-format-inlinetask-function
         :html-head-extra ,hurricane/head-extra
         ;; :html-head-include-default-style nil
         ;; :html-head-include-scripts nil
         ;; :html-head org-html-head
         ;; :html-home/up-format org-html-home/up-format
         ;; :html-html5-fancy t
         ;; :html-indent org-html-indent
         ;; :html-infojs-options org-html-infojs-options
         ;; :html-infojs-template org-html-infojs-template
         ;; :html-inline-image-rules org-html-inline-image-rules
         ;; :html-inline-images org-html-inline-images
         ;; :html-link-home org-html-link-home
         ;; :html-link-org-files-as-html org-html-link-org-files-as-html
         ;; :html-link-up org-html-link-up
         ;; :html-link-use-abs-url org-html-link-use-abs-url
         ;; :html-mathjax-options org-html-mathjax-options
         ;; :html-mathjax-template org-html-mathjax-template
         ;; :html-metadata-timestamp-format org-html-metadata-timestamp-format
         ;; org-html-postamble-format.
         ;; :html-postamble-format t
         ;; org-html-postamble.
         :html-postamble ,hurricane/postamble
         ;; :html-preamble-format org-html-preamble-format
         ;; org-html-preamble.
         :html-preamble ,hurricane/preamble
         ;; :html-self-link-headlines org-html-self-link-headlines
         ;; :html-table-align-individual-field de{org-html-table-align-individual-fields
         ;; :html-table-attributes org-html-table-default-attributes
         ;; :html-table-caption-above org-html-table-caption-above
         ;; :html-table-data-tags org-html-table-data-tags
         ;; :html-table-header-tags org-html-table-header-tags
         ;; :html-table-row-tags org-html-table-row-tags
         ;; :html-table-use-header-tags-for-first-column org-html-table-use-header-tags-for-first-column
         ;; :html-tag-class-prefix org-html-tag-class-prefix
         ;; :html-text-markup-alist org-html-text-markup-alist
         ;; :html-todo-kwd-class-prefix org-html-todo-kwd-class-prefix
         ;; :html-toplevel-hlevel org-html-toplevel-hlevel
         ;; :html-use-infojs org-html-use-infojs
         ;; :html-validation-link org-html-validation-link
         ;; :html-viewport org-html-viewport
         ;; :html-wrap-src-lines org-html-wrap-src-lines
         ;; :html-xml-declaration org-html-xml-declaration
         ;; }}

         ;; {{
         ;; Markdown specific properties.
         ;; :md-footnote-format org-md-footnote-format
         ;; :md-footnotes-section org-md-footnotes-section
         ;; :md-headline-style org-md-headline-style
         ;; }}

         ;; {{
         ;; Other options
         :table-of-contents t
         ;; :style "<link rel=\"stylesheet\" href=\"../other/mystyle.css\" type=\"text/css\" />"
         ;; }}
         :auto-sitemap nil
         :exclude "node_modules"
         :sitemap-title "Hurricane"
         :sitemap-sort-files anti-chronologically
         ;; :sitemap-function hurricane/org-publish-sitemap
         ;; :sitemap-format-entry sitemap-format-entry
         :sitemap-filename "index.org"
         )

        ;; Static assets.
        ("images"
         :base-directory ,(concat deft-dir (file-name-as-directory "notes") (file-name-as-directory "./static"))
         :base-extension "css\\|js\\|png\\|jpg\\|gif\\|svg\\|svg\\|json\\|pdf"
         :publishing-directory ,(concat blog-dir (file-name-as-directory "./static"))
         :exclude "node_modules"
         :recursive t
         :publishing-function org-publish-attachment
         )

        ("website" :components ("orgfiles" "images"))
        ("statics" :components ("images"))
        )))

(add-to-list 'safe-local-eval-forms '(blog-site-project-setup))

(defun hurricane/connect-baiduyun ()
  (interactive)
  (find-file "/ssh:c@182.61.145.178:/home/c/site/public/"))

(defun hurricane//org-video-link-export (path desc backend)
  (let ((ext (file-name-extension path)))
    (cond
     ((eq 'html backend)
      (format "<video preload='metadata' controls='controls'><source type='video/%s' src='%s' /></video>" ext path))
     ;; fall-through case for everything else.
     (t
      path))))

(defun hurricane//org-time-today ()
  "Time in seconds today at 0:00.
Returns the float number of seconds since the beginning of the
epoch to the beginning of today (00:00)."
  (float-time (apply 'encode-time
                     (append '(0 0 0) (nthcdr 3 (decode-time))))))

(defadvice org-capture-target-buffer (after make-static-directory-maybe (file) activate)
  "Create screenshot image directory if not exists while visiting node."
  (unless (file-exists-p file)
    (let* ((local-variable-list nil)
           (relative-img-dir (concat "./static/" (file-name-base (file-name-nondirectory file)) "/"))
           (absolute-img-dir (concat org-roam-directory relative-img-dir)))
      (add-to-list 'local-variable-list `(eval . (setq org-media-note-screenshot-image-dir (concat default-directory ,(format "%s" relative-img-dir)))))
      (customize-push-and-save 'safe-local-variable-values local-variable-list)
      (when absolute-img-dir
        (unless (file-exists-p absolute-img-dir)
          (make-directory absolute-img-dir t))))))

(defun hurricane//collect-backlinks-string (backend)
  (when (org-roam-node-at-point)
    (let* ((source-node (org-roam-node-at-point))
           (source-file (org-roam-node-file source-node))
           (nodes-in-file (--filter (s-equals? (org-roam-node-file it) source-file)
                                    (org-roam-node-list)))
           (nodes-start-position (-map 'org-roam-node-point nodes-in-file))
           ;; Nodes don't store the last position, so get the next headline position
           ;; and subtract one character (or, if no next headline, get point-max)
           (nodes-end-position (-map (lambda (nodes-start-position)
                                       (goto-char nodes-start-position)
                                       (if (org-before-first-heading-p) ;; file node
                                           (point-max)
                                         (call-interactively
                                          'org-forward-heading-same-level)
                                         (if (> (point) nodes-start-position)
                                             (- (point) 1) ;; successfully found next
                                           (point-max)))) ;; there was no next
                                     nodes-start-position))
           ;; sort in order of decreasing end position
           (nodes-in-file-sorted (->> (-zip nodes-in-file nodes-end-position)
                                      (--sort (> (cdr it) (cdr other))))))
      (dolist (node-and-end nodes-in-file-sorted)
        (-when-let* (((node . end-position) node-and-end)
                     (backlinks (--filter (and (> (org-roam-node-level node) 0)
                                               (->> (org-roam-backlink-source-node it)
                                                    (org-roam-node-file)
                                                    (s-contains? "private/") (not)))
                                          (org-roam-backlinks-get node)))
                     (heading (format "\n\n%s Backlinks\n"
                                      (s-repeat (+ (org-roam-node-level node) 1) "*")))
                     (details-tag-heading "#+BEGIN_EXPORT html\n<details>\n  <summary>Click to expand!</summary>\n\n<blockquote>\n#+END_EXPORT
")
                     (details-tag-ending "#+BEGIN_EXPORT html\n  </blockquote>\n</details>\n\n#+END_EXPORT")
                     (reference-and-footnote-string-list
                      (-map (lambda (backlink)
                              (let* ((source-node (org-roam-backlink-source-node backlink))
                                     (source-file (org-roam-node-file source-node))
                                     (properties (org-roam-backlink-properties backlink))
                                     (outline (when-let ((outline (plist-get properties :outline)))
                                                (when (> (length outline) 1)
                                                  (mapconcat #'org-link-display-format outline " > "))))
                                     (point (org-roam-backlink-point backlink))
                                     (text (s-replace "\n" " " (org-roam-preview-get-contents
                                                                source-file
                                                                point)))
                                     (reference (format "%s [[id:%s][%s]]\n%s\n%s\n\n"
                                                        (s-repeat (+ (org-roam-node-level node) 2) "*")
                                                        (org-roam-node-id source-node)
                                                        (org-roam-node-title source-node)
                                                        (if outline (format "%s (/%s/)"
                                                                            (s-repeat (+ (org-roam-node-level node) 3) "*") outline) "")
                                                        text))
                                     (label-list (with-temp-buffer
                                                   (insert-file-contents source-file)
                                                   (org-element-map (org-element-parse-buffer) 'footnote-reference
                                                     (lambda (reference)
                                                       (org-element-property :label reference)))))
                                     (footnote-list
                                      (with-temp-buffer
                                        (insert-file-contents source-file)
                                        (-map (lambda (label) (buffer-substring-no-properties
                                                               (nth 1 (org-footnote-get-definition label))
                                                               (nth 2 (org-footnote-get-definition label))))
                                              label-list)))
                                     (footnote-string-list (string-join footnote-list "\n"))
                                     (reference-and-footnote-string (format "%s\n%s" reference footnote-string-list)))
                                reference-and-footnote-string)
                              ) backlinks)))
          (goto-char end-position)
          (insert (format "%s\n%s\n%s\n%s" heading details-tag-heading (string-join reference-and-footnote-string-list "\n")  details-tag-ending)))))))

(add-hook 'org-export-before-processing-hook 'hurricane//collect-backlinks-string)
(add-hook 'org-export-before-processing-hook 'org-transclusion-add-all)
(add-hook 'org-export-before-processing-hook 'org-transclusion-inhibit-read-only)

(defun hurricane/publish ()
  (interactive)
  (rassq-delete-all 'html-mode auto-mode-alist)
  (rassq-delete-all 'web-mode auto-mode-alist)
  (fset 'web-mode (symbol-function 'fundamental-mode))
  (call-interactively 'org-publish-all))

;; Republish all files, even if no changes made to the page content.
;; (for example, if you want backlinks to be regenerated).
(defun hurricane/republish ()
  (interactive)
 (let ((current-prefix-arg 4))
    (rassq-delete-all 'web-mode auto-mode-alist)
    (fset 'web-mode (symbol-function 'fundamental-mode))
    (call-interactively 'org-publish-all)))

(defun hurricane//pointer-string ()
  (if (use-region-p)
      ;; Get region string if mark is set.
      (buffer-substring-no-properties (region-beginning) (region-end))
    ;; Get current symbol or string, and remove prefix char before return.
    (let* ((current-string (if (hurricane//in-string-p)
                               (buffer-substring-no-properties
                                (1+ (car (hurricane//string-start+end-points)))
                                (cdr (hurricane//string-start+end-points)))
                             ""))
           (current-symbol (if (or (string-empty-p current-string)
                                   (string-match-p "[[:space:]]" current-string))
                               ;; Get symbol around point if string around point is empty or include spaces.
                               (thing-at-point 'symbol)
                             ;; Otherwise, get string around point.
                             current-string)))
      (cond ((string-prefix-p "." current-symbol)
             (string-remove-prefix "." current-symbol))
            ((string-prefix-p "#" current-symbol)
             (string-remove-prefix "#" current-symbol))
            (t current-symbol)))))

(defun hurricane//current-parse-state ()
  "Return parse state of point from beginning of defun."
  (let ((point (point)))
    (beginning-of-defun)
    (parse-partial-sexp (point) point)))

(defun hurricane//in-string-p (&optional state)
  (or (nth 3 (or state (hurricane//current-parse-state)))
      (and
       (eq (get-text-property (point) 'face) 'font-lock-string-face)
       (eq (get-text-property (- (point) 1) 'face) 'font-lock-string-face))
      (and
       (eq (get-text-property (point) 'face) 'font-lock-doc-face)
       (eq (get-text-property (- (point) 1) 'face) 'font-lock-doc-face))))

(defun hurricane//string-start+end-points (&optional state)
  "Return a cons of the points of open and close quotes of the string.
The string is determined from the parse state STATE, or the parse state
from the beginning of the defun to the point.
This assumes that `hurricane//in-string-p' has already returned true, i.e.
that the point is already within a string."
  (save-excursion
    (let ((start (nth 8 (or state (hurricane//current-parse-state)))))
      (goto-char start)
      (forward-sexp 1)
      (cons start (1- (point))))))

(defun hurricane//org-html-wrap-blocks-in-code (src backend info)
  "Wrap a source block in <pre><code class=\"lang\">.</code></pre>."
  (when (org-export-derived-backend-p backend 'html)
    (replace-regexp-in-string
     "\\(</pre>\\)" "</code>\n\\1"
     (replace-regexp-in-string "<pre class=\"src src-\\([^\"]*?\\)\">"
                               "<pre>\n<code class=\"\\1\">\n" src))))

(with-eval-after-load 'ox-html
  (add-to-list 'org-export-filter-src-block-functions
               #'hurricane//org-html-wrap-blocks-in-code))

(defun hurricane//string-starts-with (string prefix)
  "Return t if STRING starts with prefix."
  (and (string-match (rx-to-string `(: bos ,prefix) t) string) t))

(defun hurricane//html-process-field-blocks (text backend info)
  "Filter special blocks from latex export."
  (when (eq backend 'html)
    (if (hurricane//string-starts-with text "<div class=\"FIELD\"")
        (make-string 0 ?x)
      text)))

(with-eval-after-load 'ox-html
  (add-to-list 'org-export-filter-special-block-functions
               #'hurricane//html-process-field-blocks))

(defun hurricane//org-image-link-open (orgfn context &optional args)
  (when-let ((link (plist-get context 'link))
             (type (plist-get link ':type))
             (path (plist-get link ':path)))
    (if (string-match "\\(?:jpg\\|jpeg\\|png\\|gif\\|xpm\\)" path)
	         (funcall
	          (helm :sources
		              `((name . "Action")
		                (candidates . ,(append
				                            (loop for f in '(find-file
						                                         org-open-file)
					                                collect (cons (symbol-name f) f))
				                            '(
                                      ("delete image file and link" . (lambda (path)
                                                                        (let* ((link-list (org-element-map (org-element-parse-buffer) 'link
                                                                                            (lambda (link)
                                                                                              (when (string= (org-element-property :type link) "file")
                                                                                                (list (org-element-property :path link)
                                                                                                      (org-element-property :begin link)
                                                                                                      (org-element-property :end link))))))
                                                                               (file-full-path (concat default-directory "/" path))
                                                                               (begin-end-list (hurricane//find-org-link-begin-and-end link-list path)))
                                                                          (progn
                                                                            (if (yes-or-no-p "Do you want to delete the image link?")
                                                                                (hurricane//do-delete-link-function begin-end-list))
                                                                            (if (yes-or-no-p
                                                                                 "Do you really want to delete the image file? This can't be revert!")
                                                                                (progn
                                                                                  (delete-file file-full-path)
                                                                                  ))))))
                                      ("copy file link" . (lambda (path)
                                                            (kill-new (format "%s" (concat default-directory "/" path)))))
                                      ("copy org link" . (lambda (path)
							                                             (kill-new (format "[[file:%s]]" path))))
                                      ("dired" . (lambda (path)
						                                       (dired (file-name-directory path))
						                                       (re-search-forward (file-name-nondirectory path)))))))
		                (action . identity)))
	          path)
      (funcall orgfn context))
      ))

(advice-add 'org-link-open :around #'hurricane//org-image-link-open)

(defun hurricane/html-table-to-org-table-converter ()
  (interactive)
  (eshell-command "pandoc --from html --to org =(pbpaste) -o - | pbcopy"))

(defun extract-value-from-keyword (key)
  (nth 0 (org-element-map (org-element-parse-buffer) 'keyword
           (lambda (element)
             (when (string= key (org-element-property :key element))
               (org-element-property :value element))))))
