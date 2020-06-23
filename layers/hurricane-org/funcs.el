(defvar killed-file-list nil
  "List of recently killed files.")

(defcustom org-dot-image-dir-name "dotImg"
  "Default directory name for org dot image."
  :type 'string)

(defcustom org-screenshot-image-dir-name "screenshotImg"
  "Default directory name for org dot image."
  :type 'string)

(setq buffer-replace-string-rule-lists '(("，" . ",")
                                         ("。" . ".")
                                         ("！" . "!")
                                         ("？" . "?")
                                         ("【" . "[")
                                         ("】" . "]")
                                         ("（" . "(")
                                         ("）" . ")")
                                         ("％" . "%")
                                         ("＃" . "#")
                                         ("＠" . "@")
                                         ("＆" . "&")
                                         ("１" . "1")
                                         ("２" . "2")
                                         ("３" . "3")
                                         ("４" . "4")
                                         ("５" . "5")
                                         ("６" . "6")
                                         ("７" . "7")
                                         ("８" . "8")
                                         ("９" . "9")
                                         ("０" . "0")
                                         ("、" . ",")
                                         ("；" . ",")
                                         ("“" . "\"")
                                         ("”" . "\"")
                                         ("：" . ":")))

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
  "Replace query string with the replace string in the entire buffer.
`query': the string will be replaced.
`replace': the string used to replace.
Subexp:
1. used in function `replace-match'.
2. represent `replace' argument will be implemented in which one match group."
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
  "Find link from plist whose link is equal to `string', return a
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

(defun do-delete-link-function (be-list)
  "Goto the begining of link and delete it, `be-list' is a list
just like `((name begin-position end-position))'"
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
         (begin-end-list (find-org-link-begin-and-end link-list (concat img-dir "/" temp-name))))
    (progn
      (if (yes-or-no-p "Do you want to delete the image links?")
        (do-delete-link-function begin-end-list))
      (if (yes-or-no-p
           "Do you really want to delete the image file? This can't be revert!")
        (progn
          (delete-file file-full-path)
          )))))

(defun org-delete-screenshot-image-file-and-link ()
  (interactive)
  (delete-image-file-and-link org-screenshot-image-dir-name))

(defun org-delete-image-file-and-link ()
  (interactive)
  (delete-image-file-and-link (or org-screenshot-image-dir-name org-dot-image-dir-name)))

(defun replace-symbols-dollar-and-times ()
  "Used in markdown file , replace unexpected symbols:
           '\(' -> ' $'
           '\)' -> '$'
       '\times' -> '×'
  '![img](...)' -> '![img][...]\r' <- '\r' : new line
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
            ((equal src-code-type "call")
             (insert "#+CALL: createTree(toInclude=\"*.*\", toExclude=\"\", directory=\"\", createLink=\"true\")"))
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
    (kill-buffer-and-window)))

(defun save-buffer-filter ()
  "Replace the expected charaters except `funcs.el<hurricane-org>' file."
  (interactive)
  (save-buffer)
  (and (not (string-equal (buffer-file-name) "/Users/c/.spacemacs.d/layers/hurricane-org/funcs.el"))
      (progn
        (dolist (replace-string-rule buffer-replace-string-rule-lists)
          (replace-in-the-entire-buffer (car replace-string-rule) (cdr replace-string-rule) nil)))))

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

(defun hurricane/filter-by-tags ()
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
          (org-clock-sum tstart tend 'hurricane/filter-by-tags)
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

(defun hurricane/notify-osx (title message)
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
;; @see: https://pngmini.com/
;; Download & install.
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
            (eshell-command cmd)))
        (install-monitor-file-exists absolute-full-file-path 1 #'callback-imageoptim)
        (insert (concat "[[file:" full-file-path "]]"))
        (evil-normal-state)))))
;; }}

(defun org-screenshot-and-ocr ()
  "Take a screenshot into a user specified file in the current buffer file directory
and insert a link to this file."
  (interactive)
  (let* ((img-dir org-screenshot-image-dir-name))
    (progn
      (if (file-exists-p img-dir)
          (print (format "Screnshot image directory: '%s' already exists." img-dir))
        (mkdir img-dir))
      ;; 统一将截图和下载图片存放的文件夹, 为以文件的同一目录下的 `screenshotImg' 文件夹.
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

(defun org-insert-caption-and-target ()
  (interactive)
  (let* ((current-symbol (color-rg-pointer-string))
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

(defun save-and-publish-website()
    "Save all buffers and publish."
  (interactive)
  (when (yes-or-no-p "Really save and publish current project?")
    (save-some-buffers t)
    (org-publish-project "website" t)
    (message "Site published done.")))

(defun save-and-publish-statics ()
  "Just copy statics like js, css, and image file .etc."
  (interactive)
  (org-publish-project "statics" t)
  (message "Copy statics done."))

(defun save-and-publish-file ()
    "Save current buffer and publish."
  (interactive)
  (save-buffer t)
  (blog-site-project-setup)
  (org-publish-current-file t))

(defun save-and-publish-index-file ()
  "Save current buffer and publish."
  (interactive)
  (save-buffer t)
  (org-publish-current-file t))

(defun delete-org-and-html ()
  "Delete current org and the relative html when it exists."
  (interactive)
  (when (yes-or-no-p "Really delete current org and the relative html?")
    (let ((fileurl (concat "~/site/public/" (file-name-base (buffer-name)) ".html")))
      (if (file-exists-p fileurl)
          (delete-file fileurl))
      (delete-file (buffer-file-name))
      (kill-this-buffer)
      (message "Delete org and the relative html done."))))

(defun just-delete-relative-html ()
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

(defun preview-current-buffer-in-browser ()
  "Open current buffer as html."
  (interactive)
  (let ((fileurl (concat "http://127.0.0.1:8080/" (file-name-nondirectory (directory-file-name (file-name-directory buffer-file-name))) "/" (file-name-base (buffer-name)) ".html")))
  (let ((fileurl (concat "http://127.0.0.1:8080/" (file-relative-name (file-name-directory buffer-file-name) (concat deft-dir "notes/")) (file-name-base (buffer-file-name)) ".html")))
    (save-and-publish-file)
    (unless (httpd-running-p) (httpd-start))
    (browse-url fileurl)))

;; {{
;; Fix: ox-publish 导出的静态博客网页代码片段中代码行末尾有乱码.
;; @see: https://github.com/alpaker/Fill-Column-Indicator/issues/45#issuecomment-108911964
(defun fci-mode-override-advice (&rest args))

(advice-add 'org-html-fontify-code :around
            (lambda (fun &rest args)
              (advice-add 'fci-mode :override #'fci-mode-override-advice)
              (let ((result  (apply fun args)))
                (advice-remove 'fci-mode #'fci-mode-override-advice)
                result)))
;; }}

(defun blog-site-project-setup ()
  (interactive)
  (set (make-local-variable 'org-publish-project-alist)
       `(("orgfiles"
         ;; Sources and destinations for files.
         ;; local org files directory.
         :base-directory ,(concat deft-dir (file-name-as-directory "notes"))
         ;; :publishing directory "/ssh:c@182.61.145.178:/home/c/site/public/"
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
         ;; :htmlized-source

         ;; Options for the exporters.

         ;; {{
         ;; Generic properties.
         ;; :archived-trees	org-export-with-archived-trees
         ;; :exclude-tags	org-export-exclude-tags
         ;; org-export-headline-levels.
         :headline-levels 4
         ;; :language	org-export-default-language
         ;; :preserve-breaks	org-export-preserve-breaks
         ;; org-export-with-section-numbers.
         :section-numbers nil
         ;; :select-tags	org-export-select-tags
         ;; org-export-with-author.
         :with-author "Hurricane Chen"
         ;; :with-broken-links	org-export-with-broken-links
         ;; org-export-with-clocks.
         ;; :with-clocks	t
         ;; org-export-with-creator.
         ;; :with-creator nil
         ;; :with-date org-export-with-date
         ;; :with-drawers	org-export-with-drawers
         ;; :with-email	org-export-with-email
         ;; :with-emphasize	org-export-with-emphasize
         ;; :with-fixed-width org-export-with-fixed-width
         ;; :with-footnotes	org-export-with-footnotes
         ;; :with-latex	org-export-with-latex
         ;; :with-planning	org-export-with-planning
         ;; org-export-with-priority.
         :with-priority t
         ;; :with-properties	org-export-with-properties
         ;; :with-special-strings	org-export-with-special-strings
         ;; :with-sub-superscript	org-export-with-sub-superscripts
         ;; :with-tables	org-export-with-tables
         ;; :with-tags	org-export-with-tags
         ;; :with-tasks	org-export-with-tasks
         ;; :with-timestamps	org-export-with-timestamps
         ;; :with-title	org-export-with-title
         ;; org-export-with-toc.
         :with-toc t
         ;; :with-todo-keywords	org-export-with-todo-keywords
         ;; }}

         ;; {{
         ;;  HTML specific properties
         ;; :html-allow-name-attribute-in-anchors	org-html-allow-name-attribute-in-anchors
         ;; :html-checkbox-type	org-html-checkbox-type
         ;; :html-container	org-html-container-element
         ;; :html-divs	org-html-divs
         ;; org-html-doctype.
         :html-doctype "html5"
         ;; :html-extension	org-html-extension
         ;; org-html-footnote-format.
         ;; :html-footnote-format nil
         ;; :html-footnote-separator	org-html-footnote-separator
         ;; :html-footnotes-section	org-html-footnotes-section
         ;; :html-format-drawer-function	org-html-format-drawer-function
         ;; :html-format-headline-function	org-html-format-headline-function
         ;; :html-format-inlinetask-function	org-html-format-inlinetask-function
         ;; :html-head-extra	org-html-head-extra
         ;; :html-head-include-default-style	org-html-head-include-default-style
         ;; :html-head-include-scripts	org-html-head-include-scripts
         ;; :html-head	org-html-head
         ;; :html-home/up-format	org-html-home/up-format
         ;; :html-html5-fancy	org-html-html5-fancy
         ;; :html-indent	org-html-indent
         ;; :html-infojs-options	org-html-infojs-options
         ;; :html-infojs-template	org-html-infojs-template
         ;; :html-inline-image-rules	org-html-inline-image-rules
         ;; :html-inline-images	org-html-inline-images
         ;; :html-link-home	org-html-link-home
         ;; :html-link-org-files-as-html	org-html-link-org-files-as-html
         ;; :html-link-up	org-html-link-up
         ;; :html-link-use-abs-url	org-html-link-use-abs-url
         ;; :html-mathjax-options	org-html-mathjax-options
         ;; :html-mathjax-template	org-html-mathjax-template
         ;; :html-metadata-timestamp-format	org-html-metadata-timestamp-format
         ;; org-html-postamble-format.
         ;; :html-postamble-format t
         ;; org-html-postamble.
         ;; :html-postamble t
         ;; :html-preamble-format	org-html-preamble-format
         ;; org-html-preamble.
         ;; :html-preamble nil
         ;; :html-self-link-headlines	org-html-self-link-headlines
         ;; :html-table-align-individual-field	de{org-html-table-align-individual-fields
         ;; :html-table-attributes	org-html-table-default-attributes
         ;; :html-table-caption-above	org-html-table-caption-above
         ;; :html-table-data-tags	org-html-table-data-tags
         ;; :html-table-header-tags	org-html-table-header-tags
         ;; :html-table-row-tags	org-html-table-row-tags
         ;; :html-table-use-header-tags-for-first-column	org-html-table-use-header-tags-for-first-column
         ;; :html-tag-class-prefix	org-html-tag-class-prefix
         ;; :html-text-markup-alist	org-html-text-markup-alist
         ;; :html-todo-kwd-class-prefix	org-html-todo-kwd-class-prefix
         ;; :html-toplevel-hlevel	org-html-toplevel-hlevel
         ;; :html-use-infojs	org-html-use-infojs
         ;; :html-validation-link	org-html-validation-link
         ;; :html-viewport	org-html-viewport
         ;; :html-wrap-src-lines	org-html-wrap-src-lines
         ;; :html-xml-declaration	org-html-xml-declaration
         ;; }}

         ;; {{
         ;; Markdown specific properties.
         ;; :md-footnote-format	org-md-footnote-format
         ;; :md-footnotes-section	org-md-footnotes-section
         ;; :md-headline-style	org-md-headline-style
         ;; }}

         ;; {{
         ;; Other options
         :table-of-contents t
         ;; :style "<link rel=\"stylesheet\" href=\"../other/mystyle.css\" type=\"text/css\" />"
         ;; }}
         )

        ;; Static assets.
        ("js"
         :base-directory (concat deft-dir (file-name-as-directory "js"))
         :base-extension "js"
         :publishing-directory(concat deft-dir (file-name-as-directory "public") (file-name-as-directory "js"))
         :recursive t
         :publishing-function org-publish-attachment
         )
        ("css"
         :base-directory (concat deft-dir (file-name-as-directory "css"))
         :base-extension "css"
         :publishing-directory (concat deft-dir (file-name-as-directory "public") (file-name-as-directory "css"))
         :recursive t
         :publishing-function org-publish-attachment
         )
        ("images"
         :base-directory (concat deft-dir (file-name-as-directory "images"))
         :base-extension "jpg\\|gif\\|png\\|svg\\|gif"
         :publishing-directory (concat deft-dir (file-name-as-directory "public") (file-name-as-directory "images"))
         :recursive t
         :publishing-function org-publish-attachment
         )
        ("assets"
         :base-directory (concat deft-dir (file-name-as-directory "assets"))
         :base-extension "mp3"
         :publishing-directory (concat deft-dir (file-name-as-directory "public") (file-name-as-directory"assets"))
         :recursive t
         :publishing-function org-publish-attachment
         )

        ("website" :components ("orgfiles" "js" "css" "images"))
        ("statics" :components ("js" "css" "images" "assets"))
        )))

(add-to-list 'safe-local-eval-forms '(blog-site-project-setup))

(defun connect-baiduyun ()
  (interactive)
  (find-file "/ssh:c@182.61.145.178:/home/c/site/public/"))

(defun org-video-link-export (path desc backend)
  (let ((ext (file-name-extension path)))
    (cond
     ((eq 'html backend)
      (format "<video preload='metadata' controls='controls'><source type='video/%s' src='screenshotImg/%s' /></video>" ext path))
     ;; fall-through case for everything else
     (t
      path))))
