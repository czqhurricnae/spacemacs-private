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
  (interactive)
  "vcs: version control system."
  (if (czqhurricane/vcs-project-root)
      (counsel-git)
    (if (projectile-project-p)
        (projectile-find-file)
      (ido-find-file))))

(defcustom org-screenshot-dir-name  "img"
  "Default image directory name for org screenshot."
  :type 'string
  )

(defcustom dot-image-dir-name "/Users/c/dotimg"
  "Default image directory name for dot." 
  :type 'string
  )

(defun tobase64 (filename)
  (base64-encode-string
   (with-temp-buffer
     (insert-file-contents filename)
     (buffer-string))))

(defun trim-space-in-string (string)
  (replace-regexp-in-string "[\t\n ]+" "" string)
  )

(defun dot-image-to-base64-converter ()
  (interactive)
  (let* ((img-dir dot-image-dir-name))
    (progn
      (setq temp-name (ivy-read "Please select a image name"
                                (delete ".." (delete "." (directory-files img-dir)))))
      (setq fullpath (concat img-dir "/" (file-name-base temp-name) ".png"))
      (image-to-base64-converter fullpath fullpath)))
  )

;; fullpath: the full path of image which will be converted
;; temp-name: the temporary name to be used for image label name in markdown file
(defun image-to-base64-converter (fullpath temp-name)
  (interactive)
    (progn
      (setq result (trim-space-in-string (format "data:image/png;base64,%s" (tobase64 fullpath))))
      (insert (concat "[" temp-name "]:" result ))))

(defun org-screenshot ()
  "Take a screenshot into a user specified file in the current
  buffer file directory and insert a link to this file."
  (interactive)
  (let* ((img-dir org-screenshot-dir-name))
    (progn
      (if (file-exists-p img-dir)
          (print (format "yes 'directory': %s exists" img-dir))
        (mkdir img-dir))
      (let ((temp-name (ivy-read "please selete a image name"
                                 (delete ".." (delete "." (directory-files img-dir))))))
        (setq filename (concat img-dir "/" (file-name-base temp-name) ".png"))
        (call-process-shell-command "screencapture" nil nil nil nil "-i" (concat
                                                                          "\"" filename "\"" ))
        (setq fullpath (concat default-directory filename))
        (image-to-base64-converter fullpath temp-name)))))

(defun find-org-link-begin-and-end (plist string)
  "Find link from plist whose link is equal to string, return a
  list just like `((name begin-position end-position))`'"
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
  just like `((name begin-position end-position))`"
  (while be-list
    (progn
      (goto-char (car (car be-list)))
      (delete-char (- (car (cdr (car be-list)))
                      (car (car be-list))))
      (setq be-list (cdr be-list)))))

(defun delete-org-screenshot-image-file-and-link ()
  (interactive)
  (let* ((link-list (org-element-map (org-element-parse-buffer) 'link
                      (lambda (link)
                        (when (string= (org-element-property :type link) "file")
                          (list (org-element-property :path link)
                                (org-element-property :begin link)
                                (org-element-property :end link))))))
         (img-dir org-screenshot-dir-name)
         (temp-name (concat "./" img-dir "/"
                                 (ivy-read "please selete a image name you want to delete"
                                           (delete ".." (delete "." (directory-files img-dir))))))
         (begin-end-list (find-org-link-begin-and-end link-list temp-name)))
    (progn
      (if (yes-or-no-p "Do you really want to delete the image file? This can't be revert!!")
          (delete-file temp-name))
      (if (yes-or-no-p "Do you also want to delete the image links?")
          (do-delete-link-function begin-end-list)))))

(defun replace-in-the-entire-buffer (query replace)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward query nil t)
        (replace-match replace nil nil))))

(defun replace-symbols-dollar-and-times ()
  "Used in markdown file to replace `\(\)` with `$$`
  and replace `\times` with `×`"
  (interactive)
  (replace-in-the-entire-buffer "\\\\(" " $ ")
  (replace-in-the-entire-buffer "\\\\)" " $ ")
  (replace-in-the-entire-buffer "\\\\times" "×"))

(defun create-graphviz ()
<<<<<<< HEAD
  "默认存放在主目录的dotimg文件夹下面"
=======
>>>>>>> 8fc97ff14aeceff4090b0afc73d85e6d1b777e7c
  (interactive)
  (let* ((img-dir dot-image-dir-name))
    (progn
      (setq graph-name (ivy-read "Please select a image name"
                                 (delete ".."
                                 (delete "." (directory-files img-dir)))))
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
      (insert (format "#+BEGIN_SRC dot :file ~/dotimg/%s.png :var input=create-%s-from-tables :exports results\n" graph-name graph-name))
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

;; 在org-mode为快捷插入代码块创建的自动补全函数
(defun org-insert-src-block (src-code-type)
  "Insert a `SRC-CODE-TYPE' type source code block in org-mode."
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
    ;; (if (equal src-code-type "ipython")
    ;;     (insert (format "#+BEGIN_SRC %s :preamble # -*- coding: utf-8 -*- :results raw drawer output :exports both :session\n" src-code-type))
    ;;   (insert (format "#+BEGIN_SRC %s\n" src-code-type)))
      (cond ((equal src-code-type "ipython")
            (insert (format "#+BEGIN_SRC %s :preamble # -*- coding: utf-8 -*- :results raw drawer output list :exports both :session\n" src-code-type)))
          ((equal src-code-type "example")
            (insert "#+BEGIN_SRC ipython :preamble # -*- coding: utf-8 -*- :results raw drawer output list :exports both :session example\n"))
          ((equal src-code-type "value")
            (insert "#+BEGIN_SRC ipython :preamble # -*- coding: utf-8 -*- :results raw drawer output value :exports both :session \n" src-code-type)) 
          ((equal src-code-type "C")
            (insert (format "#+BEGIN_SRC %s :includes <stdio.h> :results output list :exports both\n" src-code-type)))
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
