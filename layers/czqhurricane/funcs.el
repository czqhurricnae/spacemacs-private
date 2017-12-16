(defun occur-dwin ()
; Call 'occur' with a same default
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

(defun tobase64 (filename)
  (base64-encode-string
   (with-temp-buffer
     (insert-file-contents filename)
     (buffer-string))))

(defcustom org-screenshot-dir-name  "img"
  "default image directory name for org screenshot"
  :type 'string
  )

(defun org-screenshot ()
  (interactive)
  "Take a screenshot into a user specified file in the current
     buffer file directory and insert a link to this file."
  (let* ((img-dir org-screenshot-dir-name))
    (progn
      (if (file-exists-p img-dir)
          (print "yes")
        (mkdir img-dir))
      (let ((temp-name (ivy-read "please selete a image name"
                                 (delete ".." (delete "." (directory-files img-dir))))))
        (setq filename (concat img-dir "/" (file-name-base temp-name) ".png"))
        (call-process-shell-command "screencapture" nil nil nil nil "-i" (concat
                                                                          "\"" filename "\"" ))
        (setq fullpath (concat "/Users/c/" filename))
        (setq result (replace-regexp-in-string "[\t\n ]+" "" (format "data:image/png;base64,%s" (tobase64 fullpath))))
        (print result)
        (insert (concat "[" temp-name "]:" result ))))))

(defun find-org-link-begin-and-end (plist string)
  "find link from plist whose link is equal to string, return a
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
  "goto the begining of link and delete it, be-list is a list
just like `((name begin-position end-position))'"
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
