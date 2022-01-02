(defun hurricane//indent-buffer()
  (interactive)
  (indent-region (point-min) (point-max)))

(defun hurricane/indent-region-or-buffer()
  (interactive)
  (save-excursion
    (if (region-active-p)
        (progn
          (indent-region (region-beginning) (region-end))
          (message "Indent selected region."))
      (progn
        (hurricane//indent-buffer)
        (message "Indent buffer.")))))

;; {{
;; @see: http://emacsredux.com/blog/2013/03/26/smarter-open-line/
(defun hurricane/smart-open-line ()
  "Insert an empty line after the current line.
Position the cursor at its beginning, according to the current mode."
  (interactive)
  (move-end-of-line nil)
  (newline-and-indent))
;; }}

(defun hurricane/rename-file-and-buffer ()
  "Rename the current buffer and file that is visiting."
  (interactive)
  (let ((file-name (buffer-file-name)))
    (if (not (and file-name (file-exists-p file-name)))
        (message "Buffer is not visiting a file!")
      (let ((new-name (read-file-name "New name: " file-name)))
        (cond
         ((vc-backend file-name) (vc-rename-file file-name new-name))
         (t
          (rename-file file-name new-name t)
          (set-visited-file-name new-name t t)))))))

(defun hurricane/yank-to-end-of-line ()
  "Yank to end of line."
  (interactive)
  (evil-yank (point) (point-at-eol)))

;; {{
;; @see:https://oremacs.com/2015/01/26/occur-dwim/
(defun hurricane/occur-dwin ()
  "Call `occur' with a sane default."
  (interactive)
  (push (if (region-active-p)
            (buffer-substring-no-properties
             (region-beginning)
             (region-end))
          (let ((sym (thing-at-point 'symbol)))
            (when (stringp sym)
              (regexp-quote sym))))
        regexp-history)
  (deactivate-mark)
  (call-interactively 'occur))
;; }}

(defun hurricane/occur-non-ascii ()
  "Find any non-ascii characters in the current buffer."
  (interactive)
  (occur "[^[:ascii:]]"))

(defun dired-get-size ()
  (interactive)
  (let ((files (dired-get-marked-files)))
    (with-temp-buffer
      (apply 'call-process "/usr/bin/du" nil t nil "-sch" files)
      (message
       "Size of all marked files: %s"
       (progn
         (re-search-backward "\\(^[ 0-9.,]+[A-Za-z]+\\).*total$")
         (match-string 1))))))

(defun hurricane//dired-start-process (cmd &optional file-list)
  (interactive
   (let ((files (dired-get-marked-files
                 t current-prefix-arg)))
     (list
      (dired-read-shell-command "& on %s: "
                                current-prefix-arg files)
      files)))
  (let (list-switch)
    (start-process
     cmd nil shell-file-name
     shell-command-switch
     (format
      "nohup 1>/dev/null 2>/dev/null %s \"%s\""
      (if (and (> (length file-list) 1)
               (setq list-switch
                     (cadr (assoc cmd dired-filelist-cmd))))
          (format "%s %s" cmd list-switch)
        cmd)
      (mapconcat #'expand-file-name file-list "\" \"")))))

(defun hurricane/dired-open-terminal ()
  "Open an `ansi-term' that corresponds to current directory."
  (interactive)
  (let* ((current-dir (dired-current-directory))
         (buffer (if (get-buffer "*zshell*")
                     (switch-to-buffer "*zshell*")
                   (ansi-term "/bin/zsh" "zshell")))
         (proc (get-buffer-process buffer)))
    (term-send-string
     proc
     (if (file-remote-p current-dir)
         (let ((v (tramp-dissect-file-name current-dir t)))
           (format "ssh %s@%s\n"
                   (aref v 1) (aref v 2)))
       (format "cd '%s'\n" current-dir)))))

(defun hurricane/dired-copy-file-here (file)
  "This command copies the file oldname to newname.
An error is signaled if oldname is not a regular file.
If newname names a directory, it copies oldname into that directory, preserving its final name component."
  (interactive "fCopy file: ")
  (copy-file file default-directory))

(defun hurricane//dired-find-file ()
  "Open buffer in another window."
  (interactive)
  (let ((file-name (dired-get-filename nil t)))
    (if (car (file-attributes file-name))
        (dired-find-alternate-file)
      (dired-find-file-other-window))))

(defun hurricane/dired-do-command (command)
  "Run `command' on marked files. Any files not already open will be opened.
After this command has been run, any buffers it's modified will remain open and unsaved."
  (interactive "CRun on marked files M-x ")
  (save-window-excursion
    (mapc (lambda (file-name)
            (find-file file-name)
            (call-interactively command))
          (dired-get-marked-files))))

(defun hurricane/insert-space-after-point ()
  (interactive)
  (save-excursion (insert " ")))

(defmacro dakra-define-up/downcase-dwim (case)
  (let ((func (intern (concat "dakra-" case "-dwim")))
        (doc (format "Like `%s-dwim' but %s from beginning when no region is active." case case))
        (case-region (intern (concat case "-region")))
        (case-word (intern (concat case "-word"))))
    `(defun ,func (arg)
       ,doc
       (interactive "*p")
       (save-excursion
         (if (use-region-p)
             (,case-region (region-beginning) (region-end))
           (beginning-of-thing 'symbol)
           (,case-word arg))))))

(dakra-define-up/downcase-dwim "upcase")
(dakra-define-up/downcase-dwim "downcase")
(dakra-define-up/downcase-dwim "capitalize")

(defun hurricane//evil-keyboard-quit ()
  "Keyboard quit and force normal state."
  (interactive)
  (and evil-mode (evil-force-normal-state))
  (progn
    (keyboard-quit)))

(defun hurricane//dired-store-link (orig-fun &rest args)
  (if (or (derived-mode-p 'dired-mode) (derived-mode-p 'org-mode))
      (cond
       ((derived-mode-p 'dired-mode)
        (let ((file (dired-get-filename nil t)))
          (setf file (if file
                         (dired-make-relative (expand-file-name file)
                                              (file-name-directory default-directory))
                       default-directory))
          (let ((link (concat "file:" file))
                (desc  file))
            (if (string-match ".*?\\.\\(?:png\\|jpg\\|mp4\\)\\(.*\\)$" (file-name-nondirectory file))
                (push (list link nil) org-stored-links)
              (push (list link desc) org-stored-links))
            (message "Stored: %s" (or desc link))
            (car org-stored-links))))
       ((derived-mode-p 'org-mode)
        (when (or (org-in-regexp org-radio-target-regexp)
               (org-in-regexp org-target-regexp))
        (let ((target nil))
        (setf target (string-trim (match-string 0) "<<" ">>"))
        (let ((link target)
              (desc (concat "See " target)))
          (push (list link desc) org-stored-links)
          (message "Stored: %s" (or link desc))
          (car org-stored-links)))))
       (t (apply orig-fun args)))))

;; (advice-add 'org-store-link :around #'hurricane//dired-store-link)

(advice-add 'org-insert-link :after #'org-display-inline-images)

(define-advice show-paren-function (:around (fn) fix-show-paren-function)
  "Highlight enclosing parens."
  (cond ((looking-at-p "\\s(") (funcall fn))
        (t (save-excursion
             (ignore-errors (backward-up-list))
             (funcall fn)))))

(defun hurricane//open-file-in-external-app (file)
  "Open `file' in external application."
  (interactive)
  (let ((file-path file))
    (if file-path
        (cond
         ((spacemacs/system-is-mswindows) (w32-shell-execute "open" (replace-regexp-in-string "/" "\\\\" file-path)))
         ((spacemacs/system-is-mac) (shell-command (format "open \"%s\"" file-path)))
         ((spacemacs/system-is-linux) (let ((process-connection-type nil))
                                        (start-process "" nil "xdg-open" file-path))))
      (message "No file associated to this buffer."))))

(defun hurricane//find-file-in-git-repo (repo)
  (if (file-directory-p repo)
      (let* ((default-directory repo)
             (files (split-string (shell-command-to-string (format "cd %s && git ls-files" repo)) "\n" t)))
        (ivy-read "files:" files
                  :action 'find-file
                  :caller 'hurricane//find-file-in-git-repo))
    (message "%s is not a valid directory." repo)))

(defun hurricane//ivy-ff-checksum ()
  "Calculate the checksum of `file'. The checksum is copied to kill-ring."
  (interactive)
  (let ((file (expand-file-name (ivy-state-current ivy-last) ivy--directory))
        (algo (intern (ivy-read
                       "Algorithm: "
                       '(md5 sha1 sha224 sha256 sha384 sha512)))))
    (kill-new (with-temp-buffer
                (insert-file-contents-literally file)
                (secure-hash algo (current-buffer))))
    (message "Checksum copied to kill-ring.")))

(defun hurricane//ivy-ff-checksum-action (x)
  (hurricane//ivy-ff-checksum))

(defun hurricane/open-file-with-projectile-or-counsel-git ()
  (interactive)
  (if (hurricane//git-project-root)
      (counsel-git)
    (if (projectile-project-p)
        (projectile-find-file)
      (counsel-file-jump))))

;; @see: https://emacs-china.org/t/topic/6119
(defun set-image-mode-mwheel-scroll-function ()
  (setq-local mwheel-scroll-down-function 'image-scroll-down)
  (setq-local mwheel-scroll-up-function 'image-scroll-up))

(add-hook 'image-mode-hook #'set-image-mode-mwheel-scroll-function)
(add-hook 'org-mode-hook #'set-image-mode-mwheel-scroll-function)
