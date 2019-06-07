(defun czqhurricane/vcs-project-root ()
  "return the project root for current buffer."
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

(defun czqhurricane/load-yasnippet ()
  (progn
    (setq my-snippet-dir (expand-file-name snippet-dir))
    (setq yas-snippet-dirs  my-snippet-dir)
    (yas-load-directory my-snippet-dir)
    (setq yas-wrap-around-region t)
    (setq yas-indent-line 'fixed)
    (setq yas-verbosity 0)
    (yas-minor-mode)))

(defun my-project-name-contains-substring (regex)
  (let ((dir (if (buffer-file-name)
                (file-name-directory (buffer-file-name))
              "")))
    (string-match-p regex dir)))

(defvar my-tags-update-time nil)

(defun my-create-tags-if-needed (src-dir &optional force)
  "Return the full path of tags file."
  )

;; {{
;; @see https://www.youtube.com/watch?v=sBhQ2NIcrLQ&list=PLVfFIUHWy-aNaF08m34sO81dsVr4L7uI-&index=13
(defadvice sgml-delete-tag (after reindent-buffer activate)
  (cleanup-buffer))
;; }}
