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

(defun czqhurricane/load-yasnippet ()
  (interactive)
  (unless yas-global-mode
    (progn
      (yas-global-mode 1)
      (setq my-snippet-dir (expand-file-name snippet-dir))
      (setq yas-snippet-dirs  my-snippet-dir)
      (yas-load-directory my-snippet-dir)
      (setq yas-wrap-around-region t)
      (setq yas-indent-line 'fixed)
      (setq yas-verbosity 0)
      (yas-minor-mode 1))))

(defun my-project-name-contains-substring (REGEX)
  (let ((dir (if (buffer-file-name)
                (file-name-directory (buffer-file-name))
              "")))
    (string-match-p REGEX dir)))

(defvar my-tags-update-time nil)

(defun my-create-tags-if-needed (SRC-DIR &optional FORCE)
  "Return the full path of tags file."
  )

(with-eval-after-load 'web-mode
  (add-hook 'web-mode-hook (lambda ()
(when (equal "js" (file-name-extension buffer-file-name))
    (add-to-list (make-local-variable 'yas-snippet-dirs) "~/.spacemacs.d/snippets/React")))))
