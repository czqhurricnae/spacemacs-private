(defun hurricane/vcs-project-root ()
  "Return the project root for current buffer."
  (let ((directory default-directory))
    (locate-dominating-file directory ".git")))

;; (defun hurricane/open-file-with-projectile-or-counsel-git ()
;;   (if (hurricane/vcs-project-root)
;;       (counsel-git)
;;     (if (projectile-project-p)
;;         (projectile-find-file)
;;       (ido-find-file))))

(defun hurricane/load-yasnippet ()
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
  (let ((dir (file-name-as-directory (file-truename SRC-DIR)))))
  )

;; {{
;; @see: https://www.youtube.com/watch?v=sBhQ2NIcrLQ&list=PLVfFIUHWy-aNaF08m34sO81dsVr4L7uI-&index=13
(defadvice sgml-delete-tag (after reindent-buffer activate)
  (cleanup-buffer))
;; }}

;; {{
;; @see: https://gist.github.com/CodyReichert/9dbc8bd2a104780b64891d8736682cea
;; @see: https://github.com/flycheck/flycheck/issues/997
;; @see: https://github.com/codesuki/add-node-modules-path/issues/7#issuecomment-385388439
(defun web-mode-init-hook ()
  ;; BUG: (void function add-node-modules-path)
  ;; (add-node-modules-path)
  ;; (js2-minor-mode)
  (when (equal "js" (file-name-extension (or (buffer-file-name) "")))
      (js2-mode))
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (emmet-mode))
;; }}
