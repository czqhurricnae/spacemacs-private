;; {{
;; @See: https://gist.github.com/CodyReichert/9dbc8bd2a104780b64891d8736682cea
;; @See: https://github.com/flycheck/flycheck/issues/997
;; @See: https://github.com/codesuki/add-node-modules-path/issues/7#issuecomment-385388439
(defun web-mode-init-hook ()
  ;; BUG: (void function add-node-modules-path)
  ;; (add-node-modules-path)
  ;; (js2-minor-mode)
  (when (equal "js" (file-name-extension (or (buffer-file-name) "")))
      (js2-mode))
  (setq css-indent-offset 2)
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-attr-indent-offset 2)
  (add-to-list 'web-mode-indentation-params '("lineup-args" . nil))
  (add-to-list 'web-mode-indentation-params '("lineup-concats" . nil))
  (add-to-list 'web-mode-indentation-params '("lineup-calls" . nil))
  (emmet-mode))
;; }}

(defun hurricane/dired-insert-relative-path ()
  (interactive)
  (let* ((make-file (file-name-parent-directory buffer-file-name))
         (other-window (get-window-with-predicate
                        (lambda (window)
                          (with-current-buffer (window-buffer window)
                            (and (not (equal window (selected-window)))
                                 (equal major-mode 'dired-mode))))))
         (mark-files (and other-window
                          (with-current-buffer (window-buffer other-window)
                            (dired-get-marked-files nil)))))
    (dolist (file mark-files)
      (progn
        (insert (concat (file-relative-name file make-file) " \\"))
        (newline-and-indent)))))
