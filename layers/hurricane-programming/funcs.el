(defun hurricane//load-yasnippet ()
  (progn
    (setq my-snippet-dir (expand-file-name snippet-dir))
    (setq yas-snippet-dirs '(my-snippet-dir))
    (yas-load-directory my-snippet-dir)
    (setq yas-wrap-around-region t)
    (setq yas-indent-line 'fixed)
    (setq yas-verbosity 0)
    (yas-minor-mode)))

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
