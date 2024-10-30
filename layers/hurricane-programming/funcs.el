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

(defun hurricane/dired-insert-relative-file-path ()
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

(defun hurricane/dired-insert-relative-directory ()
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
    (when mark-files
      (->> mark-files
           (mapcar (lambda (file)
                     (file-name-directory (file-relative-name file make-file))))
           (delete-dups)
           (mapcar (lambda (directory) (progn (insert (concat directory " \\"))
                                         (newline-and-indent))))))))

;; @See: https://emacs.stackexchange.com/questions/62/hide-compilation-window
(add-hook 'compilation-finish-functions
          (lambda (buf str)
            (if (null (string-match ".*exited abnormally.*" str))
                ;;no errors, make the compilation window go away in a few seconds
                (progn
                  (run-at-time
                   "2 sec" nil 'delete-windows-on
                   (get-buffer-create "*compilation*"))
                  (message "No Compilation Errors!")))))

(require 'which-func)

(defun compilation--file-known-p ()
  "Say whether the file under point can be found."
  (when-let* ((msg (get-text-property (point) 'compilation-message))
              (loc (compilation--message->loc msg))
              (elem (compilation-find-file-1
                     (point-marker)
                     (caar (compilation--loc->file-struct loc))
                     (cadr (car (compilation--loc->file-struct loc)))
                     (compilation--file-struct->formats
                      (compilation--loc->file-struct loc)))))
    (car elem)))

(defun shorten-string (str)
  (if (> (length str) 30)
      (concat (substring str 0 15) "…" (substring str -14))
    str))

(defun exec/rg-hack()
  (interactive)
  (let* ((msg (get-text-property (point) 'compilation-message)))
    (if msg
        (let* (
               (loc (compilation--message->loc msg))
               (file (caar (compilation--loc->file-struct loc)))
               (line (compilation--loc->line loc))
               (col (compilation--loc->col loc))
               (function_name (exec/lsp-which-function file line col))
               (text (format "%-30s │" (shorten-string  (or function_name ""))))
               (ov (make-overlay (line-beginning-position) (1+ (line-beginning-position)) nil t)))

          (overlay-put ov 'before-string
                       (propertize text 'face 'font-lock-property-name-face)
                       )
          (overlay-put ov 'evaporate t))
      )))

(defun exec/rg-hint-all(&rest args)
  (interactive)
  (sit-for 0.01)
  (with-current-buffer rg-buffer-name
    (defvar-local rg-hack-lines 1)
    (while (and (not (eobp)) (< rg-hack-lines 300))
      (exec/rg-hack)
      (forward-line)
      (setq-local rg-hack-lines (1+ rg-hack-lines))
      (if (= (% rg-hack-lines 10) 0 )
          (sit-for 0.01)))
    (goto-char (point-min))
    ))

;; (defun exec/setup-rg-hint(&rest args)
;;   (add-to-list 'compilation-finish-functions 'exec/rg-hint-all))

;; (add-hook 'rg-mode-hook 'exec/setup-rg-hint)
