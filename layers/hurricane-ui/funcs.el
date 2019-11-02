(defun hurricane/layout-format-name (name pos)
  "Format the layout name given by `name' for display in mode-line."
  (let* ((layout-name (if (file-directory-p name)
                          (file-name-nondirectory (directory-file-name name))
                        name))
         (string-name (format "%s" layout-name))
         (current (equal name (spacemacs//current-layout-name)))
         (caption (concat (number-to-string (if (eq 9 pos) 0 (1+ pos)))
                          ". " string-name)))
    (if current
        ;; (propertize (concat "❰❰ " caption " ❱❱") 'face 'warning)
        (propertize (concat "★ " caption) 'face 'warning)
      caption)))

(defun hurricane/layouts-for-title-bar ()
  "Return a one liner string containing all the layout names."
  (let* ((persp-list (or (persp-names-current-frame-fast-ordered)
                         (list persp-nil-name)))
         (spaces (if (< (display-pixel-width) 1300)
                     "    "
                   "          "))
         (formatted-persp-list
          (concat " "
                  (mapconcat (lambda (persp)
                               (hurricane/layout-format-name
                                persp (position persp persp-list)))
                             persp-list spaces)))
         (file (if (projectile-project-p)
                    (if (buffer-file-name)
                        (s-replace (projectile-project-root) (format "[%s]" (projectile-project-name)) (buffer-file-name))
                      (buffer-name))
                 (if (buffer-file-name)
                     (if (string-match (concat "^" (getenv "HOME")) (buffer-file-name))
                         (concat "~" (substring (buffer-file-name) (length (getenv "HOME"))))
                       (buffer-file-name)) (buffer-name)))))
    (concat file "     -     " formatted-persp-list)))

(defun hurricane/default-title-bar ()
  (if (projectile-project-p)
      (concat
       (projectile-project-name)
       (if (buffer-file-name)
           (concat "  ✈  " (substring (buffer-file-name) (length (projectile-project-root))))
         (concat "  ✈  "(buffer-name))))
    (if (buffer-file-name)
        (if (string-match (concat "^" (getenv "HOME")) (buffer-file-name))
            (concat "~" (substring (buffer-file-name) (length (getenv "HOME"))))
          (buffer-file-name)) (buffer-name))))

(defun hurricane/toggle-title-format()
  (interactive)
  (if (equal frame-title-format '(:eval (hurricane/layouts-for-title-bar)))
      (setq frame-title-format '(:eval (hurricane/default-title-bar)))
    (setq frame-title-format '(:eval (hurricane/layouts-for-title-bar))))
  (redraw-frame))(defun hurricane/layout-format-name (name pos)
  "Format the layout name given by `name' for display in mode-line."
  (let* ((layout-name (if (file-directory-p name)
                          (file-name-nondirectory (directory-file-name name))
                        name))
         (string-name (format "%s" layout-name))
         (current (equal name (spacemacs//current-layout-name)))
         (caption (concat (number-to-string (if (eq 9 pos) 0 (1+ pos)))
                          ". " string-name)))
    (if current
        ;; (propertize (concat "❰❰ " caption " ❱❱") 'face 'warning)
        (propertize (concat "★ " caption) 'face 'warning)
      caption)))

(defun hurricane/layouts-for-title-bar ()
  "Return a one liner string containing all the layout names."
  (let* ((persp-list (or (persp-names-current-frame-fast-ordered)
                         (list persp-nil-name)))
         (spaces (if (< (display-pixel-width) 1300)
                     "    "
                   "          "))
         (formatted-persp-list
          (concat " "
                  (mapconcat (lambda (persp)
                               (hurricane/layout-format-name
                                persp (position persp persp-list)))
                             persp-list spaces)))
         (file (if (projectile-project-p)
                    (if (buffer-file-name)
                        (s-replace (projectile-project-root) (format "[%s]" (projectile-project-name)) (buffer-file-name))
                      (buffer-name))
                 (if (buffer-file-name)
                     (if (string-match (concat "^" (getenv "HOME")) (buffer-file-name))
                         (concat "~" (substring (buffer-file-name) (length (getenv "HOME"))))
                       (buffer-file-name)) (buffer-name)))))
    (concat file "     -     " formatted-persp-list)))

(defun hurricane/default-title-bar ()
  (if (projectile-project-p)
      (concat
       (projectile-project-name)
       (if (buffer-file-name)
           (concat "  ✈  " (substring (buffer-file-name) (length (projectile-project-root))))
         (concat "  ✈  "(buffer-name))))
    (if (buffer-file-name)
        (if (string-match (concat "^" (getenv "HOME")) (buffer-file-name))
            (concat "~" (substring (buffer-file-name) (length (getenv "HOME"))))
          (buffer-file-name)) (buffer-name))))

(defun hurricane/toggle-title-format()
  (interactive)
  (if (equal frame-title-format '(:eval (hurricane/layouts-for-title-bar)))
      (setq frame-title-format '(:eval (hurricane/default-title-bar)))
    (setq frame-title-format '(:eval (hurricane/layouts-for-title-bar))))
  (redraw-frame))

;; {{
;; @see: https://emacs-china.org/t/inconsolata/7997/11
;; @see: https://blog.csdn.net/xh_acmagic/article/details/78939246
(defun hurricane/better-font()
  (interactive)
  ;; English font.
  (if (display-graphic-p)
      (progn
        (set-face-attribute 'default nil :font (format   "%s:pixelsize=%d" "Inconsolata" 20)) ;; 11 13 17 19 23
        ;; Chinese font.
        (dolist (charset '(kana han symbol cjk-misc bopomofo))
          (set-fontset-font (frame-parameter nil 'font)
                            charset
                            (font-spec :family "Sarasa Mono SC")))) ;; 14 16 20 22 28
    ))

(defun hurricane/init-font(frame)
  (with-selected-frame frame
    (if (display-graphic-p)
        (hurricane/better-font))))

(if (and (fboundp 'daemonp) (daemonp))
    (add-hook 'after-make-frame-functions #'hurricane/init-font)
  (hurricane/better-font))
