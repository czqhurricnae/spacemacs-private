(defun hurricane//layout-format-name (name pos)
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

(defun hurricane//layouts-for-title-bar ()
  "Return a one liner string containing all the layout names."
  (let* ((persp-list (or (persp-names-current-frame-fast-ordered)
                         (list persp-nil-name)))
         (spaces (if (< (display-pixel-width) 1300)
                     "    "
                   "          "))
         (formatted-persp-list
          (concat " "
                  (mapconcat (lambda (persp)
                               (hurricane//layout-format-name
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

(defun hurricane//default-title-bar ()
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

(defun hurricane/toggle-title-format ()
  (interactive)
  (if (equal frame-title-format '(:eval (hurricane//layouts-for-title-bar)))
      (setq frame-title-format '(:eval (hurricane//default-title-bar)))
    (setq frame-title-format '(:eval (hurricane//layouts-for-title-bar))))
  (redraw-frame))(defun hurricane//layout-format-name (name pos)
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

(defun hurricane//layouts-for-title-bar ()
  "Return a one liner string containing all the layout names."
  (let* ((persp-list (or (persp-names-current-frame-fast-ordered)
                         (list persp-nil-name)))
         (spaces (if (< (display-pixel-width) 1300)
                     "    "
                   "          "))
         (formatted-persp-list
          (concat " "
                  (mapconcat (lambda (persp)
                               (hurricane//layout-format-name
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

(defun hurricane//default-title-bar ()
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

(defun hurricane/toggle-title-format ()
  (interactive)
  (if (equal frame-title-format '(:eval (hurricane//layouts-for-title-bar)))
      (setq frame-title-format '(:eval (hurricane//default-title-bar)))
    (setq frame-title-format '(:eval (hurricane//layouts-for-title-bar))))
  (redraw-frame))

;; {{
;; @See: https://emacs-china.org/t/inconsolata/7997/11
;; @See: https://blog.csdn.net/xh_acmagic/article/details/78939246
(defun hurricane/better-font ()
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

(defun hurricane/init-font (frame)
  (with-selected-frame frame
    (if (display-graphic-p)
        (hurricane/better-font))))

(if (and (fboundp 'daemonp) (daemonp))
    (add-hook 'after-make-frame-functions #'hurricane/init-font)
  (hurricane/better-font))

(with-eval-after-load 'tab-bar
  ;; 使用 super-1 super-2 ... 来切换 tab
  (customize-set-variable 'tab-bar-select-tab-modifiers '(super))

  ;; 自动截取 tab name，并且添加在每个 tab 上添加数字，方便用快捷键切换
  (setq tab-bar-tab-name-function
        (lambda () (let* ((raw-tab-name (buffer-name (window-buffer (minibuffer-selected-window))))
                     (count (length (window-list-1 nil 'nomini)))
                     (truncated-tab-name (if (< (length raw-tab-name)
                                                tab-bar-tab-name-truncated-max)
                                             raw-tab-name
                                           (truncate-string-to-width raw-tab-name
                                                                     tab-bar-tab-name-truncated-max
                                                                     nil nil tab-bar-tab-name-ellipsis))))
                (if (> count 1)
                    (concat truncated-tab-name "(" (number-to-string count) ")")
                  truncated-tab-name))))

  ;; 给 tab 两边加上空格，更好看
  (setq tab-bar-tab-name-format-function
        (lambda (tab i)
          (let ((face (funcall tab-bar-tab-face-function tab)))
            (concat
             (propertize " " 'face face)
             (propertize (number-to-string i) 'face `(:inherit ,face :weight ultra-bold :underline t))
             (propertize (concat " " (alist-get 'name tab) " ") 'face face)))))

  (set-face-attribute 'tab-bar-tab nil :background "dark orange" :foreground "black")

  ;; WORKAROUND: update tab-bar for daemon
  (when (daemonp)
    (add-hook 'after-make-frame-functions
              #'(lambda (&rest _) (force-mode-line-update))))

  (defun hurricane//back-to-primary-frame (&optional TAB-NUMBER)
    (unless (eq (selected-frame) (car (visible-frame-list)))
      (select-frame-set-input-focus (car (visible-frame-list)))))

  (advice-add #'tab-bar-select-tab :before #'hurricane//back-to-primary-frame))

(defun remove-key (keymap key)
  (define-key keymap key nil)
  (setq key (cl-mapcan (lambda (k)
                         (if (and (integerp k)
                                  (/= (logand k ?\M-\^@) 0))
                             (list ?\e (- k ?\M-\^@))
                           (list k)))
                       key))
  (if (= (length key) 1)
      (delete key keymap)
    (let* ((prefix (vconcat (butlast key)))
           (submap (lookup-key keymap prefix)))
      (delete (last key) submap)
      (when (= (length submap) 1)
        (remove-key keymap prefix)))))
