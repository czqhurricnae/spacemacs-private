(setq auto-coding-regexp-alist
      (delete (rassoc 'utf-16be-with-signature auto-coding-regexp-alist)
              (delete (rassoc 'utf-16le-with-signature auto-coding-regexp-alist)
                      (delete (rassoc 'utf-8-with-signature auto-coding-regexp-alist)
                              auto-coding-regexp-alist))))

(defun ffap-hexl-mode ()
  (interactive)
  (let ((ffap-file-finder 'hexl-find-file))
    (call-interactively 'ffap)))

(when (spacemacs/window-system-is-mac)
  (setq ns-pop-up-frames nil))

(global-prettify-symbols-mode 1)
(setq-default fill-column 80)

(setq recenter-positions '(top middle bottom))

;; Delete the selection with a key press
(delete-selection-mode t)

;;Add auto format paste code
(dolist (command '(yank yank-pop))
  (eval
   `(defadvice ,command (after indent-region activate)
      (and (not current-prefix-arg)
           (member major-mode
                   '(emacs-lisp-mode
                     lisp-mode
                     clojure-mode
                     scheme-mode
                     haskell-mode
                     ruby-mode
                     rspec-mode
                     python-mode
                     c-mode
                     c++-mode
                     objc-mode
                     latex-mode
                     js-mode
                     plain-tex-mode))
           (let ((mark-even-if-inactive transient-mark-mode))
             (indent-region (region-beginning) (region-end) nil))))))

;; {{
;; see: https://stackoverflow.com/questions/5748814/how-does-one-disable-vc-git-in-emacs
;; see: https://www.reddit.com/r/emacs/comments/4c0mi3/the_biggest_performance_improvement_to_emacs_ive/
;; this settings will cause command 'vc-annotate' failed.
;; 如果把 vc-handled-backends去掉,那么 vc-follow-symlinks 这个选项就会失效.
;; 进而, 如果你访问一个在版本控制里面的alias的话,它不会自动去访问原文件,这个是非常不爽的.
;; (setq vc-handled-backends ())
(remove-hook 'find-file-hooks 'vc-find-file-hook)
;; }}

;; {{
;; see: http://batsov.com/emacsredux/blog/2015/05/09/emacs-on-os-x/
(setq large-file-warning-threshold 100000000)
;; }}

(setq save-abbrevs nil)

;; Turn on abbrev mode globally
(setq-default abbrev-mode t)

(setq url-show-status nil)

;; Don't ask me when close emacs with process is running.
(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
  "Prevent annoying \"Active processes exist\" query when you quit Emacs."
  (flet ((process-list ())) ad-do-it))

;; Don't ask me when kill process buffer.
(setq kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function
            kill-buffer-query-functions))

;; Cleanup recent files
(defun czqhuricane/cleanup-recentf ()
  (progn
    (and (fboundp 'recentf-cleanup)
         (recentf-cleanup))))

(add-hook 'kill-emacs-hook #'czqhuricane/cleanup-recentf)

;; Change evil initial mode state
(menu-bar-mode t)

(add-hook 'before-save-hook
          (lambda ()
            (when buffer-file-name
              (let ((dir (file-name-directory buffer-file-name)))
                (when (and (not (file-exists-p dir))
                           (y-or-n-p (format "Directory %s does not exist. Create it?" dir)))
                  (make-directory dir t))))))

;; {{
;; see: http://emacs.stackexchange.com/questions/13970/fixing-double-capitals-as-i-type
(defun dcaps-to-scaps ()
  "Convert word in DOuble CApitals to Single Capitals."
  (interactive)
  (and (= ?w (char-syntax (char-before)))
       (save-excursion
         (and (if (called-interactively-p)
                  (skip-syntax-backward "w")
                (= -3 (skip-syntax-backward "w")))
              (let (case-fold-search)
                (looking-at "\\b[[:upper:]]\\{2\\}[[:lower:]]"))
              (capitalize-word 1)))))
;; }}

(define-minor-mode dubcaps-mode
  "Toggle 'dubcaps-mode'.Converts words in DOuble CApitals to Single Capitals as you type."
  :init-value nil
  :lighter (" DC")
  (if dubcaps-mode
      (add-hook 'post-self-insert-hook #'dcaps-to-scaps nil 'local)
    (remove-hook 'post-self-insert-hook #'dcaps-to-scaps 'local)))

(defun spacemacs/check-large-file ()
  (when (> (buffer-size) 5000000)
    (progn (fundamental-mode)
           (hl-line-mode -1)))
  (if (and (executable-find "wc")
           (> (string-to-number (shell-command-to-string (format "wc -l %s" (buffer-file-name))))
              5000))
      (linum-mode -1)))

(add-hook 'find-file-hook 'spacemacs/check-large-file)

(defadvice find-file (before make-directory-maybe
                             (filename &optional wildcards) activate)
  "Create parent directory if not exists while visiting file."
  (unless (file-exists-p filename)
    (let ((dir (file-name-directory filename)))
      (when dir
        (unless (file-exists-p dir)
          (make-directory dir t))))))

(add-hook 'minibuffer-inactive-mode-hook
          #'(lambda() (set (make-local-variable 'semantic-mode) nil)
              (set (make-local-variable 'electric-pair-mode) nil)))

;; {{
;; see: http://trey-jackson.blogspot.com/2010/04/emacs-tip-36-abort-minibuffer-when.html
(defun czqhuricane/stop-using-minibuffer ()
  "kill the minibuffer"
  (when (and (>= (recursion-depth) 1) (active-minibuffer-window))
    (abort-recursive-edit)))
;; }}

(add-hook 'mouse-leave-buffer-hook 'czqhuricane/stop-using-minibuffer)

(setq tags-add-tables nil)

(electric-pair-mode t)

;; {{
;; see: https://www.reddit.com/r/emacs/comments/4xhxfw/how_to_tune_the_behavior_of_eletricpairmode/
(setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit)
(show-paren-mode t)
;; }}

;; {{
;; see: http://oremacs.com/2015/01/17/setting-up-ediff/
(defmacro csetq (variable value)
  `(funcall (or (get ',variable 'custom-set)
                'set-default)
            ',variable ,value))
;; }}

(csetq ediff-diff-options "-w")

(setq backup-by-copying t
      make-backup-files nil
      create-lockfiles nil)

(global-linum-mode t)

;; Auto switch to `occur buffer`
(add-hook 'occur-hook
          '(lambda ()
             (switch-to-buffer-other-window "*Occur*")))
