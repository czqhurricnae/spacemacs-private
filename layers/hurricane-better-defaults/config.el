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

;; Prevent dired window press `o' to split into three column.
(setq-default split-width-threshold 200)

(setq recenter-positions '(top middle bottom))

;; Delete the selection with a key press.
(delete-selection-mode t)

;; Add auto format paste code.
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
;; @See: https://stackoverflow.com/questions/5748814/how-does-one-disable-vc-git-in-emacs
;; @See: https://www.reddit.com/r/emacs/comments/4c0mi3/the_biggest_performance_improvement_to_emacs_ive/
;; This settings will cause command `vc-annotate' failed.
;; 如果把 `vc-handled-backends' 去掉，那么 `vc-follow-symlinks' 这个选项就会失效。
;; 进而，如果你访问一个在版本控制里面的 alias的话，它不会自动去访问原文件， 这个是非常不爽的。
;; (setq vc-handled-backends ())
(remove-hook 'find-file-hooks 'vc-find-file-hook)
;; }}

;; {{
;; @See: http://batsov.com/emacsredux/blog/2015/05/09/emacs-on-os-x/
(setq large-file-warning-threshold 100000000)
;; }}

(setq save-abbrevs nil)

;; Turn on abbrev mode globally.
(setq-default abbrev-mode t)

(setq url-show-status nil)

(require 'cl-lib)
;; Don't ask me when close emacs with process is running.
(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
  "Prevent annoying `Active processes exist' query when you quit Emacs."
  (cl-letf (((symbol-function #'process-list) (lambda ())))  ad-do-it))

;; Don't ask me when kill process buffer.
(setq kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function
            kill-buffer-query-functions))

;; Cleanup recent files.
(defun czqhuricane//cleanup-recentf ()
  (progn
    (and (fboundp 'recentf-cleanup)
         (recentf-cleanup))))

(add-hook 'kill-emacs-hook #'czqhuricane//cleanup-recentf)

;; Change evil initial mode state.
(menu-bar-mode t)

(add-hook 'before-save-hook
  (lambda ()
    (when buffer-file-name
      (let ((dir (file-name-directory buffer-file-name)))
        (when (and (not (file-exists-p dir))
           (y-or-n-p (format "Directory %s does not exist. Create it?" dir)))
          (make-directory dir t))))))

;; {{
;; @See: http://emacs.stackexchange.com/questions/13970/fixing-double-capitals-as-i-type
(defun dcaps-to-scaps ()
  "Convert word in `DOuble CApitals' to `Single Capitals'."
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
  "Toggle `dubcaps-mode'.
Converts words in `DOuble CApitals' to `Single Capitals' as you type."
  :init-value nil
  :lighter (" DC")
  (if dubcaps-mode
      (add-hook 'post-self-insert-hook #'dcaps-to-scaps nil 'local)
    (remove-hook 'post-self-insert-hook #'dcaps-to-scaps 'local)))

(defun spacemacs/check-large-file ()
  (when (and (not (equal (file-name-extension (buffer-file-name)) "pdf")) (> (buffer-size) 5000000))
    (progn (fundamental-mode)
           (hl-line-mode -1)))
  (if (and (executable-find "wc")
           (> (string-to-number (shell-command-to-string (format "wc -l %s" (buffer-file-name))))
              5000))
      (display-line-numbers-mode -1)))

(add-hook 'find-file-hook 'spacemacs/check-large-file)

(defadvice find-file (before make-static-directory-maybe
                             (file-name &optional wildcards) activate)
  "Create parent directory if not exists while visiting file."
  (unless (file-exists-p file-name)
    (let ((dir (file-name-directory file-name)))
      (when dir
        (unless (file-exists-p dir)
          (make-directory dir t))))))

(add-hook 'minibuffer-inactive-mode-hook
          #'(lambda() (set (make-local-variable 'semantic-mode) nil)
              (set (make-local-variable 'electric-pair-mode) nil)))

;; {{
;; @See: http://trey-jackson.blogspot.com/2010/04/emacs-tip-36-abort-minibuffer-when.html
(defun hurricane/stop-using-minibuffer ()
  "kill the minibuffer."
  (when (and (>= (recursion-depth) 1) (active-minibuffer-window))
    (abort-recursive-edit)))
;; }}

(add-hook 'mouse-leave-buffer-hook 'hurricane/stop-using-minibuffer)

(setq tags-add-tables nil)

(electric-pair-mode t)

;; {{
;; @See: https://www.reddit.com/r/emacs/comments/4xhxfw/how_to_tune_the_behavior_of_eletricpairmode/
(setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit)
;; (show-paren-mode t)
;; }}

;; {{
;; @See: http://oremacs.com/2015/01/17/setting-up-ediff/
(defmacro csetq (variable value)
  `(funcall (or (get ',variable 'custom-set)
                'set-default)
            ',variable ,value))
;; }}

(csetq ediff-diff-options "-w")

(setq backup-by-copying t
      make-backup-files nil
      create-lockfiles nil)

;; FIXME: `--vimgrep' will break ivy-occur with wgrep.
(setq counsel-async-split-string-re "\r?\n")
;; (setq counsel-ag-base-command  "ag --vimgrep --nocolor --nogroup %s")

;; {{
;; Search chinse must add this line.
;; @See: https://emacs-china.org/t/emacs-helm-ag/6764
(if (spacemacs/system-is-mswindows)
    (modify-coding-system-alist 'process "rg" '(utf-8 . chinese-gbk-dos))
  (modify-coding-system-alist 'process "rg" '(utf-8 . utf-8)))
;; }}

;; {{
;; @See: https://emacs-china.org/t/advice/7566
(defun hurricane//advice-remove-button (function)
  "Add a button to remove advice."
  (when (get-buffer "*Help*")
    (with-current-buffer "*Help*"
      (save-excursion
        (goto-char (point-min))
        ;; Around advice: `shell-command--shell-command-with-editor-mode'.
        (while (re-search-forward "^:[-a-z]+ advice: [‘'`]\\(.+\\)[’'']$" nil t)
          (let ((advice (intern-soft (match-string 1))))
            (when (and advice (fboundp advice))
              (let ((inhibit-read-only t))
                (insert " » ")
                (insert-text-button
                 "Remove"
                 'action
                 ;; In case lexical-binding is off.
                 `(lambda (_)
                    (message "Removing %s of advice from %s" ',function ',advice)
                    (advice-remove ',function #',advice)
                    (revert-buffer nil t))
                 'follow-link t)))))))))

(advice-add 'describe-function-1 :after #'hurricane//advice-remove-button)
;; }}

;; 使用 `counsel-git' 查找文件的时候，忽略指定后缀的文件。
(when (spacemacs/system-is-mswindows)
  (setq counsel-git-cmd "git ls-files --full-name -- \":!:*.js.meta\" \":!:*.meta\" \":!:.DS_Store\""))

;; Auto switch to `occur buffer`.
(add-hook 'occur-hook
          '(lambda ()
             (switch-to-buffer-other-window "*Occur*")))
