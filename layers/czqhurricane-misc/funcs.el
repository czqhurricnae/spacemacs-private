(defun czqhurricane/highlight-dwim ()
  (interactive)
  (if (use-region-p)
      (progn
        (highlight-frame-toggle)
        (deactivate-mark))
    (symbol-overlay-put)))

;; @see https://bitbucket.org/lyro/evil/issue/511/let-certain-minor-modes-key-bindings
(defmacro adjust-major-mode-keymap-with-evil (m &optional r)
  `(eval-after-load (quote ,(if r r m))
     '(progn
        (evil-make-overriding-map ,(intern (concat m "-mode-map")) 'normal)
        ;; force update evil keymaps after git-timemachine-mode loaded
        (add-hook (quote ,(intern (concat m "-mode-hook"))) #'evil-normalize-keymaps))))

(defun czqhurricane/insert-semicolon-at-the-end-of-this-line ()
  "insert`;'at the end of current line"
  (interactive)
  (save-excursion
    (end-of-line)
    (insert ";")))

(defun czqhurricane/delete-semicolon-at-the-end-of-this-line ()
  (interactive)
  (save-excursion
    (end-of-line)
    (if (looking-back ";")
        (progn
          (backward-char)
          (delete-char 1)))))

(defun czqhurricane/insert-comma-at-the-end-of-this-line ()
  (interactive)
  (save-excursion
    (end-of-line)
    (insert ",")))

(defun czqhurricane/delete-comma-at-the-end-of-this-line ()
  (interactive)
  (save-excursion
    (end-of-line)
    (if (looking-back ",")
        (progn
          (backward-char)
          (delete-char 1)))))

(defun czqhurricane/load-my-layout ()
  (interactive)
  (persp-load-state-from-file (concat persp-save-dir "czqhurricane")))

(defun czqhurricane/save-my-layout ()
  (interactive)
  (persp-save-state-to-file (concat persp-save-dir "czqhurricane")))

;; http://blog.binchen.org/posts/use-ivy-mode-to-search-bash-history.html
;; http://wikemacs.org/wiki/Shell#Search_the_bash.2C_zsh_or_fish_history_with_Ivy-mode
(defun counsel-yank-bash-history ()
  "Yank the zsh history"
  (interactive)
  (let (hist-cmd collection val)
    (shell-command "history -r") ; reload history
    (setq collection
          (nreverse
           (split-string (with-temp-buffer (insert-file-contents (file-truename "~/.zsh_history"))
                                           (buffer-string))
                         "\n"
                         t)))
    (setq collection (mapcar (lambda (item) (replace-regexp-in-string ".*;" "" item)) collection))
    (when (and collection (> (length collection) 0)
               (setq val (if (= 1 (length collection)) (car collection)
                           (ivy-read (format "Zsh history:") collection))))
      ;; (setq val (replace-regexp-in-string "^:[^;]*;" "" val))
      ;; (setq val (replace-regexp-in-string ".*;" "" val))
      (kill-new val)
      (message "%s => kill-ring" val))))

(defun czqhurricane/indent-region(numSpaces)
  (progn
    ;; default to start and end of current line
    (setq regionStart (line-beginning-position))
    (setq regionEnd (line-end-position))

    ;; if there's a selection, use that instead of the current line
    (when (use-region-p)
      (setq regionStart (region-beginning))
      (setq regionEnd (region-end))
      )

    (save-excursion                          ;; restore the position afterwards
      (goto-char regionStart)                ;; go to the start of region
      (setq start (line-beginning-position)) ;; save the start of the line
      (goto-char regionEnd)                  ;; go to the end of region
      (setq end (line-end-position))         ;; save the end of the line

      (indent-rigidly start end numSpaces)   ;; indent between start and end
      (setq deactivate-mark nil)             ;; restore the selected region
      )))

(defun czqhurricane/tab-region (N)
  (interactive "p")
  (if (use-region-p)
      (czqhurricane/indent-region 4)         ;; region was selected, call indent-region
    (insert "    ")                          ;; else insert four spaces as expected
    ))

(defun czqhurricane/untab-region (N)
  (interactive "p")
  (czqhurricane/indent-region -4))

(defun czqhurricane/hack-tab-key ()
  (interactive)
  (local-set-key (kbd "<tab>") 'czqhurricane/tab-region)
  (local-set-key (kbd "<S-tab>") 'czqhurricane/untab-region)
  )

;; I'm don't like this settings too much.
;; (add-hook 'prog-mode-hook 'czqhurricane/hack-tab-key)
(defun endless/fill-or-unfill ()
  "Like `fill-paragraph', but unfill if used twice."
  (interactive)
  (let ((fill-column
         (if (eq last-command 'endless/fill-or-unfill)
             (progn (setq this-command nil)
                    (point-max))
           fill-column)))
    (call-interactively #'fill-paragraph)))

(defun my-unwind-git-timemachine ()
  (if (not (eq last-command-event 13))
      (git-timemachine-quit)))

;; http://blog.binchen.org/posts/new-git-timemachine-ui-based-on-ivy-mode.html
(defun my-git-timemachine-show-selected-revision ()
  "Show last (current) revision of file."
  (interactive)
  (let (collection)
    (setq collection
          (mapcar (lambda (rev)
                    ;; re-shape list for the ivy-read
                    (cons (concat (substring (nth 0 rev) 0 7) "|" (nth 5 rev) "|" (nth 6 rev)) rev))
                  (git-timemachine--revisions)))
    (ivy-read "commits:"
              collection
              :unwind #'my-unwind-git-timemachine
              :action (lambda (rev)
                        (git-timemachine-show-revision (cdr rev))))))

(defun my-git-timemachine ()
  "Open git snapshot with the selected version.  Based on ivy-mode."
  (interactive)
  (unless (featurep 'git-timemachine)
    (require 'git-timemachine))
  (git-timemachine--start #'my-git-timemachine-show-selected-revision))

(defun czqhurricane/helm-hotspots ()
  "Helm interface to my hotspots, which includes my locations,
org-files and bookmarks"
  (interactive)
  (helm :buffer "*helm: utities*"
        :sources `(,(czqhurricane//hotspots-sources))))

(defun czqhurricane//hotspots-sources ()
  "Construct the helm sources for my hotspots"
  `((name . "Mail and News")
    (candidates . (("Calendar" . (lambda ()  (browse-url "https://www.google.com/calendar/render")))
                   ("RSS" . elfeed)
                   ("Blog" . blog-admin-start)
                   ("Github" . (lambda() (helm-github-stars)))
                   ("Calculator" . (lambda () (helm-calcul-expression)))
                   ("Run current flie" . (lambda () (czqhurricane/run-current-file)))
                   ("Agenda" . (lambda () (org-agenda "" "a")))
                   ("sicp" . (lambda() (browse-url "http://mitpress.mit.edu/sicp/full-text/book/book-Z-H-4.html#%_toc_start")))))
    (candidate-number-limit)
    (action . (("Open" . (lambda (x) (funcall x)))))))

(defun czqhurricane/now ()
  "Insert string for the current time formatted like '2:34 PM'."
  (interactive)                 ; permit invocation in minibuffer
  (insert (format-time-string "%D %-I:%M %p")))

(defun czqhurricane/today ()
  "Insert string for today's date nicely formatted in American style,
e.g. Sunday, September 17, 2000."
  (interactive)                 ; permit invocation in minibuffer
  (insert (format-time-string "%A, %B %e, %Y")))

;; https://github.com/syohex/emacs-browser-refresh/blob/master/browser-refresh.el
(defun czqhurricane/browser-refresh--chrome-applescript ()
  (interactive)
  (do-applescript
   (format
    "
  tell application \"Chrome\"
    set winref to a reference to (first window whose title does not start with \"Developer Tools - \")
    set winref's index to 1
    reload active tab of winref
  end tell
" )))

(define-minor-mode
  shadowsocks-proxy-mode
  :global t
  :init-value nil
  :lighter " SS"
  (if shadowsocks-proxy-mode
      (setq url-gateway-method 'socks)
    (setq url-gateway-method 'native)))

(define-global-minor-mode
  global-shadowsocks-proxy-mode shadowsocks-proxy-mode shadowsocks-proxy-mode
  :group 'shadowsocks-proxy)

(defun czqhurricane/open-file-with-projectile-or-counsel-git ()
  (interactive)
  (if (czqhurricane/git-project-root)
      (counsel-git)
    (if (projectile-project-p)
        (projectile-find-file)
      (counsel-file-jump))))

(defun czqhurricane/goto-match-paren (arg)
  "Go to the matching  if on (){}[], similar to vi style of % "
  (interactive "p")
  ;; first, check for "outside of bracket" positions expected by forward-sexp, etc
  (cond ((looking-at "[\[\(\{]") (evil-jump-item))
        ((looking-back "[\]\)\}]" 1) (evil-jump-item))
        ;; now, try to succeed from inside of a bracket
        ((looking-at "[\]\)\}]") (forward-char) (evil-jump-item))
        ((looking-back "[\[\(\{]" 1) (backward-char) (evil-jump-item))
        (t nil)))

(defun czqhurricane/hidden-dos-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))

(defun czqhurricane/remove-dos-eol ()
  "Replace DOS eolns CR LF with Unix eolns CR"
  (interactive)
  (goto-char (point-min))
  (while (search-forward "\r" nil t) (replace-match "")))

(defun czqhurricane/retrieve-chrome-current-tab-url ()
  "Get the URL of the active tab of the first window"
  (interactive)
  (let ((result (do-applescript
                 (concat
                  "set frontmostApplication to path to frontmost application\n"
                  "tell application \"Google Chrome\"\n"
                  "    set theUrl to get URL of active tab of first window\n"
                  "    set theResult to (get theUrl) \n"
                  "end tell\n"
                  "activate application (frontmostApplication as text)\n"
                  "set links to {}\n"
                  "copy theResult to the end of links\n"
                  "return links as string\n"))))
    (format "%s" (s-chop-suffix "\"" (s-chop-prefix "\"" result)))))

(defun czqhurricane/insert-chrome-current-tab-url ()
  "Get the URL of the active tab of the first window"
  (interactive)
  (insert (czqhurricane/retrieve-chrome-current-tab-url)))

(defun czqhurricane/copy-chrome-current-tab-url ()
  (interactive)
  (kill-new (czqhurricane/retrieve-chrome-current-tab-url)))

;; Remove all the duplicated emplies in current buffer
(defun czqhurricane/single-lines-only ()
  "Replace multiple blank lines with a single one"
  (interactive)
  (goto-char (point-min))
  (while (re-search-forward "\\(^\\s-*$\\)\n" nil t)
    (replace-match "\n")
    (forward-char 1)))

;; For running long run ansi-term
(defun czqhurricane/named-term (name)
  (interactive "sName: ")
  (ansi-term "/bin/zsh" name))

(defun czqhurricane/ash-term-hooks ()
  ;; Dabbrev-expand in term
  (define-key term-raw-escape-map "/"
    (lambda ()
      (interactive)
      (let ((beg (point)))
        (dabbrev-expand nil)
        (kill-region beg (point)))
      (term-send-raw-string (substring-no-properties (current-kill 0)))))
  ;; yank in term (bound to C-c C-y)
  (define-key term-raw-escape-map "\C-y"
    (lambda ()
      (interactive)
      (term-send-raw-string (current-kill 0)))))

(defun czqhurricane/terminal ()
  "Switch to terminal. Launch if nonexistent."
  (interactive)
  (if (get-buffer "*ansi-term*")
      (switch-to-buffer-other-window "*ansi-term*")
    (progn
      (split-window-right-and-focus)
      (ansi-term "/bin/zsh")))
  (get-buffer-process "*ansi-term*"))

(defalias 'tt 'czqhurricane/terminal)

;; {{
;; @see: http://kuanyui.github.io/2014/01/18/count-chinese-japanese-and-english-words-in-emacs/
;; Add count for chinese, mainly used for writing chinese blog post
(defvar wc-regexp-chinese-char-and-punc
  (rx (category chinese)))

(defvar wc-regexp-chinese-punc
  "[.,！？；：「」『』()、【】《》〈〉※—]")

(defvar wc-regexp-english-word
  "[a-zA-Z0-9-]+")

(defun czqhurricane/word-count-for-chinese ()
  "`比较精确地' 统计中/日/英文字数.
- 文章中的注解不算在字数内.
- 平假名与片假名亦包含在`中日字数' 内, 每个平/片假名都算单独一个字 (但片假名不含连音 `-').
- 英文只计算`单子数', 不含标点.
- 韩文不包含在内."
  (interactive)
  (let* ((v-buffer-string
          (progn
            (if (eq major-mode 'org-mode) ;; 去掉 org 文件的 OPTIONS(以#+开头)
                (setq v-buffer-string (replace-regexp-in-string "^#\\+.+" ""
                                                                (buffer-substring-no-properties (point-min) (point-max))))
              (setq v-buffer-string (buffer-substring-no-properties (point-min) (point-max))))
            (replace-regexp-in-string (format "^ *%s *.+" comment-start) "" v-buffer-string)))
                                          ;; 把注释行删掉(不把注释算进字数内).
         (chinese-char-and-punc 0)
         (chinese-punc 0)
         (english-word 0)
         (chinese-char 0))
    (with-temp-buffer
      (insert v-buffer-string)
      (goto-char (point-min))
      ;; 中文(含标点, 片假名).
      (while (re-search-forward wc-regexp-chinese-char-and-punc nil :no-error)
        (setq chinese-char-and-punc (1+ chinese-char-and-punc)))
      ;; 中文标点符号.
      (goto-char (point-min))
      (while (re-search-forward wc-regexp-chinese-punc nil :no-error)
        (setq chinese-punc (1+ chinese-punc)))
      ;; 英文字数(不含标点).
      (goto-char (point-min))
      (while (re-search-forward wc-regexp-english-word nil :no-error)
        (setq english-word (1+ english-word))))
    (setq chinese-char (- chinese-char-and-punc chinese-punc))
    (message
     (format "中日文字数(不含标点):%s
中日文字数(包含标点):%s
英文字数(不含标点):%s
=======================
中英文合计(不含标点):%s"
             chinese-char chinese-char-and-punc english-word
             (+ chinese-char english-word)))))
;; }}

(defun czqhurricane/evil-quick-replace (beg end )
  (interactive "r")
  (when (evil-visual-state-p)
    (evil-exit-visual-state)
    (let ((selection (regexp-quote (buffer-substring-no-properties beg end))))
      (setq command-string (format "%%s /%s//g" selection))
      (minibuffer-with-setup-hook
          (lambda () (backward-char 2))
        (evil-ex command-string)))))

(defun czqhurricane/git-project-root ()
  "Return the project root for current buffer."
  (let ((directory default-directory))
    (locate-dominating-file directory ".git")))

;; {{
;; @see: http://xuchunyang.me/Opening-iTerm-From-an-Emacs-Buffer/
(defun czqhurricane/iterm-shell-command (command &optional prefix)
  "Cd to `default-directory' then run COMMAND in iTerm.
with PREFIX, cd to project root."
  (interactive (list (read-shell-command
                      "iTerm Shell Command: ")
                     current-prefix-arg))
  (let* ((dir (if prefix (czqhurricane/git-project-root)
                default-directory))
         ;; If COMMAND is empty, just change directory.
         (cmd (format "cd %s ;%s" dir command)))
    (do-applescript
     (format
      "
tell application \"iTerm2\"
     activate
     set _session to current session of current window
     tell _session
          set command to get the clipboard
          write text \"%s\"
     end tell
end tell
  " cmd))))
;; }}

(defadvice persp-switch (after my-quit-helm-perspectives activate)
  (setq hydra-deactivate t))

(defun czqhurricane/my-mc-mark-next-like-this ()
  (interactive)
  (if (region-active-p)
      (mc/mark-next-like-this 1)
    (er/expand-region 1)))

(defun wrap-sexp-with-new-round-parens ()
  (interactive)
  (insert "()")
  (backward-char)
  (sp-forward-slurp-sexp))

(defun evil-paste-after-from-0 ()
  (interactive)
  (let ((evil-this-register ?0))
    (call-interactively 'evil-paste-after)))

(defun my-erc-hook (match-type nick message)
  "Shows a terminal notification, when user's nick was mentioned.
If the buffer is currently not visible, makes it sticky."
  (unless (posix-string-match "^\\** *Users on #" message)
    (czqhurricane/notify-osx
     (concat "ERC: : " (buffer-name (current-buffer)))
     message
     t
     )))

(defun my-swiper-search (p)
  (interactive "P")
  (let ((current-prefix-arg nil))
    (call-interactively
     (if p #'spacemacs/swiper-region-or-symbol
       #'counsel-grep-or-swiper))))

(defun ivy-ff-checksum ()
  "Calculate the checksum of FILE. The checksum is copied to kill-ring."
  (interactive)
  (let ((file (expand-file-name (ivy-state-current ivy-last) ivy--directory))
        (algo (intern (ivy-read
                       "Algorithm: "
                       '(md5 sha1 sha224 sha256 sha384 sha512)))))
    (kill-new (with-temp-buffer
                (insert-file-contents-literally file)
                (secure-hash algo (current-buffer))))
    (message "Checksum copied to kill-ring.")))

(defun ivy-ff-checksum-action (x)
  (ivy-ff-checksum))

(defun my-find-file-in-git-repo (repo)
  (if (file-directory-p repo)
      (let* ((default-directory repo)
             (files (split-string (shell-command-to-string (format "cd %s && git ls-files" repo)) "\n" t)))
        (ivy-read "files:" files
                  :action 'find-file
                  :caller 'my-find-file-in-git-repo))
    (message "%s is not a valid directory." repo)))

(defun my-open-file-in-external-app (file)
  "Open file in external application."
  (interactive)
  (let ((default-directory (czqhurricane/git-project-root))
        (file-path file))
    (if file-path
        (cond
         ((spacemacs/system-is-mswindows) (w32-shell-execute "open" (replace-regexp-in-string "/" "\\\\" file-path)))
         ((spacemacs/system-is-mac) (shell-command (format "open \"%s\"" file-path)))
         ((spacemacs/system-is-linux) (let ((process-connection-type nil))
                                        (start-process "" nil "xdg-open" file-path))))
      (message "No file associated to this buffer."))))

(defun ivy-insert-action (x)
  (with-ivy-window
    (insert x)))

(defun ivy-kill-new-action (x)
  (with-ivy-window
    (kill-new x)))

(defun counsel-goto-recent-directory ()
  "Recent directories"
  (interactive)
  (unless recentf-mode (recentf-mode 1))
  (let ((collection
         (delete-dups
          (append (mapcar 'file-name-directory recentf-list)
                  ;; fasd history
                  (if (executable-find "fasd")
                      (split-string (shell-command-to-string "fasd -ld") "\n" t))))))
    (ivy-read "directories:" collection
              :action 'dired
              :caller 'counsel-goto-recent-directory)))

(defun counsel-find-file-recent-directory ()
  "Find file in recent git repository."
  (interactive)
  (unless recentf-mode (recentf-mode 1))
  (let ((collection
         (delete-dups
          (append (mapcar 'file-name-directory recentf-list)
                  ;; fasd history
                  (if (executable-find "fasd")
                      (split-string (shell-command-to-string "fasd -ld") "\n" t))))))
    (ivy-read "directories:" collection
              :action 'my-find-file-in-git-repo
              :caller 'counsel-find-file-recent-directory)))

(defun czqhurricane/magit-visit-pull-request ()
  "Visit the current branch's PR on GitHub."
  (interactive)
  (let ((remote-branch (magit-get-current-branch)))
    (cond
     ((null remote-branch)
      (message "No remote branch"))
     (t
      (browse-url
       (format "%s"
               (replace-regexp-in-string
                "\\`.+github\\.com:\\(.+\\)\\.git\\'" "\\1"
                (magit-get "remote"
                           (magit-get-remote)
                           "url"))
               remote-branch))))))

(defun czqhurricane/markdown-to-html ()
  (interactive)
  (start-process "grip" "*gfm-to-html*" "grip" (buffer-file-name) "5000")
  (browse-url (format "http://localhost:5000/%s.%s" (file-name-base) (file-name-extension (buffer-file-name)))))

(defun github-browse-file--relative-url ()
  "Return \"username/repo\" for current repository.
  Error out if this isn't a GitHub repo."
  (require 'vc-git)
  (let ((url (vc-git--run-command-string nil "config" "remote.origin.url")))
    (unless url (error "Not in a GitHub repo"))
    (when (and url (string-match "github.com:?/?\\(.*\\)" url))
      (replace-regexp-in-string "\\.git$" "" (match-string 1 url)))))

(defun czqhurricane/github-browse-commit ()
  "Show the GitHub page for the current commit."
  (interactive)
  (let* ((commit git-messenger:last-commit-id)
         (url (concat "https://github.com/"
                      (github-browse-file--relative-url)
                      "/commit/"
                      commit)))
    (browse-url url)
    (git-messenger:popup-close)))

(defun czqhurricane/search-in-fireball ()
  (interactive)
  (helm-do-ag (expand-file-name "~/Github/fireball/")))

(defun czqhurricane/show-current-buffer-major-mode ()
  (interactive)
  (describe-variable 'major-mode))

(defun czqhurricane/counsel-imenu ()
  (interactive)
  (counsel-imenu)
  (evil-set-jump))

(defun append-string-to-file (string filename)
  "Append STRING to FILENAME"
  (interactive)
  (append-to-file string nil filename))

(defun get-filename-from-url (url)
  "Get the substring of url which after the final slash."
  (with-temp-buffer
    (insert url)
    (let ((filename-origin 0))
      (goto-char filename-origin)
      (while (string-match "/" url filename-origin)
        (progn
          (goto-char filename-origin)
          (setq filename-start (re-search-forward "/"))
          (setq filename-origin (match-end 0))))
      (substring (buffer-string) (- filename-start 1)))))

(defun pandoc-converter (input-file output-file read-format write-format)
  "Call pandoc-mode to convert file."
  (let ((command-string (concat "pandoc " input-file " -f " read-format " -t "
                                write-format " -s -o " output-file)))
    (shell-command command-string)))

(defun install-monitor (file secs func)
  "Pretend to monitor the given file (AS FILE) by issuing a check every secs (AS SECS) seconds.
If a change in `file-attributes` happended call func."
  (let ((monitor-attributes (file-attributes file))
        (fun func))
    (run-with-timer
     0 secs
     (lambda (f p)
       (let ((att (file-attributes f)))
         (unless (or (null monitor-attributes) (equalp monitor-attributes att))
           (funcall fun))
         (setq monitor-attributes att)))
     file secs)))

(defun install-monitor-file-exists (file secs func)
  (setq inner-timer
    (run-with-idle-timer
     secs t
     (lambda (file func)
       (let ((file file)
             (func func))
        (unless (not (file-exists-p file))
          (progn
            (funcall func)
            (cancel-timer inner-timer)))))
     file func)))

(defun unexpected-strings-filter (filename replace-string-rule-lists)
  (with-temp-buffer
    (insert-file-contents filename)
    (dolist (replace-string-rule replace-string-rule-lists)
      (replace-in-the-entire-buffer (car replace-string-rule) (cdr replace-string-rule) nil))
    (write-file filename)))

(defvar question-string-pattern-list
  '("<div class=\"post-text\" itemprop=\"text\">" . "</div>"))
(defvar answer-string-pattern-list
  '("\\(<div class=\"answercell post-layout--right\">\\|<div style=\"display: block;\" class=\"comment-body\">\\|<div class=\"comment-body\" style=\"display: block;\" >\\)" . "</div>"))
;; Used to replace unexpected strings in raw extracted html file.
(defvar html-replace-string-rule-lists
  '(("<span class=\"comment-date\" dir=\"ltr\">\.*" . "")
                                         ("<span\[^>\]*>" . "<blockquote>")
                                         ("</span\[^>\]*>" . "</blockquote>")
                                         ("<h1>" . "")
                                         ("<h2>" . "")
                                         ("<h3>" . "")
                                         ("<h4>" . "")
                                         ("<h5>" . "")
                                         ("<h6>" . "")
                                         ("</h1>" . "")
                                         ("</h2>" . "")
                                         ("</h3>" . "")
                                         ("</h4>" . "")
                                         ("</h5>" . "")
                                         ("</h6>" . "")
                                         ("{%" . "<")
                                         ("%}" . ">")))

(defvar image-url-pattern-list '("<img src=\"" . "\""))

(defun download-all-images (file)
  ;; Use `org-download-image' to download image from `html' file.
  (with-temp-buffer
    (insert-file-contents file)
    (let ((search-origin 0) )
      (while (string-match (car image-url-pattern-list) (buffer-string) search-origin)
        (progn
          (goto-char search-origin)
          (setq image-url-start (re-search-forward (car image-url-pattern-list)))
          (setq image-url-end (re-search-forward (cdr image-url-pattern-list)))
          (setq image-url (buffer-substring  image-url-start (- image-url-end 1)))
          (if image-url
              (progn
                (setq org-download-image-dir (concat default-directory "/screenshotImg"))
                (org-download-image image-url)
                (setq search-origin (match-end 0)))))))))

(defun extract-content-from-stackoverflow-to-org-file (src-code-type)
  ;; Insert a `SRC-CODE-TYPE' type source code block in org-mode.
  (interactive
   (let ((src-code-types
          '("ipython" "emacs-lisp" "python" "comment" "C" "sh" "java" "js" "clojure" "C++" "css"
            "calc" "asymptote" "dot" "gnuplot" "ledger" "lilypond" "mscgen"
            "octave" "oz" "plantuml" "R" "sass" "screen" "sql" "awk" "ditaa"
            "haskell" "latex" "lisp" "matlab" "ocaml" "org" "perl" "ruby"
            "scheme" "sqlite" "graphviz")))
     (list (ido-completing-read "Source code type: " src-code-types))))

    (setq begin-marker "" url "" html-file-name "" org-file-name "")
    ;; Used to replace unexpected strings in org file generated by pandoc.
    (setq org-replace-string-rule-lists '(("#\\+BEGIN_QUOTE\[\t\r\n \]*#\\+END_QUOTE" .  "")
                                            ("\\\\_" . "_")
                                            ("#\\+END_EXAMPLE" . "#+END_SRC")
                                            ("\\[\\[\.*png\.*/\\(.*\\)\\]\\]" . "[[file:screenshotImg/\\1")
                                            ("\\[\\[http.*/" . "[[file:screenshotImg/")))
    ;; Read url string from minibuffer, while the string read is empty, this loop will not stop.
    (setq url "")
    (while (string-equal url "")
      (setq url (read-string "Please input the StackOverFlow url to extract: "
                             nil nil "" nil)))
    ;; Create begin-marker used to replace unexpected string `#+BEGIN_EXAMPLE'.
    (cond ((string-equal src-code-type "")
           (setq begin-marker (concat "#+BEGIN_SRC " "python")))
          (t
           (setq begin-marker (concat "#+BEGIN_SRC " src-code-type))))
    (add-to-list 'org-replace-string-rule-lists `("#\\+BEGIN_EXAMPLE" . ,begin-marker))
    ;; Get filename from url to create html and org file.
    (setq html-file-name (concat
                          (concat "~/"
                                  (get-filename-from-url url))
                          ".html")
          org-file-name (concat
                         (concat "~/"
                                 (get-filename-from-url url))
                         ".org"))
    ;; Extract content to file.
    (with-current-buffer (url-retrieve-synchronously url)
      ;; Remove the "^M" character in html file.
      (dos2unix)
      ;; Extract question strings to file.
      (progn
        (goto-char 0)
        (setq question-start (re-search-forward (car question-string-pattern-list)))
        (goto-char question-start)
        (setq question-end (re-search-forward (cdr question-string-pattern-list)))
        (setq question-string (buffer-substring question-start (- question-end 6)))
        (append-string-to-file "{%h1%}Question{%/h1%}" html-file-name)
        (append-string-to-file question-string html-file-name))
      ;; Extract image and comment strings to file.
      (let ((answer-search-origin 0) (answer-number 1))
        (while (string-match (car answer-string-pattern-list) (buffer-string) answer-search-origin)
          (progn
            (goto-char answer-search-origin)
            (setq answer-string-start (re-search-forward (car answer-string-pattern-list)))
            (setq answer-string-end (re-search-forward (cdr answer-string-pattern-list)))
            (setq answer-string (buffer-substring  answer-string-start (- answer-string-end 6)))
            (if answer-string
                (progn
                  (if (string-suffix-p "</span>" (replace-regexp-in-string "[\t\n\r ]+" "" answer-string))
                      (append-string-to-file "{%h2%}Comment{%/h2%}" html-file-name)
                    (progn
                      (append-string-to-file (concat (concat "{%h1%}Answer" (number-to-string answer-number)) "{%/h1%}") html-file-name)
                      (setq answer-number (+ 1 answer-number))))
                  (append-string-to-file answer-string html-file-name)
                  (setq answer-search-origin (match-end 0))))))))

    (unexpected-strings-filter html-file-name html-replace-string-rule-lists)

    (pandoc-converter html-file-name org-file-name "html" "org")

    (defun callback-unexpected-strings-filter ()
      (unexpected-strings-filter org-file-name org-replace-string-rule-lists))

    (install-monitor-file-exists org-file-name 1 #'callback-unexpected-strings-filter)

    (append-string-to-file
     "\n#+BEGIN_SRC comment :results valuse list :exports both\n\n#+END_SRC"
     org-file-name)

    ;; Download all images.
    (download-all-images html-file-name)
)

(defun czqhurricane/org-as-mac-iTerm2-get-link ()
  (do-applescript
   (concat
    "tell application \"iTerm2\"\n"
    " set theName to custom title in tab 1 of window 1\n"
    " do script \"pwd | pbcopy\" in window 1\n"
    " set theUrl to do shell script \"pbpaste\"\n"
    " return theUrl & \"::split::\" & theName\n"
    "end tell")))

(defun czqhurricane/autoinsert-yas-expand ()
  "Replace text in yasnippet template."
  (yas-expand-snippet (buffer-string) (point-min) (point-max)))

