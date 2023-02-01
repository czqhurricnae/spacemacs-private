(defconst sys/macp
  (eq system-type 'darwin)
  "Are we running on a Mac system?")

;; @see: https://bitbucket.org/lyro/evil/issue/511/let-certain-minor-modes-key-bindings
(defmacro adjust-major-mode-keymap-with-evil (m &optional r)
  `(eval-after-load (quote ,(if r r m))
     '(progn
        (evil-make-overriding-map ,(intern (concat m "-mode-map")) 'normal)
        ;; Force update evil keymaps after git-timemachine-mode loaded.
        (add-hook (quote ,(intern (concat m "-mode-hook"))) #'evil-normalize-keymaps))))

(defun hurricane/insert-semicolon-at-the-end-of-this-line ()
  "Insert `;' at the end of current line."
  (interactive)
  (save-excursion
    (end-of-line)
    (insert ";")))

(defun hurricane/delete-semicolon-at-the-end-of-this-line ()
  (interactive)
  (save-excursion
    (end-of-line)
    (if (looking-back ";")
        (progn
          (backward-char)
          (delete-char 1)))))

(defun hurricane/insert-comma-at-the-end-of-this-line ()
  (interactive)
  (save-excursion
    (end-of-line)
    (insert ",")))

(defun hurricane/delete-comma-at-the-end-of-this-line ()
  (interactive)
  (save-excursion
    (end-of-line)
    (if (looking-back ",")
        (progn
          (backward-char)
          (delete-char 1)))))

(defun hurricane/load-my-layout ()
  (interactive)
  (persp-load-state-from-file (concat persp-save-dir "hurricane")))

(defun hurricane/save-my-layout ()
  (interactive)
  (persp-save-state-to-file (concat persp-save-dir "hurricane")))

;; {{
;; @see: http://blog.binchen.org/posts/use-ivy-mode-to-search-bash-history.html
;; @see: http://wikemacs.org/wiki/Shell#Search_the_bash.2C_zsh_or_fish_history_with_Ivy-mode
(defun hurricane/counsel-yank-bash-history ()
  "Yank the `zsh' history."
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
;; }}

(defun hurricane//indent-region(spaces)
  (progn
    ;; Default to start and end of current line.
    (setq start (line-beginning-position))
    (setq end (line-end-position))

    ;; If there's a selection, use that instead of the current line.
    (when (use-region-p)
      (setq start (region-beginning))
      (setq end (region-end))
      )

    (save-excursion                          ;; Restore the position afterwards.
      (goto-char start)                      ;; Go to the start of region.
      (setq start (line-beginning-position)) ;; Save the start of the line.
      (goto-char end)                        ;; Go to the end of region.
      (setq end (line-end-position))         ;; Save the end of the line.

      (indent-rigidly start end spaces)      ;; Indent between start and end.
      (setq deactivate-mark nil)             ;; Restore the selected region.
      )))

(defun hurricane/tab-region (N)
  (interactive "p")
  (if (use-region-p)
      (hurricane//indent-region 4)            ;; Region was selected, call indent-region.
    (insert "    ")))                        ;; Else insert four spaces as expected.

(defun hurricane/untab-region (N)
  (interactive "p")
  (hurricane//indent-region -4))

(defun hurricane/hack-tab-key ()
  (interactive)
  (local-set-key (kbd "<tab>") 'hurricane/tab-region)
  (local-set-key (kbd "<S-tab>") 'hurricane/untab-region))

;; I'm don't like this settings too much.
;; (add-hook 'prog-mode-hook 'hurricane/hack-tab-key)
(defun endless/fill-or-unfill ()
  "Like `fill-paragraph', but unfill if used twice."
  (interactive)
  (let ((fill-column
         (if (eq last-command 'endless/fill-or-unfill)
             (progn (setq this-command nil)
                    (point-max))
           fill-column)))
    (call-interactively #'fill-paragraph)))

(defun hurricane//unwind-git-timemachine ()
  (if (not (eq last-command-event 13))
      (git-timemachine-quit)))

;; @see: http://blog.binchen.org/posts/new-git-timemachine-ui-based-on-ivy-mode.html
(defun hurricane//git-timemachine-show-selected-revision ()
  "Show last (current) revision of file."
  (let (collection)
    (setq collection
          (mapcar (lambda (rev)
                    ;; Re-shape list for the ivy-read.
                    (cons (concat (substring (nth 0 rev) 0 7) "|" (nth 3 rev) "|" (nth 5 rev) "|" (nth 6 rev)) rev))
                  (git-timemachine--revisions)))
    (ivy-read "commits:"
              collection
              :unwind #'hurricane//unwind-git-timemachine
              :action (lambda (rev)
                        (git-timemachine-show-revision (cdr rev))))))

(defun hurricane/git-timemachine ()
  "Open `git snapshot' with the selected version.
Based on ivy-mode."
  (interactive)
  (unless (featurep 'git-timemachine)
    (require 'git-timemachine))
  (git-timemachine--start #'hurricane//git-timemachine-show-selected-revision))

(defun hurricane/helm-hotspots ()
  "Helm interface to my hotspots, which includes my locations,
org-files and bookmarks."
  (interactive)
  (helm :buffer "*helm: utities*"
        :sources `(,(hurricane//hotspots-sources))))

(defun hurricane//hotspots-sources ()
  "Construct the helm sources for my hotspots."
  `((name . "Mail and News")
    (candidates . (
                   ("Github" . (lambda() (helm-github-stars)))
                   ("Calculator" . (lambda () (helm-calcul-expression)))
                   ("Run current flie" . (lambda () (hurricane/run-current-file)))
                   ("Agenda" . (lambda () (org-agenda "" "a")))))
    (candidate-number-limit)
    (action . (("Open" . (lambda (x) (funcall x)))))))

(defun hurricane/now ()
  "Insert string for the current time formatted like `2:34 PM'."
  ;; Permit invocation in minibuffer.
  (interactive)
  (insert (format-time-string "%D %-I:%M %p")))

(defun hurricane/today ()
  "Insert string for today's date nicely formatted in American style,
e.g. `Sunday, September 17, 2000'."
  ;; Permit invocation in minibuffer.
  (interactive)
  (insert (format-time-string "%A, %B %e, %Y")))

;; @see: https://github.com/syohex/emacs-browser-refresh/blob/master/browser-refresh.el
(defun hurricane//browser-refresh--chrome-applescript ()
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

(defun hurricane/goto-match-paren (arg)
  "Go to the matching  if on `(){}[]', similar to vi style of `%'."
  (interactive "p")
  ;; First, check for "outside of bracket" positions expected by forward-sexp, etc.
  (cond ((looking-at "[\[\(\{]") (evil-jump-item))
        ((looking-back "[\]\)\}]" 1) (evil-jump-item))
        ;; Now, try to succeed from inside of a bracket.
        ((looking-at "[\]\)\}]") (forward-char) (evil-jump-item))
        ((looking-back "[\[\(\{]" 1) (backward-char) (evil-jump-item))
        (t nil)))

(defun hurricane/hidden-dos-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))

(defun hurricane/remove-dos-eol ()
  "Replace DOS eolns CR LF with Unix eolns CR."
  (interactive)
  (goto-char (point-min))
  (while (search-forward "\r" nil t) (replace-match "")))

(defun hurricane//retrieve-chrome-current-tab-url ()
  "Get the `URL' of the active tab of the first window."
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

(defun hurricane/insert-chrome-current-tab-url ()
  "Insert the `URL' of the active tab of the first window."
  (interactive)
  (insert (hurricane//retrieve-chrome-current-tab-url)))

(defun hurricane/copy-chrome-current-tab-url ()
  (interactive)
  (kill-new (hurricane//retrieve-chrome-current-tab-url)))

;; Remove all the duplicated emplies in current buffer.
(defun hurricane/single-lines-only ()
  "Replace multiple blank lines with a single one."
  (interactive)
  (goto-char (point-min))
  (while (re-search-forward "\\(^\\s-*$\\)\n" nil t)
    (replace-match "\n")
    (forward-char 1)))

;; For running long run ansi-term.
(defun hurricane/named-term (name)
  (interactive "sName: ")
  (ansi-term "/bin/zsh" name))

(defun hurricane/ash-term-hooks ()
  ;; Dabbrev-expand in term.
  (define-key term-raw-escape-map "/"
    (lambda ()
      (interactive)
      (let ((beg (point)))
        (dabbrev-expand nil)
        (kill-region beg (point)))
      (term-send-raw-string (substring-no-properties (current-kill 0)))))
  ;; Yank in term (bound to C-c C-y).
  (define-key term-raw-escape-map "\C-y"
    (lambda ()
      (interactive)
      (term-send-raw-string (current-kill 0)))))

(defun hurricane/terminal ()
  "Switch to terminal. Launch if nonexistent."
  (interactive)
  (if (get-buffer "*ansi-term*")
      (switch-to-buffer-other-window "*ansi-term*")
    (progn
      (split-window-right-and-focus)
      (ansi-term "/bin/zsh")))
  (get-buffer-process "*ansi-term*"))

(defalias 'tt 'hurricane/terminal)

;; {{
;; @see: http://kuanyui.github.io/2014/01/18/count-chinese-japanese-and-english-words-in-emacs/
;; Add count for chinese, mainly used for writing chinese blog post.
(defvar wc-regexp-chinese-char-and-punc
  (rx (category chinese)))

(defvar wc-regexp-chinese-punc
  "[.,!?,:「」『』(),[]《》〈〉※—]")

(defvar wc-regexp-english-word
  "[a-zA-Z0-9-]+")

(defun hurricane/word-count-for-chinese ()
  "比较精确地统计中/日/英文字数.
- 文章中的注解不算在字数内.
- 平假名与片假名亦包含在 `中日字数' 内, 每个平/片假名都算单独一个字 (但片假名不含连音 `-').
- 英文只计算 `单子数', 不含标点.
- 韩文不包含在内."
  (interactive)
  (let* ((v-buffer-string
          (progn
            (if (eq major-mode 'org-mode) ;; 去掉 org 文件的 OPTIONS (以#+开头).
                (setq v-buffer-string (replace-regexp-in-string "^#\\+.+" ""
                                                                (buffer-substring-no-properties (point-min) (point-max))))
              (setq v-buffer-string (buffer-substring-no-properties (point-min) (point-max))))
            (replace-regexp-in-string (format "^ *%s *.+" comment-start) "" v-buffer-string)))
                                          ;; 把注释行删掉 (不把注释算进字数内).
         (chinese-char-and-punc 0)
         (chinese-punc 0)
         (english-word 0)
         (chinese-char 0))
    (with-temp-buffer
      (insert v-buffer-string)
      (goto-char (point-min))
      ;; 中文 (含标点, 片假名).
      (while (re-search-forward wc-regexp-chinese-char-and-punc nil :no-error)
        (setq chinese-char-and-punc (1+ chinese-char-and-punc)))
      ;; 中文标点符号.
      (goto-char (point-min))
      (while (re-search-forward wc-regexp-chinese-punc nil :no-error)
        (setq chinese-punc (1+ chinese-punc)))
      ;; 英文字数 (不含标点).
      (goto-char (point-min))
      (while (re-search-forward wc-regexp-english-word nil :no-error)
        (setq english-word (1+ english-word))))
    (setq chinese-char (- chinese-char-and-punc chinese-punc))
    (message
     (format "中日文字数 (不含标点): %s
中日文字数 (包含标点): %s
英文字数 (不含标点): %s
=======================
中英文合计 (不含标点): %s"
             chinese-char chinese-char-and-punc english-word
             (+ chinese-char english-word)))))
;; }}

(defun hurricane//evil-quick-replace (beg end)
  (interactive "r")
  (when (evil-visual-state-p)
    (evil-exit-visual-state)
    (let ((selection (regexp-quote (buffer-substring-no-properties beg end))))
      (setq command-string (format "%%s /%s//g" selection))
      (minibuffer-with-setup-hook
          (lambda () (backward-char 2))
        (evil-ex command-string)))))

(defun hurricane//git-project-root ()
  "Return the project root for current buffer."
  (let ((directory default-directory))
    (locate-dominating-file directory ".git")))

;; {{
;; @see: http://xuchunyang.me/Opening-iTerm-From-an-Emacs-Buffer/
(defun hurricane//iterm-shell-command (command &optional prefix)
  "Cd to `default-directory' then run COMMAND in iTerm.
with PREFIX, cd to project root."
  (interactive (list (read-shell-command
                      "iTerm Shell Command: ")
                     current-prefix-arg))
  (let* ((dir (if prefix (hurricane//git-project-root)
                default-directory))
         ;; If `command' is empty, just change directory.
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

(defun hurricane//my-mc-mark-next-like-this ()
  (interactive)
  (if (region-active-p)
      (mc/mark-next-like-this 1)
    (er/expand-region 1)))

(defun hurricane/wrap-sexp-with-new-round-parens ()
  (interactive)
  (insert "()")
  (backward-char)
  (sp-forward-slurp-sexp))

(defun hurricane//evil-paste-after-from-0 ()
  (interactive)
  (let ((evil-this-register ?0))
    (call-interactively 'evil-paste-after)))

(defun hurricane//erc-hook (match-type nick message)
  "Shows a terminal notification, when user's nick was mentioned.
If the buffer is currently not visible, makes it sticky."
  (unless (posix-string-match "^\\** *Users on #" message)
    (hurricane//notify-osx
     (concat "ERC: : " (buffer-name (current-buffer)))
     message
     t)))

(defun hurricane/swiper-search (p)
  (interactive "P")
  (let ((current-prefix-arg nil))
    (call-interactively
     (if p #'swiper-thing-at-point
       #'counsel-grep-or-swiper))))

(defun hurricane/counsel-goto-recent-directory ()
  "Recent directories."
  (interactive)
  (unless recentf-mode (recentf-mode 1))
  (let ((collection
         (delete-dups
          (append (mapcar 'file-name-directory recentf-list)
                  ;; Fasd history.
                  (if (executable-find "fasd")
                      (split-string (shell-command-to-string "fasd -ld") "\n" t))))))
    (ivy-read "directories:" collection
              :action 'dired
              :caller 'hurricane/counsel-goto-recent-directory)))

(defun hurricane/counsel-find-file-recent-directory ()
  "Find file in recent git repository."
  (interactive)
  (unless recentf-mode (recentf-mode 1))
  (let ((collection
         (delete-dups
          (append (mapcar 'file-name-directory recentf-list)
                  ;; Fasd history.
                  (if (executable-find "fasd")
                      (split-string (shell-command-to-string "fasd -ld") "\n" t))))))
    (shell-command "git config --global core.quotepath false")
    (ivy-read "directories:" collection
              :action 'hurricane//find-file-in-git-repo
              :caller 'hurricane/counsel-find-file-recent-directory)))

(defun hurricane//magit-visit-pull-request ()
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

(defun hurricane//markdown-to-html ()
  (interactive)
  (start-process "grip" "*gfm-to-html*" "grip" (buffer-file-name) "5000")
  (browse-url (format "http://localhost:5000/%s.%s" (file-name-base) (file-name-extension (buffer-file-name)))))

(defun hurricane//github-browse-file--relative-url ()
  "Return `username/repo' for current repository.
Error out if this isn't a GitHub repo."
  (require 'vc-git)
  (let ((url (vc-git--run-command-string nil "config" "remote.origin.url")))
    (unless url (error "Not in a GitHub repo"))
    (when (and url (string-match "github.com:?/?\\(.*\\)" url))
      (replace-regexp-in-string "\\.git$" "" (match-string 1 url)))))

(defun hurricane/github-browse-commit ()
  "Show the GitHub page for the current commit."
  (interactive)
  (let* ((commit git-messenger:last-commit-id)
         (url (concat "https://github.com/"
                      (hurricane//github-browse-file--relative-url)
                      "/commit/"
                      commit)))
    (browse-url url)
    (git-messenger:popup-close)))

(defun hurricane/show-current-buffer-major-mode ()
  (interactive)
  (describe-variable 'major-mode))

(defun hurricane/counsel-imenu ()
  (interactive)
  (counsel-imenu)
  (evil-set-jump))

(defun append-string-to-file (string file-name)
  "Append STRING to FILE-NAME."
  (interactive)
  (append-to-file string nil file-name))

(defun get-file-name-from-url (url)
  "Get the substring of URL which after the final slash."
  (with-temp-buffer
    (insert url)
    (let ((file-name-origin 0))
      (goto-char file-name-origin)
      (while (string-match "/" url file-name-origin)
        (progn
          (goto-char file-name-origin)
          (setq file-name-start (re-search-forward "/"))
          (setq file-name-origin (match-end 0))))
      (substring (buffer-string) (- file-name-start 1)))))

(defun pandoc-converter (input-file-name output-file-name read-format write-format)
  "Call pandoc-mode to convert file."
  (let* ((input-file (shell-quote-argument input-file-name))
         (output-file (shell-quote-argument output-file-name))
         (command-string (concat "pandoc " input-file " -f " read-format " -t "
                                write-format " -s -o " output-file)))
    (shell-command command-string)))

(defun install-monitor (file secs func)
  "Pretend to monitor the given FILE by issuing a check every SECS seconds.
If a change in `file-attributes' happended call FUNC."
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

(defun replace-unexpected-string-in-file (file-name replace-string-rule-lists)
  (with-temp-buffer
    (insert-file-contents file-name)
    (dolist (replace-string-rule replace-string-rule-lists)
      (replace-region-or-buffer (car replace-string-rule) (cdr replace-string-rule) nil))
    (write-file file-name)))

(defvar unhtml-string-pattern-list
  '(("&amp;" . "&")
    ("&lt;" . "<")
    ("&gt;" . ">")
    ))

(defun unshift-prefix-if-necessary (string prefix new-prefix)
  "If STRING do not starts with PREFIX, concat NEW-PREFIX STRING and then encode.
Else, returns STRING."
  (if (not (string-prefix-p prefix string))
      (browse-url-encode-url (concat new-prefix string))
    (progn
      (with-temp-buffer
        (insert string)
        (dolist (replace-string-rule unhtml-string-pattern-list)
          (replace-region-or-buffer (car replace-string-rule) (cdr replace-string-rule) nil))
        (buffer-string))))
  )

(defun replace-url-with-file-path-in-org (file-name url-and-file-path-map-list)
  (with-temp-buffer
    (insert-file-contents (concat file-name ".org"))
    (let ((image-directory (concat org-screenshot-image-dir-name "/" file-name)))
      (dolist (url-file-path-map url-and-file-path-map-list)
      (progn
        (goto-char (point-min))
        (replace-string (unshift-prefix-if-necessary (car url-file-path-map) "http" "file:") (concat "file:" image-directory "/" (cdr url-file-path-map))))))
    (write-file (concat file-name ".org"))))

(defvar question-string-pattern-list
  '("<div class=\"s-prose js-post-body\" itemprop=\"text\">\\|<div class=\"post-text\" itemprop=\"text\">" . "</div>"))

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

(defvar stackoverflow-image-url-pattern-list '("<img src=\"" . "\""))

(defvar stackoverflow-all-images-url-list)

(defun hurricane/extract-content-from-stackoverflow-to-org-file (src-code-type)
  (interactive
   (let ((src-code-type
          '("ipython" "emacs-lisp" "python" "comment" "C" "sh" "java" "js" "clojure" "C++" "css"
            "calc" "asymptote" "dot" "gnuplot" "ledger" "lilypond" "mscgen"
            "octave" "oz" "plantuml" "R" "sass" "screen" "sql" "awk" "ditaa"
            "haskell" "latex" "lisp" "matlab" "ocaml" "org" "perl" "ruby"
            "scheme" "sqlite" "graphviz")))
     (list (ido-completing-read "Source code type: " src-code-type))))

    (setq begin-marker "" url "" html-file-name "" org-file-name "")

    ;; Read `url' string from minibuffer, while the string read is empty, this loop will not stop.
    (setq url "")
    (while (string-equal url "")
      (setq url (read-string "Please input the StackOverFlow url to extract: "
                             nil nil "" nil)))

    ;; Get `file-name' from `url' to create html and org file.
    (setq file-name (replace-regexp-in-string
                     "-"
                     "_"
                     (get-file-name-from-url url)))
    (setq html-file-name (concat
                          file-name
                          ".html")
          org-file-name (concat
                         file-name
                         ".org"))

    ;; Used to replace unexpected strings in org file generated by pandoc.
    (setq org-image-url-template (concat "[[file:./static/" file-name "/\\1"))
    (setq org-replace-string-rule-lists '(("#\\+BEGIN_QUOTE\[\t\r\n \]*#\\+END_QUOTE" . "")
                                          ("\\\\_" . "_")
                                          ("#\\+END_EXAMPLE" . "#+END_SRC")
                                          ;; ("\\[\\[http.*/" . "[[file:./static/")
                                          ))
    (add-to-list 'org-replace-string-rule-lists `("\\[\\[\.*jpeg\.*/\\(.*\\)\\]\\]" . ,org-image-url-template))

    ;; Create begin-marker used to replace unexpected string `#+BEGIN_EXAMPLE'.
    (cond ((string-equal src-code-type "")
           (setq begin-marker (concat "#+BEGIN_SRC " "python")))
          (t
           (setq begin-marker (concat "#+BEGIN_SRC " src-code-type))))
    (add-to-list 'org-replace-string-rule-lists `("#\\+BEGIN_EXAMPLE" . ,begin-marker))

    ;; Extract content to file.
    (with-current-buffer (url-retrieve-synchronously url)
      ;; Remove the `^M' character in html file.
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

    (replace-unexpected-string-in-file html-file-name html-replace-string-rule-lists)

    (setq stackoverflow-all-images-url-list (hurricane//get-all-images-url html-file-name stackoverflow-image-url-pattern-list))
    (message "%s" stackoverflow-all-images-url-list)

    ;; Download all images.
    (with-proxy (download-all-images stackoverflow-all-images-url-list file-name))

    (pandoc-converter html-file-name org-file-name "html" "org")

    (replace-url-with-file-path-in-org file-name stackoverflow-all-images-url-list)

    (defun callback-replace-unexpected-string-in-file ()
      (replace-unexpected-string-in-file org-file-name org-replace-string-rule-lists))

    (install-monitor-file-exists org-file-name 1 #'callback-replace-unexpected-string-in-file)
    (insert-header-to-org-content file-name))

(defun hurricane//org-as-mac-iTerm2-get-link ()
  (do-applescript
   (concat
    "tell application \"iTerm2\"\n"
    " set theName to custom title in tab 1 of window 1\n"
    " do script \"pwd | pbcopy\" in window 1\n"
    " set theUrl to do shell script \"pbpaste\"\n"
    " return theUrl & \"::split::\" & theName\n"
    "end tell")))

(defun hurricane//autoinsert-yas-expand ()
  "Replace text in yasnippet template."
  (yas-expand-snippet (buffer-string) (point-min) (point-max)))

;; {{
;; @see: https://emacs-china.org/t/macos/10219
;; (setq english-ID-map '("1" . "美国"))
;; (setq chinese-ID-map '("2" . "搜狗拼音"))

;; (defun hurricane//switch-input-source (ID-map)
;;   (let ((script
;;          (format
;;            (mapconcat
;;              #'identity
;;              '("tell application \"System Events\" to tell process \"SystemUIServer\""
;;                "set result to get the value of the first menu bar item of menu bar 1 whose description is \"text input\""
;;                "set englishInputSourceIsSelected to result is \"%s\""
;;                "  if englishInputSourceIsSelected is false then"
;;                "      click menu bar item 5 of menu bar 1"
;;                "      click menu item \"%s\" of menu 1 of menu bar item 5 of menu bar 1"
;;                "  end if"
;;                "end tell")
;;              "\n")
;;            (car ID-map)
;;            (cdr ID-map))))
;;     (thread-first script
;;       (do-applescript)
;;       (string-trim "\"\n" "\n\"")
;;       (split-string "\n"))))

;; (add-hook 'evil-insert-state-entry-hook (lambda () (hurricane//switch-input-source chinese-ID-map)))
;; (add-hook 'evil-insert-state-exit-hook (lambda () (hurricane//switch-input-source english-ID-map)))
;;}}

(setq english-ID-map '("com.apple.keylayout.US" . "美国"))
(setq chinese-ID-map '("com.sogou.inputmethod.sogou.pinyin" . "搜狗拼音"))

(defun hurricane//switch-input-source (ID-map)
  (if (not (string-equal (mac-input-source) (car ID-map)))
      (mac-select-input-source (car ID-map))))

;; (add-hook 'evil-insert-state-entry-hook (lambda () (hurricane//switch-input-source chinese-ID-map)))
(cond (sys/macp (add-hook 'evil-insert-state-exit-hook (lambda () (hurricane//switch-input-source english-ID-map)))))

;; {{
;; @see: https://emacs-china.org/t/topic/5518
(defun hurricane//chrome-tabs ()
  "Return `Chrome' tabs."
  (let ((script
         (mapconcat
          #'identity
          '("set titleString to return"
            ""
            "tell application \"Google Chrome\""
            "  set window_list to every window"
            "  set window_counter to 0"
            ""
            "  repeat with the_window in window_list"
            "    set window_counter to window_counter + 1"
            "    set tab_list to every tab in the_window"
            "    set tab_counter to 0"
            ""
            "    repeat with the_tab in tab_list"
            "      set tab_counter to tab_counter + 1"
            "      set coordinate to window_counter & \"⋘⋙\" & tab_counter"
            "      set the_title to the title of the_tab"
            "      set the_url to get URL of the_tab"
            "      set titleString to titleString & coordinate & \"⋘⋙\" & the_title & \"⋘⋙\" & the_url & return"
            "    end repeat"
            "  end repeat"
            "end tell")
          "\n")))
    (thread-first script
      (do-applescript)
      (string-trim "\"\n" "\n\"")
      (split-string "\n"))))

;; (hurricane//chrome-tabs)
;; => ("1⋘⋙1⋘⋙Google⋘⋙www.google.com" "1⋘⋙2⋘⋙Home - BBC News⋘⋙www.bbc.com")
;; 1 - 第一个窗口
;; 1 - 第一个标签
;; Google - 标题
;; www.google.com - 地址

(defun hurricane//chrome-switch-tab-1 (window-id tab-id)
  ;; FIXME: 不知道如何处理多余一个窗口的情况。
  (do-applescript
   (concat "tell application \"Google Chrome\"\n"
           (format "  set active tab index of first window to %s\n" tab-id)
           "  activate\n"
           "end tell\n")))

(defun hurricane//chrome-close-tab-1 (window-id tab-id)
  (do-applescript
   (concat (format "tell window %s of application \"Google Chrome\"\n" window-id)
           (format "  close (tab index %s)\n" tab-id)
           "end tell\n")))

(defun hurricane//chrome-get-window-id-and-tab-id-and-tab-url-from-x (x)
  (let*
      ((tabs (hurricane//chrome-tabs))
       (window-id-and-tab-id-and-tab-url
        (seq-some (lambda (s)
                    (and (string-match
                          (rx-to-string
                           `(and
                             string-start
                             (group (1+ num)) "⋘⋙" (group (1+ num)) "⋘⋙"
                             ,x "⋘⋙" (group (1+ not-newline))
                             string-end))
                          s)
                         (list (match-string 1 s)
                               (match-string 2 s)
                               (match-string 3 s)
                               )))
                  tabs)))
    window-id-and-tab-id-and-tab-url))

(defun hurricane//chrome-switch-tab-action (x)
  (let ((window-id-and-tab-id-and-tab-url (hurricane//chrome-get-window-id-and-tab-id-and-tab-url-from-x x)))
    (hurricane//chrome-switch-tab-1 (nth 0 window-id-and-tab-id-and-tab-url) (nth 1 window-id-and-tab-id-and-tab-url))))

(defun hurricane//chrome-close-tab-action (x)
  (let ((window-id-and-tab-id-and-tab-url (hurricane//chrome-get-window-id-and-tab-id-and-tab-url-from-x x)))
    (run-with-idle-timer 0.5 nil #'hurricane//chrome-close-tab-1 (nth 0 window-id-and-tab-id-and-tab-url) (nth 1 window-id-and-tab-id-and-tab-url))))

(defun hurricane//chrome-copy-tab-url-action (x)
  (let ((window-id-and-tab-id-and-tab-url (hurricane//chrome-get-window-id-and-tab-id-and-tab-url-from-x x)))
    (kill-new (nth 2 window-id-and-tab-id-and-tab-url))))

(defun hurricane//chrome-insert-tab-url-action (x)
  (let ((window-id-and-tab-id-and-tab-url (hurricane//chrome-get-window-id-and-tab-id-and-tab-url-from-x x)))
    (insert (nth 2 window-id-and-tab-id-and-tab-url))))

(defun hurricane/manage-chrome-tabs ()
  (interactive)
  (ivy-read "Chrome Tab (default activate): " (mapcar
                                               (lambda (s)
                                                 (and (string-match
                                                       (rx string-start
                                                           (1+ num) "⋘⋙" (1+ num) "⋘⋙"
                                                           (group (1+ not-newline))
                                                           "⋘⋙" (1+ not-newline)
                                                           string-end)
                                                       s)
                                                      (match-string 1 s)))
                                               (hurricane//chrome-tabs))
            :action 'hurricane//chrome-switch-tab-action
            ))

(with-eval-after-load 'ivy
 (ivy-add-actions
 'hurricane/manage-chrome-tabs
 '(("d" hurricane//chrome-close-tab-action "close tab(s)")
   ("y" hurricane//chrome-copy-tab-url-action "copy tab(s) url")
   ("I" hurricane//chrome-insert-tab-url-action "insert tab(s) url")
   )))
;; }}

(defun hurricane/open-link-in-chrome ()
  "Open `url' under cursor in Chrome.
Work in macOS only."
  (interactive)
  (let* (($inputStr (if (use-region-p)
                        (buffer-substring-no-properties (region-beginning) (region-end))
                      (let ($p0 $p1 $p2
                        ($pathStops "^  \t\n\"`'‘’“”|[]{}「」<>〔〕〈〉《》【】〖〗«»‹›❮❯❬❭〘〙·。\\"))
                        (setq $p0 (point))
                        (skip-chars-backward $pathStops)
                        (setq $p1 (point))
                        (goto-char $p0)
                        (skip-chars-forward $pathStops)
                        (setq $p2 (point))
                        (goto-char $p0)
                        (buffer-substring-no-properties $p1 $p2))))
         ($path
          (replace-regexp-in-string
           "^file:///" "/"
           (replace-regexp-in-string
            ":\\'" "" $inputStr))))
  (shell-command
   (format "open -a Google\\ Chrome.app \"%s\"" $path))))

(cond (sys/macp
   (progn
    (eval-and-compile
      (if (fboundp 'window-inside-edges)
          ;; Emacs devel.
          (defalias 'th-window-edges
            'window-inside-edges)
        ;; Emacs 21.
        (defalias 'th-window-edges
          'window-edges)
        ))

    (defun th-point-position ()
      "Return the location of POINT as positioned on the selected frame.
    Return a cons cell `(x . y)'."
      (let* ((w (selected-window))
             (f (selected-frame))
             (edges (th-window-edges w))
             (col (current-column))
             (row (count-lines (window-start w) (point)))
             (x (+ (car edges) col))
             (y (+ (car (cdr edges)) row)))
        (cons x y)))

    (defun get-point-pixel-position ()
      "Return the position of point in pixels within the frame."
      (let ((point-pos (th-point-position)))
        (th-get-pixel-position (car point-pos) (cdr point-pos))))


    (defun th-get-pixel-position (x y)
      "Return the pixel position of location X Y (1-based) within the frame."
      (let ((old-mouse-pos (mouse-position)))
        (set-mouse-position (selected-frame)
                            ;; The fringe is the 0th column, so x is OK
                            x
                            (1- y))
        (let ((point-x (car (cdr (mouse-pixel-position))))
              (point-y (cdr (cdr (mouse-pixel-position)))))
          ;; On Linux with the Enlightenment window manager restoring the
          ;; mouse coordinates didn't work well, so for the time being it
          ;; is enabled for Windows only.
          (when (eq window-system 'w32)
            (set-mouse-position
             (selected-frame)
             (cadr old-mouse-pos)
             (cddr old-mouse-pos)))
          (cons point-x point-y))))

    (defun hurricane//display-current-input-method-title (arg1 &optional arg2 arg3)
      "Display current input method name."
      (when current-input-method-title
        (set-mouse-position (selected-frame) (car (th-point-position)) (cdr (th-point-position)))
        (x-show-tip current-input-method-title (selected-frame) nil 1  20 -30)))

    (advice-add 'evil-insert :after 'hurricane//display-current-input-method-title))))

(defvar official-accounts-all-images-url-list)

(defvar official-accounts-image-url-pattern-list
  '("<img data-s=\"300,640\" data-type=\"png\" data-src=\"\\|data-s=\"300,640\" data-src=\"\\|<img data-s=\"300,640\" data-type=\"jpeg\" data-src=\"\\|data-src=\"\\|src=\"" . "\"\\|?"))

(defvar official-accounts-content-pattern-list
  '("<div class=\"rich_media_content                                                                     \"
            id=\"js_content\" style=\"visibility: hidden;\">
\\|<div class=\"rich_media_content                                       \"
            id=\"js_content\" style=\"visibility: hidden;\">" . "</div>"))

(defun hurricane//get-all-images-url (file image-url-pattern-list)
  "Return a image file name and image URL map list, extract from html FILE.

Image file name is generated from `match-end' position string."
  (with-temp-buffer
    (insert-file-contents file)
    (let ((image-url-list nil)
          (search-origin 0) )
      (while (string-match (car image-url-pattern-list) (buffer-string) search-origin)
        (progn
          (goto-char search-origin)
          (setq image-url-start (re-search-forward (car image-url-pattern-list)))
          (setq image-url-end (re-search-forward (cdr image-url-pattern-list)))
          (setq image-url (buffer-substring image-url-start (- image-url-end 1)))
          (if image-url
              (progn
                (push (cons image-url (concat (number-to-string (match-end 0)) ".jpeg")) image-url-list)
                (setq search-origin (match-end 0))))))
      image-url-list)))

(defun download-all-images (url-list file-name)
  "Use `org-download--image' to download image from URL-LIST,FILE-NAME be used to format directory name."
  (let ((image-directory (concat org-screenshot-image-dir-name "/" file-name)))
   (cl-loop for url in url-list
           do (progn
                (unless (file-exists-p image-directory)
                  (make-directory image-directory t))
                (ignore-errors (org-download--image (car url) (concat image-directory "/" (cdr url))))
                (message "Begin to download: %s" url)
                (sleep-for 1)))))

(defun insert-header-to-org-content (file-name)
  (with-temp-buffer
    (progn
      (insert-file-contents (concat file-name ".org"))
      (goto-char (point-min))
      (insert (format "# -*- eval: (setq org-download-image-dir (concat default-directory \"./static/%s/\")); -*-\n" file-name))
      (insert (format ":PROPERTIES:\n:ID:       %s\n:END:\n" (org-id-new)))
      (insert (format "#+DATE: <%s>\n" (format-time-string "%Y-%m-%d %b %H:%M")))
      (insert (format "#+TITLE: %s\n" file-name))
      (insert "#+ROAM_KEY:\n#+PDF_KEY:\n#+PAGE_KEY:\n\n")
      (write-file (concat file-name ".org")))))

(setq html-image-url-pattern-list
  '(" src=\"\\|\]\(" . "\"\\|)"))

(defun hurricane/find-file-html-or-markdown-to-org (&optional in-file)
  (interactive)
  (setq in-file-org (if (and in-file (file-exists-p in-file))
                           (concat (file-name-sans-extension in-file) ".org")
                          (concat (read-string "Please input the Org file name: "
                                       nil nil "" t) ".org")))
  (setq in-file (if (not in-file)
                   (if (derived-mode-p 'dired-mode)
                       (dired-get-file-for-visit)
                     buffer-file-name)
                  in-file))
  (setq in-file-extension (pcase (file-name-extension in-file)
                            ("md" "markdown")
                            ("html" "html")))
  (setq html-all-images-url-list (hurricane//get-all-images-url in-file html-image-url-pattern-list))

  (message "url-list %s" html-all-images-url-list)

  (download-all-images html-all-images-url-list (file-name-nondirectory (file-name-sans-extension in-file-org)))

  (sleep-for 1)

  (pandoc-converter in-file in-file-org in-file-extension "org")

  (sleep-for 1)

  (replace-url-with-file-path-in-org (file-name-nondirectory (file-name-sans-extension in-file-org)) html-all-images-url-list)

  (defun callback-insert-header-to-org-content ()
    (insert-header-to-org-content (file-name-sans-extension in-file-org)))

  (install-monitor-file-exists in-file-org 1 #'callback-insert-header-to-org-content)
  )

(defun hurricane/extract-content-from-official-accounts-to-org-file ()
  (interactive)
    ;; Read `url' string from minibuffer, while the string read is empty, this loop will not stop.
    (setq url "")
    (setq file-name "")
    (while (string-equal url "")
      (setq url (read-string "Please input the Official Accounts url to extract: "
                             nil nil "" nil)))
    (while (string-equal file-name "")
      (setq file-name (read-string "Please input the File name: "
                             nil nil "" nil)))
    (setq html-file-name (concat file-name ".html")
          org-file-name  (concat file-name ".org"))

    ;; Extract content to file.
    (with-current-buffer (url-retrieve-synchronously url)
      ;; Remove the `^M' character in html file.
      (dos2unix)
      (progn
        (goto-char 0)
        (setq content-start (re-search-forward (car official-accounts-content-pattern-list)))
        (goto-char content-start)
        (setq content-end (re-search-forward (cdr official-accounts-content-pattern-list)))
        (setq content-string (buffer-substring content-start (- content-end 6)))
        (append-string-to-file content-string html-file-name)))

    (setq official-accounts-all-images-url-list (hurricane//get-all-images-url html-file-name official-accounts-image-url-pattern-list))
    (message "%s" official-accounts-all-images-url-list)

    ;; Download all images.
    (download-all-images official-accounts-all-images-url-list file-name)

    (replace-unexpected-string-in-file html-file-name '(("data-src" . "src")))
    (sleep-for 1)
    (pandoc-converter html-file-name org-file-name "html" "org")

    (replace-url-with-file-path-in-org file-name official-accounts-all-images-url-list)

    (defun callback-insert-header-to-org-content ()
      (insert-header-to-org-content file-name))

    (install-monitor-file-exists org-file-name 1 #'callback-insert-header-to-org-content))

(defvar hurricane-proxy  "127.0.0.1:1080")

;; Network Proxy
(defun hurricane/proxy-http-show ()
  "Show HTTP/HTTPS proxy."
  (interactive)
  (if url-proxy-services
      (message "Current HTTP proxy is `%s'" hurricane-proxy)
    (message "No HTTP proxy")))

(defun hurricane/proxy-http-enable ()
  "Enable HTTP/HTTPS proxy."
  (interactive)
  (setq url-proxy-services
        `(("http" . ,hurricane-proxy)
          ("https" . ,hurricane-proxy)
          ("no_proxy" . "^\\(localhost\\|192.168.*\\|10.*\\)")))
  (hurricane/proxy-http-show))

(defun hurricane/proxy-http-disable ()
  "Disable HTTP/HTTPS proxy."
  (interactive)
  (setq url-proxy-services nil)
  (hurricane/proxy-http-show))

(defun hurricane/proxy-http-toggle ()
  "Toggle HTTP/HTTPS proxy."
  (interactive)
  (if (bound-and-true-p url-proxy-services)
      (hurricane/proxy-http-disable)
    (hurricane/proxy-http-enable)))

(defun proxy-socks-show ()
  "Show SOCKS proxy."
  (interactive)
  (when (fboundp 'cadddr)                ; defined 25.2+
    (if (bound-and-true-p socks-noproxy)
        (message "Current SOCKS%d proxy is %s:%d"
                 (cadddr socks-server) (cadr socks-server) (caddr socks-server))
      (message "No SOCKS proxy"))))

(defun hurricane/proxy-socks-enable ()
  "Enable SOCKS proxy."
  (interactive)
  (require 'socks)
  (setq url-gateway-method 'socks
        socks-noproxy '("localhost")
        socks-server '("Default server" "127.0.0.1" 1080 5))
  (proxy-socks-show))

(defun hurricane/proxy-socks-disable ()
  "Disable SOCKS proxy."
  (interactive)
  (setq url-gateway-method 'native
        socks-noproxy nil)
  (proxy-socks-show))

(defun hurricane/proxy-socks-toggle ()
  "Toggle SOCKS proxy."
  (interactive)
  (if (bound-and-true-p socks-noproxy)
      (hurricane/proxy-socks-disable)
    (hurricane/proxy-socks-enable)))

(defun hurricane/dired-duplicate-this-file ()
  "Duplicate file on this line."
  (interactive)
  (let* ((this (dired-get-filename t))
         (name-base (file-name-base this))
         (extension (file-name-extension this))
         (ctr 1)
         (new (format "%s_%d.%s" name-base ctr extension)))
    (while (file-exists-p new)
      (setq ctr  (1+ ctr)
            new  (format "%s_%d.%s" name-base ctr extension)))
    (dired-copy-file this new nil))
  (revert-buffer))

(defun hurricane/org-replace-link-by-link-description ()
  "Replace an org link by its description or if empty its address"
  (interactive)
  (if (org-in-regexp org-link-bracket-re 1)
      (save-excursion
        (let ((remove (list (match-beginning 0) (match-end 0)))
              (description
               (if (match-end 2)
                   (org-match-string-no-properties 2)
                 (org-match-string-no-properties 1))))
          (apply 'delete-region remove)
          (insert description)))))

(defun rime-predicate-blink-search-p ()
  "Whether a blink-search keymap is activated.

Can be used in `rime-disable-predicates' and `rime-inline-predicates'."
  (and (featurep 'blink-search)
       (bound-and-true-p blink-search-mode-map)))

;; {{
;; Audio note (English) 用的是 knowclip.app 提供的模板。
;; @See: https://github.com/nicehiro/.emacs.d/blob/master/lisp/init-anki.el
(defvar anki-connect-host "127.0.0.1"
  "Anki connect server host.")

(defvar anki-connect-port "8765"
  "Anki connect server port.")

(defvar anki-deck-name "English (Generated by Knowclip)"
  "Shengci in anki deck name.")

(defun anki-add-card (deck front back &optional screenshot)
  "Add anki basic card which contains FRONT and BACK elements to the DECK."
  (let* ((req-params (list `("note" . ,(list `("deckName" . ,deck)
                                             '("modelName" . "Audio note (English)")
                                             `("fields" . ,(list `("sound" . ,front)
                                                                 `("transcription" . ,back)
                                                                 `("image" . ,(or screenshot ""))))
                                             `("options" . ,(list '("closeAfterAdding" . t))))))))
    (request (format "%s:%s" anki-connect-host anki-connect-port)
      :type "POST"
      :data (json-encode (list '("action" . "addNote")
                               '("version" . 6)
                               `("params" . ,req-params)))
      :headers '(("Content-Type" . "text/json"))
      :parser 'json-read
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (message "result: %S" (assoc-default 'result data)))))))
;; }}
