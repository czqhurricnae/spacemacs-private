(defconst hurricane-org-packages
  '(
    (org :location built-in)
    (org-mac-link :location built-in)
    (org-pomodoro :location (recipe :fetcher github
                                    :repo "lolownia/org-pomodoro"))
    (ox-latex :location built-in)
    (ox-md :location built-in)
    deft
    (org-protocol-capture-html :location (recipe
                                          :fetcher github
                                          :repo "alphapapa/org-protocol-capture-html"))
    ob-ipython
    (org-download :location (recipe
                             :fetcher github
                             :repo "abo-abo/org-download"))
    (org2ctex :location (recipe
                             :fetcher github
                             :repo "tumashu/org2ctex"))
    org-tree-slide))

(defun hurricane-org/post-init-org-pomodoro ()
  (progn
    (add-hook 'org-pomodoro-finished-hook '(lambda () (hurricane/notify-osx "Pomodoro Finished" "Have a break!")))
    (add-hook 'org-pomodoro-short-break-finished-hook '(lambda () (hurricane/notify-osx "Short Break" "Ready to Go?")))
    (add-hook 'org-pomodoro-long-break-finished-hook '(lambda () (hurricane/notify-osx "Long Break" "Ready to Go?")))
    (add-hook 'org-pomodoro-kill-hook '(lambda () (hurricane/notify-osx "Pomodoro Killed" "One does not simply kill a pomodoro!")))))

;; In order to export pdf to support Chinese, I should install Latex in here:
;; @see: https://www.tug.org/mactex/
;; @see: http://freizl.github.io/posts/2012-04-06-export-orgmode-file-in-Chinese.html
;; @see: http://stackoverflow.com/questions/21005885/export-org-mode-code-block-and-result-with-different-styles
(defun hurricane-org/post-init-org ()
  (with-eval-after-load 'org
    (progn
      (spacemacs|disable-company org-mode)
      (spacemacs/set-leader-keys-for-major-mode 'org-mode "," 'org-priority)
      (org-babel-do-load-languages
       'org-babel-load-languages
       '((perl . t)
         (ruby . t)
         (shell . t)
         (dot . t)
         (js . t)
         (latex .t)
         (python . t)
         (ipython . t)
         (emacs-lisp . t)
         (plantuml . t)
         (C . t)
         (sql . t)
         (ditaa . t)))

      ;; Make Yasnippet effect when the editing org source code is JavaScript.
      (add-to-list 'org-src-lang-modes '("js" . js2))
      (add-to-list 'org-src-lang-modes '("javascript" . rjsx))

      ;; When editing org-files with source-blocks, we want the
      ;; source blocks to be themed as they would in their native mode.
      ;; Turn on native code fontification in the Org buffer.
      (setq org-src-fontify-natively t
            org-src-tab-acts-natively t
            org-confirm-babel-evaluate nil
            org-edit-src-content-indentation 0)

      (setq org-refile-use-outline-path 'file)
      (setq org-outline-path-complete-in-steps nil)
      (setq org-refile-targets
            '((nil :maxlevel . 4)
              (org-agenda-files :maxlevel . 4)))

      ;; Config stuck project.
      (setq org-stuck-projects
            '("TODO={.+}/-DONE" nil nil "SCHEDULED:\\|DEADLINE:"))

      (setq org-agenda-inhibit-startup t)
      (setq org-agenda-span 'day)
      (setq org-agenda-use-tag-inheritance nil)
      (setq org-agenda-window-setup 'current-window)
      (setq org-log-done t)

      (setq org-todo-keywords
            (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!/!)")
                    (sequence "WAITING(w@/!)" "SOMEDAY(S)" "|" "CANCELLED(c@/!)" "MEETING(m)" "PHONE(p)"))))

      ;; Change task state to STARTED when clocking in.
      (setq org-clock-in-switch-to-state "NEXT")
      ;; Save clock data and notes in the LOGBOOK drawe.
      (setq org-clock-into-drawer t)
      ;; Removes clocked tasks with 0:00 duration.
      (setq org-clock-out-remove-zero-time-clocks t)
      ;; Show the clocked-in task if any in the header line.
      (setq org-tags-match-list-sublevels nil)

      (setq org-image-actual-width '(350))

      (add-hook 'org-mode-hook '(lambda () (spacemacs/toggle-line-numbers-off)) 'append)
      (add-hook 'org-mode-hook '(lambda ()
                                  ;; Keybinding for inserting code blocks.
                                  (local-set-key (kbd "C-c s i")
                                                 'hurricane/org-insert-src-block)
                                  ;; Keybinding for editing source code blocks.
                                  (local-set-key (kbd "C-c s e")
                                                 'org-edit-special)
                                  ;; Keybinding for executing source code blocks.
                                  (local-set-key (kbd "C-c s r")
                                                 'org-src-do-at-code-block)
                                  ;; Keybinding for export org file to markdown file.
                                  (local-set-key (kbd "C-c f c")
                                                 'org-gfm-export-to-markdown-filter)
                                  (local-set-key (kbd "C-l")
                                                 'evil-insert)))

      (setq org-latex-create-formula-image-program 'dvipng)
      (setq org-latex-listings 'minted)
      (setq org-latex-minted-options
            '(("frame" "lines") ("linenos" "true") ("escapeinside" "||")))
      (add-to-list 'org-latex-packages-alist '("" "minted"))

      (add-to-list 'org-entities-user
                   '("exclamation" "\\exclamation{}" t "!" "!" "!" "!"))
      (setq org-export-backends (quote (ascii html icalendar latex md)))
      ;; Make org not to treat `_' with sub-superscript, but `_{}'.
      (setq org-export-with-sub-superscripts '{})

      ;; Reset subtask.
      (setq org-default-properties (cons "RESET_SUBTASKS" org-default-properties))

      (setq org-plantuml-jar-path
            (expand-file-name "~/.spacemacs.d/plantuml.jar"))
      (setq org-ditaa-jar-path "~/.spacemacs.d/ditaa.jar")

      (org-babel-lob-ingest scripts-file)

      ;; Copy from chinese layer.
      (defadvice org-html-paragraph (before org-html-paragraph-advice
                                            (paragraph contents info) activate)
        "Join consecutive Chinese lines into a single long line without
        unwanted space when exporting org-mode to html."
        (let* ((origin-contents (ad-get-arg 1))
               (fix-regexp "[[:multibyte:]]")
               (fixed-contents
                (replace-regexp-in-string
                 (concat
                  "\\(" fix-regexp "\\) *\n *\\(" fix-regexp "\\)") "\\1\\2" origin-contents)))
          (ad-set-arg 1 fixed-contents)))

      ;; Define the refile targets.
      (setq org-agenda-file-note (expand-file-name "notes.org" org-agenda-dir))
      (setq org-agenda-file-gtd (expand-file-name "gtd.org" org-agenda-dir))
      (setq org-agenda-file-journal (expand-file-name "journal.org" org-agenda-dir))
      (setq org-agenda-file-code-snippet (expand-file-name "snippet.org" org-agenda-dir))
      (setq org-default-notes-file (expand-file-name "gtd.org" org-agenda-dir))
      (setq org-agenda-files (list org-agenda-dir))

      (with-eval-after-load 'org-agenda
        (define-key org-agenda-mode-map (kbd "P") 'org-pomodoro)
        (spacemacs/set-leader-keys-for-major-mode 'org-agenda-mode
          "." 'spacemacs/org-agenda-transient-state/body))

      ;; {{
      ;; @see: https://emacs.stackexchange.com/questions/22396/export-without-links
      (with-eval-after-load 'ox
        (defun custom-pdf-link-filter (link backend info)
          "Rewrite `org' file links in export to preserve link text only."
          (if (eq backend `pdf)
              (replace-regexp-in-string "\\[\\[\\([^]]*\\)\\]\\]" "\\1" link)
            link))
        (add-to-list 'org-export-filter-link-functions
                     'custom-pdf-link-filter))
      ;; }}

      ;; {{
      ;; The `%i' would copy the selected text into the template.
      ;; @see: http://www.howardism.org/Technical/Emacs/journaling-org.html
      ;; Add multi-file journal.
      ;; }}
      (setq org-capture-templates
            '(("t" "Todo" entry (file+headline org-agenda-file-gtd "Workspace")
               "* TODO [#B] %?\n  %i\n"
               :empty-lines 1)
              ("n" "notes" entry (file+headline org-agenda-file-note "Quick notes")
               "* %?\n  %i\n %U"
               :empty-lines 1)
              ("s" "Code Snippet" entry
               (file org-agenda-file-code-snippet)
               "* %?\t%^g\n#+BEGIN_SRC %^{language}\n\n#+END_SRC")
              ("W" "Work" entry (file+headline org-agenda-file-gtd "Work")
               "* TODO [#A] %?\n  %i\n %U"
               :empty-lines 1)
              ("c" "Chrome" entry (file+headline org-agenda-file-note "Quick notes")
               "* TODO [#C] %?\n %(hurricane/retrieve-chrome-current-tab-url)\n %i\n %U"
               :empty-lines 1)
              ("l" "links" entry (file+headline org-agenda-file-note "Quick notes")
               "* TODO [#C] %?\n  %i\n %a \n %U"
               :empty-lines 1)
              ("j" "Journal Entry"
               entry (file+datetree org-agenda-file-journal)
               "* %U - %^{heading}\n %?"
               :empty-lines 1)
              ("p" "Protocol"
               entry (file+headline org-agenda-file-note "Quick notes")
               "* %^{Title}\n %:initial")
              ))

      ;; An entry without a cookie is treated just like priority `B'.
      ;; So when create new task, they are default `重要且紧急'.
      (setq org-agenda-custom-commands
            '(
              ("w" . "任务安排")
              ("wa" "重要且紧急的任务" tags-todo "+PRIORITY=\"A\"")
              ("wb" "重要且不紧急的任务" tags-todo "-Weekly-Monthly-Daily+PRIORITY=\"B\"")
              ("wc" "不重要且紧急的任务" tags-todo "+PRIORITY=\"C\"")
              ("p" . "项目安排")
              ("pw" tags-todo "PROJECT+WORK+CATEGORY=\"cocos2d-x\"")
              ("pl" tags-todo "PROJECT+DREAM+CATEGORY=\"hurricane\"")
              ("W" "Weekly Review"
               ((stuck "") ;; Review stuck projects as designated by org-stuck-projects.
                (tags-todo "PROJECT") ;; Review all projects (assuming you use todo keywords to designate projects).
                ))))

      ;; Used by hurricane/org-clock-sum-today-by-tags.
      (add-hook 'org-after-todo-statistics-hook 'hurricane/org-summary-todo)

      (define-key org-mode-map (kbd "s-p") 'org-priority)
      (spacemacs/set-leader-keys-for-major-mode 'org-mode
        "tl" 'org-toggle-link-display)
      (define-key evil-normal-state-map (kbd "C-c C-w") 'org-refile)

      ;; Hack for org headline toc.
      (defun org-html-headline (headline contents info)
        "Transcode a `headline' element from Org to HTML.
        `contents' holds the contents of the headline.
        `info' is a plist holding contextual information."
        (unless (org-element-property :footnote-section-p headline)
          (let* ((numberedp (org-export-numbered-headline-p headline info))
                 (numbers (org-export-get-headline-number headline info))
                 (section-number (and numbers
                                      (mapconcat #'number-to-string numbers "-")))
                 (level (+ (org-export-get-relative-level headline info)
                           (1- (plist-get info :html-toplevel-hlevel))))
                 (todo (and (plist-get info :with-todo-keywords)
                            (let ((todo (org-element-property :todo-keyword headline)))
                              (and todo (org-export-data todo info)))))
                 (todo-type (and todo (org-element-property :todo-type headline)))
                 (priority (and (plist-get info :with-priority)
                                (org-element-property :priority headline)))
                 (text (org-export-data (org-element-property :title headline) info))
                 (tags (and (plist-get info :with-tags)
                            (org-export-get-tags headline info)))
                 (full-text (funcall (plist-get info :html-format-headline-function)
                                     todo todo-type priority text tags info))
                 (contents (or contents ""))
                 (ids (delq nil
                            (list (org-element-property :CUSTOM_ID headline)
                                  (org-export-get-reference headline info)
                                  (org-element-property :ID headline))))
                 (preferred-id (car ids))
                 (extra-ids
                  (mapconcat
                   (lambda (id)
                     (org-html--anchor
                      (if (org-uuidgen-p id) (concat "ID-" id) id)
                      nil nil info))
                   (cdr ids) "")))
            (if (org-export-low-level-p headline info)
                ;; This is a deep sub-tree: export it as a list item.
                (let* ((type (if numberedp 'ordered 'unordered))
                       (itemized-body
                        (org-html-format-list-item
                         contents type nil info nil
                         (concat (org-html--anchor preferred-id nil nil info)
                                 extra-ids
                                 full-text))))
                  (concat (and (org-export-first-sibling-p headline info)
                               (org-html-begin-plain-list type))
                          itemized-body
                          (and (org-export-last-sibling-p headline info)
                               (org-html-end-plain-list type))))
              (let ((extra-class (org-element-property :HTML_CONTAINER_CLASS headline))
                    (first-content (car (org-element-contents headline))))
                ;; Standard headline.
                ;; Export it as a section.
                (format "<%s id=\"%s\" class=\"%s\">%s%s</%s>\n"
                        (org-html--container headline info)
                        (org-export-get-reference headline info)
                        (concat (format "outline-%d" level)
                                (and extra-class " ")
                                extra-class)
                        (format "\n<h%d id=\"%s\">%s%s</h%d>\n"
                                level
                                preferred-id
                                extra-ids
                                (concat
                                 (and numberedp
                                      (format
                                       "<span class=\"section-number-%d\">%s</span> "
                                       level
                                       (mapconcat #'number-to-string numbers ".")))
                                 full-text)
                                level)
                        ;; When there is no section, pretend there is an
                        ;; empty one to get the correct <div
                        ;; class="outline-...> which is needed by
                        ;; 'org-info.js'.
                        (if (eq (org-element-type first-content) 'section) contents
                          (concat (org-html-section first-content "" info) contents))
                        (org-html--container headline info)))))))

      )))

(defun hurricane-org/init-org-mac-link ()
  (use-package org-mac-link
    :after org
    :commands org-mac-grab-link
    :init
    (progn
      (add-hook 'org-mode-hook
                (lambda ()
                  (define-key org-mode-map (kbd "C-c g") 'org-mac-grab-link))))))
;; FIXME:
(defun hurricane-org/post-init-ox-reveal ()
  (setq org-reveal-root "file:///Users/guanghui/.emacs.d/reveal-js"))

(defun hurricane-org/init-org-tree-slide ()
  (spacemacs|use-package-add-hook org
    :post-config
    (require 'org-tree-slide)
    (spacemacs/set-leader-keys "oto" 'org-tree-slide-mode)))

(defun hurricane-org/init-worf ()
  (spacemacs|use-package-add-hook org
    :post-config
    (require 'worf)
    (add-hook 'org-mode-hook 'worf-mode)))

(defun hurricane-org/post-init-deft ()
  (progn
    (setq deft-use-filter-string-for-filename t)
    (setq deft-recursive t)
    (setq deft-extension "org")
    (setq deft-directory deft-dir)))

(defun hurricane-org/init-org-protocol ()
  (use-package org-protocol))

(defun hurricane-org/init-org-protocol-capture-html ()
  (spacemacs|use-package-add-hook org-protocol
    :post-config (require 'org-protocol-capture-html)))

(defun hurricane-org/init-ox-latex ()
  (spacemacs|use-package-add-hook org :post-config (require 'ox-latex)))

(defun hurricane-org/init-ox-md ()
  (spacemacs|use-package-add-hook org :post-config (require 'ox-md)))

(defun hurricane-org/init-org-compat ()
  (spacemacs|use-package-add-hook org :post-config (require 'org-compat)))

(defun hurricane-org/init-org-habit ()
  (spacemacs|use-package-add-hook org
    :post-config
    (require 'org-habit)
    (add-to-list 'org-modules 'org-habit)))

(defun hurricane-org/init-org-emacs-lisp ()
  (spacemacs|use-package-add-hook org :post-config (require 'org-emacs-lisp)))

;; {{
;; @see: https://github.com/gregsexton/ob-ipython
;; Set ob-ipython-command to the path of jupyter, must be be corresponding to
;; the path of ipython virtual envirnment which is setting by
;; '(setq venv-location virtualenv-dir)'.
;; Must install ipython and jupyter in ipy virtual envirnment first.
;; $ pip install ipython
;; $ pip install --upgrade jupyter
(defun hurricane-org/post-init-ob-ipython ()
    (progn
      (setq ob-ipython-command jupyter-bin)
      (with-eval-after-load 'company
        (add-to-list 'company-backends 'company-ob-ipython))
      ;; Display/update images in the buffer after I evaluate.
      (add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append)))
;; }}

(defun hurricane-org/init-ob-lisp ()
  (spacemacs|use-package-add-hook org :post-config (require 'ob-lisp)))

(defun hurricane-org/post-init-org-download ()
  (use-package org-download
  ;; :ensure-system-package (pngpaste . "brew install pngpaste")
  :init
  (progn
    ;; (setq org-download-screenshot-method "screencapture -i %s")
    (setq org-download-screenshot-method "pngpaste %s")
    (setq org-download-heading-lvl nil)
    (setq org-download-screenshot-file (expand-file-name "screenshot.jpg" temporary-file-directory))
    ;; Drag-and-drop to `dired`.
    (add-hook 'dired-mode-hook 'org-download-enable))))

;; {{
;; @see: https://github.com/tumashu/org2ctex
(defun hurricane-org/init-org2ctex ()
  (use-package org2ctex
    :after org
    :config
      (progn
        (org2ctex-toggle t)
        (add-to-list 'org2ctex-latex-classes '("my-article" "\\documentclass[20pt]{ctexart}
                    [NO-DEFAULT-PACKAGES]
                    \\usepackage{xeCJK}
                    \\usepackage[T1]{fontenc}
                    \\usepackage{fixltx2e}
                    \\usepackage{graphicx}
                    \\usepackage{longtable}
                    \\usepackage{float}
                    \\usepackage{wrapfig}
                    \\usepackage{rotating}
                    \\usepackage[normalem]{ulem}
                    \\usepackage{amsmath}
                    \\usepackage{textcomp}
                    \\usepackage{marvosym}
                    \\usepackage{wasysym}
                    \\usepackage{amssymb}
                    \\usepackage{booktabs}
                    \\usepackage[colorlinks,linkcolor=black,anchorcolor=black,citecolor=black]{hyperref}
                    \\tolerance=1000
                    \\usepackage{listings}
                    \\usepackage{xcolor}
                    \\usepackage{parskip}
                    \\usepackage{minted}
                    \\usepackage{color}
                    % 设置段首不缩进并且段间间隔
                    \\setlength{\\parindent}{0pt}
                    \\lstset{
                    % 行号
                    numbers=left,
                    % 背景框
                    framexleftmargin=10mm,
                    frame=none,
                    % 背景色
                    % backgroundcolor=\\color[rgb]{1,1,0.76},
                    backgroundcolor=\\color[RGB]{245,245,244},
                    % 样式
                    keywordstyle=\\bf\\color{blue},
                    identifierstyle=\\bf,
                    numberstyle=\\color[RGB]{0,192,192},
                    commentstyle=\\it\\color[RGB]{0,96,96},
                    stringstyle=\\rmfamily\\slshape\\color[RGB]{128,0,0},
                    % 显示空格
                    showstringspaces=false
                    }
                    % 设置章节标题格式
                    \\ctexset{
                       section={
                         titleformat=\\raggedright,
                         name={,. },
                         number=\\chinese{section},
                       }
                   }
                   \\ctexset{
                      subsection={
                        titleformat=\\raggedright,
                        name={,. },
                        number=\\arabic{subsection},
                      }
                   }
                   \\ctexset{
                      subsubsection={
                        titleformat=\\raggedright,
                        name={,. },
                        number=\\alph{subsubsection},
                      }
                   }"
                   ("\\section{%s}" . "\\section*{%s}")
                   ("\\subsection{%s}" . "\\subsection*{%s}")
                   ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                   ("\\paragraph{%s}" . "\\paragraph*{%s}")
                   ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
    (add-to-list 'org2ctex-latex-classes '("my-report"
                   "\\documentclass[11pt]{ctexrep}
                    %设置段首不缩进并且段间间隔
                    \\setlength{\\parindent}{0pt}"
                   ;; 自定义 LaTex 输出中文章节名
                   ("\\chapter{%s}" .
                    "{\\ctexset{chapter={numbering=false}}\\chapter{%s}}")
                   ("\\section{%s}" . "\\section*{%s}")
                   ("\\subsection{%s}" . "\\subsection*{%s}")
                   ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                   ("\\paragraph{%s}" . "\\paragraph*{%s}")))
    (add-to-list 'org2ctex-latex-classes '("my-exam"
                   "\\documentclass[addpoints, answers]{exam}
                    [NO-DEFAULT-PACKAGES]
                    % 写中文要用到
                    \\usepackage{xeCJK}
                    % 可以把题号变为中文
                    \\usepackage{zhnumber}
                    % 插入图片
                    \\usepackage{graphicx}
                    % 图文混排
                    % \\usepackage{picins}
                    % 插入链接
                    \\usepackage{hyperref}
                    % 数学符号
                    \\usepackage{amsmath}
                    % 表格样式
                    \\usepackage{booktabs}
                    % 定义页眉和页脚
                    \\pagestyle{headandfoot}
                    \\firstpageheadrule
                    \\firstpageheader{航线D组}{蒋勇明组}{7月份内测}
                    \\runningheader{航线D组}
                    {蒋勇明组}
                    {7月份内测}
                    \\runningheaderrule
                    \\firstpagefooter{}{第\\thepage\\ 页 (共\\numpages 页) }{}
                    \\runningfooter{}{第\\thepage\\ 页 (共\\numpages 页) }{}
                    % no box for solutions
                    % \\unframedsolutions
                    % 定义分数样式
                    \\pointname{ 分}
                    \\pointformat{ (\\thepoints) }
                    \\totalformat{共\\totalpoints 分}
                    \\setlength\\linefillheight{.5in}
                    \\renewcommand{\\solutiontitle}{\\noindent\\textbf{答:}}
                    \\renewcommand{\\thequestion}{\\zhnum{question}}
                    \\renewcommand{\\questionlabel}{\\thequestion .}
                    \\renewcommand{\\thepartno}{\\arabic{partno}}
                    \\renewcommand{\\partlabel}{\\thepartno .}"
                    ("\\section{%s}" . "\\section*{%s}")
                    ("\\question{%s}" . "\\subsection*{%s}")
                    ("\\begin{solution}{%s}\\end{solution}" . "\\subsubsection*{%s}"))))))
;; }}
