(defconst hurricane-org-packages
  `(
    (org :location built-in)
    (org-mac-link :location built-in)
    (org-pomodoro :location (recipe
                             :fetcher github
                             :repo "lolownia/org-pomodoro"))
    (ox-latex :location built-in)
    (ox-md :location built-in)
    deft
    (org-protocol-capture-html :location (recipe
                                          :fetcher github
                                          :repo "alphapapa/org-protocol-capture-html"))
    ,@(when sys/macp '(ob-ipython))
    (org-download :location (recipe
                             :fetcher github
                             :repo "abo-abo/org-download"))
    (org2ctex :location (recipe
                         :fetcher github
                         :repo "tumashu/org2ctex"))
    org-tree-slide
    (ox-html :location built-in)
    (ox-publish :location built-in)
    simple-httpd
    org-roam))

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
      (when sys/macp
        (push '(ipython . t) org-babel-load-languages))
      (org-babel-do-load-languages
       'org-babel-load-languages
       '((perl . t)
         (ruby . t)
         (shell . t)
         (dot . t)
         (js . t)
         (latex .t)
         (python . t)
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

      (org-link-set-parameters "video" :export 'org-video-link-export)

      (define-key org-mode-map (kbd "s-p") 'org-priority)
      (spacemacs/set-leader-keys-for-major-mode 'org-mode
        "tl" 'org-toggle-link-display)
      (define-key evil-normal-state-map (kbd "C-c C-w") 'org-refile)
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
(when sys/macp
  (defun hurricane-org/init-ob-ipython ()
    (spacemacs|use-package-add-hook org
      :post-config
      (require 'ob-ipython)
      (progn
        (when (file-exists-p jupyter-bin) (setq ob-ipython-command jupyter-bin))
        (with-eval-after-load 'company
          (add-to-list 'company-backends 'company-ob-ipython))
        ;; Display/update images in the buffer after I evaluate.
        (add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append)))))
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
        (org2ctex-mode t)
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

(defun hurricane-org/init-ox-html ()
  (spacemacs|use-package-add-hook org
    :post-config
    (require 'ox-html)
    (setq user-full-name "Hurricane Chen")))

(defun hurricane-org/init-ox-publish ()
  (spacemacs|use-package-add-hook org
    :post-config
    (require 'ox-publish)
    (setq org-publish-project-alist
          `(("orgfiles"
             ;; Sources and destinations for files.
             ;; local org files directory.
             :base-directory ,(concat deft-dir (file-name-as-directory "notes"))
             ;; :publishing directory "/ssh:c@182.61.145.178:/home/c/site/public/"
             :publishing-directory ,(concat deft-dir (file-name-as-directory "public"))
             ;; :preparation-function
             ;; :complete-function

             ;; Selecting files.
             :base-extension "org"
             ;; :exclude "PrivatePage.org"
             ;; :include
             :recursive t

             ;; Publishing action.
             :publishing-function org-html-publish-to-html
             ;; :htmlized-source

             ;; Options for the exporters.

             ;; {{
             ;; Generic properties.
             ;; :archived-trees	org-export-with-archived-trees
             ;; :exclude-tags	org-export-exclude-tags
             ;; org-export-headline-levels.
             :headline-levels 4
             ;; :language	org-export-default-language
             ;; :preserve-breaks	org-export-preserve-breaks
             ;; org-export-with-section-numbers.
             :section-numbers nil
             ;; :select-tags	org-export-select-tags
             ;; org-export-with-author.
             :with-author "Hurricane Chen"
             ;; :with-broken-links	org-export-with-broken-links
             ;; org-export-with-clocks.
             ;; :with-clocks	t
             ;; org-export-with-creator.
             ;; :with-creator nil
             ;; :with-date org-export-with-date
             ;; :with-drawers	org-export-with-drawers
             ;; :with-email	org-export-with-email
             ;; :with-emphasize	org-export-with-emphasize
             ;; :with-fixed-width org-export-with-fixed-width
             ;; :with-footnotes	org-export-with-footnotes
             ;; :with-latex	org-export-with-latex
             ;; :with-planning	org-export-with-planning
             ;; org-export-with-priority.
             :with-priority t
             ;; :with-properties	org-export-with-properties
             ;; :with-special-strings	org-export-with-special-strings
             ;; :with-sub-superscript	org-export-with-sub-superscripts
             ;; :with-tables	org-export-with-tables
             ;; :with-tags	org-export-with-tags
             ;; :with-tasks	org-export-with-tasks
             ;; :with-timestamps	org-export-with-timestamps
             ;; :with-title	org-export-with-title
             ;; org-export-with-toc.
             :with-toc t
             ;; :with-todo-keywords	org-export-with-todo-keywords
             ;; }}

             ;; {{
             ;;  HTML specific properties
             ;; :html-allow-name-attribute-in-anchors	org-html-allow-name-attribute-in-anchors
             ;; :html-checkbox-type	org-html-checkbox-type
             ;; :html-container	org-html-container-element
             ;; :html-divs	org-html-divs
             ;; org-html-doctype.
             :html-doctype "html5"
             ;; :html-extension	org-html-extension
             ;; org-html-footnote-format.
             ;; :html-footnote-format nil
             ;; :html-footnote-separator	org-html-footnote-separator
             ;; :html-footnotes-section	org-html-footnotes-section
             ;; :html-format-drawer-function	org-html-format-drawer-function
             ;; :html-format-headline-function	org-html-format-headline-function
             ;; :html-format-inlinetask-function	org-html-format-inlinetask-function
             ;; :html-head-extra	org-html-head-extra
             ;; :html-head-include-default-style	org-html-head-include-default-style
             ;; :html-head-include-scripts	org-html-head-include-scripts
             ;; :html-head	org-html-head
             ;; :html-home/up-format	org-html-home/up-format
             ;; :html-html5-fancy	org-html-html5-fancy
             ;; :html-indent	org-html-indent
             ;; :html-infojs-options	org-html-infojs-options
             ;; :html-infojs-template	org-html-infojs-template
             ;; :html-inline-image-rules	org-html-inline-image-rules
             ;; :html-inline-images	org-html-inline-images
             ;; :html-link-home	org-html-link-home
             ;; :html-link-org-files-as-html	org-html-link-org-files-as-html
             ;; :html-link-up	org-html-link-up
             ;; :html-link-use-abs-url	org-html-link-use-abs-url
             ;; :html-mathjax-options	org-html-mathjax-options
             ;; :html-mathjax-template	org-html-mathjax-template
             ;; :html-metadata-timestamp-format	org-html-metadata-timestamp-format
             ;; org-html-postamble-format.
             ;; :html-postamble-format t
             ;; org-html-postamble.
             ;; :html-postamble t
             ;; :html-preamble-format	org-html-preamble-format
             ;; org-html-preamble.
             ;; :html-preamble nil
             ;; :html-self-link-headlines	org-html-self-link-headlines
             ;; :html-table-align-individual-field	de{org-html-table-align-individual-fields
             ;; :html-table-attributes	org-html-table-default-attributes
             ;; :html-table-caption-above	org-html-table-caption-above
             ;; :html-table-data-tags	org-html-table-data-tags
             ;; :html-table-header-tags	org-html-table-header-tags
             ;; :html-table-row-tags	org-html-table-row-tags
             ;; :html-table-use-header-tags-for-first-column	org-html-table-use-header-tags-for-first-column
             ;; :html-tag-class-prefix	org-html-tag-class-prefix
             ;; :html-text-markup-alist	org-html-text-markup-alist
             ;; :html-todo-kwd-class-prefix	org-html-todo-kwd-class-prefix
             ;; :html-toplevel-hlevel	org-html-toplevel-hlevel
             ;; :html-use-infojs	org-html-use-infojs
             ;; :html-validation-link	org-html-validation-link
             ;; :html-viewport	org-html-viewport
             ;; :html-wrap-src-lines	org-html-wrap-src-lines
             ;; :html-xml-declaration	org-html-xml-declaration
             ;; }}

             ;; {{
             ;; Markdown specific properties.
             ;; :md-footnote-format	org-md-footnote-format
             ;; :md-footnotes-section	org-md-footnotes-section
             ;; :md-headline-style	org-md-headline-style
             ;; }}

             ;; {{
             ;; Other options
             :table-of-contents t
             ;; :style "<link rel=\"stylesheet\" href=\"../other/mystyle.css\" type=\"text/css\" />"
             ;; }}
             )

            ;; Static assets.
            ("js"
             :base-directory ,(concat deft-dir (file-name-as-directory "js"))
             :base-extension "js"
             :publishing-directory ,(concat deft-dir (file-name-as-directory "public") (file-name-as-directory "js"))
             :recursive t
             :publishing-function org-publish-attachment
             )
            ("css"
             :base-directory ,(concat deft-dir (file-name-as-directory "css"))
             :base-extension "css"
             :publishing-directory ,(concat deft-dir (file-name-as-directory "public") (file-name-as-directory "css"))
             :recursive t
             :publishing-function org-publish-attachment
             )
            ("images"
             :base-directory ,(concat deft-dir (file-name-as-directory "images"))
             :base-extension "jpg\\|gif\\|png\\|svg\\|gif"
             :publishing-directory ,(concat deft-dir (file-name-as-directory "public") (file-name-as-directory "images"))
             :recursive t
             :publishing-function org-publish-attachment
             )
            ("assets"
             :base-directory ,(concat deft-dir (file-name-as-directory "assets"))
             :base-extension "mp3"
             :publishing-directory ,(concat deft-dir (file-name-as-directory "public") (file-name-as-directory"assets"))
             :recursive t
             :publishing-function org-publish-attachment
             )

            ("website" :components ("orgfiles" "js" "css" "images"))
            ("statics" :components ("js" "css" "images" "assets"))
            ))))

(defun hurricane-org/init-simple-httpd ()
  (use-package simple-httpd
    :ensure t
    :config
    (setq httpd-root blog-dir)))

(defun hurricane-org/init-org-roam ()
  (use-package org-roam
    :hook
    (after-init . org-roam-mode)
    :custom
    (org-roam-directory deft-dir)
    (org-roam-capture-templates
     '(("d" "default" plain (function org-roam-capture--get-point)
       "%?"
       :file-name "${slug}"
       :head "#+DATE: %<<%F %a %R>> \n#+TITLE: ${title}\n"
       :unnarrowed t)))
    :init
    (progn
      (spacemacs/declare-prefix "ar" "org-roam")
      (spacemacs/set-leader-keys
        "arl" 'org-roam
        "art" 'org-roam-dailies-today
        "arf" 'org-roam-find-file
        "arg" 'org-roam-graph)

      (spacemacs/declare-prefix-for-mode 'org-mode "mr" "org-roam")
      (spacemacs/set-leader-keys-for-major-mode 'org-mode
        "rl" 'org-roam
        "rt" 'org-roam-dailies-today
        "rb" 'org-roam-switch-to-buffer
        "rf" 'org-roam-find-file
        "ri" 'org-roam-insert
        "rg" 'org-roam-graph))))
