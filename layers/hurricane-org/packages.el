(defconst hurricane-org-packages
  `(
    (org :location built-in)
    (org-mac-link :location built-in)
    (org-pomodoro :location (recipe
                             :fetcher github
                             :repo "lolownia/org-pomodoro"))
    (ox-latex :location built-in)
    (ox-md :location built-in)
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
    (anki-editor :location (recipe
                            :fetcher github
                            :repo "leoc/anki-editor"
                            :branch "develop"))
    (org-media-note :location (recipe
                               :fetcher github
                               :repo "yuchen-lea/org-media-note"))
    (mpv :location (recipe
                    :fetcher github
                    :repo "kljohann/mpv.el"))
    (shrface :location (recipe
                        :fetcher github
                        :repo "chenyanming/shrface"))
    org-roam
    (e2ansi :location (recipe
                       :fetcher github
                       :repo "Lindydancer/e2ansi"))
    (org-super-links :location (recipe
                                :fetcher github
                                :repo "toshism/org-super-links"))
    (incremental-reading :location local)
    (popweb :location (recipe
                       :fetcher github
                       :repo "czqhurricnae/popweb"
                       :files ("*.*" "extension")))
    (org-latex-impatient :location (recipe
                                    :fetcher github
                                    :repo "yangsheng6810/org-latex-impatient"))
    (org-ql :location (recipe
                       :fetcher github
                       :repo "alphapapa/org-ql"
                       :exclude "helm-org-ql.le"))
    (org-pandoc-import :location (recipe
                                  :fetcher github
                                  :repo "tecosaur/org-pandoc-import"
                                  :files ("*.el" "filters" "preprocessors")))
    (org-roam-backlink-collections :location local)
    (org-imagine :location (recipe
                            :fetcher github
                            :repo "metaescape/org-imagine"))
    )
  )

(defun hurricane-org/post-init-org-pomodoro ()
  (progn
    (add-hook 'org-pomodoro-finished-hook #'(lambda () (hurricane//notify-osx "Pomodoro Finished" "Have a break!")))
    (add-hook 'org-pomodoro-short-break-finished-hook #'(lambda () (hurricane//notify-osx "Short Break" "Ready to Go?")))
    (add-hook 'org-pomodoro-long-break-finished-hook #'(lambda () (hurricane//notify-osx "Long Break" "Ready to Go?")))
    (add-hook 'org-pomodoro-kill-hook #'(lambda () (hurricane//notify-osx "Pomodoro Killed" "One does not simply kill a pomodoro!")))))

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
         (ditaa . t)
         (calc . t)))

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

      (add-hook 'org-mode-hook #'(lambda () (spacemacs/toggle-line-numbers-off)) 'append)
      (add-hook 'org-mode-hook #'(lambda ()
                                  ;; Keybinding for inserting code blocks.
                                  (local-set-key (kbd "C-c s i")
                                                 'hurricane/org-insert-src-block)
                                  ;; Keybinding for editing source code blocks.
                                  (local-set-key (kbd "C-c s e")
                                                 'org-edit-special)
                                  ;; Keybinding for executing source code blocks.
                                  (local-set-key (kbd "C-c s r")
                                                 'org-src-do-at-code-block)
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
      (setq org-use-sub-superscripts '{})

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
      (setq org-agenda-files (list org-agenda-dir (concat deft-dir (file-name-as-directory "notes")) org-agenda-file-note))

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
      ;; %a          Annotation, normally the link created with org-store-link.
      ;; %A          Like %a, but prompt for the description part.
      ;; @see: https://stackoverflow.com/questions/28417026/org-mode-capture-templates
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
              ;; ("W" "Work" entry (file+headline org-agenda-file-gtd "Work")
              ;;  "* TODO [#A] %?\n  %i\n %U"
              ;;  :empty-lines 1)
              ("c" "Chrome" entry (file+headline org-agenda-file-note "Quick notes")
               "* TODO [#C] %?\n %(hurricane/retrieve-chrome-current-tab-url)\n %i\n %U"
               :empty-lines 1)
              ("l" "links" entry (file+headline org-agenda-file-note "Quick notes")
               "* TODO [#C] %?\n  %i\n %a \n %U"
               :empty-lines 1)
              ("j" "Journal Entry"
               ;; https://github.com/akirak/org-reverse-datetree
               entry (file+datetree org-agenda-file-journal)
               "* %U - %^{heading}\n %?"
               :empty-lines 1)
              ;; ("p" "Protocol"
              ;;  entry (file+headline org-agenda-file-note "Quick notes")
              ;;  "* %^{Title}\n %:initial")
              ("w" "Web site" entry
               (file "")
               "* %a :website:\n\n%U %?\n\n%:initial")
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

      (org-link-set-parameters "video" :export 'hurricane//org-video-link-export)

      (define-key evil-normal-state-map (kbd "C-c C-w") #'org-refile)
      (define-key global-map (kbd "<f9>") #'popweb-org-roam-node-preview-select)
      (define-key org-mode-map (kbd "<f9>") #'popweb-org-roam-link-preview-select)
      (define-key org-mode-map (kbd "<f8>") #'popweb-org-roam-node-backlinks-preview)
      (define-key org-mode-map (kbd "<f11>") #'org-transclusion-make-from-link)
      (define-key org-mode-map (kbd "<f12>") #'org-transclusion-mode)

      (setq org-publish-project-alist
            `(("orgfiles"
               ;; Sources and destinations for files.
               ;; local org files directory.
               :base-directory ,(concat deft-dir (file-name-as-directory "notes"))
               :publishing-directory ,blog-dir
               ;; :preparation-function
               ;; :complete-function

               ;; Selecting files.
               :base-extension "org"
               ;; :exclude "PrivatePage.org"
               ;; :include
               :recursive t

               ;; Publishing action.
               :publishing-function org-html-publish-to-html
               :htmlized-source nil

               ;; Options for the exporters.

               ;; {{
               ;; Generic properties.
               ;; :archived-trees org-export-with-archived-trees
               ;; :exclude-tags org-export-exclude-tags
               ;; org-export-headline-levels.
               :headline-levels 4
               ;; :language org-export-default-language
               ;; :preserve-breaks org-export-preserve-breaks
               ;; org-export-with-section-numbers.
               :section-numbers nil
               ;; :select-tags org-export-select-tags
               ;; org-export-with-author.
               :with-author "Hurricane Chen"
               ;; :with-broken-links org-export-with-broken-links
               ;; org-export-with-clocks.
               ;; :with-clocks t
               ;; org-export-with-creator.
               ;; :with-creator nil
               ;; :with-date org-export-with-date
               ;; :with-drawers org-export-with-drawers
               ;; :with-email org-export-with-email
               ;; :with-emphasize org-export-with-emphasize
               ;; :with-fixed-width org-export-with-fixed-width
               :with-footnotes org-export-with-footnotes
               ;; :with-latex org-export-with-latex
               ;; :with-planning org-export-with-planning
               ;; org-export-with-priority.
               :with-priority t
               ;; :with-properties org-export-with-properties
               ;; :with-special-strings org-export-with-special-strings
               ;; :with-sub-superscript org-export-with-sub-superscripts
               ;; :with-tables org-export-with-tables
               ;; :with-tags org-export-with-tags
               ;; :with-tasks org-export-with-tasks
               ;; :with-timestamps org-export-with-timestamps
               ;; :with-title org-export-with-title
               ;; org-export-with-toc.
               :with-toc t
               ;; :with-todo-keywords org-export-with-todo-keywords
               ;; }}

               ;; {{
               ;;  HTML specific properties
               ;; :html-allow-name-attribute-in-anchors org-html-allow-name-attribute-in-anchors
               ;; :html-checkbox-type org-html-checkbox-type
               :html-container "section"
               ;; :html-divs org-html-divs
               ;; org-html-doctype.
               :html-doctype "html5"
               ;; :html-extension org-html-extension
               ;; org-html-footnote-format.
               ;; :html-footnote-format nil
               ;; :html-footnote-separator org-html-footnote-separator
               ;; :html-footnotes-section org-html-footnotes-section
               ;; :html-format-drawer-function org-html-format-drawer-function
               ;; :html-format-headline-function org-html-format-headline-function
               ;; :html-format-inlinetask-function org-html-format-inlinetask-function
               :html-head-extra ,hurricane/head-extra
               ;; :html-head-include-default-style nil
               ;; :html-head-include-scripts nil
               ;; :html-head org-html-head
               ;; :html-home/up-format org-html-home/up-format
               ;; :html-html5-fancy t
               ;; :html-indent org-html-indent
               ;; :html-infojs-options org-html-infojs-options
               ;; :html-infojs-template org-html-infojs-template
               ;; :html-inline-image-rules org-html-inline-image-rules
               ;; :html-inline-images org-html-inline-images
               ;; :html-link-home org-html-link-home
               ;; :html-link-org-files-as-html org-html-link-org-files-as-html
               ;; :html-link-up org-html-link-up
               ;; :html-link-use-abs-url org-html-link-use-abs-url
               ;; :html-mathjax-options org-html-mathjax-options
               ;; :html-mathjax-template org-html-mathjax-template
               ;; :html-metadata-timestamp-format org-html-metadata-timestamp-format
               ;; org-html-postamble-format.
               ;; :html-postamble-format t
               ;; org-html-postamble.
               :html-postamble ,hurricane/postamble
               ;; :html-preamble-format org-html-preamble-format
               ;; org-html-preamble.
               ;; :html-preamble ,hurricane/preamble
               ;; :html-self-link-headlines org-html-self-link-headlines
               ;; :html-table-align-individual-field de{org-html-table-align-individual-fields
               ;; :html-table-attributes org-html-table-default-attributes
               ;; :html-table-caption-above org-html-table-caption-above
               ;; :html-table-data-tags org-html-table-data-tags
               ;; :html-table-header-tags org-html-table-header-tags
               ;; :html-table-row-tags org-html-table-row-tags
               ;; :html-table-use-header-tags-for-first-column org-html-table-use-header-tags-for-first-column
               ;; :html-tag-class-prefix org-html-tag-class-prefix
               ;; :html-text-markup-alist org-html-text-markup-alist
               ;; :html-todo-kwd-class-prefix org-html-todo-kwd-class-prefix
               ;; :html-toplevel-hlevel org-html-toplevel-hlevel
               ;; :html-use-infojs org-html-use-infojs
               ;; :html-validation-link org-html-validation-link
               ;; :html-viewport org-html-viewport
               ;; :html-wrap-src-lines org-html-wrap-src-lines
               ;; :html-xml-declaration org-html-xml-declaration
               ;; }}

               ;; {{
               ;; Markdown specific properties.
               ;; :md-footnote-format org-md-footnote-format
               ;; :md-footnotes-section org-md-footnotes-section
               ;; :md-headline-style org-md-headline-style
               ;; }}

               ;; {{
               ;; Other options
               :table-of-contents t
               ;; :style "<link rel=\"stylesheet\" href=\"../other/mystyle.css\" type=\"text/css\" />"
               ;; }}
               ;; :auto-sitemap nil
               ;; :exclude "node_modules"
               ;; :sitemap-title "Hurricane"
               ;; :sitemap-sort-files anti-chronologically
               ;; :sitemap-function hurricane/org-publish-sitemap
               ;; :sitemap-format-entry sitemap-format-entry
               ;; :sitemap-filename "index.org"
               )

              ;; Static assets.
              ("images"
               :base-directory ,(concat deft-dir (file-name-as-directory "notes") (file-name-as-directory "./static"))
               :base-extension "css\\|js\\|png\\|jpg\\|gif\\|png\\|svg\\|gif\\|jpeg"
               :publishing-directory ,(concat blog-dir (file-name-as-directory "./static"))
               :exclude "node_modules"
               :recursive t
               :publishing-function org-publish-attachment
               )

              ("website" :components ("orgfiles" "images"))
              ("statics" :components ("images"))
              ))
      (setq org-hugo-base-dir "..")

      ;; {{
      ;; @see: https://github.com/vascoferreira25/org-mode-incremental-reading
      ;; org-protocol support for opening a file - needed for ‘my-anki-editor-backlink’.
      (add-to-list
       'org-protocol-protocol-alist
       '("org-open-file" :protocol "open-file" :function org-protocol-open-file))

      (defun org-protocol-open-file (fname)
        "Process an org-protocol://open-file?url= style URL with FNAME.
      Change a filename by mapping URLs to local filenames as set
      in `org-protocol-project-alist'.
      The location for a browser's bookmark should look like this:
      javascript:location.href = \\='org-protocol://open-file?url=\\=' + \\
        encodeURIComponent(location.href)"
        ;; As we enter this function for a match on our protocol, the return value
        ;; defaults to nil.
        (let ((f (org-protocol-sanitize-uri
                  (plist-get (org-protocol-parse-parameters fname nil '(:file))
                             :file))))
          f))
      ;; }}

      ;; {{
      ;; @see: https://discourse.devontechnologies.com/t/org-mode-emacs-support/22396/6
      (defun hurricane//org-dtp-open (record-location)
        "Visit the dtp message with the given Message-ID."
        (eshell-command (concat "open x-devonthink-item:" record-location)))

      (org-link-set-parameters
       "x-devonthink-item"
       :follow 'hurricane//org-dtp-open
       :export (lambda (path desc backend)
                 (cond
                  ((eq 'html backend)
                   (format "<font color=\"red\"> <a href=\"x-devonthink-item:%s\">%s </a> </font>"
                           path
                           desc))))
       :face '(:foreground "red")
       :help-echo "Click me for devonthink link.")
      ;; }}
      (pixel-scroll-mode)
      )))

(defun hurricane-org/init-org-mac-link ()
  (use-package org-mac-link
    :after org
    :commands org-mac-grab-link
    :init
    (progn
      (add-hook 'org-mode-hook
                #'(lambda ()
                  (define-key org-mode-map (kbd "C-c g") 'org-mac-grab-link))))))

;; FIXME:
(defun hurricane-org/post-init-ox-reveal ()
  (setq org-reveal-root "file:///Users/c/.emacs.d/reveal-js"))

(defun hurricane-org/init-org-tree-slide ()
  (spacemacs|use-package-add-hook org
    :post-config
    (require 'org-tree-slide)
    (spacemacs/set-leader-keys "oto" #'org-tree-slide-mode)))

(defun hurricane-org/init-worf ()
  (spacemacs|use-package-add-hook org
    :post-config
    (require 'worf)
    (add-hook 'org-mode-hook #'worf-mode)))

(defun hurricane-org/init-org-protocol ()
  (use-package org-protocol))

(defun hurricane-org/init-org-protocol-capture-html ()
  (use-package org-protocol-capture-html))

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
  (progn
    ;; (setq org-download-screenshot-method "screencapture -i %s")
    (setq org-download-screenshot-method "pngpaste %s")
    (setq org-download-heading-lvl nil)
    (setq org-download-screenshot-file (expand-file-name "screenshot.jpg" temporary-file-directory))
    ;; Drag-and-drop to `dired`.
    (add-hook 'dired-mode-hook 'org-download-enable)
    ;; Disable DOWNLOADED attribute.
    (setq org-download-annotate-function (lambda (_link) ""))
    (spacemacs/set-leader-keys-for-major-mode 'org-mode
      "iDp" 'org-download-clipboard)))

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
    :post-init
    (require 'ox-html)))

(defun hurricane-org/init-ox-publish ()
  (spacemacs|use-package-add-hook org
    :post-init
    (require 'ox-publish)))

(defun hurricane-org/init-simple-httpd ()
  (use-package simple-httpd
    :ensure t
    :config
    (setq httpd-root blog-dir)))

;; Version: 20220121.2350
(defun hurricane-org/pre-init-org-roam ()
  (spacemacs|use-package-add-hook org-roam
  :post-init
  (setq org-roam-directory (concat deft-dir (file-name-as-directory "notes")))
  (setq org-roam-db-location (concat org-roam-directory "org-roam.db"))
  (setq org-roam-capture-templates
    '(
      ("d" "default" plain "%?"
       :target (file+head "${slug}.org"
                          "#+ROAM_KEY:\n#+PDF_KEY:\n#+PAGE_KEY:\n\n")
       :unnarrowed t)
      ))
  (setq org-roam-capture-ref-templates
    '(
      ("a" "Annotation" plain
       "%U ${body}\n"
       :target (file+head "${slug}.org"
                          "#+TITLE: ${title}\n#+ROAM_KEY: ${ref}\n\n")
       ;; :immediate-finish t
       :unnarrowed t
       )
      ("r" "ref" plain ""
       :target (file+head "${slug}.org"
                          "#+TITLE: ${title}\n#+ROAM_KEY: ${ref}\n\n")
       :unnarrowed t)))
  ))

(defun hurricane-org/post-init-org-roam ()
  (with-eval-after-load 'org-roam
   (progn
  (defun hurricane//display-line-numbers-customize ()
    (setq display-line-numbers 't)
    (org-display-inline-images))

  (add-hook 'org-mode-hook #'hurricane//display-line-numbers-customize)

  (advice-add 'org-roam-buffer-persistent-redisplay :before
			        #'(lambda () (remove-hook 'org-mode-hook 'hurricane//display-line-numbers-customize)))
  (advice-add 'org-roam-buffer-persistent-redisplay :after
			        #'(lambda () (add-hook 'org-mode-hook 'hurricane//display-line-numbers-customize)))

  (cl-defmethod org-roam-node-hierarchy ((node org-roam-node))
    "Return the hierarchy for the node."
    (let ((title (org-roam-node-title node))
          (olp (org-roam-node-olp node))
          (level (org-roam-node-level node))
          (filetitle (org-roam-node-file-title node)))
      (concat
       (if (> level 0) (concat filetitle " > "))
       (if (> level 1) (concat (string-join olp " > ") " > "))
       title))
    )

  (setq org-roam-node-display-template "${hierarchy:*} ${tags:8}")
  (setq org-roam-extract-new-file-path "${slug}.org")

  (cl-defmethod org-roam-node-slug ((node org-roam-node))
    "Return the slug of NODE."
    (let ((title (org-roam-node-title node))
          (slug-trim-chars '(;; Combining Diacritical Marks https://www.unicode.org/charts/PDF/U0300.pdf
                             768 ; U+0300 COMBINING GRAVE ACCENT
                             769 ; U+0301 COMBINING ACUTE ACCENT
                             770 ; U+0302 COMBINING CIRCUMFLEX ACCENT
                             771 ; U+0303 COMBINING TILDE
                             772 ; U+0304 COMBINING MACRON
                             774 ; U+0306 COMBINING BREVE
                             775 ; U+0307 COMBINING DOT ABOVE
                             776 ; U+0308 COMBINING DIAERESIS
                             777 ; U+0309 COMBINING HOOK ABOVE
                             778 ; U+030A COMBINING RING ABOVE
                             780 ; U+030C COMBINING CARON
                             795 ; U+031B COMBINING HORN
                             803 ; U+0323 COMBINING DOT BELOW
                             804 ; U+0324 COMBINING DIAERESIS BELOW
                             805 ; U+0325 COMBINING RING BELOW
                             807 ; U+0327 COMBINING CEDILLA
                             813 ; U+032D COMBINING CIRCUMFLEX ACCENT BELOW
                             814 ; U+032E COMBINING BREVE BELOW
                             816 ; U+0330 COMBINING TILDE BELOW
                             817 ; U+0331 COMBINING MACRON BELOW
                             )))
        (cl-flet* ((nonspacing-mark-p (char) (memq char slug-trim-chars))
                 (strip-nonspacing-marks (s)
                                           (string-glyph-compose
                                            (apply #'string
                                                   (seq-remove #'nonspacing-mark-p
                                                               (string-glyph-decompose s)))))
                   (cl-replace (title pair) (replace-regexp-in-string (car pair) (cdr pair) title)))
        (let* ((pairs `(;; ("[^[:alnum:][:digit:]]" . "-") ;; convert anything not alphanumeric
                        ;; ("--*" . "-")                   ;; remove sequential underscores
                        ("^-" . "")                     ;; remove starting underscore
                        ("-$" . "")))                   ;; remove ending underscore
               (slug (-reduce-from #'cl-replace (strip-nonspacing-marks title) pairs)))
           slug))))

  (defun hurricane//org-roam-strip-ANKI-CARD-drawers (s)
    (with-temp-buffer
      (insert s)
      (goto-char (point-min))
      (org-element-map (org-element-parse-buffer) 'drawer
        (lambda (drawer)
          (if (string= "ANKI-CARD" (org-element-property :drawer-name drawer))
              (let* ((begin (org-element-property :begin drawer))
                     (end (org-element-property :end drawer)))
                (delete-region begin end)))))
      (buffer-string)))

  (add-to-list 'org-roam-preview-postprocess-functions
               #'hurricane//org-roam-strip-ANKI-CARD-drawers)

  (defun hurricane//org-roam-strip-property-drawers (s)
    (with-temp-buffer
      (insert s)
      (goto-char (point-min))
      (org-element-map (org-element-parse-buffer) 'property-drawer
        (lambda (property-drawer)
          (let* ((begin (org-element-property :begin property-drawer))
                 (end (org-element-property :end property-drawer)))
            (delete-region begin end))))
      (buffer-string)))

  (add-to-list 'org-roam-preview-postprocess-functions
               #'hurricane//org-roam-strip-property-drawers)

  (dolist (func org-roam-mode-section-functions)
    (advice-add func :after #'(lambda (node) (org-display-inline-images))))

  ;; ;; @See: https://github.com/org-roam/org-roam/issues/2029
  ;; (defun hurricane//org-roam-db-map-links (fns)
  ;;   "Run FNS over all links in the current buffer."
  ;;   (org-with-point-at 1
  ;;     (while (re-search-forward org-link-any-re nil :no-error)
  ;;       ;; `re-search-forward' let the cursor one character after the link, we need to go backward one char to
  ;;       ;; make the point be on the link.
  ;;       (backward-char)
  ;;       (let* ((element (org-element-context))
  ;;              (type (org-element-type element))
  ;;              link bounds)
  ;;         (cond
  ;;          ;; Links correctly recognized by Org Mode
  ;;          ((eq type 'link)
  ;;           (setq link element))
  ;;          ;; Prevent self-referencing links in ROAM_REFS
  ;;          ((and (eq type 'node-property)
  ;;                (org-roam-string-equal (org-element-property :key element) "ROAM_REFS"))
  ;;           nil))
  ;;         (when link
  ;;           (dolist (fn fns)
  ;;             (funcall fn link)))))))

  ;; (advice-add #'org-roam-db-map-links :override #'hurricane//org-roam-db-map-links)
  ))
)

(defun hurricane-org/post-init-org-transclusion ()
  (progn
    (setq org-transclusion-include-first-section t)
    (setq org-transclusion-exclude-elements '(property-drawer org-drawer keyword))

    (defun hurricane//org-transclusion-add-org-id (link plist)
      "Return a list for Org-ID LINK object and PLIST.
Return nil if not found."
      (when (string= "id" (org-element-property :type link))
        ;; when type is id, the value of path is the id
        (let* ((id (org-element-property :path link))
               (mkr (or (ignore-errors (org-id-find id t))
                        (with-current-buffer (find-file-noselect (aref (org-roam-node-from-id id) 1))
                                             (goto-char (aref (org-roam-node-from-id id) 8))
                                             (point-marker))))
               (payload '(:tc-type "org-id"))
               (content (org-transclusion-content-org-marker mkr plist))
               (footnote-content))
          (if mkr
              (progn
                (message "%s" mkr)
                (let* ((footnote-label-list
                        (with-temp-buffer
                          (insert (plist-get (org-transclusion-content-org-marker mkr plist) :src-content))
                          (org-element-map (org-element-parse-buffer) 'footnote-reference
                            (lambda (reference)
                              (org-element-property :label reference))))))
                  (if (and mkr (marker-buffer mkr) (buffer-live-p (marker-buffer mkr)) footnote-label-list)
                      (with-temp-buffer
                        (insert-buffer (marker-buffer mkr))
                        (-map (lambda (label)
                                (setq footnote-content
                                      (concat footnote-content (buffer-substring-no-properties
                                                                (nth 1 (org-footnote-get-definition label))
                                                                (nth 2 (org-footnote-get-definition label))))))
                              footnote-label-list)
                        ))
                  (setq content (plist-put content ':src-content (concat (plist-get content :src-content) "\n" footnote-content)))
                  )
                (append payload content)
                )
            (message
             (format "No transclusion done for this ID. Ensure it works at point %d, line %d"
                     (point) (org-current-line)))
            nil))))

    (push 'hurricane//org-transclusion-add-org-id org-transclusion-add-functions)
    ))

(defun hurricane-org/init-anki-editor ()
  (use-package anki-editor
    :diminish anki-editor-mode))

(defun hurricane-org/init-org-media-note ()
  (use-package org-media-note
    :defer t
    :hook (after-init . org-media-note-mode)
    :init
    (spacemacs/set-leader-keys "av" #'org-media-note-hydra/body)
    :config
    (make-variable-buffer-local 'org-media-note-screenshot-image-dir)))

(defun hurricane-org/init-mpv ()
  (use-package mpv
    :defer t))

(defun hurricane-org/init-shrface ()
  (use-package shrface
    :defer t
    :config
    (shrface-basic)
    (shrface-trial)
    (shrface-default-keybindings) ; setup default keybindings
    (setq shrface-href-versatile t)))

(defun hurricane-org/init-e2ansi ()
  (use-package e2ansi))

(defun hurricane-org/init-org-super-links ()
  (use-package org-super-links))

(defun hurricane-org/init-incremental-reading ()
  (use-package incremental-reading
    :diminish incremental-reading-mode
    :init
    (defvar incremental-reading-map
      (let ((map (make-sparse-keymap)))
        (define-key map (kbd "c") #'anki-editor-cloze-region)
        (define-key map (kbd "C") #'anki-editor-cloze-dwim)
        (define-key map (kbd "C-c C-c") #'incremental-reading-parse-cards)
        map))

    (defvar incremental-reading-extract-functions
      '(incremental-reading-extract-basic
        incremental-reading-extract-basic-no-back
        incremental-reading-extract-cloze
        incremental-reading-extract-cloze-no-back))

    (defun hurricane//add-incremental-reading-keymap ()
      (interactive)
      (org-element-map (org-element-parse-buffer) 'special-block
        (lambda (special-block)
          (when (string= "ANKI" (s-upcase (org-element-property :type special-block)))
            (let ((context-bg-eds (list))
                  (produce-list (list)))
              (org-element-map special-block 'special-block
                (lambda (field)
                  (when (string= "FIELD" (s-upcase (org-element-property :type field)))
                    ;; Get the each field's context begin and end position list.
                    (push (org-element-property :contents-begin field) context-bg-eds)
                    (push (org-element-property :contents-end field) context-bg-eds))))
              (while context-bg-eds
                (push (pop context-bg-eds) produce-list))
              (while produce-list
                (let ((begin (pop produce-list))
                      (end (pop produce-list)))
                  (if (and begin end)
                   (add-text-properties begin end
                                       `(local-map ,incremental-reading-map))))
                ))))))

    (dolist (func incremental-reading-extract-functions)
      (advice-add func :after #'hurricane//add-incremental-reading-keymap))

    (add-hook 'incremental-reading-mode-hook #'anki-editor-mode)
    (add-hook 'incremental-reading-mode-hook #'hurricane//add-incremental-reading-keymap)
    (add-hook 'after-save-hook #'hurricane//add-incremental-reading-keymap)
    :hook (org-mode . incremental-reading-mode)
    :custom
    (incremental-reading--basic-template ":ANKI-CARD:
#+ATTR_DECK: %s
#+ATTR_TYPE: Basic
#+ATTR_TAGS: %s
#+BEGIN_ANKI org
#+ATTR_FIELD: Front
#+BEGIN_FIELD
#+END_FIELD

#+ATTR_FIELD: Back
#+BEGIN_FIELD
%s
#+END_FIELD
#+END_ANKI
:END:
\n")))

;; /usr/bin/env python3 -m pip install PyQt6 PyQtWebEngine epc
;; python3 -m pip install PyQt6 PyQt6-Qt6 PyQt6-sip PyQt6-WebEngine PyQt6-WebEngine-Qt6
;; 系统中存在 Python3.9 和 Python3.10 两种版本，如果原来的3.9 版本安装依赖，升级到3.10 后将无法使用，需要重新安装。
(defun hurricane-org/init-popweb ()
  (use-package popweb
    :ensure t
    :load-path ("elpa/28.2/develop/popweb-20230208.13018" "elpa/28.2/develop/popweb-20230208.13018/extension/latex" "elpa/28.2/develop/popweb-20230208.13018/extension/dict" "elpa/28.2/develop/popweb-20230208.13018/extension/org-roam" "elpa/28.2/develop/popweb-20230208.13018/extension/url-preview")
    :init
    (require 'popweb-dict)
    (require 'popweb-latex)
    (require 'popweb-org-roam-link)
    (defun hurricane/popweb-dict-eudic-liju-search-at-point ()
      (interactive)
      (if (display-graphic-p)
          (popweb-dict-eudic-liju-input nil (lc-corpus--sentence))))

    (defun popweb-dict-join-dirs (root dir file)
      (file-name-concat root dir file))

    (defun popweb-dict-js-file-path (file)
      (popweb-dict-join-dirs (file-name-directory popweb-dict-module-path) "js" file))

    (popweb-dict-create "eudic-liju"
                        "https://dict.eudic.net/liju/en/%s"
                        ""
                        (popweb-dict-js-file-path "eudic-liju.js"))
    :config
    (setq popweb-org-roam-link-popup-window-height-scale 1.0)
    (setq popweb-org-roam-link-popup-window-width-scale 1.0)
    (setq gnus-button-url-regexp "\\b\\(\\(www\\.\\|\\(s?https?\\|ftp\\|file\\|gopher\\|nntp\\|news\\|telnet\\|wais\\|mailto\\|info\\):\\)\\(//[-a-z0-9_.]+:[0-9]*\\)?\\(?:[-a-z0-9_=#$@~%&*+\\/[:word:]!?:;.,]+([-a-z0-9_=#$@~%&*+\\/[:word:]!?:;.,]+[-a-z0-9_=#$@~%&*+\\/[:word:]]*)\\(?:[-a-z0-9_=#$@~%&*+\\/[:word:]!?:;.,]+[-a-z0-9_=#$@~%&*+\\/[:word:]]\\)?\\|[-a-z0-9_=#$@~%&*+\\/[:word:]!?:;.,]+[-a-z0-9_=#$@~%&*+\\/[:word:]]\\)\\)")
    (with-eval-after-load 'ivy
      (ivy-set-actions
       'popweb-org-roam-node-preview-select
       '(("I" (lambda (x)
                (let* ((node (cdr x))
                       (note-id (org-roam-node-id node))
                       (note-title (org-roam-node-title node)))
                  (insert
                   (format
                    "[[id:%s][%s]]\n\n"
                    note-id
                    note-title)))) "Insert link")
         ("i" (lambda (x)
                (let* ((node (cdr x))
                       (note-id (org-roam-node-id node))
                       (note-title (org-roam-node-title node)))
                  (insert
                   (format
                    "#+transclude: [[id:%s][%s]]\n\n"
                    note-id
                    note-title)))) "Insert links with transclusions")
         )))

    (advice-add #'org-roam-node-read :override #'popweb-org-roam-node-preview-select)
    :custom
    (popweb-proxy-type provixy-type)
    (popweb-proxy-host provixy-host)
    (popweb-proxy-port provixy-port)
    ;; (popweb-enable-developer-tools t)
    :bind
    (("C-c y" . hurricane/popweb-dict-eudic-liju-search-at-point)
     ("C-c Y" . my-youdao-search-at-point))
    ))

;; npm install mathjax-node-cli
(defun hurricane-org/init-org-latex-impatient ()
  (use-package org-latex-impatient
    :defer t
    :hook (org-mode . org-latex-impatient-mode)
    :init
    (setq org-latex-impatient-tex2svg-bin
          ;; location of tex2svg executable
          "~/node_modules/mathjax-node-cli/bin/tex2svg")))

(defun hurricane-org/init-org-ql ()
  (use-package org-ql))

(defun hurricane-org/init-org-pandoc-import ()
  (use-package org-pandoc-import
    :ensure t))

(defun hurricane-org/init-org-roam-backlink-collections ()
  (use-package org-roam-backlink-collections))

(defun hurricane-org/init-org-imagine ()
  (use-package org-imagine))
