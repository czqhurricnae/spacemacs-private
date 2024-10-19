(defconst hurricane-org-packages
  `(
    (org :location built-in)
    ;; (ox-latex :location built-in)
    ;; (ox-md :location built-in)
    ;; (org-protocol-capture-html :location (recipe
    ;;                                       :fetcher github
    ;;                                       :repo "alphapapa/org-protocol-capture-html"))
    ;; ,@(when sys/macp '(ob-ipython))
    (org-download :location (recipe
                             :fetcher github
                             :repo "abo-abo/org-download"))
    ;; (org2ctex :location (recipe
    ;;                      :fetcher github
    ;;                      :repo "tumashu/org2ctex"))
    ;; org-tree-slide
    ;; (ox-html :location built-in)
    ;; (ox-publish :location built-in)
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
    ;; (shrface :location (recipe
    ;;                     :fetcher github
    ;;                     :repo "chenyanming/shrface"))
    org-roam
    ;; (e2ansi :location (recipe
    ;;                    :fetcher github
    ;;                    :repo "Lindydancer/e2ansi"))
    (popweb :location (recipe
                       :fetcher github
                       :repo "czqhurricnae/popweb"
                       :files ("*.*" "extension")))
    ;; (org-latex-impatient :location (recipe
    ;;                                 :fetcher github
    ;;                                 :repo "yangsheng6810/org-latex-impatient"))
    ;; (org-ql :location (recipe
    ;;                    :fetcher github
    ;;                    :repo "alphapapa/org-ql"
    ;;                    :exclude "helm-org-ql.le"))
    ;; (org-pandoc-import :location (recipe
    ;;                               :fetcher github
    ;;                               :repo "tecosaur/org-pandoc-import"
    ;;                               :files ("*.el" "filters" "preprocessors")))
    ;; (org-roam-backlink-collections :location local)
    ;; (org-imagine :location (recipe
    ;;                         :fetcher github
    ;;                         :repo "metaescape/org-imagine"
    ;;                         :files ("*.el" "view")))
    (org-link-edit :location (recipe
                              :fetcher github
                              :repo "emacsmirror/org-link-edit"))
    ;; (org-gtd :location (recipe
    ;;                     :fetcher github
    ;;                     :repo "Trevoke/org-gtd.el"))
    ;; (org-super-agenda :location (recipe
    ;;                              :fetcher github
    ;;                              :repo "alphapapa/org-super-agenda"))
    (anki-helper :location (recipe
                            :fetcher github
                            :repo "Elilif/emacs-anki-helper"))
    (org-noter :location (recipe
                          :fetcher github
                          :repo "org-noter/org-noter"
                          :files ("*.el" "modules" "other")))
    ;; (helm-org-ql :location (recipe
    ;;                         :fetcher github
    ;;                         :repo "alphapapa/org-ql"
    ;;                         :files ("helm-org-ql.el")))
    ;; (oer-reveal :location (recipe
    ;;                        :fetcher github
    ;;                        :repo "emacsmirror/oer-reveal"
    ;;                        :files ("*.el" "LICENSES" "css" "examples" "org" "title-slide")))
    ;; (emacsconf-el :location (recipe
    ;;                          :fetcher github
    ;;                          :repo "emacsconf/emacsconf-el"
    ;;                          :files ("*.el")))
    ;; (org-drawio :location (recipe
    ;;                        :fetcher github
    ;;                        :repo "kimim/org-drawio"))
    (org-remark :location (recipe
                           :fetcher github
                           :repo "nobiot/org-remark"))
    (org-modern-indent :location (recipe
                                  :fetcher github
                                  :repo "jdtsmith/org-modern-indent"))
    ;; (org-bars :location (recipe
    ;;                      :fetcher github
    ;;                      :repo "tonyaldon/org-bars"))
    ;; (org-tags-filter :location local)
    ;; (org-roam-dblocks :location local)
    (org-web-tools :location (recipe
                              :fetcher github
                              :repo "alphapapa/org-web-tools"))
    )
  )

(defun hurricane-org/post-init-org-pomodoro ()
  (progn
    (add-hook 'org-pomodoro-finished-hook #'(lambda () (hurricane//notify-osx "Pomodoro Finished" "Have a break!")))
    (add-hook 'org-pomodoro-short-break-finished-hook #'(lambda () (hurricane//notify-osx "Short Break" "Ready to Go?")))
    (add-hook 'org-pomodoro-long-break-finished-hook #'(lambda () (hurricane//notify-osx "Long Break" "Ready to Go?")))
    (add-hook 'org-pomodoro-kill-hook #'(lambda () (hurricane//notify-osx "Pomodoro Killed" "One does not simply kill a pomodoro!")))))

;; In order to export pdf to support Chinese, I should install Latex in here:
;; @See: https://www.tug.org/mactex/
;; @See: http://freizl.github.io/posts/2012-04-06-export-orgmode-file-in-Chinese.html
;; @See: http://stackoverflow.com/questions/21005885/export-org-mode-code-block-and-result-with-different-styles
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
            (quote ((sequence "TODO(t)" "NEXT(n)" "WAIT(w)" "|" "DONE(d!/!)")
                    (sequence "SOMEDAY(S)" "|" "CNCL(c@/!)" "MEETING(m)" "PHONE(p)"))))

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
      (setq org-agenda-files (list org-agenda-dir org-agenda-file-note))

      (with-eval-after-load 'org-agenda
        (define-key org-agenda-mode-map (kbd "P") 'org-pomodoro)
        (spacemacs/set-leader-keys-for-major-mode 'org-agenda-mode
          "." 'spacemacs/org-agenda-transient-state/body))

      ;; {{
      ;; @See: https://emacs.stackexchange.com/questions/22396/export-without-links
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
      ;; @See: http://www.howardism.org/Technical/Emacs/journaling-org.html
      ;; %a          Annotation, normally the link created with org-store-link.
      ;; %A          Like %a, but prompt for the description part.
      ;; @See: https://stackoverflow.com/questions/28417026/org-mode-capture-templates
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
      ;; (setq org-agenda-custom-commands
      ;;       '(
      ;;         ("w" . "任务安排")
      ;;         ("wa" "重要且紧急的任务" tags-todo "+PRIORITY=\"A\"")
      ;;         ("wb" "重要且不紧急的任务" tags-todo "-Weekly-Monthly-Daily+PRIORITY=\"B\"")
      ;;         ("wc" "不重要且紧急的任务" tags-todo "+PRIORITY=\"C\"")
      ;;         ("p" . "项目安排")
      ;;         ("pw" tags-todo "PROJECT+WORK+CATEGORY=\"cocos2d-x\"")
      ;;         ("pl" tags-todo "PROJECT+DREAM+CATEGORY=\"hurricane\"")
      ;;         ("W" "Weekly Review"
      ;;          ((stuck "") ;; Review stuck projects as designated by org-stuck-projects.
      ;;           (tags-todo "PROJECT") ;; Review all projects (assuming you use todo keywords to designate projects).
      ;;           ))))

      ;; (org-link-set-parameters "video" :export 'hurricane//org-video-link-export)

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
      ;; @See: https://github.com/vascoferreira25/org-mode-incremental-reading
      ;; org-protocol support for opening a file - needed for ‘my-anki-editor-backlink’.
      (with-eval-after-load 'org-protocol
        (add-to-list
         'org-protocol-protocol-alist
         '("org-noter-pdf" :protocol "open-pdf" :function org-protocol-open-pdf)))

      (defun org-protocol-open-pdf(fname)
        "Process an org-protocol://open-file?url= style URL with FNAME.
      Change a filename by mapping URLs to local filenames as set
      in `org-protocol-project-alist'.
      The location for a browser's bookmark should look like this:
      javascript:location.href = \\='org-protocol://open-pdf?url=\\=' + \\
        encodeURIComponent(location.href)"
        ;; As we enter this function for a match on our protocol, the return value
        ;; defaults to nil.
        (let ((f (org-protocol-sanitize-uri
                  (plist-get (org-protocol-parse-parameters fname nil '(:file))
                             :file))))
          f))
      ;; }}

      ;; {{
      ;; @See: https://discourse.devontechnologies.com/t/org-mode-emacs-support/22396/6
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

(defun hurricane-org/init-org-tree-slide ()
  (spacemacs|use-package-add-hook org
    :post-config
    (require 'org-tree-slide)))

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
;; @See: https://github.com/gregsexton/ob-ipython
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
;; @See: https://github.com/tumashu/org2ctex
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
    (require 'ox-html)))

(defun hurricane-org/init-ox-publish ()
  (spacemacs|use-package-add-hook org
    :post-config
    (require 'ox-publish)))

(defun hurricane-org/init-simple-httpd ()
  (use-package simple-httpd
    :config
    (setq httpd-root blog-dir)))

;; Version: 20220121.2350
(defun hurricane-org/pre-init-org-roam ()
  (spacemacs|use-package-add-hook org-roam
    :post-config
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
             :unnarrowed t)
            ))))

(defun hurricane-org/post-init-org-roam ()
  (with-eval-after-load 'org-roam
    (progn
      (defun hurricane//display-line-numbers-customize ()
        (setq display-line-numbers 't)
        (org-display-inline-images))

      (add-hook 'org-mode-hook #'hurricane//display-line-numbers-customize)

      (advice-add #'org-roam-buffer-persistent-redisplay :before
                  #'(lambda () (remove-hook 'org-mode-hook 'hurricane//display-line-numbers-customize)))
      (advice-add #'org-roam-buffer-persistent-redisplay :after
                  #'(lambda () (add-hook 'org-mode-hook 'hurricane//display-line-numbers-customize)))

      ;; (cl-defmethod org-roam-node-hierarchy ((node org-roam-node))
      ;;   "Return the hierarchy for the node."
      ;;   (let ((title (org-roam-node-title node))
      ;;         (olp (nreverse (org-roam-node-olp node)))
      ;;         (level (org-roam-node-level node))
      ;;         (filetitle (org-roam-node-file-title node)))
      ;;     (concat
      ;;      title
      ;;      (if (> level 1) (concat " < " (string-join olp " < ")))
      ;;      (if (> level 0) (concat " < " filetitle ))
      ;;      )))

      (cl-defmethod org-roam-node-hierarchy ((node org-roam-node))
        "Return hierarchy for NODE, constructed of its file title, OLP and direct title.
      If some elements are missing, they will be stripped out."
        (let ((title     (org-roam-node-title node))
              (olp       (nreverse (org-roam-node-olp node)))
              (level     (org-roam-node-level node))
              (filetitle (org-roam-node-file-title node))
              (separator (propertize " < " 'face 'shadow))
              )
          (cl-case level
            ;; node is a top-level file
            (0 filetitle)
            ;; node is a level 1 heading
            (1 (concat title separator (propertize filetitle 'face '(shadow italic))))
            ;; node is a heading with an arbitrary outline path
            (t (concat title
                       separator
                       (propertize (string-join olp " < ") 'face '(shadow italic))
                       separator
                       (propertize filetitle 'face '(shadow italic))
                       )))))

      (defun hurricane//org-roam-node-formatter (node)
        (let ((title     (org-roam-node-title node))
              (olp       (nreverse (org-roam-node-olp node)))
              (level     (org-roam-node-level node))
              (filetitle (org-roam-node-file-title node))
              (separator (propertize " < " 'face 'shadow))
              )
          (cl-case level
            ;; node is a top-level file
            (0 filetitle)
            ;; node is a level 1 heading
            (1 (concat title separator (propertize filetitle 'face '(shadow italic))))
            ;; node is a heading with an arbitrary outline path
            (t (concat title
                       separator
                       (propertize (string-join olp " < ") 'face '(shadow italic))
                       separator
                       (propertize filetitle 'face '(shadow italic))
                       )))))

      (setq org-roam-node-formatter #'hurricane//org-roam-node-formatter)

      (defun hurricane//org-roam-node-formatter (node)
        (let ((title     (org-roam-node-title node))
              (olp       (org-roam-node-olp node))
              (level     (org-roam-node-level node))
              (filetitle (org-roam-node-file-title node))
              (separator " > ")
              )
          (pcase level
            ;; node is a top-level file
            (0 filetitle)
            ;; node is a level 1 heading
            (1 (concat filetitle separator file))
            ;; node is a heading with an arbitrary outline path
            (_ (concat filetitle
                       separator
                       (string-join olp " > ")
                       separator
                       title
                       )))))

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
      (org-roam-db-autosync-mode)
      )))

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
    :ensure t
    :diminish anki-editor-mode))

;; bilibili：必须使用猫抓获取真实视频地址。
(defun hurricane-org/init-org-media-note ()
  (use-package org-media-note
    :ensure t
    :hook (after-init . org-media-note-mode)
    :init
    (spacemacs/set-leader-keys "av" #'org-media-note-pretty-hydra/body)
    :custom
    (org-media-note-mpv-online-website-options-alist
     '(("youtube\\.com"
        ;; best audio and best video that is 4K or lower, not using the av01 codec.
        "--ytdl-format=bestvideo[height<=?2160][vcodec!=?av01]+bestaudio/best"
        ;; download both automatically generated and manually created subtitles.
        "--ytdl-raw-options=write-subs=,write-auto-subs=,sub-langs=\"en,zh-Hans\",no-simulate=,skip-download=,proxy=http://127.0.0.1:7890")
       ("bilibili\\.com"
        ;; download subtitles and danmaku
        "--ytdl-raw-options=use-postprocessor=danmaku:when=before_dl,write-subs=,sub-langs=all,all-subs=,no-simulate=,skip-download=,cookies-from-browser=chrome,proxy=http://127.0.0.1:7890"
        ;; "--ytdl-raw-options-append=proxy=http://127.0.0.1:7890"
        ;; "--ytdl-raw-options=proxy=[http://127.0.0.1:7890]"
        )))
    :config
    (make-variable-buffer-local 'org-media-note-screenshot-image-dir)
    (require 'psearch)

    ;; (with-eval-after-load 'psearch
    ;;   (psearch-patch org-media-note--mpv-play-online-video
    ;;     (psearch-replace '`(if ,p1 ,p2 ,p3)
    ;;                      '`(if ,p1
    ;;                            (if (string-match-p (regexp-quote "bilibili") video-url)
    ;;                                (mpv-start video-url "--referrer=https://www.bilibili.com" "-v" "--no-resume-playback")
    ;;                              (mpv-start video-url))
    ;;                            ,p3))))

    (with-eval-after-load 'psearch
      (require 'org-media-note-mpv)

      (psearch-patch org-media-note-play-online-video
        (psearch-replace '`(read-string "Url to play: ")
                         '`(read-string "Url to play: " (hurricane//retrieve-chrome-current-tab-url))))
      (psearch-patch org-media-note--insert-file-link
        (psearch-replace '`(format "[[file:%s]]"
                                   (org-media-note--format-file-path file-path))
                         '`(format "[[file:%s]] "
                                   (org-media-note--format-file-path file-path)))))

    (require 'pretty-hydra)

    (with-eval-after-load 'pretty-hydra
      (pretty-hydra-define org-media-note-pretty-hydra
        (:color red
                :title (org-media-note--ui-title)
                :hint nil)
        ("File"
         (("o" org-media-note-play-smart
           (org-media-note--ui-play-smart-title)
           :width 20
           :exit t)
          ("j"
           (mpv-cycle-property "sub")
           "toggle subtitles")
          ("T"
           (mpv-cycle-property "ontop")
           "toggle ontop")
          ("]"
           (org-media-note-change-speed-by 0.1)
           "increase speed")
          ("["
           (org-media-note-change-speed-by -0.1)
           "decrease speed")
          ("z" org-media-note-mpv-toggle-speed "reset speed"))
         "Playback"
         (("<SPC>" mpv-pause "Play/Pause")
          ("l"
           (mpv-run-command "ab-loop")
           (org-media-note--ui-ab-loop-title)
           :width 31)
          ("g" org-media-note-goto-timestamp "Jump to timestamp")
          ("<left>" (org-media-note-seek 'backward) (format "Backward %s" (org-media-note--ui-seek-step t)))
          ("<right>" (org-media-note-seek 'forward) (format "Forward %s" (org-media-note--ui-seek-step t)))
          ("C-<left>"
           (mpv-run-command "sub-seek" -1)
           "Previous subtitle")
          ("C-<right>"
           (mpv-run-command "sub-seek" 1)
           "Next subtitle")
          ("<prior>"
           (mpv-run-command "add" "chapter" -1)
           "Previous Chapter")
          ("<next>"
           (mpv-run-command "add" "chapter" 1)
           "Next Chapter"))
         "Volume"
         (("+"
           (org-media-note-change-volume-by 5)
           "Up")
          ("-"
           (org-media-note-change-volume-by -5)
           "Down")
          ("0" org-media-note-mpv-toggle-volume "toggle")
          ("m"
           (mpv-cycle-property "mute")
           "(un)mute"))
         "Note"
         (("i" org-media-note-insert-link "Insert timestamp")
          ("a" org-media-note-adjust-timestamp-offset "Adjust timestamp")
          ("S"
           (if (org-media-note--ab-loop-p)
               (org-media-note-capture-ab-loop-and-insert)
             (org-media-note-insert-screenshot))
           (if (org-media-note--ab-loop-p)
               "Insert ab-loop clip"
             "Insert Screenshot"))
          ("s" org-media-note-insert-sub-text "Insert subtitle")
          ("c"
           (if (org-media-note--ab-loop-p)
               (let* ((time-a (mpv-get-property "ab-loop-a"))
                      (time-b (mpv-get-property "ab-loop-b"))
                      (timestamp-a (org-media-note--seconds-to-timestamp time-a))
                      (timestamp-b (org-media-note--seconds-to-timestamp time-b)))
                 (anki-clip-mp3 timestamp-a timestamp-b))
             (user-error "[org-media-note] You need to finish setting A-B loop."))
           "Clip Mp3 of A-B loop")
          ("C"
           (if (org-media-note--ab-loop-p)
               (let* ((time-a (mpv-get-property "ab-loop-a"))
                      (time-b (mpv-get-property "ab-loop-b"))
                      (pos (mpv-get-playback-position))
                      (timestamp-a (org-media-note--seconds-to-timestamp time-a))
                      (timestamp-b (org-media-note--seconds-to-timestamp time-b)))
                 (org-media-note-insert-clip timestamp-a timestamp-b))
             (user-error "[org-media-note] You need to finish setting A-B loop."))
           "Insert clip of A-B loop")
          ("n" org-return-indent "Insert Org newline")
          ("H-m" org-media-note-merge-item "Merge items"))
         "Import"
         (("I p" org-media-note-insert-note-from-pbf
           "from pbf")
          ("I n" org-media-note-insert-note-from-noted
           "from Noted")
          ("I t" org-media-note-convert-from-org-timer
           "from org-timer")
          ("I s" org-media-note-insert-note-from-subtitle
           "from subtitle")
          ("I c" org-media-note-insert-note-from-chapter-list
           "from chapters"))
         "Config"
         (("t m" org-media-note-toggle-auto-insert-item
           "Auto insert media item" :toggle org-media-note-auto-insert-item)
          ("t s" org-media-note-toggle-save-screenshot
           "Auto insert screenshot" :toggle org-media-note-save-screenshot-p)
          ("t S" org-media-note-toggle-screenshot-with-sub
           "Screenshot with sub" :toggle org-media-note-screenshot-with-sub)
          ("t l" org-media-note-set-ab-loop-capture-method
           (format "AB-loop Clip: %s"
                   (if org-media-note-capture-ab-loop-ask-each-time
                       "always ask" org-media-note-default-capture-ab-loop-function-name)))
          ("t c" org-media-note-toggle-refcite
           "Cite key instead of path" :toggle org-media-note-use-refcite-first)
          ("t p" org-media-note-toggle-pause-after-insertion
           "Pause after insert link" :toggle org-media-note-pause-after-insert-link)
          ("t t" org-media-note-toggle-timestamp-pattern
           (format "Timestamp format: %s"
                   (cond
                    ((eq org-media-note-timestamp-pattern 'hms) "hh:mm:ss")
                    ((eq org-media-note-timestamp-pattern 'hmsf) "hh:mm:ss.fff")))
           :width 29)
          ("t M" org-media-note-set-separator
           (format "Separator when merge: %s" org-media-note-separator-when-merge))
          ("t <right>" org-media-note-set-seek-method
           (format "Seek step: %s" (org-media-note--ui-seek-step t)))))))))

(defun hurricane-org/init-mpv ()
  (use-package mpv
    :ensure t))

(defun hurricane-org/init-shrface ()
  (use-package shrface
    :ensure t
    :config
    (shrface-basic)
    (shrface-trial)
    (shrface-default-keybindings) ; setup default keybindings
    (setq shrface-href-versatile t)))

(defun hurricane-org/init-e2ansi ()
  (use-package e2ansi
    :ensure t))

;; /usr/bin/env python3 -m pip install PyQt6 PyQtWebEngine epc
;; python3 -m pip install PyQt6 PyQt6-Qt6 PyQt6-sip PyQt6-WebEngine PyQt6-WebEngine-Qt6
;; 系统中存在 Python3.9 和 Python3.10 两种版本，如果原来的3.9 版本安装依赖，升级到3.10 后将无法使用，需要重新安装。
;; (setq popweb-enable-developer-tools t)
(defun hurricane-org/init-popweb ()
  (use-package popweb
    :ensure t
    :load-path ("elpa/29.3/develop/popweb-20241016.22412" "elpa/29.3/develop/popweb-20241016.22412/extension/latex" "elpa/29.3/develop/popweb-20241016.22412/extension/dict" "elpa/29.3/develop/popweb-20241016.22412/extension/org-roam" "elpa/29.3/develop/popweb-20241016.22412/extension/anki-review")
    :init
    (require 'popweb-latex)
    (require 'popweb-dict)
    (require 'popweb-org-roam-link)
    (require 'popweb-anki-review)
    (require 'youdao-dictionary)
    ;; (require 'anki-editor)

    (defun hurricane/popweb-dict-eudic-liju-search-at-point ()
      (interactive)
      (if (display-graphic-p)
          (popweb-dict-eudic-liju-input nil (lc-corpus--sentence) (org-export-string-as
                                                                   (youdao-dictionary--format-result (youdao-dictionary--request (popweb-dict-region-or-word)))
                                                                   anki-editor--ox-anki-html-backend
                                                                   t
                                                                   anki-editor--ox-export-ext-plist))))
    (defun popweb-dict-join-dirs (root dir file)
      (file-name-concat root dir file))

    (defun popweb-dict-js-file-path (file)
      (popweb-dict-join-dirs (file-name-directory dotspacemacs-directory) "Backup" file))

    (popweb-dict-create "eudic-dicts"
                        "https://dict.eudic.net/dicts/en/%s"
                        (concat
                         "document.querySelectorAll('[title=\"Advertisement\"]')[0].style.display='none';"
                         "Array.from(document.getElementsByClassName('adsbygoogle adsbygoogle-noablate')).forEach(e => { e.style.display = 'none' });"
                         )
                        (popweb-dict-js-file-path "eudic-dicts.js"))

    (popweb-dict-create "eudic-liju"
                        "https://dict.eudic.net/liju/en/%s"
                        (concat
                         "document.querySelectorAll('[title=\"Advertisement\"]')[0].style.display='none';"
                         "Array.from(document.getElementsByClassName('adsbygoogle adsbygoogle-noablate')).forEach(e => { e.style.display = 'none' });"
                         )
                        (popweb-dict-js-file-path "eudic-liju.js"))

    (defun hurricane//popweb-translation-show (sentence translation)
      (let ((popweb-org-roam-link-popup-window-height-scale 0.8)
            (popweb-org-roam-link-popup-window-width-scale 0.8))
        (popweb-org-roam-link-show sentence nil translation)))
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

    ;; (advice-add #'org-roam-node-read :override #'popweb-org-roam-node-preview-select)
    :custom
    (popweb-python-command "python3.11")
    ;; (popweb-proxy-type provixy-type)
    ;; (popweb-proxy-host provixy-host)
    ;; (popweb-proxy-port provixy-port)
    (popweb-anki-review-media-directory Anki-media-dir)
    (popweb-org-roam-link-preview-media-directory Anki-media-dir)
    (popweb-anki-review-callback "popweb-dict-eudic-dicts-input")
    (popweb-org-roam-link-preview-callback "popweb-dict-eudic-dicts-input")
    :bind
    ("C-c Y" . hurricane/popweb-dict-eudic-liju-search-at-point)
    ("<f2>" . popweb-anki-review-show)
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

(defun hurricane-org/init-org-link-edit ()
  (use-package org-link-edit))

(defun hurricane-org/init-org-gtd ()
  (use-package org-gtd
    :init
    (require 'org-agenda)

    (defun hurricane//org-gtd-engage ()
      "Display `org-agenda' customized by org-gtd."
      (interactive)
      (org-gtd-core-prepare-agenda-buffers)
      (with-org-gtd-context
       (let* ((project-format-prefix
               (format " %%i %%-%d:(org-gtd-agenda--prefix-format) "
                       org-gtd-engage-prefix-width))
              (org-agenda-custom-commands
               `(("g" "Scheduled today and all NEXT items"
                  ((agenda ""
                           ((org-agenda-span 1)
                            (org-agenda-start-day nil)
                            (org-agenda-skip-additional-timestamps-same-entry t)
                            (org-super-agenda-groups
                             '((:name "Today"
                                      :discard (:and (:todo "WAIT" :scheduled past :deadline past)
                                                     :and (:todo "WAIT" :scheduled future :deadline past)
                                                     :and (:todo "WAIT" :scheduled future :deadline future)
                                                     :and (:todo "WAIT" :scheduled past :deadline future))
                                      )
                               (:name "Overdue 超过截止日期"
                                      :and (:scheduled past :deadline past))
                               (:name "Schedule past 超过起始日期"
                                      :and (:scheduled past :deadline future))
                               (:name "Due Today 今日截至或者今日起始"
                                      :deadline today
                                      :scheduled today)
                               (:name "Due Soon 即将起始"
                                      :and (:todo ("TODO" "NEXT") :scheduled future :deadline future))
                               (:name "Important"
                                      :priority "A")))))
                   (todo org-gtd-next
                         ((org-agenda-overriding-header "All actions ready to be executed.")
                          (org-super-agenda-groups
                           '((:name "Filter"
                                    :discard (:scheduled t :deadline t))
                             (:name "Ready"
                                    :anything)
                             ))

                          ;; (org-agenda-prefix-format
                          ;;  '((todo . ,project-format-prefix)))

                          ))
                   (todo org-gtd-wait
                         ((org-agenda-overriding-header "Delegated items.")
                          (org-super-agenda-groups
                           '(
                             (:discard (:scheduled today :deadline today))
                             (:name "Overdue 超过截止日期"
                                    :and (:scheduled past :deadline past))
                             (:name "Schedule past 超过起始日期"
                                    :and (:scheduled past :deadline future))
                             (:name "Due Soon 即将起始"
                                    :and (:scheduled future :deadline future))
                             ))))
                   (search "Incubate"
                           ((org-agenda-overriding-header "Blocked items.")
                            (org-super-agenda-groups
                             '(
                               (:name "Blocked 搁置"
                                      :children t
                                      )
                               ))))

                   )))))
         (org-agenda nil "g")
         (goto-char (point-min))
         )))

    (defun hurricane//org-gtd-delegate-item-at-point (&optional delegated-to checkin-date)
      "Delegate item at point.  Use this if you do not want to refile the item.

  You can pass DELEGATED-TO as the name of the person to whom this was delegated
  and CHECKIN-DATE as the YYYY-MM-DD string of when you want `org-gtd' to remind
  you if you want to call this non-interactively.
  If you call this interactively, the function will ask for the name of the
  person to whom to delegate by using `org-gtd-delegate-read-func'."
      (declare (modes org-mode)) ;; for 27.2 compatibility
      (interactive)
      (let ((delegated-to (or delegated-to
                              (apply org-gtd-delegate-read-func nil)))
            (date (or checkin-date
                      (org-read-date t nil nil "When do you want to check in on this task? ")))
            (org-inhibit-logging 'note))
        (org-set-property org-gtd-delegate-property delegated-to)
        (save-excursion
          (org-back-to-heading)
          (next-line)
          (open-line 1)
          (insert (format "DEADLINE: <%s> SCHEDULED: <%s>" date date)))
        (org-todo org-gtd-wait)
        (save-excursion
          (goto-char (org-log-beginning t))
          (insert (format "programmatically delegated to %s\n" delegated-to)))))

    (defun hurricane//org-gtd-incubate--apply (&optional reminder-date)
      "Incubate this item through org-gtd.

If you want to call this non-interactively,
REMINDER-DATE is the YYYY-MM-DD string for when you want this to come up again."

      (setq-local org-gtd--organize-type 'incubated)
      (org-gtd-organize-apply-hooks)
      (org-gtd-refile--do org-gtd-incubate org-gtd-incubate-template))

    :config
    (advice-add #'org-gtd-engage :override #'hurricane//org-gtd-engage)
    (advice-add #'org-gtd-delegate-item-at-point :override #'hurricane//org-gtd-delegate-item-at-point)
    (advice-add #'org-gtd-incubate--apply :override #'hurricane//org-gtd-incubate--apply)
    (add-hook 'org-mode-hook #'org-gtd-mode)

    :custom
    (org-gtd-directory deft-dir)
    (org-gtd-organize-hooks nil)))

(defun hurricane-org/init-org-super-agenda ()
  (use-package org-super-agenda
    :config
    (setq project-format-prefix
          (format " %%i %%-%d:(org-gtd-agenda--prefix-format) "
                  org-gtd-engage-prefix-width))
    (setq org-agenda-custom-commands
          `(("g" "Scheduled today and all NEXT items"
             ((agenda ""
                      ((org-agenda-span 1)
                       (org-agenda-start-day nil)
                       (org-agenda-skip-additional-timestamps-same-entry t)
                       (org-super-agenda-groups
                        '((:name "Today"
                                 :discard (:and (:todo "WAIT" :scheduled past :deadline past)
                                                :and (:todo "WAIT" :scheduled future :deadline past)
                                                :and (:todo "WAIT" :scheduled future :deadline future)
                                                :and (:todo "WAIT" :scheduled past :deadline future))
                                 )
                          (:name "Overdue 超过截止日期"
                                 :and (:scheduled past :deadline past))
                          (:name "Schedule past 超过起始日期"
                                 :and (:scheduled past :deadline future))
                          (:name "Due Today 今日截至或者今日起始"
                                 :deadline today
                                 :scheduled today)
                          (:name "Due Soon 即将起始"
                                 :and (:todo ("TODO" "NEXT") :scheduled future :deadline future))
                          (:name "Important"
                                 :priority "A")))))
              (todo org-gtd-next
                    ((org-agenda-overriding-header "All actions ready to be executed.")
                     (org-super-agenda-groups
                      '((:name "Filter"
                               :discard (:scheduled t :deadline t))
                        (:name "Ready"
                               :anything)
                        ))

                     ;; (org-agenda-prefix-format
                     ;;  '((todo . ,project-format-prefix)))

                     ))
              (todo org-gtd-wait
                    ((org-agenda-overriding-header "Delegated items.")
                     (org-super-agenda-groups
                      '(
                        (:discard (:scheduled today :deadline today))
                        (:name "Overdue 超过截止日期"
                               :and (:scheduled past :deadline past))
                        (:name "Schedule past 超过起始日期"
                               :and (:scheduled past :deadline future))
                        (:name "Due Soon 即将起始"
                               :and (:scheduled future :deadline future))
                        ))))
              (search "Incubate"
                      ((org-agenda-overriding-header "Blocked items.")
                       (org-super-agenda-groups
                        '(
                          (:name "Blocked 搁置"
                                 :children t
                                 )
                          ))))

              ))))
    (org-super-agenda-mode)))

(defun hurricane-org/init-anki-helper ()
  (use-package anki-helper
    :config
    (defun hurricane//anki-helper-fields-get-default ()
      "Default function for get filed info of the current entry."
      (let* ((elt (plist-get (org-element-at-point) 'headline))
             ;; (front (plist-get elt :raw-value))
             (front (string-join (org-get-outline-path t) " > "))
             (contents-begin (plist-get elt :contents-begin))
             (robust-begin (or (plist-get elt :robust-begin)
                               contents-begin))
             (beg (if (or (= contents-begin robust-begin)
                          (= (+ 2 contents-begin) robust-begin))
                      contents-begin
                    (1+ robust-begin)))
             (contents-end (plist-get elt :contents-end))
             (back (buffer-substring-no-properties
                    contents-begin (1- contents-end))))
        (list front back)))

    (advice-add #'anki-helper-fields-get-default :override #'hurricane//anki-helper-fields-get-default)

    (defun hurricane//anki-helper--ox-html-link (text backend info)
      (when (eq backend 'html)
        (setq link-path-property nil)
        (when-let*
            ((link (nth anki-helper--org2html-image-counter
                        (org-element-map (plist-get info :parse-tree) 'link 'identity)))
             (link-path (org-element-property :path link))
             (file-exists-p (file-exists-p link-path))
             (file-extension (file-name-extension link-path))
             (link-type (org-element-property :type link))
             (hash (md5 (format "%s%s%s" (random) text (recent-keys))))
             (new-name (file-name-with-extension hash file-extension))
             (full-path (file-name-concat
                         anki-helper-media-directory
                         new-name)))
          ;; 提供 when-let* 范围外使用。
          (setq link-path-property link-path)
          (cond
           ((and (plist-get info :html-inline-images)
                 (org-export-inline-image-p link
                                            (plist-get info :html-inline-image-rules)))
            (copy-file link-path full-path)
            (when (string-suffix-p "edraw.svg" link-path)
              (setq text (get-svg-xml full-path)))
            ;; 没替换前，导出到 Anki 的文本格式：
            ;; <img src="static/STM32%20入门/1.png" alt="1.png">
            ;; <img src="static/STM32%20入门/test.edraw.svg" alt="test.edraw.svg" class="org-svg">
            ;; get-svg-xml 得到的 text 没有 img scr="xxx"，所以以下只替换非 edraw.svg 格式的图片链接
            (setq text (replace-regexp-in-string "img src=\"\\(.*?\\)\"" new-name text
                                                 nil nil 1)))
           ;; 这里逻辑似乎有问题，file 链接要是既有图片也有音频，而其中一者无法被发送至Anki。
           ((member file-extension anki-helper-audio-formats)
            (copy-file link-path full-path)
            (setq text (format "<br>[sound:%s]" new-name)))))

        ;; 对于 text: <a href="STM32F10xxx 参考手册（中文）.html#ID-1465F803-9159-4625-8D94-33570B46486D">14.4.7 捕获/比较模式寄存器 1(TIMx_CCMR1)</a>，是不会进入 when-let* 处理，所以以下的替换逻辑放在 when-let* 外。
        ;; 对于get-svg-xml 得到的 text 同样含有 # 字符，所以必须排除 svg 产生的 text。
        ;; 对于 text: <a href="STM32F10xxx 参考手册（中文）.html#ID-1465F803-9159-4625-8D94-33570B46486D">14.4.7 捕获/比较模式寄存器 1(TIMx_CCMR1)</a>，是不会进入 when-let* 处理，所以不能用 (not (string-suffix-p "edraw.svg" link-path))，
                                        ;；必须使用 (setq link-path-property link-path)
        (when (and (string-match "\\(#[^\"]*\\)" text)
                   (not (string-suffix-p "edraw.svg" link-path-property)))
          (setq text (format "%s%s" text (match-string 0 text))))
        )

      (cl-incf anki-helper--org2html-image-counter)
      text)

    (advice-add #'anki-helper--ox-html-link :override #'hurricane//anki-helper--ox-html-link)

    (defun hurricane/anki-helper-skip ()
      "Skip headlines without \"ID\" property."
      (unless (org-entry-get nil "ID")
        (point)))

    (setq anki-helper-skip-function #'hurricane/anki-helper-skip)

    (defun hurricane//anki-helper--org2html (string)
      (let ((org-export-filter-link-functions '(anki-helper--ox-html-link))
            (org-export-filter-latex-environment-functions anki-helper-ox-filter-latex-env-functions)
            (org-export-filter-latex-fragment-functions anki-helper-ox-filter-latex-frag-functions)
            (org-export-filter-src-block-functions '(hurricane//org-html-wrap-blocks-in-code))
            (org-html-htmlize-output-type 'inline-css)
            (anki-helper--org2html-image-counter 0))
        (org-export-string-as string 'html t '(:with-toc nil))))

    (advice-add #'anki-helper--org2html :override #'hurricane//anki-helper--org2html)

    :custom
    (anki-helper-media-directory Anki-media-dir)
    (anki-helper-cloze-use-emphasis 'bold)
    (anki-helper-fields-get-alist
     '(("Basic" . anki-helper-fields-get-default)
       ("Cloze" . anki-helper-fields-get-cloze)
       ("Image Occlusion Enhanced" . anki-helper-fields-get-default)))
    (anki-helper-note-types
     '(("Basic" "Front" "Back")
       ("Basic (and reversed card)" "Front" "Back")
       ("Basic (optional reversed card)" "Front" "Back")
       ("Cloze" "Text" "Back Extra")
       ("Image Occlusion Enhanced" "Header" "Image")))
    ))
(defun hurricane-org/init-org-noter ()
  (use-package org-noter
    :ensure t
    :custom
    (org-noter-always-create-frame nil)
    :config
    (defun hurricane//org-noter-start-from-dired ()
      "In Dired, open sessions for marked files or file at point.

If there are multiple marked files, focus will be on the last
marked file."
      (interactive)
      (let ((files (or (dired-get-marked-files)
                       (dired-get-filename))))
        (dolist (filename files)
          (let ((eaf-pdf-extension-list '("xps" "oxps" "cbz" "epub" "fb2" "fbz")))
            (find-file filename))
          (save-excursion (org-noter))
          (bury-buffer))
        (other-frame 1)))

    (advice-add #'org-noter-start-from-dired :override #'hurricane//org-noter-start-from-dired)

    (defun hurricane//insert-org-drawio ()
      (interactive)
      (let* ((relative-img-dir (concat org-screenshot-image-dir-name "/" (file-name-sans-extension (file-name-nondirectory (buffer-file-name))))))
        (if (file-exists-p relative-img-dir)
            (print (format "Screnshot image directory: '%s' already exists." relative-img-dir))
          (mkdir relative-img-dir))
        (let ((temp-name (select-or-enter-file-name relative-img-dir)))
          (setq name-base (file-name-base temp-name))
          (insert (format "#+drawio:\"%s\" :input-dir \"%s\" :output-dir \"%s\"" name-base relative-img-dir relative-img-dir))
          (newline-and-indent 1)
          (previous-line 1)
          (org-drawio-open))))

    (defun hurricane//pdf-view-extract-region-image (regions &optional page size
                                                             output-buffer no-display-p)
      ;; TODO: what is "resp."? Avoid contractions.
      "Create a PNG image of REGIONS.

  REGIONS should have the same form as `pdf-view-active-region',
  which see.  PAGE and SIZE are the page resp. base-size of the
  image from which the image-regions will be created; they default
  to `pdf-view-current-page' resp. `pdf-view-image-size'.

  Put the image in OUTPUT-BUFFER, defaulting to \"*PDF region
  image*\" and display it, unless NO-DISPLAY-P is non-nil.

  In case of multiple regions, the resulting image is constructed
  by joining them horizontally.  For this operation (and this only)
  the `convert' program is used."

      (interactive
       (list (if (pdf-view-active-region-p)
                 (pdf-view-active-region t)
               '((0 0 1 1)))))
      (unless page
        (setq page (pdf-view-current-page)))
      (unless size
        (setq size (pdf-view-image-size)))
      (unless output-buffer
        (setq output-buffer (get-buffer-create (org-noter--session-notes-buffer org-noter--session))))
      (setq notes-file-path (org-noter--session-notes-file-path org-noter--session))
      (let* ((images (mapcar (lambda (edges)
                               (let ((file (make-temp-file "pdf-view"))
                                     (coding-system-for-write 'binary))
                                 (write-region
                                  (pdf-info-renderpage
                                   page (car size)
                                   :crop-to edges)
                                  nil file nil 'no-message)
                                 file))
                             regions))
             result)
        (unwind-protect
            (progn
              (if (= (length images) 1)
                  (setq result (car images))
                (setq result (make-temp-file "pdf-view"))
                ;; Join the images horizontally with a gap of 10 pixel.
                (pdf-util-convert
                 "-noop" ;; workaround limitations of this function
                 result
                 :commands `("("
                             ,@images
                             "-background" "white"
                             "-splice" "0x10+0+0"
                             ")"
                             "-gravity" "Center"
                             "-append"
                             "+gravity"
                             "-chop" "0x10+0+0")
                 :apply '((0 0 0 0))))

              (with-current-buffer output-buffer
                (let* ((relative-img-dir (concat org-screenshot-image-dir-name "/" (file-name-sans-extension (file-name-nondirectory notes-file-path)) "/"))
                       deck
                       (elt (plist-get (org-element-at-point) 'headline))
                       (front (string-join (org-get-outline-path t) " > "))
                       (contents-begin (plist-get elt :contents-begin))
                       (robust-begin (or (plist-get elt :robust-begin)
                                         contents-begin))
                       (beg (if (or (= contents-begin robust-begin)
                                    (= (+ 2 contents-begin) robust-begin))
                                contents-begin
                              (1+ robust-begin)))
                       (contents-end (plist-get elt :contents-end))
                       (back (buffer-substring-no-properties
                              contents-begin (1- contents-end))))
                  (progn
                    (if (file-exists-p relative-img-dir)
                        (print (format "Screnshot image directory: '%s' already exists." relative-img-dir))
                      (mkdir relative-img-dir))
                    (let ((temp-name (select-or-enter-file-name relative-img-dir)))
                      (setq absolute-img-dir (concat default-directory relative-img-dir "/"))
                      (setq name-base (file-name-base temp-name))
                      (setq file-name (concat name-base ".png"))
                      (setq full-file-path (concat relative-img-dir file-name))
                      (with-temp-buffer
                        (insert-file-contents-literally result)
                        (write-region (point-min) (point-max) full-file-path))
                      (setq absolute-full-file-path (concat absolute-img-dir file-name))
                      (insert (concat "[[file:" full-file-path "]]"))
                      (evil-normal-state)
                      (org-display-inline-images)
                      (save-excursion
                        (goto-char (point-min))
                        (setq deck (or (hurricane//extract-value-from-keyword "ANKI_DECK") (hurricane//headline-property "ANKI_DECK"))))
                      (eaf-open-image-occlusion (expand-file-name absolute-full-file-path) (list deck front back))
                      )))
                (unless no-display-p
                  (pop-to-buffer (current-buffer))))
              )

          (dolist (f (cons result images))
            (when (file-exists-p f)
              (delete-file f))))))

    (advice-add #'pdf-view-extract-region-image :override #'hurricane//pdf-view-extract-region-image)

    (defun hurricane//pdf-view-mouse-set-region-rectangle (event)
      "Like `pdf-view-mouse-set-region' but displays as a rectangle.

EVENT is the mouse event.

This is more useful for commands like
`pdf-view-extract-region-image'."
      (interactive "@e")
      (pdf-view-mouse-set-region event nil t)
      (cond ((eq major-mode 'pdf-view-mode)
             (let* ((file (file-name-base (pdf-view-buffer-file-name)))
                    (page (number-to-string
                           (org-noter--get-location-page
                            (org-noter-pdf--approx-location-cons
                             'pdf-view-mode
                             (org-noter-pdf--pdf-view-get-precise-info
                              'pdf-view-mode
                              (get-buffer-window))))))
                    (quote (if (pdf-view-active-region-p)
                               (replace-regexp-in-string "\n" " "
                                                         (mapconcat 'identity (pdf-view-active-region-text) ? ))))
                    (desc (concat file ".pdf: Page " page (when quote (concat "; Quoting: " quote))))
                    (link (hurricane//org-noter-get-link)))
               (if pdf-view--have-rectangle-region
                   (kill-new (format "<a onclick=\"(function() {javascript:location.href = \'org-protocol://open-pdf?pdf-tools=%s\'})()\">%s</a>" (url-hexify-string (format "[[%s]]" link)) (or desc link)))
                 (kill-new (format "[[%s]][[%s]]" link desc)))
               ))))

    (advice-add #'pdf-view-mouse-set-region-rectangle :override #'hurricane//pdf-view-mouse-set-region-rectangle)

    (defun hurricane//org-noter-start-from-dired ()
      "In Dired, open sessions for marked files or file at point.

If there are multiple marked files, focus will be on the last
marked file."
      (interactive)
      (let ((files (or (dired-get-marked-files)
                       (dired-get-filename))))
        (dolist (filename files)
          (let ((eaf-pdf-extension-list '("xps" "oxps" "cbz" "epub" "fb2" "fbz")))
            (find-file filename))
          (save-excursion (org-noter))
          (bury-buffer))
        (other-frame 1)))

    (org-noter-enable-org-roam-integration)

    (defun hurricane//org-noter-set-highlight (&rest _arg)
      "Highlight current org-noter note."
      (save-excursion
        (with-current-buffer (org-noter--session-notes-buffer org-noter--session)
          (remove-overlays (point-min) (point-max) 'org-noter-current-hl t)
          (goto-char (org-entry-beginning-position))
          (let* ((hl (org-element-context))
                 (hl-begin (plist-get  (plist-get hl 'headline) :begin))
                 (hl-end (1- (plist-get  (plist-get hl 'headline) :contents-begin)))
                 (hl-ov (make-overlay hl-begin hl-end)))
            (overlay-put hl-ov 'face 'tab-bar-tab)
            (overlay-put hl-ov 'org-noter-current-hl t))
          (org-cycle-hide-drawers 'all))))

    (advice-add #'org-noter--focus-notes-region
                :after #'hurricane//org-noter-set-highlight)
    (advice-add #'org-noter-insert-note
                :after #'hurricane//org-noter-set-highlight)

    (defun hurricane//counsel-org-goto-action (x)
      "Go to headline in candidate X."
      (org-goto-marker-or-bmk (cdr x))
      (when org-noter--session
        (org-noter-sync-current-note)))

    (advice-add #'counsel-org-goto-action :override #'hurricane//counsel-org-goto-action)

    (defun hurricane//org-noter-get-link ()
      (format "%s:%s#%s" org-noter-property-note-location (buffer-file-name) (org-noter-pdf--approx-location-cons 'pdf-view-mode (org-noter-pdf--pdf-view-get-precise-info 'pdf-view-mode (get-buffer-window)))))

    (defun hurricane//org-noter-store-link ()
      (interactive)
      (cond ((eq major-mode 'pdf-view-mode)
             (let* ((file (file-name-base (pdf-view-buffer-file-name)))
                    (page (number-to-string
                           (org-noter--get-location-page
                            (org-noter-pdf--approx-location-cons
                             'pdf-view-mode
                             (org-noter-pdf--pdf-view-get-precise-info
                              'pdf-view-mode
                              (get-buffer-window))))))
                    (quote (if (pdf-view-active-region-p)
                               (replace-regexp-in-string "\n" " "
                                                         (mapconcat 'identity (pdf-view-active-region-text) ? ))))
                    (desc (concat file ".pdf: Page " page (when quote (concat "; Quoting: " quote))))
                    (link (hurricane//org-noter-get-link)))
               (if pdf-view--have-rectangle-region
                   (kill-new (format "<a onclick=\"(function() {javascript:location.href = \'org-protocol://open-pdf?pdf-tools=%s\'})()\">%s</a>" (url-hexify-string (format "[[%s]]" link)) (or desc link)))
                 (kill-new (format "[[%s]][[%s]]" link desc)))
               (org-link-store-props
                :type org-noter-property-note-location
                :link link
                :description desc)))
            ((eq major-mode 'eaf-mode)
             (let* ((info (eaf-call-sync "execute_function" eaf--buffer-id "store_link"))
                    (file (nth 0 info))
                    (raw-location (nth 1 info))
                    (page (number-to-string (car raw-location)))
                    (location (cons page (cons (car (cdr raw-location)) (cadr (cdr raw-location)))))
                    (quote (nth 2 info))
                    (desc (concat (file-name-nondirectory file) ": Page " page (when quote (concat "; Quoting: " quote))))
                    (link (format "%s:%s#%s" org-noter-property-note-location file location)))
               (org-link-store-props
                :type org-noter-property-note-location
                :link link
                :description desc)))))

    (defun hurricane//org-noter-link-follow (link)
      (let* ((splitted (string-split link "#"))
             (pdf-file-path (nth 0 splitted))
             (location (nth 1 splitted))
             (value (car (read-from-string location)))
             (notes-file-path (buffer-file-name))
             (eaf-pdf-extension-list '("xps" "oxps" "cbz" "epub" "fb2" "fbz")))
        (when location
          (org-noter-pdf--goto-location
           'pdf-view-mode
           (cond ((and (consp value) (integerp (car value)) (numberp (cdr value))) value)
                 ((and (consp value) (integerp (car value)) (consp (cdr value)) (numberp (cadr value)) (numberp (cddr value))) value)
                 ((integerp value) (cons value 0)))
           (let ((org-link-frame-setup
                  (cl-acons 'file 'find-file-other-frame org-link-frame-setup)))
             (if (bound-and-true-p org-noter--session)
                 (org-noter--with-valid-session
                  (let ((doc (with-selected-window
                                 (org-noter--get-doc-window)
                               buffer-file-name)))
                    (if (string-equal doc pdf-file-path)
                        (select-window
                         (org-noter--get-doc-window))
                      (get-buffer-window (org-open-file pdf-file-path 1)))))
               (get-buffer-window (org-open-file pdf-file-path 1))))))))

    (defun hurricane//org-noter-link-export (path desc backend)
      (let ((ext (file-name-extension path)))
        (cond
         ((eq 'html backend)
          (format "<a onclick=\"(function() {javascript:location.href = \'org-protocol://open-pdf?pdf-tools=%s\'})()\">%s</a>" (url-hexify-string (format "[[%s:%s]]" org-noter-property-note-location path)) (or desc path))
          )
         ;; fall-through case for everything else.
         (t
          path))))

    (org-link-set-parameters org-noter-property-note-location
                             :follow #'hurricane//org-noter-link-follow
                             :store #'hurricane//org-noter-store-link
                             :export #'hurricane//org-noter-link-export)))

(defun hurricane-org/init-helm-org-ql ()
  (use-package helm-org-ql
    :ensure t))

(with-eval-after-load 'org-re-reveal
  (progn
    (setq org-re-reveal-revealjs-version "4"
          org-re-reveal-root "/reveal.js"
          org-re-reveal-with-tts nil)
    (setq org-re-reveal-extra-scripts '("https://cdnjs.cloudflare.com/ajax/libs/RecordRTC/5.6.2/RecordRTC.js" "https://unpkg.com/imsc@1.1.3/dist/imsc.all.min.js" "./reveal.js/third-party-plugins/imscJS.js" "./reveal.js/third-party-plugins/createSubtitle.js"))
    (add-to-list 'org-re-reveal-plugin-config '(audioslideshow "RevealAudioSlideshow" "plugin/audio-slideshow/plugin.js"))
    (add-to-list 'org-re-reveal-plugin-config '(audiorecorder "RevealAudioRecorder" "https://cdn.jsdelivr.net/npm/reveal.js-plugins@latest/audio-slideshow/recorder.js"))
    (add-to-list 'org-re-reveal-plugin-config '(anything "RevealAnything" "https://cdn.jsdelivr.net/npm/reveal.js-plugins@latest/anything/plugin.js"))
    (add-to-list 'org-re-reveal-plugin-config '(customcontrols "RevealCustomControls" "https://cdn.jsdelivr.net/npm/reveal.js-plugins@latest/customcontrols/plugin.js"))
    ))

(defun hurricane-org/init-oer-reveal ()
  (use-package oer-reveal
    :ensure t
    :config
    (require 'oer-reveal-publish)

    (setq oer-reveal-plugin-4-config
          "audioslideshow RevealAudioSlideshow plugin/audio-slideshow/plugin.js
audiorecorder RevealAudioRecorder https://cdn.jsdelivr.net/npm/reveal.js-plugins@latest/audio-slideshow/recorder.js
anything RevealAnything https://cdn.jsdelivr.net/npm/reveal.js-plugins@latest/anything/plugin.js
customcontrols RevealCustomControls https://cdn.jsdelivr.net/npm/reveal.js-plugins@latest/customcontrols/plugin.js")
    (setq oer-reveal-export-dir reveal-project-directory)))

(defun hurricane-org/init-emacsconf-el ()
  (use-package emacsconf-el
    :ensure t
    :config
    (require 'emacsconf-mail)
    (require 'emacsconf-spookfox)))

(defun hurricane-org/init-org-modern-indent ()
  (use-package org-modern-indent
    :ensure t
    :config
    (add-hook 'org-mode-hook #'org-modern-indent-mode 90)))

(defun hurricane-org/init-org-drawio ()
  (use-package org-drawio
    :commands (org-drawio-add
               org-drawio-open)
    :custom ((org-drawio-command-drawio "/Applications/draw.io.app/Contents/MacOS/draw.io")
             (org-drawio-input-dir "./draws")
             (org-drawio-output-dir "./images")
             (org-drawio-output-page "0")
             ;; set to t, if you want to crop the image.
             (org-drawio-crop nil))))

(defun hurricane-org/init-org-remark ()
  (use-package org-remark
    :bind (;; :bind keyword also implicitly defers org-remark itself.
           ;; Keybindings before :map is set for global-map.
           ("C-c n m" . org-remark-mark)
           ("C-c n l" . org-remark-mark-line)
           :map org-remark-mode-map
           ("C-c n o" . org-remark-open)
           ("C-c n ]" . org-remark-view-next)
           ("C-c n [" . org-remark-view-prev)
           ("C-c n r" . org-remark-remove)
           ("C-c n d" . org-remark-delete))
    ;; Alternative way to enable `org-remark-global-tracking-mode' in
    ;; `after-init-hook'.
    ;; :hook (after-init . org-remark-global-tracking-mode)
    :init
    ;; It is recommended that `org-remark-global-tracking-mode' be
    ;; enabled when Emacs initializes. Alternatively, you can put it to
    ;; `after-init-hook' as in the comment above
    (org-remark-global-tracking-mode +1)
    ;; (setopt org-remark-notes-file-name (expand-file-name "marginalia.org" (project-root (project-current t))))
    :config
    (use-package org-remark-info :after info :config (org-remark-info-mode +1))
    (use-package org-remark-eww  :after eww  :config (org-remark-eww-mode +1))
    (use-package org-remark-nov  :after nov  :config (org-remark-nov-mode +1))))

(defun hurricane-org/init-org-bars ()
  (use-package org-bars
    :ensure t))

(defun hurricane-org/init-org-visual-outline ()
  (use-package org-visual-outline
    :ensure t
    :config
    (require 'org-dynamic-bullets)
    (require 'org-visual-indent)
    :hook
    (org-mode . org-visual-indent-mode)
    (org-mode . org-dynamic-bullets-mode)))

(defun hurricane-org/init-org-tags-filter ()
  (use-package org-tags-filter))

(defun hurricane-org/init-org-roam-dblocks ()
  (use-package org-roam-dblocks
    :hook (org-mode . org-roam-dblocks-autoupdate-mode)))

(defun hurricane-org/init-org-web-tools ()
  (use-package org-web-tools))
