(require 'package)
(add-to-list 'load-path "~/.emacs.d/elpa/develop/org-roam-20210128.1341")
(add-to-list 'load-path "~/.emacs.d/elpa/develop/s-20180406.808")
(add-to-list 'load-path "~/.emacs.d/elpa/develop/dash-20210210.1449")
(add-to-list 'load-path "~/.emacs.d/elpa/develop/f-20191110.1357")
(add-to-list 'load-path "~/.emacs.d/elpa/develop/emacsql-20200714.828")
(add-to-list 'load-path "~/.emacs.d/elpa/develop/emacsql-sqlite3-20200914.508")
(add-to-list 'load-path "~/.emacs.d/elpa/develop/htmlize-20200816.746")

(package-initialize)

;; Uncomment this stuff the first time you run, or if you need to update some packages.
;; Uses versions of these packages for the publish process.
;; Didn't yet figure out a way to use packages already installed.
;; (and if this was to move to a CI process, would still be needed there).

;; (add-to-list 'package-archives '(("melpa-cn" . "https://elpa.emacs-china.org/melpa/")
                         ;; ("org-cn"   . "https://elpa.emacs-china.org/org/")))

;; (package-refresh-contents)

;; (package-install 'ox-publish)
;; (package-install 'ox-html)
;; (package-install 'htmlize)
;; (package-install 'org-roam)
;; (package-install 's)

(require 'font-lock)
(require 'subr-x) ;; for `when-let'
(require 'ox-publish)
(require 'ox-html)
(require 'htmlize)
(require 'org-roam)
(require 'org-roam-db)
(require 's)

;; Don't create backup files (those ending with ~) during the publish process.
(setq make-backup-files nil)

;;;;;;;;;;;;;;;;;
;; org-publish ;;
;;;;;;;;;;;;;;;;;

;; standard stuff here.
(setq org-roam-directory "~/Library/Mobile Documents/iCloud~com~appsonthemove~beorg/Documents/org-notes/notes/")
(setq org-roam-db-location "~/Library/Mobile Documents/iCloud~com~appsonthemove~beorg/Documents/org-notes/notes/org-roam.db")
(setq hurricane/project-dir "~/Library/Mobile Documents/iCloud~com~appsonthemove~beorg/Documents/org-notes/notes/")
(setq hurricane/publish-dir "~/Library/Mobile Documents/iCloud~com~appsonthemove~beorg/Documents/org-notes/public/")

(setq hurricane/preamble "")
(setq hurricane/postamble "This page last updated: %C.")
(setq hurricane/head-extra "
<link href='https://fonts.googleapis.com/css?family=Nunito:400,700&display=swap' rel='stylesheet'>
<link href='https://unpkg.com/tippy.js@6.2.3/themes/light.css' rel='stylesheet'>
<script src='https://unpkg.com/@popperjs/core@2'></script>
<script src='https://unpkg.com/vis-network@8.2.0/dist/vis-network.min.js'></script>
<script src='https://unpkg.com/@popperjs/core@2'></script>
<script src='https://unpkg.com/tippy.js@6'></script>
<script src='//cdn.bootcss.com/jquery/3.4.1/jquery.min.js'></script>

<link rel='shortcut icon' href='/static/assets/images/2020/2020-blue.png' type='image/x-icon' />
<link rel='stylesheet' href='//cdn.bootcss.com/animate.css/3.7.2/animate.min.css' />
<link rel='stylesheet' href='//cdn.bootcss.com/font-awesome/5.11.2/css/all.min.css' />

<link rel='stylesheet' type='text/css' href='/static/vendor/css/stylesheet.css'/>

<script src='/static/vendor/js/darkreader.js'></script>
<script src='/static/vendor/js/main.js'></script>
<script src='/static/vendor/js/lunr.min.js'></script>
<script src='/static/vendor/js/search.js'></script>
<script src='/static/vendor/js/URI.js'></script>
<script src='/static/vendor/js/page.js'></script>

<script src='//cdnjs.cloudflare.com/ajax/libs/highlight.js/10.7.2/highlight.min.js'></script>
<link rel='stylesheet' href='//cdnjs.cloudflare.com/ajax/libs/highlight.js/10.7.2/styles/atom-one-light.min.css'>
<script>hljs.highlightAll();</script>
")

;; Make org not to treat `_' with sub-superscript, but `_{}'.
(setq org-export-with-sub-superscripts '{})

(setq org-src-fontify-natively t)
(setq org-src-tab-acts-natively t)
(setq org-confirm-babel-evaluate nil)
(setq org-edit-src-content-indentation 0)
(setq org-export-in-background t)
(setq org-html-htmlize-output-type nil)
(setq org-html-head-include-default-style nil)
(setq org-html-head-include-scripts nil)

;;{{ @see https://vicarie.in/posts/blogging-with-org.html
(defun hurricane/sitemap-format-entry (entry _style project)
  "Return string for each ENTRY in PROJECT."
  (format "@@html:<span class=\"archive-item\"><span class=\"archive-date\">@@ %s @@html:</span>@@ [[file:%s][%s]] @@html:</span>@@"
          (format-time-string "%d %h %Y"
                              (org-publish-find-date entry project))
          entry
          (org-publish-find-title entry project)))
;; }}

(defun hurricane/org-publish-sitemap (title list)
  "Customized site map, as a string.
TITLE is the title of the site map.  LIST is an internal
representation for the files to include, as returned by
`org-list-to-lisp'.  PROJECT is the current project."
  (concat "#+TITLE: " title "\n\n" "
#+HTML: <div class='container-fluid'>
#+HTML:    <div class='row'>
#+HTML:      <div class='col-xs-12 col-md-3'>
#+HTML:        <div id='filter' class='input-group'>
#+HTML:          <input type='text' id='filter-query' placeholder='Search file name or content.' class='form-control input-sm'>
#+HTML:          <a id='filter-clear-query' title='Clear current search...' class='input-group-addon input-sm'>
#+HTML:            <i class='glyphicon glyphicon-remove'>&#x274c;</i>
#+HTML:          </a>
#+HTML:        </div>
#+HTML:      </div>
#+HTML:    </div>
#+HTML: </div>
#+HTML: <ul class='unstyled' id='filter-results'></ul>
" "\n\n"
(org-list-to-org list)))

(setq org-publish-project-alist
            `(("orgfiles"
               ;; Sources and destinations for files.
               ;; local org files directory.
               :base-directory ,hurricane/project-dir
               :publishing-directory ,hurricane/publish-dir
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
               :html-container "section"
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
               :html-head-extra	,hurricane/head-extra
               ;; :html-head-include-default-style nil
               ;; :html-head-include-scripts nil
               ;; :html-head	org-html-head
               ;; :html-home/up-format	org-html-home/up-format
               ;; :html-html5-fancy t
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
               :html-postamble ,hurricane/postamble
               ;; :html-preamble-format	org-html-preamble-format
               ;; org-html-preamble.
               ;; :html-preamble ,hurricane/preamble
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
               :auto-sitemap t
               :exclude "node_modules"
               :sitemap-title "Hurricane"
               :sitemap-sort-files anti-chronologically
               :sitemap-function hurricane/org-publish-sitemap
               :sitemap-format-entry hurricane/sitemap-format-entry
               :sitemap-filename "index.org"
               )

              ;; Static assets.
              ("images"
               :base-directory ,hurricane/project-dir
               :base-extension "css\\|js\\|png\\|jpg\\|gif\\|svg\\|svg\\|json\\|pdf"
               :publishing-directory ,hurricane/publish-dir
               :exclude "node_modules"
               :recursive t
               :publishing-function org-publish-attachment
               )

              ("website" :components ("orgfiles" "images"))
              ("statics" :components ("images"))
              ))

(defun hurricane/org-roam-title-to-slug (title)
  "Convert TITLE to a filename-suitable slug.
Use hyphens rather than underscores."
  (cl-flet* ((nonspacing-mark-p (char)
                                (eq 'Mn (get-char-code-property char 'general-category)))
             (strip-nonspacing-marks (s)
                                     (apply #'string (seq-remove #'nonspacing-mark-p
                                                                 (ucs-normalize-NFD-string s))))
             (cl-replace (title pair)
                         (replace-regexp-in-string (car pair) (cdr pair) title)))
    (let* ((pairs `(;; ("[^[:alnum:][:digit:]]" . " ")  ;; convert anything not alphanumeric
                    ;; ("__*" . "_")  ;; remove sequential underscores
                    ("^_" . "")  ;; remove starting underscore
                    ("_$" . "")))  ;; remove ending underscore
           (slug (-reduce-from #'cl-replace (strip-nonspacing-marks title) pairs)))
      slug)))

(setq org-roam-title-to-slug-function 'hurricane/org-roam-title-to-slug)

;; org-roam backlinks
;; see https://org-roam.readthedocs.io/en/master/org_export/

(defun hurricane/org-roam--backlinks-list (file)
  (if (org-roam--org-roam-file-p file)
      (--reduce-from
       (concat acc (format "- [[file:%s][%s]]\n"
                           (file-relative-name (car it) org-roam-directory)
                           (org-roam-db--get-title (car it))))
       "" (org-roam-db-query [:select [source] :from links :where (= dest $s1)] file))
    ""))

(defun hurricane/org-export-preprocessor (backend)
  (let ((links (hurricane/org-roam--backlinks-list (buffer-file-name))))
    (unless (string= links "")
      (save-excursion
        (goto-char (point-max))
        (insert (concat "\n* Backlinks\n") links)))))

(add-hook 'org-export-before-processing-hook 'hurricane/org-export-preprocessor)

(eval-after-load "ox-html"
  '(defun org-html-template (contents info)
     (concat (org-html-doctype info)
             "<html lang=\"en\">
            <head>"
             (org-html--build-meta-info info)
             (org-html--build-head info)
             (org-html--build-mathjax-config info)
             "</head>
            <body>"
             (org-html--build-pre/postamble 'preamble info)
             "<div class='grid-container'><div class='ds-grid'>"
             (unless (string= (org-export-data (plist-get info :title) info) "The Map")
               "<div class='page'>")
             ;; Document contents.
             (let ((div (assq 'content (plist-get info :html-divs))))
               (format "<%s id=\"%s\">\n" (nth 1 div) (nth 2 div)))
             ;; Document title.
             (when (plist-get info :with-title)
               (let ((title (and (plist-get info :with-title)
                                 (plist-get info :title)))
                     (subtitle (plist-get info :subtitle))
                     (html5-fancy (org-html--html5-fancy-p info)))
                 (when title
                   (format
                    (if html5-fancy
                        "<header>\n<h1 class=\"title\">%s</h1> <a class='rooter' href='%s'>*</a>\n%s</header>"
                      "<h1 class=\"title\">%s<a class='rooter' href='%s'>*</a></h1>\n")
                    (org-export-data title info)
                    (file-name-nondirectory (plist-get info :output-file))
                    (if subtitle
                        (format
                         (if html5-fancy
                             "<p class=\"subtitle\">%s</p>\n"
                           (concat "\n" (org-html-close-tag "br" nil info) "\n"
                                   "<span class=\"subtitle\">%s</span>\n"))
                         (org-export-data subtitle info))
                      "")))))
             ;; "<script type='text/javascript'>"
             ;; (with-temp-buffer
             ;;   (insert-file-contents "/home/shared/hurricane/graph.json")
             ;;   (buffer-string))
             ;; "</script>"
             (if (string= (org-export-data (plist-get info :title) info) "The Map")
                 (with-temp-buffer
                   (insert-file-contents "/home/shared/hurricane/graph.svg")
                   (buffer-string)))
             contents
             (format "</%s>\n" (nth 1 (assq 'content (plist-get info :html-divs))))
             "<div id='temp-network' style='display:none'></div>"
             "</div></div>"
             (unless (string= (org-export-data (plist-get info :title) info) "The Map")
               "</div>")
             (org-html--build-pre/postamble 'postamble info)
             "</body>
          </html>")))

(defun hurricane/publish ()
  (rassq-delete-all 'html-mode auto-mode-alist)
  (rassq-delete-all 'web-mode auto-mode-alist)
  (fset 'web-mode (symbol-function 'fundamental-mode))
  (call-interactively 'org-publish-all))

;; Republish all files, even if no changes made to the page content.
;; (for example, if you want backlinks to be regenerated).
(defun hurricane/republish ()
	(let ((current-prefix-arg 4))
    (rassq-delete-all 'web-mode auto-mode-alist)
    (fset 'web-mode (symbol-function 'fundamental-mode))
    (call-interactively 'org-publish-all)))

;;;;;;;;;;;;;;;;;;;
;; Graph-related ;;
;;;;;;;;;;;;;;;;;;;

(defvar hurricane/graph-node-extra-config
        '(("shape"      . "rectangle")
          ("style"      . "rounded,filled")
          ("fillcolor"  . "#EEEEEE")
          ("fontname" . "sans")
          ("fontsize" . "10px")
          ("labelfontname" . "sans")
          ("color"      . "#C9C9C9")
          ("fontcolor"  . "#111111")))

;; Change the look of the graphviz graph a little.
(setq org-roam-graph-node-extra-config hurricane/graph-node-extra-config)

(defun hurricane/web-graph-builder (file)
  (concat (url-hexify-string (file-name-sans-extension (file-name-nondirectory file))) ".html"))

;; `org-roam-graph-node-url-builder` is not in master org-roam, I've added it to my local version.
;; see: https://github.com/ngm/org-roam/commit/82f40c122c836684a24a885f044dcc508212a17d
;; It's to allow setting a different URL for nodes on the graph.
(setq org-roam-graph-node-url-builder 'hurricane/web-graph-builder)

(setq org-roam-graph-exclude-matcher '("sitemap" "index" "recentchanges"))

;; Called from the Makefile.
;; It builds the graph and puts graph.dot and graph.svg in a place where I can publish them.
;; I exclude a few extra files from the graph here.
;; (I can't remember why I don't have them in the exclude-matcher!)
(defun hurricane/build-graph ()
  (let* ((node-query `[:select [titles:file titles:title tags:tags] :from titles
                               :left :join tags
                               :on (= titles:file tags:file)
                               :where :not (like title '"%2020%")
                               :and :not (like title '"%2019%")
                               :and :not (like title '"%All pages%")
                               :and :not (like title '"%Some books%")
                               :and :not (like title '"%Home%")])
         (graph      (org-roam-graph--dot node-query))
         (temp-dot (make-temp-file "graph." nil ".dot" graph))
         (temp-graph (make-temp-file "graph." nil ".svg")))
    (call-process "dot" nil 0 nil temp-dot "-Tsvg" "-o" temp-graph)
    (sit-for 5) ; TODO: switch to make-process (async) and callback to not need this.
    (copy-file temp-dot (concat hurricane/project-dir "/graph.dot") 't)
    (copy-file temp-graph (concat hurricane/project-dir "/graph.svg") 't)))


(defun hurricane/external-link-format (text backend info)
  (when (org-export-derived-backend-p backend 'html)
    (when (string-match-p (regexp-quote "http") text)
      (s-replace "<a" "<a target='_blank' rel='noopener noreferrer'" text))))

(add-to-list 'org-export-filter-link-functions
             'hurricane/external-link-format)

(setq org-roam-server-network-label-wrap-length 20)
(setq org-roam-server-network-label-truncate t)
(setq org-roam-server-network-label-truncate-length 60)
(setq org-roam-server-extra-node-options nil)
(setq org-roam-server-extra-edge-options nil)
(setq org-roam-server-network-arrows nil)

(defun hurricane/build-graph-json ()
  (let* ((node-query `[:select [titles:file titles:title tags:tags] :from titles
                               :left :join tags
                               :on (= titles:file tags:file)
                               :where :not (like title '"%2020%")
                               :and :not (like title '"%2019%")
                               :and :not (like title '"%All pages%")
                               :and :not (like title '"%Some books%")
                               :and :not (like title '"%Home%")])
         (temp-graph (make-temp-file "graph." nil ".json")))
    (write-region (hurricane/visjs-json node-query) nil temp-graph)
    ;(sit-for 5) ; TODO: switch to make-process (async) and callback to not need this.
    (copy-file temp-graph (concat hurricane/project-dir "/graph.json") 't)))

(defun hurricane/visjs-json (node-query)
  "Convert `org-roam` NODE-QUERY db query to the visjs json format."
  (org-roam-db--ensure-built)
  (org-roam--with-temp-buffer nil
    (let* ((-compare-fn (lambda (x y) (string= (car x) (car y))))
           (nodes (-distinct (org-roam-db-query node-query)))
           (edges-query
            `[:with selected :as [:select [file] :from ,node-query]
                    :select :distinct [to from] :from links
                    :where (and (in to selected) (in from selected))])
           (edges-cites-query
            `[:with selected :as [:select [file] :from ,node-query]
                    :select :distinct [file from] :from links
                    :inner :join refs :on (and (= links:to refs:ref)
                                               (= links:type "cite")
                                               (= refs:type "cite"))
                    :where (and (in file selected) (in from selected))])
           (edges       (org-roam-db-query edges-query))
           (edges-cites (org-roam-db-query edges-cites-query))
           (graph (list (cons 'nodes (list))
                        (cons 'edges (list)))))
      (dotimes (idx (length nodes))
        (let* ((file (xml-escape-string (car (elt nodes idx))))
               (title (or (cadr (elt nodes idx))
                          (org-roam--path-to-slug file)))
               (tags (elt (elt nodes idx) 2)))
          (push (append (list (cons 'id (org-roam--path-to-slug file))
                              (cons 'title title)
                              (cons 'tags tags)
                              (cons 'label (s-word-wrap
                                            org-roam-server-network-label-wrap-length
                                            (if org-roam-server-network-label-truncate
                                                (s-truncate
                                                 org-roam-server-network-label-truncate-length
                                                 title)
                                              title)))
                              (cons 'url (concat "org-protocol://roam-file?file="
                                                 (url-hexify-string file)))
                              (cons 'path file))
                        (pcase org-roam-server-extra-node-options
                          ('nil nil)
                          ((pred functionp)
                           (funcall org-roam-server-extra-node-options (elt nodes idx)))
                          ((pred listp)
                           org-roam-server-extra-node-options)
                          (wrong-type
                           (error "Wrong type of org-roam-server-extra-node-options: %s"
                                  wrong-type))))
                (cdr (elt graph 0)))))
      (dolist (edge edges)
        (let* ((title-source (org-roam--path-to-slug (elt edge 0)))
               (title-target (org-roam--path-to-slug (elt edge 1))))
          (push (remove nil (append (list (cons 'from title-source)
                                          (cons 'to title-target)
                                          (cons 'arrows org-roam-server-network-arrows))
                                    (pcase org-roam-server-extra-edge-options
                                      ('nil nil)
                                      ((pred functionp)
                                       (funcall org-roam-server-extra-edge-options edge))
                                      ((pred listp)
                                       org-roam-server-extra-edge-options)
                                      (wrong-type
                                       (error "Wrong type of org-roam-server-extra-edge-options: %s"
                                              wrong-type)))))
                (cdr (elt graph 1)))))
      (dolist (edge edges-cites)
        (let* ((title-source (org-roam--path-to-slug (elt edge 0)))
               (title-target (org-roam--path-to-slug (elt edge 1))))
          (push (remove nil (list (cons 'from title-source)
                                  (cons 'to title-target)
                                  (cons 'arrows org-roam-server-network-arrows)))
                (cdr (elt graph 1)))))
      (json-encode graph))))

(org-link-set-parameters "video" :export 'org-video-link-export)

(defun org-video-link-export (path desc backend)
  (let ((ext (file-name-extension path)))
    (cond
     ((eq 'html backend)
      (format "<video preload='metadata' controls='controls'><source type='video/%s' src='%s' /></video>" ext path))
     ;; fall-through case for everything else.
     (t
      path))))

(defun hurricane/org-html-wrap-blocks-in-code (src backend info)
  "Wrap a source block in <pre><code class=\"lang\">.</code></pre>"
  (when (org-export-derived-backend-p backend 'html)
    (replace-regexp-in-string
     "\\(</pre>\\)" "</code>\n\\1"
     (replace-regexp-in-string "<pre class=\"src src-\\([^\"]*?\\)\">"
                               "<pre>\n<code class=\"\\1\">\n" src))))

(with-eval-after-load 'ox-html
  (add-to-list 'org-export-filter-src-block-functions
               'hurricane/org-html-wrap-blocks-in-code))
