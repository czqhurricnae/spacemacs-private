(progn
  "Settings of `org-export'."
  (setq org-export-in-background t
        ;; Hide html built-in style and script.
        org-html-htmlize-output-type 'inline-css ;; 保留代码块高亮。
        org-html-head-include-default-style nil
        org-html-head-include-scripts nil
        ))

(define-minor-mode auto-save-and-publish-file-mode
  "Toggle auto save and publish current file."
  :global nil
  :lighter ""
  (if auto-save-and-publish-file-mode
      ;; When the mode is enabled
      (progn
        (add-hook 'after-save-hook #'save-and-publish-file :append :local))
    ;; When the mode is disabled
    (remove-hook 'after-save-hook #'save-and-publish-file :local)))

(setq org-link-file-path-type 'relative)

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

<script src='/static/vendor/json/jieba.json'></script>"
)

;; {{
;; Fiddle with the HTML output.
;; TODO: note - a bad idea to override org-html-template!!
;; For now I couldn't figure out another way to hook into the HTML
;; to add the required markup for grid-container, grid, and page.
;; Came across this here: https://github.com/ereslibre/ereslibre.es/blob/b28ea388e2ec09b1033fc7eed2d30c69ba3ee827/config/default.el
;; Perhaps an alternative here?  https://vicarie.in/posts/blogging-with-org.html
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
          </html>"))
  )
;; }}

;;{{ @see https://vicarie.in/posts/blogging-with-org.html
(defun sitemap-format-entry (entry _style project)
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
  (concat "#+SETUPFILE: ../theme-rose.setup\n#+TITLE: " title "\n\n" "
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
;;; config.el ends here
