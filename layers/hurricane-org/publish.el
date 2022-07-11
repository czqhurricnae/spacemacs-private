(require 'package)
(add-to-list 'load-path "~/.emacs.d/elpa/27.2/develop/s-20210616.619")
(add-to-list 'load-path "~/.emacs.d/elpa/27.2/develop/dash-20210826.1149")
(add-to-list 'load-path "~/.emacs.d/elpa/27.2/develop/f-20210624.1103")
(add-to-list 'load-path "~/.emacs.d/elpa/27.2/develop/emacsql-20220102.220")
(add-to-list 'load-path "~/.emacs.d/elpa/27.2/develop/emacsql-sqlite-20220101.1820")
(add-to-list 'load-path "~/.emacs.d/elpa/27.2/develop/org-9.5.2")
(add-to-list 'load-path "~/.emacs.d/elpa/27.2/develop/org-roam-20220121.2350")
(add-to-list 'load-path "~/.emacs.d/elpa/27.2/develop/ox-hugo-20220624.431")
(add-to-list 'load-path "~/.emacs.d/elpa/27.2/develop/htmlize-20210825.2150")
(add-to-list 'load-path "~/.emacs.d/elpa/27.2/develop/magit-section-20220101.841")
(add-to-list 'load-path "~/.emacs.d/elpa/27.2/develop/tomelr-0.4.3")
(package-initialize)

(require 's)
(require 'find-lisp)
(require 'font-lock)
(require 'subr-x) ;; for `when-let'
(require 'ox-publish)
(require 'ox-html)
(require 'htmlize)
(require 'emacsql)
(require 'emacsql-sqlite)
(require 'org)
(require 'org-roam)
(require 'org-roam-db)
(require 'ox-hugo)
(require 'htmlize)
(require 'magit-section)
(require 'tomelr)

(setq org-confirm-babel-evaluate nil)
(setq org-roam-directory "~/Library/Mobile Documents/iCloud~com~appsonthemove~beorg/Documents/braindump/notes/")
(setq org-roam-db-location "~/Library/Mobile Documents/iCloud~com~appsonthemove~beorg/Documents/braindump/notes/org-roam.db")
(setq org-id-extra-files (find-lisp-find-files org-roam-directory "\.org$"))

(org-roam-mode)

(org-link-set-parameters
 "x-devonthink-item"
 :export (lambda (path desc backend)
           (cond
            ((eq 'md backend)
             (format "<font color=\"red\"> <a href=\"x-devonthink-item:%s\">%s </a> </font>"
                     path
                     desc)))))

(defun hurricane//org-video-link-export (path desc backend)
  (let ((ext (file-name-extension path)))
    (cond
     ((eq 'md backend)
      (format "<video preload='metadata' controls='controls'><source type='video/%s' src='%s' /></video>" ext path))
     ;; fall-through case for everything else.
     (t
      path))))

(org-link-set-parameters "video" :export 'hurricane//org-video-link-export)

(defun hurricane//collect-backlinks-string (backend)
  (when (org-roam-node-at-point)
    (let* ((source-node (org-roam-node-at-point))
           (source-file (org-roam-node-file source-node))
           (nodes-in-file (--filter (s-equals? (org-roam-node-file it) source-file)
                                    (org-roam-node-list)))
           (nodes-start-position (-map 'org-roam-node-point nodes-in-file))
           ;; Nodes don't store the last position, so get the next headline position
           ;; and subtract one character (or, if no next headline, get point-max)
           (nodes-end-position (-map (lambda (nodes-start-position)
                                       (goto-char nodes-start-position)
                                       (if (org-before-first-heading-p) ;; file node
                                           (point-max)
                                         (call-interactively
                                          'org-forward-heading-same-level)
                                         (if (> (point) nodes-start-position)
                                             (- (point) 1) ;; successfully found next
                                           (point-max)))) ;; there was no next
                                     nodes-start-position))
           ;; sort in order of decreasing end position
           (nodes-in-file-sorted (->> (-zip nodes-in-file nodes-end-position)
                                      (--sort (> (cdr it) (cdr other))))))
      (dolist (node-and-end nodes-in-file-sorted)
        (-when-let* (((node . end-position) node-and-end)
                     (backlinks (--filter (->> (org-roam-backlink-source-node it)
                                               (org-roam-node-file)
                                               (s-contains? "private/") (not))
                                          (org-roam-backlinks-get node)))
                     (heading (format "\n\n%s Links to this node\n"
                                      (s-repeat (+ (org-roam-node-level node) 1) "*")))
                     (properties-drawer ":PROPERTIES:\n:HTML_CONTAINER_CLASS: references\n:END:\n"))
          (goto-char end-position)
          (insert "#+BEGIN_EXPORT html
<details>
  <summary>Click to expand!</summary>

<div class=\"bl-section\"><aside class=\"bl-aside\"><blockquote>
#+END_EXPORT")
          (insert heading)
          (insert properties-drawer)
          (dolist (backlink backlinks)
            (let* ((source-node (org-roam-backlink-source-node backlink))
                   (source-file (org-roam-node-file source-node))
                   (properties (org-roam-backlink-properties backlink))
                   (outline (when-let ((outline (plist-get properties :outline)))
                              (when (> (length outline) 1)
                                (mapconcat #'org-link-display-format outline " > "))))
                   (point (org-roam-backlink-point backlink))
                   (text (s-replace "\n" " " (org-roam-preview-get-contents
                                              source-file
                                              point)))
                   (reference (format "%s [[id:%s][%s]]\n%s\n%s\n\n"
                                      (s-repeat (+ (org-roam-node-level node) 2) "*")
                                      (org-roam-node-id source-node)
                                      (org-roam-node-title source-node)
                                      (if outline (format "%s (/%s/)"
                                                          (s-repeat (+ (org-roam-node-level node) 3) "*") outline) "")
                                      text))
                   (label-list (with-temp-buffer
                                 (insert text)
                                 (org-element-map (org-element-parse-buffer) 'footnote-reference
                                   (lambda (reference)
                                     (org-element-property :label reference)))))
                   (footnote-string-list
                      (with-temp-buffer
                        (insert-file-contents source-file)
                        (-map (lambda (label) (buffer-substring-no-properties
                                               (nth 1 (org-footnote-get-definition label))
                                               (nth 2 (org-footnote-get-definition label))))
                              label-list))))
              (-map (lambda (footnote-string) (progn (insert footnote-string))) footnote-string-list)
              (insert reference)
              (insert "#+BEGIN_EXPORT html
  </blockquote></aside></div>
</details>

#+END_EXPORT"))))))))

(add-hook 'org-export-before-processing-hook 'hurricane//collect-backlinks-string)

(defun hurricane//org-html-wrap-blocks-in-code (src backend info)
  "Wrap a source block in <pre><code class=\"lang\">.</code></pre>"
  (when (org-export-derived-backend-p backend 'html)
    (replace-regexp-in-string
     "\\(</pre>\\)" "</code>\n\\1"
     (replace-regexp-in-string "<pre class=\"src src-\\([^\"]*?\\)\">"
                               "<pre>\n<code class=\"\\1\">\n" src))))

(with-eval-after-load 'ox-html
  (add-to-list 'org-export-filter-src-block-functions
               'hurricane//org-html-wrap-blocks-in-code))

(defun batch-export-org-files-to-md (dir)
  "Export all org files in directory DIR to markdown."
  (let ((files (directory-files-recursively dir "\\`[^.#].*\\.org\\'")))
    (dolist (file files)
      (message "process %s" file)
      (with-current-buffer (find-file-noselect file)
        (setq org-hugo-base-dir "..")
        (let ((org-id-extra-files (find-lisp-find-files org-roam-directory "\.org$")))
          (org-hugo-export-wim-to-md t))))))

(defun hurricane/publish ()
  (batch-export-org-files-to-md org-roam-directory))
