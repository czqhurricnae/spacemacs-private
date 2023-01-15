;; org-roam-backlink-collections.el --- For org-mode and org-roam -*- lexical-binding: t -*-
;;; ORG-ROAM-BACKLINK-COLLECTIONS ---
;;
;; Author: c <c@MacBook-Pro.local>
;; Copyright © 2022, c, all rights reserved.
;; Created: 25 December 2022
;;
;;; Commentary:
;;
;;
;;
;;; Code:
(require 'dash)
(require 's)

(cl-eval-when (compile)
  (require 'org)
  (require 'org-roam))

(defface org-roam-backlink-collections
  '((((class color) (min-colors 88) (background light))
     :background "#ebf6fa" :extend t)
    (((class color) (min-colors 88) (background dark))
     :background "#041529" :extend t)
    (t ))
  "Face for backlink region in the collections buffer.")

(defcustom org-roam-backlink-collections-add-all-on-activate t
  "Define whether to add all the collections on activation.
When non-nil, automatically add all on `org-roam-backlink-collections-activate'."
  :type 'boolean)

(defcustom org-roam-backlink-collections-after-add-functions nil
  "Functions to be called after a collection content has been added.
The hook runs after the content and the read-only text property
have been added so it is not supposed to manipulate the content
but to add further text properties.
The functions are called with arguments beg
and end, pointing to the beginning and end of the collected content."
  :type '(repeat function))

(defvar org-roam-backlink-collections-map
  (let ((map (make-sparse-keymap))))
  map)

(defvar-local org-roam-backlink-collections-remember-point nil
  "This variable is used to remember the current just before `save-buffer'.
It is meant to be used to remember and return to the current
point after `before-save-hook' and `after-save-hook' pair;
`org-roam-backlink-collections-before-save-buffer' and
`org-roam-backlink-collections-after-save-buffer' use this variable.")

(defvar-local org-roam-backlink-collections-remember-collections nil
  "Remember the active collections before `save-buffer'.
It is meant to be used to keep the file the current buffer is
visiting clear of the collected text content.  Instead of
blindly deactivate and activate all collections with t flag,
this variable is meant to provide mechanism to
deactivate/activate only the collections currently used to copy
a text content.

`org-roam-backlink-collections-before-save-buffer' and
`org-roam-backlink-collections-after-save-buffer' use this variable.")

(defun org-roam-backlink-collections-within-collection-p ()
  "Return t if the current point is within a collection region."
  (when (get-char-property (point) 'org-roam-backlink-collections-type) t))

(defun org-roam-backlink-collections-check-add ()
  "Return t if `org-roam-backlink-collections-add' should work on the point.
Error if collect is not allowed."
  (cond
   ((org-roam-backlink-collections-within-collection-p)
    (user-error (format "Cannot collect in another collections at point %d, line %d" (point) (org-current-line))))
   (t
    t)))

(defun org-roam-backlink-collections-content-insert (content)
  (let* ((beg (point))
         (beg-mkr (set-marker (make-marker) beg))
         (end)
         (end-mkr)
         (type "org-id")
         (content content)
         )
    (insert (org-roam-fontify-like-in-org-mode content))
    (setq end (point))
    (setq end-mkr (set-marker (make-marker) end))
    (add-text-properties beg end
                         `(local-map ,org-roam-backlink-collections-map
                                     read-only t
                                     front-sticky t
                                     rear-nonsticky t
                                     org-roam-backlink-collections-type ,type
                                     org-roam-backlink-collections-beg-mkr
                                     ,beg-mkr
                                     org-roam-backlink-collections-end-mkr
                                     ,end-mkr))
    (overlay-put (make-overlay beg end)
                 'face 'org-roam-backlink-collections)
    t))

;;;###autoload
(defun org-roam-backlink-collections-add-all (&optional narrowed)
  (interactive "P")
  (save-restriction
    (let ((marker (move-marker (make-marker) (point))))
      (unless narrowed (widen))
      (goto-char (point-min))
      (when (org-roam-node-at-point)
        (let* ((source-node (org-roam-node-at-point))
               (source-file (org-roam-node-file source-node))
               (nodes-in-file (--filter (s-equals? (org-roam-node-file it) source-file)
                                        (org-roam-node-list)))
               (nodes-start-position (-map 'org-roam-node-point nodes-in-file))
               (nodes-end-position (-map (lambda (nodes-start-position)
                                           (widen)
                                           (goto-char nodes-start-position)
                                           (if (org-before-first-heading-p) ;; file node
                                               (point-max)
                                             (call-interactively
                                              'org-next-visible-heading)
                                             (if (> (point) nodes-start-position)
                                                 (- (point) 1) ;; successfully found next
                                               (progn
                                                 (org-narrow-to-subtree)
                                                 (point-max))))) ;; there was no next
                                         nodes-start-position))
               (nodes-in-file-sorted (->> (-zip nodes-in-file nodes-end-position)
                                          (--sort (> (cdr it) (cdr other))))))
          (dolist (node-and-end nodes-in-file-sorted)
            (-when-let* (((node . end-position) node-and-end))
              (goto-char end-position)
              (unless (org-roam-backlink-collections-within-collection-p)
                (with-demoted-errors
                    "Not collected. Continue to next: %S"
                  (when (org-roam-backlink-collections-add node)
                    (message (format "Collected at point %d, line %d" (point) (org-current-line))))))))))
      (goto-char marker)
      (move-marker marker nil) ; point nowhere for GC
      t)))

;;;###autoload
(defun org-roam-backlink-collections-add (&optional node)
  (interactive)
  (when (org-roam-backlink-collections-check-add)
    (unless org-roam-backlink-collections-mode
      (let ((org-roam-backlink-collections-add-all-on-activate nil))
        (org-roam-backlink-collections-mode +1)))
    (let* ((node (or node (org-roam-node-at-point)))
           (backlinks (--filter (->> (org-roam-backlink-source-node it)
                                     (org-roam-node-file)
                                     (s-contains? "private/") (not))
                                (org-roam-backlinks-get node)))
           (content-and-footnote-string-list
            (-map (lambda (backlink)
                    (let* ((source-node (org-roam-backlink-source-node backlink))
                           (source-file (org-roam-node-file source-node))
                           (properties (org-roam-backlink-properties backlink))
                           (outline (if-let ((outline (plist-get properties :outline)))
                                        (mapconcat #'org-link-display-format outline " > ")))
                           (point (org-roam-backlink-point backlink))
                           (text (org-roam-preview-get-contents
                                                      source-file
                                                      point))
                           (content (format "%s [[id:%s][%s]]\n%s\n%s"
                                              (s-repeat (+ (org-roam-node-level node) 1) "*")
                                              (org-roam-node-id source-node)
                                              (org-roam-node-title source-node)
                                              (if outline (format "%s (%s)"
                                                                  (s-repeat (+ (org-roam-node-level node) 2) "*") outline) "")
                                              text))
                           (label-list (with-temp-buffer
                                         (insert-file-contents source-file)
                                         (org-element-map (org-element-parse-buffer) 'footnote-reference
                                           (lambda (reference)
                                             (org-element-property :label reference)))))
                           (footnote-list
                            (with-temp-buffer
                              (insert-file-contents source-file)
                              (-map (lambda (label) (buffer-substring-no-properties
                                                     (nth 1 (org-footnote-get-definition label))
                                                     (nth 2 (org-footnote-get-definition label))))
                                    label-list)))
                           (footnote-string-list (string-join footnote-list "\n"))
                           (content-and-footnote-string (format "%s\n%s" content footnote-string-list)))
                      content-and-footnote-string)
                    ) backlinks)))
      (if (or (string= (string-join content-and-footnote-string-list "\n") "")
              (eq content-and-footnote-string-list nil))
          ;; Keep going with program when no content `org-roam-backlink-collections-add-all'
          ;; should move to the next collection
          (progn (message
                  (format
                   "No backlink found with \"%s\".  Check the link at point %d, line %d"
                   (org-roam-node-title node) (point) (org-current-line))
                  nil))
        (let ((beg (line-beginning-position))
              (end))
          (org-roam-backlink-collections-with-inhibit-read-only
            (when (save-excursion
                    (end-of-line) (insert-char ?\n)
                    (org-roam-backlink-collections-content-insert
                     (string-join content-and-footnote-string-list "\n"))
                    (unless (eobp) (delete-char 1))
                    (setq end (point))
                    t)
              ))
          (run-hook-with-args 'org-roam-backlink-collections-after-add-functions beg end))
        t))))

(defun org-roam-backlink-collections-remove-all (&optional narrowed)
  (interactive "P")
  (save-restriction
    (let ((marker (move-marker (make-marker) (point)))
          match point list)
      (unless narrowed (widen))
      (goto-char (point-min))
      (while (setq match (text-property-search-forward 'org-roam-backlink-collections-type))
        (goto-char (prop-match-beginning match))
        (setq point (org-roam-backlink-collections-remove))
        (when point (push point list)))
      (goto-char marker)
      (move-marker marker nil) ; point nowhere for GC
      list)))

(defun org-roam-backlink-collections-remove ()
  "Remove collected text at point.
When success, return the beginning point of the keyword re-inserted."
  (interactive)
  (if-let* ((beg (marker-position (get-char-property (point)
                                                     'org-roam-backlink-collections-beg-mkr)))
            (end (marker-position (get-char-property (point)
                                                     'org-roam-backlink-collections-end-mkr))))
      (progn
        ;; Need to retain the markers of the other adjacent collections
        ;; if any.  If their positions differ after insert, move them back
        ;; beg or end
        (let ((mkr-at-beg
               ;; Check the points to look at exist in buffer.  Then look for
               ;; adjacent collections' markers if any.
               (when (>= (1- beg)(point-min))
                 (get-text-property (1- beg) 'org-roam-backlink-collections-end-mkr))))
          ;; If within live-sync, exit.  It's not absolutely
          ;; required. delete-region below will evaporate the live-sync
          ;; overlay, and text-clone's post-command correctly handles the
          ;; overlay on the source.
          (org-roam-backlink-collections-with-inhibit-read-only
           (save-excursion
             (delete-region beg end)
             )
           ;; Move markers of adjacent collections if any to their original
           ;; potisions.  Some markers move if two collections are placed
           ;; without any blank lines, and either of beg and end markers will
           ;; inevitably have the same position (location "between" lines)
           (when mkr-at-beg (move-marker mkr-at-beg beg))
           ;; Go back to the beginning of the inserted keyword line
           (goto-char beg))
          beg))
    (message "Nothing done. No collection exists here.") nil))

(defmacro org-roam-backlink-collections-with-inhibit-read-only (&rest body)
  "Run BODY with `'inhibit-read-only` t."
  (declare (debug t) (indent 0))
  (let ((modified (make-symbol "modified")))
    `(let* ((,modified (buffer-modified-p))
            (inhibit-read-only t))
       (unwind-protect
           (progn
             ,@body)
         (unless ,modified
           (restore-buffer-modified-p nil))))))

(defun org-roam-backlink-collections-inhibit-read-only (&rest _args)
  "Set `inhibit-read-only' to t for Org export functions.
Org export may need the buffer not to contain read-only elements.
This function is meant to be added to
`org-export-before-processing-hook' to temporarily inhibit
read-only."
  (setq-local inhibit-read-only t))

;;;###autoload
(define-minor-mode org-roam-backlink-collections-mode
  "Toggle Org-roam-backlink-collections minor mode."
  :init-value nil
  :lighter org-roam-backlink-collections-mode-lighter
  :global nil
  :keymap (let ((map (make-sparse-keymap)))
            map)
  (cond
   (org-roam-backlink-collections-mode
    (org-roam-backlink-collections-activate)
    (when org-roam-backlink-collections-add-all-on-activate
      (org-roam-backlink-collections-add-all)))
   (t (org-roam-backlink-collections-deactivate))))

;;;###autoload
(defun org-roam-backlink-collections-activate ()
  "Activate Org-backlink-collections hooks and other setups in the current buffer.
This function does not add collections; it merely sets up hooks
and variables."
  (interactive)
  (add-hook 'before-save-hook #'org-roam-backlink-collections-before-save-buffer nil t)
  ;; (add-hook 'after-save-hook #'org-roam-backlink-collections-after-save-buffer nil t)
  (add-hook 'kill-buffer-hook #'org-roam-backlink-collections-before-kill nil t)
  (add-hook 'kill-emacs-hook #'org-roam-backlink-collections-before-kill nil t)
  (add-hook 'org-export-before-processing-hook
            #'org-roam-backlink-collections-inhibit-read-only nil t))

(defun org-roam-backlink-collections-deactivate ()
  "Dectivate org-roam-backlink-collections hooks and other setups in the current buffer.
This function also removes all the collections in the current buffer."
  (interactive)
  (org-roam-backlink-collections-remove-all)
  (remove-hook 'before-save-hook #'org-roam-backlink-collections-before-save-buffer t)
  ;; (remove-hook 'after-save-hook #'org-roam-backlink-collections-after-save-buffer t)
  (remove-hook 'kill-buffer-hook #'org-roam-backlink-collections-before-kill t)
  (remove-hook 'kill-emacs-hook #'org-roam-backlink-collections-before-kill t)
  (remove-hook 'org-export-before-processing-hook
               #'org-roam-backlink-collections-inhibit-read-only t))

(defun org-roam-backlink-collections-before-save-buffer ()
  "Remove collections in `before-save-hook'.
This function is meant to clear the file clear of the
collections.  It also remembers the current point for
`org-roam-backlink-collections-after-save-buffer' to move it back."
  (setq org-roam-backlink-collections-remember-point (point))
  (setq org-roam-backlink-collections-remember-collections
        (org-roam-backlink-collections-remove-all)))

(defun org-roam-backlink-collections-after-save-buffer ()
  "Add collections back as they were `before-save-buffer'.
This function relies on `org-roam-backlink-collections-remember-collections'
set in `before-save-hook'.  It also move the point back to
`org-roam-backlink-collections-remember-point'."
  (unwind-protect
      (progn
        ;; Assume the list is in descending order.
        ;; pop and do from the bottom of buffer
        (dolist (p org-roam-backlink-collections-remember-collections)
          (save-excursion
            (goto-char p)
            (message (format "after-save-buffer %s" p))
            (org-roam-backlink-collections-add)))
        ;; After save and adding all collections, the modified flag should be
        ;; set to nil
        (restore-buffer-modified-p nil)
        (when org-roam-backlink-collections-remember-point
          (goto-char org-roam-backlink-collections-remember-point))
        (progn
          (setq org-roam-backlink-collections-remember-point nil)
          (setq org-roam-backlink-collections-remember-collections nil)))))

(defun org-roam-backlink-collections-before-kill ()
  "Remove collections before `kill-buffer' or `kill-emacs'.
Intended to be used with `kill-buffer-hook' and `kill-emacs-hook'
to clear the file of the collected text regions.  This function
also flags the buffer modified and `save-buffer'.  Calling the
second `org-roam-backlink-collections-remove-all' ensures the clearing process
to occur.  This is reqiured because during live-sync, some hooks
that manage the clearing process are temporarily turned
off (removed)."
  ;; Remove collections first. To deal with an edge case where collections
  ;; were added for a capture buffer -- e.g. `org-capture' or `org-roam-catpure'
  ;; --, check is done for `buffer-file-name' to see if there is a file visited
  ;; by the buffer. If a "temp" buffer, there is no file being visited.
  (when (and (org-roam-backlink-collections-remove-all)
             (buffer-file-name))
    (org-roam-backlink-collections-remove-all)))

(provide 'org-roam-backlink-collections)
;;; org-roam-backlink-collections.el ends here
