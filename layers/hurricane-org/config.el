;;; CONFIG ---
;;
;; Author: c <c@MacBook-Pro.local>
;; Copyright © 2020, c, all rights reserved.
;; Created: 12 March 2020
;;
;;; Commentary:
;;
;;
;;
;;; Code:
(progn
  "Settings of `org-export'."
  (setq org-export-in-background t
        ;; Hide html built-in style and script.
        org-html-htmlize-output-type 'inline-css ;; 保留代码块高亮
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
;;; config.el ends here
