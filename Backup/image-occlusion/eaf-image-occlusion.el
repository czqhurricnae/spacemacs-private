;;; EAF-IMAGE-OCCLUSION ---
;;
;; Author: c <c@MacBook-Pro>
;; Copyright © 2024, c, all rights reserved.
;; Created:  1 October 2024
;;
;;; Commentary:
;;
;;
;;
;;; Code:
(defcustom eaf-image-occlusion-keybinding
  '(("<f12>" . "open_devtools")
    ("x" . "insert_or_close_buffer")
    ("i" . "insert_or_focus_input")
    )
  "The keybinding of EAF Image Occlusion."
  :type 'cons)

(add-to-list 'eaf-app-binding-alist '("image-occlusion" . eaf-image-occlusion-keybinding))

(setq eaf-image-occlusion-module-path (concat (file-name-directory load-file-name) "buffer.py"))
(add-to-list 'eaf-app-module-path-alist '("image-occlusion" . eaf-image-occlusion-module-path))

;;;###autoload
(defun eaf-open-image-occlusion (url &optional args)
  (interactive "M[EAF/image-occlusion] URL: ")
  (eaf-open url "image-occlusion" args))

(provide 'eaf-image-occlusion)
;;; eaf-image-occlusion.el ends here
