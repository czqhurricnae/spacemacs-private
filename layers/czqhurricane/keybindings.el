(spacemacs/set-leader-keys "h;" 'comment-line)
(spacemacs/set-leader-keys "hs" 'org-screenshot)
(spacemacs/set-leader-keys "hS" 'org-delete-screenshot-image-file-and-link)
(spacemacs/set-leader-keys "he" 'org-image-to-base64-converter)
(spacemacs/set-leader-keys "hE" 'org-delete-image-file-and-link)
(spacemacs/set-leader-keys "hp" 'dash-at-point-with-docset)
(spacemacs/set-leader-keys "hP" 'dash-at-point)
(spacemacs/set-leader-keys "so" 'czqhurricane/occur-dwin)
(spacemacs/set-leader-keys "ii" 'ein:worksheet-insert-cell-below)
(spacemacs/set-leader-keys "iI" 'ein:worksheet-insert-cell-above)
(spacemacs/set-leader-keys "it" 'ein:worksheet-change-cell-type)
(spacemacs/set-leader-keys "id" 'ein:worksheet-kill-cell)
(spacemacs/set-leader-keys "is" 'ein:notebook-save-notebook-command)
(spacemacs/set-leader-keys "oi" 'czqhurricane/org-insert-src-block)
(spacemacs/set-leader-keys "oe" 'org-edit-special)
(spacemacs/set-leader-keys "or" 'org-src-do-at-code-block)
(spacemacs/set-leader-keys "oc" 'org-gfm-export-to-markdown-filter)
(spacemacs/set-leader-keys "fs" 'save-buffer-filter)
(define-key evil-normal-state-map (kbd "C-u") nil)
(spacemacs/set-leader-keys "jg" 'dumb-jump-go)
(spacemacs/set-leader-keys "pf" 'czqhurricane/open-file-with-projectile-or-counsel-git)
(spacemacs/set-leader-keys "po" 'my-simple-todo)
(global-set-key [(shift return)] 'czqhurricane/smart-open-line)
(global-set-key (kbd "C-c i e") 'yas-expand)
(global-set-key (kbd "C-c C-x t") 'czqhurricane/org-clock-sum-today-by-tags)
(define-key evil-normal-state-map   (kbd "C-g") #'evil-keyboard-quit)
(define-key evil-motion-state-map   (kbd "C-g") #'evil-keyboard-quit)
(define-key evil-insert-state-map   (kbd "C-g") #'evil-keyboard-quit)
(define-key evil-window-map         (kbd "C-g") #'evil-keyboard-quit)
(define-key evil-operator-state-map (kbd "C-g") #'evil-keyboard-quit)
(global-set-key (kbd "C-g") 'evil-keyboard-quit)
