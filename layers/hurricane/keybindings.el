(spacemacs/set-leader-keys "h;" #'comment-line)
;; (spacemacs/set-leader-keys "hs" #'hurricane/org-screenshot)
;; (spacemacs/set-leader-keys "hS" #'hurricane/org-delete-screenshot-image-file-and-link)
;; (spacemacs/set-leader-keys "he" #'hurricane/org-image-to-base64-converter)
;; (spacemacs/set-leader-keys "hE" #'hurricane/org-delete-image-file-and-link)
;; (spacemacs/set-leader-keys "so" #'hurricane/occur-dwin)
;; (spacemacs/set-leader-keys "ii" #'ein:worksheet-insert-cell-below)
;; (spacemacs/set-leader-keys "iI" #'ein:worksheet-insert-cell-above)
;; (spacemacs/set-leader-keys "it" #'ein:worksheet-change-cell-type)
;; (spacemacs/set-leader-keys "id" #'ein:worksheet-kill-cell)
;; (spacemacs/set-leader-keys "is" #'ein:notebook-save-notebook-command)
(spacemacs/set-leader-keys "oi" #'hurricane/org-insert-src-block)
(spacemacs/set-leader-keys "oe" #'org-edit-special)
(spacemacs/set-leader-keys "or" #'org-src-do-at-code-block)
;; (spacemacs/set-leader-keys "fs" #'save-buffer-filter)
;; (spacemacs/set-leader-keys "jg" #'hurricane/dumb-jump)
(spacemacs/set-leader-keys "pf" #'hurricane/open-file-with-projectile-or-counsel-git)
;; (spacemacs/set-leader-keys "po" #'hurricane/simple-todo)
(define-key evil-normal-state-map (kbd "C-u") nil)
(define-key evil-normal-state-map  (kbd "<escape>") #'hurricane//evil-keyboard-quit)
(define-key evil-motion-state-map  (kbd "<escape>") #'hurricane//evil-keyboard-quit)
(define-key evil-insert-state-map  (kbd "<escape>") #'hurricane//evil-keyboard-quit)
(define-key evil-window-map (kbd "<escape>") #'hurricane//evil-keyboard-quit)
(define-key evil-operator-state-map (kbd "<escape>") #'hurricane//evil-keyboard-quit)
;; (define-key evil-emacs-state-map (kbd "C-c C-c") #'eaf-pdf-outline-edit-buffer-confirm)
;; (define-key evil-emacs-state-map (kbd "C-c C-k") #'kill-buffer-and-window)
;; (define-key evil-emacs-state-map (kbd "<C-return>") #'eaf-pdf-outline-edit-jump)
(global-set-key (kbd "<escape>") 'hurricane//evil-keyboard-quit)
;; (global-set-key (kbd "C-c i r") 'hurricane/org-screenshot-and-ocr)
;; (global-set-key (kbd "C-c i c") 'hurricane/org-insert-caption-and-target)
;; (global-set-key (kbd "C-c i e") 'yas-expand)
;; (global-set-key (kbd "C-c i s") 'hurricane/org-insert-src-block)
(global-set-key [(shift return)] 'hurricane/insert-space-after-point)
;; (global-set-key (kbd "C-c C-x t") 'hurricane/org-clock-sum-today-by-tags)

;; (bind-key* "C-=" 'er/expand-region)
(bind-key* "s-d" 'tab-close)
