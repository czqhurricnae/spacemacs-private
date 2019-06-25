(defconst czqhurricane-ui-packages
  '(
    (color-theme-solarized :location elpa)
    which-func
    pangu-spacing
    (awesome-tab :location (recipe :fetcher
                            github :repo "manateelazycat/awesome-tab"))))

;; {{
;; @see: http://emacsredux.com/blog/2014/04/05/which-function-mode/
;; Show the current function name in the Power line.
(defun czqhurricane-ui/init-which-func ()
  (use-package which-func
  :init
  (progn
    (which-function-mode 1)
    (if (not (member '(which-func-mode ("" which-func-format " ")) mode-line-format))
      (progn
      (push '(which-func-mode ("" which-func-format " ")) mode-line-format)))
  )
  :defer t
  :config
    (setq mode-line-format (remove '(which-func-mode ("" which-func-format " ")) mode-line-format))))

(defun czqhurricane-ui/init-color-theme-solarized ()
  (use-package color-theme-solarized
  :init
  (spacemacs/load-theme 'solarized)
  :defer t))
;; }}

(defun czqhurricane-ui/init-pangu-spacing ()
  (use-package pangu-spacing
  :init
  (progn
    (setq pangu-spacing-chinese-after-english-regexp
          (rx (group-n 1 (in "a-zA-Z0-9,.!?])%#@&1234567890,;\":"))
          (group-n 2 (category chinse-two-byte))))
    ;; Add toggle options
    (spacemacs|add-toggle toggle-pangu-spaceing
      :status pangu-spacing-mode
      :on (global-pangu-spacing-mode)
      :off (global-pangu-spacing-mode -1)
      :documentation "Toggle pangu spacing mode"
      :evil-leader "ots")
    (add-hook 'org-mode-hook
              '(lambda ()
                  (set (make-local-variable 'pangu-spacing-real-insert-separtor) t)
                  (spacemacs/toggle-toggle-pangu-spaceing-on))))))

;; {{
;; @see: https://github.com/manateelazycat/awesome-tab/blob/master/README.md
(defun czqhurricane-ui/init-awesome-tab ()
  (use-package awesome-tab
    :ensure nil
    :load-path "~/.emacs.d/elpa/awesome-tab"
    :config
    (awesome-tab-mode t)
    (setq awesome-tab-style "bar")))
;; }}
;;; packages.el ends here
