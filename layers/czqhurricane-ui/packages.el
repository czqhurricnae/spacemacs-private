(defconst czqhurricane-ui-packages
  '(
    (doom-themes :location (recipe :fetcher
                                    github :repo "hlissner/emacs-doom-themes"))
    (all-the-icons :location (recipe :fetcher
                                     github :repo "domtronn/all-the-icons.el"))
    (doom-modeline :location (recipe :fetcher
                                     github :repo "seagle0128/doom-modeline"))
    (hide-mode-line :location (recipe :fetcher
                                      github :repo "hlissner/emacs-hide-mode-line"))
    which-func
    pangu-spacing
    (awesome-tab :location (recipe :fetcher
                                   github :repo "manateelazycat/awesome-tab"))
    ))

;; {{
;; Show the current function name in the Power line.
;; @see: http://emacsredux.com/blog/2014/04/05/which-function-mode/
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

;; {{
;; @see: https://github.com/hlissner/emacs-doom-themes
(defun czqhurricane-ui/init-doom-themes ()
  (use-package doom-themes
    :init
    (load-theme 'doom-nord t)
    :custom
    (doom-themes-enable-italic t)
    (doom-themes-enable-bold t)
    :config
    (doom-themes-neotree-config)
    (doom-themes-org-config)))
;; }}

(defun czqhurricane-ui/init-all-the-icons ()
  (use-package all-the-icons))

(defun czqhurricane-ui/init-doom-modeline ()
  (use-package doom-modeline
    :ensure t
    :defer t
    :hook
    (after-init . doom-modeline-mode)
    :custom
    (doom-modeline-icon t)
    (doom-modeline-major-mode-icon nil)
    (doom-modeline-minor-modes nil)
    :config
    (line-number-mode 0)
    (column-number-mode 0)))

;; {{
;; @see: https://github.com/hlissner/emacs-hide-mode-line
(defun czqhurricane-ui/init-hide-mode-line ()
  (use-package hide-mode-line
    :hook
    ((neotree-mode imenu-list-minor-mode minimap-mode
      spacemacs-buffer-mode ibuffer-mode help-mode deft-text-mode) . hide-mode-line-mode)))
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
    :hook
    (after-init . awesome-tab-mode)
    :ensure nil
    :load-path "~/.emacs.d/elpa/awesome-tab"
    :config
    (setq awesome-tab-style "bar")))
;; }}

;;; packages.el ends here
