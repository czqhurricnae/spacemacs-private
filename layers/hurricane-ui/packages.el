(defconst hurricane-ui-packages
  '(
    (doom-themes :location (recipe :fetcher github
                                   :repo "hlissner/emacs-doom-themes"))
    (hide-mode-line :location (recipe :fetcher github
                                      :repo "hlissner/emacs-hide-mode-line"))
    pangu-spacing
    (shackle :location (recipe :fetcher github
                               :repo "wasamasa/shackle"))
    pretty-hydra
    (activities :location (recipe :fetcher github
                                  :repo "alphapapa/activities.el"))
    (org-tidy :location (recipe :fetcher github
                              :repo "jxq0/org-tidy"))
    ))

;; {{
;; @See: https://github.com/hlissner/emacs-doom-themes
(defun hurricane-ui/init-doom-themes ()
  (use-package doom-themes
    ;; :init
    ;; (load-theme 'doom-solarized-light t)
    :custom
    (doom-themes-enable-italic t)
    (doom-themes-enable-bold t)
    :config
    ;; (doom-themes-neotree-config)
    ;; (doom-themes-org-config)
    ;; Enable flashing mode-line on errors.
    ;; (doom-themes-visual-bell-config)
    ;; (set-face-attribute 'doom-visual-bell nil
    ;;                     :background (face-foreground 'error)
    ;;                     :foreground (face-background 'default)
    ;;                     :inverse-video nil)))
    ))
;; }}

(defun hurricane-ui/pre-init-doom-modeline ()
  (spacemacs/add-to-hooks 'doom-modeline-env-update-python '(pyvenv-post-activate-hooks
                                                             pyvenv-post-deactivate-hooks
                                                             venv-postactivate-hook
                                                             venv-postdeactivate-hook)))

;; {{
;; @See: https://github.com/hlissner/emacs-hide-mode-line
(defun hurricane-ui/init-hide-mode-line ()
  (use-package hide-mode-line
    :defer t
    :hook
    ((neotree-mode
      imenu-list-minor-mode
      minimap-mode
      ibuffer-mode
      help-mode
      ) . hide-mode-line-mode)))
;; }}

(defun hurricane-ui/post-init-pangu-spacing ()
  (progn
    (add-to-list 'pangu-spacing-inhibit-mode-alist 'EAF/file-manager-rename-mode)
    (add-to-list 'pangu-spacing-inhibit-mode-alist 'eaf-pdf-outline-edit-mode)
    ;; Add toggle options.
    (spacemacs|add-toggle pangu-spaceing
      :status pangu-spacing-mode
      :on (global-pangu-spacing-mode)
      :off (global-pangu-spacing-mode -1)
      :documentation "Toggle pangu spacing mode."
      :evil-leader "ots")
    (add-hook 'org-mode-hook
              '(lambda ()
                  (set (make-local-variable 'pangu-spacing-real-insert-separtor) t)
                  (spacemacs/toggle-pangu-spaceing-on)))))

;; {{
;; @See: https://github.com/manateelazycat/awesome-tab/blob/master/README.md
(defun hurricane//define-evil-normal-keybinding (key def &rest bindings)
  "Binding keys and func for normal state map and motion state map."
  (while key
    (define-key evil-normal-state-map (kbd key) def)
    (define-key evil-motion-state-map (kbd key) def)
    (setq key (pop bindings) def (pop bindings))))
;; }}

;; Enforce rules for popups.
(defvar shackle--popup-window-list nil) ; All popup windows.
(defvar-local shackle--current-popup-window nil) ; Current popup window.
(put 'shackle--current-popup-window 'permanent-local t)

(defun hurricane-ui/init-shackle ()
  (use-package shackle
  :functions org-switch-to-buffer-other-window
  :commands shackle-display-buffer
  :hook (after-init . shackle-mode)
  :config
  (eval-and-compile
    (defun shackle-last-popup-buffer ()
      "View last popup buffer."
      (interactive)
      (ignore-errors
        (display-buffer shackle-last-buffer)))

    ;; Add keyword: `autoclose'.
    (defun shackle-display-buffer-hack (fn buffer alist plist)
      (let ((window (funcall fn buffer alist plist)))
        (setq shackle--current-popup-window window)

        (when (plist-get plist :autoclose)
          (push (cons window buffer) shackle--popup-window-list))
        window))

    (defun shackle-close-popup-window-hack (&rest _)
      "Close current popup window via `C-g'."
      (setq shackle--popup-window-list
            (cl-loop for (window . buffer) in shackle--popup-window-list
                     if (and (window-live-p window)
                             (equal (window-buffer window) buffer))
                     collect (cons window buffer)))
      ;; `C-g' can deactivate region.
      (when (and (called-interactively-p 'interactive)
                 (not (region-active-p)))
        (let (window buffer)
          (if (one-window-p)
              (progn
                (setq window (selected-window))
                (when (equal (buffer-local-value 'shackle--current-popup-window
                                                 (window-buffer window))
                             window)
                  (winner-undo)))
            (setq window (caar shackle--popup-window-list))
            (setq buffer (cdar shackle--popup-window-list))
            (when (and (window-live-p window)
                       (equal (window-buffer window) buffer))
              (progn
                (delete-window window)
                (pop shackle--popup-window-list))
                )))))

    (advice-add #'keyboard-quit :before #'shackle-close-popup-window-hack)
    (advice-add #'shackle-display-buffer :around #'shackle-display-buffer-hack))

  ;; HACK: Compatibility issuw with `org-switch-to-buffer-other-window'.
  (advice-add #'org-switch-to-buffer-other-window :override #'switch-to-buffer-other-window)

  ;; Rules.
  (setq shackle-default-size 0.4
        shackle-default-alignment 'below
        shackle-default-rule nil
        shackle-rules
        '(("*Help*" :select t :size 0.3 :align 'below :autoclose t)
          ("*Apropos*" :select t :size 0.3 :align 'below :autoclose t)
          ("*compilation*" :select t :size 0.3 :align 'below :autoclose t)
          ("*Completions*" :size 0.3 :align 'below :autoclose t)
          ("*Pp Eval Output*" :size 15 :align 'below :autoclose t)
          ("*ert*" :align 'below :autoclose t)
          ("*Backtrace*" :select t :size 15 :align 'below)
          ("*Warnings*" :size 0.3 :align 'below :autoclose t)
          ("*Messages*" :size 0.3 :align 'below :autoclose t)
          ("^\\*.*Shell Command.*\\*$" :regexp t :size 0.3 :align 'below :autoclose t)
          ("\\*[Wo]*Man.*\\*" :regexp t :select t :align 'below :autoclose t)
          ("*Calendar*" :select t :size 0.3 :align 'below)
          ("\\*ivy-occur .*\\*" :regexp t :size 0.4 :select t :align 'below)
          (" *undo-tree*" :select t)
          ("*Paradox Report*" :size 0.3 :align 'below :autoclose t)
          ("*quickrun*" :select t :size 15 :align 'below)
          ("*tldr*" :align 'below :autoclose t)
          ("*Youdao Dictionary*" :size 0.3 :align 'below :autoclose t)
          ("*Finder*" :select t :size 0.3 :align 'below :autoclose t)
          ("^\\*elfeed-entry" :regexp t :size 0.7 :align 'below :autoclose t)
          ("*lsp-help*" :size 0.3 :align 'below :autoclose t)
          ("*lsp session*" :size 0.4 :align 'below :autoclose t)
          (" *Org todo*" :select t :size 4 :align 'below :autoclose t)
          ("*Org Dashboard*" :select t :size 0.4 :align 'below :autoclose t)
          ("^\\*macro expansion\\**" :regexp t :size 0.4 :align 'below)
          (" *Install vterm" :size 0.3 :align 'below)

          (ag-mode :select t :align 'below)
          (grep-mode :select t :align 'below)
          (pt-mode :select t :align 'below)
          (rg-mode :select t :align 'below)

          (flycheck-error-list-mode :select t :size 0.3 :align 'below :autoclose t)
          (flymake-diagnostics-buffer-mode :select t :size 0.3 :align 'below :autoclose t)

          (Buffer-menu-mode :select t :size 20 :align 'below :autoclose t)
          (comint-mode :align 'below)
          (helpful-mode :select t :size 0.3 :align 'below :autoclose t)
          (process-menu-mode :select t :size 0.3 :align 'below :autoclose t)
          (cargo-process-mode :select t :size 0.3 :align 'below :autoclose t)
          (list-environment-mode :select t :size 0.3 :align 'below :autoclose t)
          (profiler-report-mode :select t :size 0.5 :align 'below)
          (tabulated-list-mode :align 'below)))))

(defun icons-displayable-p ()
  "Return non-nil if `all-the-icons' is displayable."
  (and hurricane-icon
       (display-graphic-p)
       (require 'all-the-icons nil t)))

(defun hurricane-ui/init-pretty-hydra ()
  (use-package pretty-hydra
    :ensure t))

(defun hurricane-ui/init-activities ()
  (use-package activities
     :init
     (activities-mode)
     (activities-tabs-mode)
     ;; Prevent `edebug' default bindings from interfering.
     (setq edebug-inhibit-emacs-lisp-mode-bindings t)

     :bind
     (("C-x C-a C-n" . activities-new)
      ("C-x C-a C-d" . activities-define)
      ("C-x C-a C-a" . activities-resume)
      ("C-x C-a C-s" . activities-suspend)
      ("C-x C-a C-k" . activities-kill)
      ("C-x C-a RET" . activities-switch)
      ("C-x C-a b" . activities-switch-buffer)
      ("C-x C-a g" . activities-revert)
      ("C-x C-a l" . activities-list))))

(defun hurricane-ui/init-org-tidy ()
  (use-package org-tidy
    :ensure t
    :hook
    (org-mode . org-tidy-mode)))
