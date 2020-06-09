(defconst hurricane-ui-packages
  '(
    (doom-themes :location (recipe :fetcher
                            github :repo "hlissner/emacs-doom-themes"))
    (doom-modeline :location (recipe :fetcher
                              github :repo "seagle0128/doom-modeline"))
    (hide-mode-line :location (recipe :fetcher
                               github :repo "hlissner/emacs-hide-mode-line"))
    pangu-spacing
    (awesome-tab :location (recipe :fetcher
                            github :repo "manateelazycat/awesome-tab"))
    (shackle :location (recipe :fetcher
                        github :repo "wasamasa/shackle"))
    all-the-icons-dired
    (ivy-posframe :location (recipe :fetcher
                             github :repo "tumashu/ivy-posframe"))
    ))

;; {{
;; @see: https://github.com/hlissner/emacs-doom-themes
(defun hurricane-ui/init-doom-themes ()
  (use-package doom-themes
    :init
    (load-theme 'doom-nord t)
    :custom
    (doom-themes-enable-italic t)
    (doom-themes-enable-bold t)
    :config
    (doom-themes-neotree-config)
    (doom-themes-org-config)
    ;; Enable flashing mode-line on errors
    (doom-themes-visual-bell-config)
    (set-face-attribute 'doom-visual-bell nil
                        :background (face-foreground 'error)
                        :foreground (face-background 'default)
                        :inverse-video nil)
    ))
;; }}

(defun hurricane-ui/pre-init-doom-modeline ()
  (spacemacs/add-to-hooks 'doom-modeline-env-update-python '(pyvenv-post-activate-hooks
                                                             pyvenv-post-deactivate-hooks
                                                             venv-postactivate-hook
                                                             venv-postdeactivate-hook)))

;; {{
;; @see: https://github.com/hlissner/emacs-hide-mode-line
(defun hurricane-ui/init-hide-mode-line ()
  (use-package hide-mode-line
    :hook
    ((neotree-mode
      imenu-list-minor-mode
      minimap-mode
      spacemacs-buffer-mode
      ibuffer-mode
      help-mode
      deft-text-mode
      ) . hide-mode-line-mode)))
;; }}

(defun hurricane-ui/post-init-pangu-spacing ()
  (progn
    (setq pangu-spacing-include-regexp
          (rx (group-n 1 (in "a-zA-Z0-9,.!?])%#@&1234567890,;\":\`"))
          (group-n 2 (category chinse-two-byte))))
    ;; Add toggle options.
    (spacemacs|add-toggle toggle-pangu-spaceing
      :status pangu-spacing-mode
      :on (global-pangu-spacing-mode)
      :off (global-pangu-spacing-mode -1)
      :documentation "Toggle pangu spacing mode."
      :evil-leader "ots")
    (add-hook 'org-mode-hook
              '(lambda ()
                  (set (make-local-variable 'pangu-spacing-real-insert-separtor) t)
                  (spacemacs/toggle-toggle-pangu-spaceing-on)))))

;; {{
;; @see: https://github.com/manateelazycat/awesome-tab/blob/master/README.md
(defun hurricane/define-evil-normal-keybinding (key def &rest bindings)
  "Binding keys and func for normal state map and motion state map."
  (while key
    (define-key evil-normal-state-map (kbd key) def)
    (define-key evil-motion-state-map (kbd key) def)
    (setq key (pop bindings) def (pop bindings))))

(defun hurricane-ui/init-awesome-tab ()
  (use-package awesome-tab
    :load-path "~/.emacs.d/elpa/awesome-tab"
    :commands (awesome-tab-mode)
    :defer t
    :init
    (progn
      (spacemacs|define-transient-state awesometab
        :title "Awesome-tab Transient State"
        :doc "
 Fast Move^^^^           Tab^^^^                   Window^^           Search^^      Misc^^
 ───────^^^^───────────  ─────^^^^───────────────  ─────^^──────────  ──────^^────  ─────^^───────────────────────
 [_p_/_n_] switch group  [_C-a_/_C-e_] first/last  [_-_] split below  [_b_] buffer  [_C-k_] kill buffer
 [_h_/_l_] switch tab    [_C-j_]^^ ace jump        [_v_] split right  [_g_] group   [_C-S-k_] kill others in group
 [_H_/_L_] switch other  [_C-h_/_C-l_] move        [_D_] delete       ^^            [_q_] quit
"
        :on-enter (awesome-tab-mode t)
        :on-exit (awesome-tab-mode -1)
        :bindings
        ;; Fast Move.
        ("p" awesome-tab-backward-group)
        ("n" awesome-tab-forward-group)
        ("h" awesome-tab-backward-tab)
        ("l" awesome-tab-forward-tab)
        ("H" awesome-tab-forward-tab-other-window)
        ("L" awesome-tab-backward-tab-other-window)
        ;; Tab.
        ("C-a" awesome-tab-select-beg-tab)
        ("C-e" awesome-tab-select-end-tab)
        ("C-j" awesome-tab-ace-jump)
        ("C-h" awesome-tab-move-current-tab-to-left)
        ("C-l" awesome-tab-move-current-tab-to-right)
        ;; Window.
        ("-" split-window-below)
        ("v" split-window-right)
        ("D" ace-delete-window)
        ;; Search.
        ("b" ivy-switch-buffer)
        ("g" awesome-tab-counsel-switch-group)
        ;; Misc.
        ("C-k" kill-current-buffer)
        ("C-S-k" awesome-tab-kill-other-buffers-in-current-group)
        ("q" nil :exit t))
      (hurricane/define-evil-normal-keybinding "C-t" 'spacemacs/awesometab-transient-state/body))
    :config
    (setq awesome-tab-style "bar")
    ))
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

(defun hurricane-ui/init-all-the-icons-dired ()
 (use-package all-the-icons-dired
    :diminish
    :if (icons-displayable-p)
    :hook (dired-mode . all-the-icons-dired-mode)
    :config
    ;; FIXME: Refresh after creating or renaming the files/directories.
    ;; @see https://github.com/jtbm37/all-the-icons-dired/issues/34.
    (with-no-warnings
      (advice-add 'dired-do-create-files :around #'all-the-icons-dired--refresh-advice)
      (advice-add 'dired-create-directory :around #'all-the-icons-dired--refresh-advice))

    (with-no-warnings
      (defun my-all-the-icons-dired--refresh ()
        "Display the icons of files in a dired buffer."
        (all-the-icons-dired--remove-all-overlays)
        ;; NOTE: don't display icons it too many items
        (if (<= (count-lines (point-min) (point-max)) 1000)
            (save-excursion
              (goto-char (point-min))
              (while (not (eobp))
                (when (dired-move-to-filename nil)
                  (let ((file (file-local-name (dired-get-filename 'relative 'noerror))))
                    (when file
                      (let ((icon (if (file-directory-p file)
                                      (all-the-icons-icon-for-dir file
                                                                  :face 'all-the-icons-dired-dir-face
                                                                  :height 0.9
                                                                  :v-adjust all-the-icons-dired-v-adjust)
                                    (all-the-icons-icon-for-file file :height 0.9 :v-adjust all-the-icons-dired-v-adjust))))
                        (if (member file '("." ".."))
                            (all-the-icons-dired--add-overlay (point) "  \t")
                          (all-the-icons-dired--add-overlay (point) (concat icon "\t")))))))
                (forward-line 1)))
          (message "Not display icons because of too many items.")))
      (advice-add #'all-the-icons-dired--refresh :override #'my-all-the-icons-dired--refresh))))

(defun hurricane-ui/init-ivy-posframe ()
  (use-package ivy-posframe
    :ensure t
    :after (ivy)
    :config
    (setq ivy-posframe-display-functions-alist
        '((swiper . ivy-posframe-display-at-frame-center)
          (complete-symbol . ivy-posframe-display-at-point)
          (counsel-M-x . ivy-posframe-display-at-frame-center)
          (counsel-find-file . ivy-posframe-display-at-frame-center)
          (ivy-switch-buffer . ivy-posframe-display-at-frame-center)
          (t . ivy-posframe-display-at-frame-center)))
    (ivy-posframe-enable)))
