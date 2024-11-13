(defconst hurricane-ui-packages
  '(
    ;; (doom-themes :location (recipe :fetcher github
    ;;                                :repo "hlissner/emacs-doom-themes"))
    ;; (hide-mode-line :location (recipe :fetcher github
    ;;                                   :repo "hlissner/emacs-hide-mode-line"))
    pangu-spacing
    pretty-hydra
    (activities :location (recipe :fetcher github
                                  :repo "alphapapa/activities.el"))
    (popper :location (recipe :fetcher github
                              :repo "karthink/popper"))
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
    :hook
    ((neotree-mode
      imenu-list-minor-mode
      minimap-mode
      ibuffer-mode
      help-mode)
     . hide-mode-line-mode)))
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
    ("C-x C-a C-n" . activities-new)
    ("C-x C-a C-d" . activities-define)
    ("C-x C-a C-a" . activities-resume)
    ("C-x C-a C-s" . activities-suspend)
    ("C-x C-a C-k" . activities-kill)
    ("C-x C-a RET" . activities-switch)
    ("C-x C-a b" . activities-switch-buffer)
    ("C-x C-a g" . activities-revert)
    ("C-x C-a l" . activities-list)))

(defun hurricane-ui/init-popper ()
  (use-package popper
    :custom
    (popper-group-function #'popper-group-by-directory)
    (popper-echo-dispatch-actions t)
    :bind (:map popper-mode-map
                ("C-h z"       . popper-toggle)
                ("C-M-<tab>"     . popper-cycle))
    :hook (emacs-startup . popper-echo-mode)
    :init
    (setq popper-reference-buffers
          '("\\*Messages\\*$"
            "Output\\*$" "\\*Pp Eval Output\\*$"
            "^\\*eldoc.*\\*$"
            "\\*Compile-Log\\*$"
            "\\*Completions\\*$"
            "\\*Warnings\\*$"
            "\\*Async Shell Command\\*$"
            "\\*Apropos\\*$"
            "\\*Backtrace\\*$"
            "\\*Calendar\\*$"
            "\\*Fd\\*$" "\\*Find\\*$" "\\*Finder\\*$"
            "\\*Kill Ring\\*$"
            "\\*Embark \\(Collect\\|Live\\):.*\\*$"

            bookmark-bmenu-mode
            comint-mode
            compilation-mode
            help-mode helpful-mode
            tabulated-list-mode
            Buffer-menu-mode

            flymake-diagnostics-buffer-mode
            flycheck-error-list-mode flycheck-verify-mode

            gnus-article-mode devdocs-mode
            grep-mode occur-mode rg-mode deadgrep-mode ag-mode pt-mode
            youdao-dictionary-mode osx-dictionary-mode fanyi-mode
            "^\\*gt-result\\*$" "^\\*gt-log\\*$"

            "^\\*Process List\\*$" process-menu-mode
            list-environment-mode cargo-process-mode

            "^\\*.*eshell.*\\*.*$"
            "^\\*.*shell.*\\*.*$"
            "^\\*.*terminal.*\\*.*$"
            "^\\*.*vterm[inal]*.*\\*.*$"

            "\\*DAP Templates\\*$" dap-server-log-mode
            "\\*ELP Profiling Restuls\\*" profiler-report-mode
            "\\*Paradox Report\\*$" "\\*package update results\\*$" "\\*Package-Lint\\*$"
            "\\*[Wo]*Man.*\\*$"
            "\\*ert\\*$" overseer-buffer-mode
            "\\*gud-debug\\*$"
            "\\*lsp-help\\*$" "\\*lsp session\\*$"
            "\\*quickrun\\*$"
            "\\*tldr\\*$"
            "\\*vc-.*\\**"
            "\\*diff-hl\\**"
            "^\\*macro expansion\\**"

            "\\*Agenda Commands\\*" "\\*Org Select\\*" "\\*Capture\\*" "^CAPTURE-.*\\.org*"
            "\\*Gofmt Errors\\*$" "\\*Go Test\\*$" godoc-mode
            "\\*docker-.+\\*"
            "\\*prolog\\*" inferior-python-mode inf-ruby-mode swift-repl-mode
            "\\*rustfmt\\*$" rustic-compilation-mode rustic-cargo-clippy-mode
            rustic-cargo-outdated-mode rustic-cargo-run-mode rustic-cargo-test-mode
            "\\*Dogears List\\*$" dogears-list-mode
            "\\*DeepSeek\\*$"))
    :config
    (with-no-warnings
      (defun hurricane//popper-fit-window-height (win)
        "Determine the height of popup window WIN by fitting it to the buffer's content."
        (fit-window-to-buffer
         win
         (floor (frame-height) 3)
         (floor (frame-height) 3)))
      (setq popper-window-height #'hurricane//popper-fit-window-height)

      (defun popper-close-window-advice (&rest _)
        "Close popper window via `C-g'."
        ;; `C-g' can deactivate region
        (when (and ;(called-interactively-p 'interactive)
               (not (region-active-p))
               popper-open-popup-alist)
          (when-let* ((window (caar popper-open-popup-alist))
                      (buffer (cdar popper-open-popup-alist)))
            (when (and (with-current-buffer buffer
                         (not (derived-mode-p 'eshell-mode
                                              'shell-mode
                                              'term-mode
                                              'vterm-mode)))
                       (window-live-p window))
              (delete-window window)))))
      (advice-add #'keyboard-quit :before #'popper-close-window-advice))))
