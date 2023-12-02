(setq hurricane-misc-packages
      '(
        helm-github-stars
        helm-ag
        expand-region
        projectile
        prodigy
        find-file-in-project
        multiple-cursors
        visual-regexp
        visual-regexp-steroids
        command-log
        evil
        discover-my-major
        ace-window
        avy
        tiny
        flyspell-correct
        peep-dired
        markdown-mode
        swiper
        magit
        git-messenger
        gist
        hydra
        wrap-region
        ranger
        golden-ratio
        (highlight-global :location (recipe :fetcher github :repo "glen-dai/highlight-global"))
        symbol-overlay
        browse-at-remote
        (shell-mode :location local)
        pandoc-mode
        (autoinsert :location built-in)
        use-package-ensure-system-package
        rime
        atomic-chrome
        dired-rsync
        (with-proxy :location (recipe :fetcher github :repo "twlz0ne/with-proxy.el"))
        (emacsql :location (recipe :fetcher github :repo "skeeto/emacsql"))
        (mybigword :location (recipe :fetcher github
                                     :repo "redguardtoo/mybigword"
                                     :files ("*.*")))
        emacs-everywhere
        command-log-mode
        fasd
        (auto-save :location (recipe :fetcher github :repo "manateelazycat/auto-save"))
        eaf
        (dupan :location (recipe :fetcher github :repo "lorniu/emacs-baidupan"))
        (blink-search :location (recipe :fetcher github
                                        :repo "manateelazycat/blink-search"
                                        :files ("*.*" "backend" "core" "icons")))
        (plisty :location local)
        (org-tags-filter :location local)
        (org-roam-dblocks :location local)
        (subed :location (recipe :fetcher github
                                 :repo "sachac/subed"
                                 :files ("*.*" "subed")))
        (youtube-sub-extractor :location (recipe :fetcher github
                                       :repo "agzam/youtube-sub-extractor"))
        (websocket :location (recipe :fetcher github
                                           :repo "ahyatt/emacs-websocket"))
        (websocket-bridge :location (recipe :fetcher github
                                            :repo "ginqi7/websocket-bridge"))
        (dictionary-overlay :location (recipe :fetcher github
                                              :repo "ginqi7/dictionary-overlay"
                                              :files ("*.*" "resources")))
        (ffmpeg-utils :location (recipe :fetcher github
                                        :repo "czqhurricnae/ffmpeg-utils"))
        ;; engine-mode
        (emacs-azure-tts :location (recipe :fetcher github
                                     :repo "czqhurricnae/emacs-azure-tts"
                                     :files ("*.*")))
        ;; (emacs-azure-tts :location local)
        (reverso :location (recipe :fetcher github
                                   :repo "SqrtMinusOne/reverso.el"))
        nov
        (goldendict-ng :location (recipe :fetcher github
                                         :repo "benthamite/goldendict-ng"))
        ))

(defconst sys/macp
  (eq system-type 'darwin)
  "Are we running on a Mac system?")

(defun hurricane-misc/init-browse-at-remote ()
  (use-package browse-at-remote
    :defer t
    :init
    (spacemacs/set-leader-keys "gho" 'browse-at-remote)))

(defun hurricane-misc/init-highlight-global ()
  (use-package highlight-global
    :init
      (spacemacs/set-leader-keys "hh" 'highlight-frame-toggle)
      (spacemacs/set-leader-keys "hc" 'clear-highlight-frame)
      (setq-default highlight-faces
        '(('hi-red-b . 0)
          ('hi-yellow . 0)
          ('hi-pink . 0)
          ('hi-blue-b . 0)))))

(defun hurricane-misc/post-init-symbol-overlay ()
  (with-eval-after-load 'symbol-overlay
    (progn
      (spacemacs/transient-state-register-add-bindings 'symbol-overlay
        '((">" symbol-overlay-jump-last)
          ("<" symbol-overlay-jump-first))))))

(defun hurricane-misc/post-init-golden-ratio ()
  (with-eval-after-load 'golden-ratio
    (dolist (mode '("dired-mode" "occur-mode"))
      (add-to-list 'golden-ratio-exclude-modes mode))
    (dolist (n '("COMMIT_EDITMSG"))
      (add-to-list 'golden-ratio-exclude-buffer-names n))))

;; {{
;; @see: https://emacs-china.org/t/ranger-golden-ratio/964/2
(defun hurricane-misc/post-init-ranger ()
  (defun my-ranger ()
    (interactive)
    (if golden-ratio-mode
        (progn
          (golden-ratio-mode -1)
          (ranger)
          (setq golden-ratio-previous-enable t))
      (progn
        (ranger)
        (setq golden-ratio-previous-enable nil))))

(defun my-quit-ranger ()
  (interactive)
  (if golden-ratio-previous-enable
      (progn
        (ranger-close)
        (golden-ratio-mode 1))
    (ranger-close)))

(with-eval-after-load 'ranger
  (progn
    (define-key ranger-normal-mode-map (kbd "q") 'my-quit-ranger)))

;; (spacemacs/set-leader-keys "atr" 'my-ranger)
)
;; }}

;; Copy from spacemacs `helm' layer.
(defun hurricane-misc/init-helm-ag ()
  (use-package helm-ag
    :init
    (progn
      (defun spacemacs//helm-do-ag-region-or-symbol (func &optional dir)
        "Search with `ag' with a default input."
        (require 'helm-ag)
        (cl-letf* (((symbol-value 'helm-ag-insert-at-point) 'symbol)
                   ;; Make thing-at-point choosing the active region first.
                   ((symbol-function 'this-fn) (symbol-function 'thing-at-point))
                   ((symbol-function 'thing-at-point)
                    (lambda (thing)
                      (let ((res (if (region-active-p)
                                     (buffer-substring-no-properties
                                      (region-beginning) (region-end))
                                   (this-fn thing))))
                        (when res (rxt-quote-pcre res))))))
          (funcall func dir)))

      (defun spacemacs//helm-do-search-find-tool (base tools default-inputp)
        "Create a cond form given a `tools' string list and evaluate it."
        (eval
         `(cond
           ,@(mapcar
              (lambda (x)
                `((executable-find ,x)
                  ',(let ((func
                           (intern
                            (format (if default-inputp
                                        "spacemacs/%s-%s-region-or-symbol"
                                      "spacemacs/%s-%s")
                                    base x))))
                      (if (fboundp func)
                          func
                        (intern (format "%s-%s"  base x))))))
              tools)
           (t 'helm-do-grep))))

      ;; Search in current file. ----------------------------------------------

      (defun spacemacs/helm-file-do-ag (&optional _)
        "Wrapper to execute `helm-ag-this-file.'"
        (interactive)
        (helm-ag-this-file))

      (defun spacemacs/helm-file-do-ag-region-or-symbol ()
        "Search in current file with `ag' using a default input."
        (interactive)
        (spacemacs//helm-do-ag-region-or-symbol 'spacemacs/helm-file-do-ag))

      (defun spacemacs/helm-file-smart-do-search (&optional default-inputp)
        "Search in current file using `dotspacemacs-search-tools'.
        Search for a search tool in the order provided by `dotspacemacs-search-tools'
        If `default-inputp' is non nil then the current region or symbol at point
        are used as default input."
        (interactive)
        (call-interactively
         (spacemacs//helm-do-search-find-tool "helm-file-do"
                                              dotspacemacs-search-tools
                                              default-inputp)))

      (defun spacemacs/helm-file-smart-do-search-region-or-symbol ()
        "Search in current file using `dotspacemacs-search-tools' with default input.
        Search for a search tool in the order provided by `dotspacemacs-search-tools'."
        (interactive)
        (spacemacs/helm-file-smart-do-search t))

      ;; Search in files. -----------------------------------------------------

      (defun spacemacs/helm-files-do-ag (&optional dir)
        "Search in files with `ag' using a default input."
        (interactive)
        (helm-do-ag dir))

      (defun spacemacs/helm-files-do-ag-region-or-symbol ()
        "Search in files with `ag' using a default input."
        (interactive)
        (spacemacs//helm-do-ag-region-or-symbol 'spacemacs/helm-files-do-ag))

      (defun spacemacs/helm-files-do-ack (&optional dir)
        "Search in files with `ack'."
        (interactive)
        (let ((helm-ag-base-command "ack --nocolor --nogroup"))
          (helm-do-ag dir)))

      (defun spacemacs/helm-files-do-ack-region-or-symbol ()
        "Search in files with `ack' using a default input."
        (interactive)
        (spacemacs//helm-do-ag-region-or-symbol 'spacemacs/helm-files-do-ack))

      (defun spacemacs/helm-files-do-pt (&optional dir)
        "Search in files with `pt'."
        (interactive)
        (let ((helm-ag-base-command "pt -e --nocolor --nogroup"))
          (helm-do-ag dir)))

      (defun spacemacs/helm-files-do-pt-region-or-symbol ()
        "Search in files with `pt' using a default input."
        (interactive)
        (spacemacs//helm-do-ag-region-or-symbol 'spacemacs/helm-files-do-pt))

      (defun spacemacs/helm-files-smart-do-search (&optional default-inputp)
        "Search in opened buffers using `dotspacemacs-search-tools'.
        Search for a search tool in the order provided by `dotspacemacs-search-tools'
        If `default-inputp' is non nil then the current region or symbol at point
        are used as default input."
        (interactive)
        (call-interactively
         (spacemacs//helm-do-search-find-tool "helm-files-do"
                                              dotspacemacs-search-tools
                                              default-inputp)))

      (defun spacemacs/helm-files-smart-do-search-region-or-symbol ()
        "Search in opened buffers using `dotspacemacs-search-tools'.
        with default input.
        Search for a search tool in the order provided by `dotspacemacs-search-tools'."
        (interactive)
        (spacemacs/helm-files-smart-do-search t))

      ;; Search in buffers. ---------------------------------------------------

      (defun spacemacs/helm-buffers-do-ag (&optional _)
        "Wrapper to execute `helm-ag-buffers.'"
        (interactive)
        (helm-do-ag-buffers))

      (defun spacemacs/helm-buffers-do-ag-region-or-symbol ()
        "Search in opened buffers with `ag' with a default input."
        (interactive)
        (spacemacs//helm-do-ag-region-or-symbol 'spacemacs/helm-buffers-do-ag))

      (defun spacemacs/helm-buffers-do-ack (&optional _)
        "Search in opened buffers with `ack'."
        (interactive)
        (let ((helm-ag-base-command "ack --nocolor --nogroup"))
          (helm-do-ag-buffers)))

      (defun spacemacs/helm-buffers-do-ack-region-or-symbol ()
        "Search in opened buffers with `ack' with a default input."
        (interactive)
        (spacemacs//helm-do-ag-region-or-symbol 'spacemacs/helm-buffers-do-ack))

      (defun spacemacs/helm-buffers-do-pt (&optional _)
        "Search in opened buffers with `pt'."
        (interactive)
        (let ((helm-ag-base-command "pt -e --nocolor --nogroup"))
          (helm-do-ag-buffers)))

      (defun spacemacs/helm-buffers-do-pt-region-or-symbol ()
        "Search in opened buffers with `pt' using a default input."
        (interactive)
        (spacemacs//helm-do-ag-region-or-symbol 'spacemacs/helm-buffers-do-pt))

      (defun spacemacs/helm-buffers-smart-do-search (&optional default-inputp)
        "Search in opened buffers using `dotspacemacs-search-tools'.
        Search for a search tool in the order provided by `dotspacemacs-search-tools'
        If `default-inputp' is non nil then the current region or symbol at point
        are used as default input."
        (interactive)
        (call-interactively
         (spacemacs//helm-do-search-find-tool "helm-buffers-do"
                                              dotspacemacs-search-tools
                                              default-inputp)))

      (defun spacemacs/helm-buffers-smart-do-search-region-or-symbol ()
        "Search in opened buffers using `dotspacemacs-search-tools' with default input.
        Search for a search tool in the order provided by `dotspacemacs-search-tools'."
        (interactive)
        (spacemacs/helm-buffers-smart-do-search t))

      ;; Search in project. ---------------------------------------------------

      (defun spacemacs/helm-project-do-ag ()
        "Search in current project with `ag'."
        (interactive)
        (let ((dir (projectile-project-root)))
          (if dir
              (helm-do-ag dir)
            (message "error: Not in a project."))))

      (defun spacemacs/helm-project-do-ag-region-or-symbol ()
        "Search in current project with `ag' using a default input."
        (interactive)
        (let ((dir (projectile-project-root)))
          (if dir
              (spacemacs//helm-do-ag-region-or-symbol 'helm-do-ag dir)
            (message "error: Not in a project."))))

      (defun spacemacs/helm-project-do-ack ()
        "Search in current project with `ack'."
        (interactive)
        (let ((dir (projectile-project-root)))
          (if dir
              (spacemacs/helm-files-do-ack dir)
            (message "error: Not in a project."))))

      (defun spacemacs/helm-project-do-ack-region-or-symbol ()
        "Search in current project with `ack' using a default input."
        (interactive)
        (let ((dir (projectile-project-root)))
          (if dir
              (spacemacs//helm-do-ag-region-or-symbol
               'spacemacs/helm-files-do-ack dir)
            (message "error: Not in a project."))))

      (defun spacemacs/helm-project-do-pt ()
        "Search in current project with `pt'."
        (interactive)
        (let ((dir (projectile-project-root)))
          (if dir
              (spacemacs/helm-files-do-pt dir)
            (message "error: Not in a project."))))

      (defun spacemacs/helm-project-do-pt-region-or-symbol ()
        "Search in current project with `pt' using a default input."
        (interactive)
        (let ((dir (projectile-project-root)))
          (if dir
              (spacemacs//helm-do-ag-region-or-symbol
               'spacemacs/helm-files-do-pt dir)
            (message "error: Not in a project."))))

      (defun spacemacs/helm-project-smart-do-search (&optional default-inputp)
        "Search in current project using `dotspacemacs-search-tools'.
        Search for a search tool in the order provided by `dotspacemacs-search-tools'
        If `default-inputp' is non nil then the current region or symbol at point
        are used as default input."
        (interactive)
        (let ((projectile-require-project-root nil))
          (call-interactively
           (spacemacs//helm-do-search-find-tool "helm-project-do"
                                                dotspacemacs-search-tools
                                                default-inputp))))

      (defun spacemacs/helm-project-smart-do-search-region-or-symbol ()
        "Search in current project using `dotspacemacs-search-tools' with
         default input.
         Search for a search tool in the order provided by `dotspacemacs-search-tools'."
        (interactive)
        (spacemacs/helm-project-smart-do-search t))

      ;; This overrides the default C-s action in helm-projectile-switch-project.
      ;; To search using ag/pt/whatever instead of just grep.
      (with-eval-after-load 'helm-projectile
        (defun spacemacs/helm-project-smart-do-search-in-dir (dir)
          (interactive)
          (let ((default-directory dir))
            (spacemacs/helm-project-smart-do-search)))
        (define-key helm-projectile-projects-map
          (kbd "C-s")
          (lambda ()
            (interactive)
            (helm-exit-and-execute-action
             'spacemacs/helm-project-smart-do-search-in-dir))))
      )
    :config
    (progn
      (advice-add 'helm-ag--save-results :after 'spacemacs//gne-init-helm-ag)
      (evil-define-key 'normal helm-ag-map "SPC" spacemacs-default-map)
      ;; Evilify the helm-grep buffer.
      (evilified-state-evilify-map helm-grep-mode-map
        :mode helm-grep-mode
        :bindings
        (kbd "RET") 'helm-grep-mode-jump-other-window
        (kbd "q") 'quit-window)
      (evilified-state-evilify-map helm-ag-mode-map
        :mode helm-ag-mode
        :bindings
        (kbd "RET") 'helm-ag-mode-jump-other-window
        (kbd "gr") 'helm-ag--update-save-results
        (kbd "q") 'quit-window))))

(defun hurricane-misc/post-init-hydra ()
  (progn
    (defhydra hydra-hotspots (:color blue)
      "Hotspots."
      ("g" helm-github-stars "helm github stars")
      ("r" hurricane/run-current-file "run current file"))

    (defhydra multiple-cursors-hydra (:hint nil)
      "
       ^Up^            ^Down^        ^Other^
       -----------------------------------------------
       [_p_]   Preview [_n_]   Next   [_l_] Edit lines
       [_P_]   Skip    [_N_]   Skip   [_a_] Mark all
       [_M-p_] Unmark  [_M-n_] Unmark [_r_] Mark by regexp
       ^ ^             ^ ^ [_q_] Quit
       "
      ("l" mc/edit-lines :exit t)
      ("a" mc/mark-all-like-this :exit t)
      ("n" mc/mark-next-like-this)
      ("N" mc/skip-to-next-like-this)
      ("M-n" mc/unmark-next-like-this)
      ("p" mc/mark-previous-like-this)
      ("P" mc/skip-to-previous-like-this)
      ("M-p" mc/unmark-previous-like-this)
      ("r" mc/mark-all-in-region-regexp :exit t)
      ("q" nil))

    (defhydra
      hydra-apropos (:color blue)
      "Apropos."
      ("a" apropos "apropos")
      ("c" apropos-command "cmd")
      ("d" apropos-documentation "doc")
      ("e" apropos-value "val")
      ("l" apropos-library "lib")
      ("o" apropos-user-option "option")
      ("u" apropos-user-option "option")
      ("v" apropos-variable "var")
      ("i" info-apropos "info")
      ("t" tags-apropos "tags")
      ("z" hydra-customize-apropos/body "customize"))

    (defhydra
      hydra-customize-apropos (:color blue)
      "Apropos (customize)."
      ("a" customize-apropos "apropos")
      ("f" customize-apropos-faces "faces")
      ("g" customize-apropos-groups "groups")
      ("o" customize-apropos-options "options"))

    ;; (define-key global-map (kbd "<f1>") 'hydra-hotspots/body)
    (spacemacs/set-leader-keys "oo" 'hydra-hotspots/body)
    ;; (bind-key*  "<f4>" 'hydra-apropos/body)
    (spacemacs/set-leader-keys "oh" 'hydra-apropos/body)

    ))

(defun hurricane-misc/post-init-gist ()
  (use-package gist
    :defer t
    :init
    (setq gist-list-format
          '((files "File" 30 nil "%s")
            (id "Id" 10 nil identity)
            (created "Created" 20 nil "%D %R")
            (visibility "Visibility" 10 nil
                        (lambda
                          (public)
                          (or
                           (and public "public")
                           "private")))
            (description "Description" 0 nil identity)))
    :config
    (progn
      (spacemacs|define-transient-state gist-list-mode
        :title "Gist-mode Transient State"
        :bindings
        ("k" gist-kill-current "delete gist")
        ("e" gist-edit-current-description "edit gist title")
        ("+" gist-add-buffer "add a file")
        ("-" gist-remove-file "delete a file")
        ("y" gist-print-current-url "print url")
        ("b" gist-browse-current-url "browse gist in browser")
        ("*" gist-star "star gist")
        ("^" gist-unstar "unstar gist")
        ("f" gist-fork "fork gist")
        ("q" nil "quit" :exit t)
        ("<escape>" nil nil :exit t))
      (spacemacs/set-leader-keys-for-major-mode 'gist-list-mode
        "." 'spacemacs/gist-list-mode-transient-state/body))
    ))

;; Preview files in dired.
(defun hurricane-misc/init-peep-dired ()
  (use-package peep-dired
    :defer t
    :commands (peep-dired-next-file
               peep-dired-prev-file)
    :bind (:map dired-mode-map
                ("P" . peep-dired))))

(defun hurricane-misc/post-init-flyspell-correct ()
  (progn
    (with-eval-after-load 'flyspell
      (define-key flyspell-mode-map (kbd "C-;") 'flyspell-correct-previous-word-generic))
    (setq flyspell-correct-interface 'flyspell-correct-ivy)))

(defun hurricane-misc/post-init-smartparens ()
  (progn
    (smartparens-global-mode t)
    (global-set-key (kbd "C-(") 'hurricane/wrap-sexp-with-new-round-parens)
    ;; 写 lisp 时不成对补全 "'" 和 "`"。
    (sp-local-pair 'emacs-lisp-mode "'" nil :actions nil)
    (sp-local-pair 'lisp-interaction-mode "'" nil :actions nil)
    (setq sp-highlight-pair-overlay t)
    (evil-define-key 'normal sp-keymap
      (kbd ")>") 'sp-forward-slurp-sexp
      (kbd ")<") 'sp-forward-barf-sexp
      (kbd "(>") 'sp-backward-barf-sexp
      (kbd "(<") 'sp-backward-slurp-sexp)))

(defun hurricane-misc/init-tiny ()
  (use-package tiny
    :defer t
    :init
    (spacemacs/set-leader-keys "oe" 'tiny-expand)))

(defun hurricane-misc/init-helm-github-stars ()
  (use-package helm-github-stars
    :defer t
    :commands (helm-github-stars)
    :init
    (setq helm-github-stars-username "czqhurricnae")))

(defun hurricane-misc/post-init-command-log ()
  (with-eval-after-load 'global-command-log-mode
    (setq clm/log-command-exceptions* (append clm/log-command-exceptions*
                                              '(evil-next-visual-line
                                                evil-previous-visual-line)))))

(defun hurricane-misc/init-litable ()
  (use-package litable
    :defer t))

(defun hurricane-misc/init-osx-dictionary ()
  (use-package osx-dictionary
    :init
    (progn
      (evilified-state-evilify-map osx-dictionary-mode-map
        :mode osx-dictionary-mode)
      (setq osx-dictionary-use-chinese-text-segmentation t))))

(defun hurricane-misc/post-init-avy ()
  (progn
    (global-set-key (kbd "C-M-'") 'avy-goto-char-2)
    (global-set-key (kbd "M-'") 'avy-goto-char)))

(defun hurricane-misc/post-init-ace-window ()
  (global-set-key (kbd "C-x C-o") #'ace-window))

(defun hurricane-misc/init-discover-my-major ()
  (use-package discover-my-major
    :defer t
    :init
    (progn
      (spacemacs/set-leader-keys (kbd "mhm") #'discover-my-major))))

(defun hurricane-misc/post-init-evil ()
  (progn
    (require 'cl)
    (setcdr evil-insert-state-map nil)
    (define-key evil-insert-state-map [escape] 'evil-normal-state)

    ;; Disable highlight when use swiper or evil ex search, this option won't effect evil-ex-search-next command.
    (setq-default evil-ex-search-persistent-highlight nil)

    (push "TAGS" spacemacs-useless-buffers-regexp)

    (adjust-major-mode-keymap-with-evil "git-timemachine")
    (adjust-major-mode-keymap-with-evil "tabulated-list")

    (define-key evil-visual-state-map "p" 'hurricane//evil-paste-after-from-0)
    (define-key evil-insert-state-map (kbd "C-r") 'evil-paste-from-register)

    ;; Change evil initial mode state.
    (loop for (mode . state) in
          '((shell-mode . normal))
          do (evil-set-initial-state mode state))

    ;; Mimic "nzz" behaviou in vim.
    (defadvice evil-search-next (after advice-for-evil-search-next activate)
      (evil-scroll-line-to-center (line-number-at-pos)))

    (defadvice evil-search-previous (after advice-for-evil-search-previous activate)
      (evil-scroll-line-to-center (line-number-at-pos)))

    (define-key evil-normal-state-map (kbd ",/") 'evilnc-comment-or-uncomment-lines)

    (defun hurricane//evil-yank ()
      (interactive)
      (save-excursion
        (call-interactively 'evil-yank))
      (backward-char))

    (define-key evil-visual-state-map (kbd "y") 'hurricane//evil-yank)

    (define-key evil-normal-state-map
      (kbd "Y") 'hurricane/yank-to-end-of-line)

    (define-key evil-normal-state-map (kbd "M-y") 'counsel-yank-pop)

    ;; {{
    ;; Unbinding Evil's mappings.
    ;; @see: https://stackoverflow.com/questions/24988406/unbinding-evils-c-w-mappings
    (eval-after-load "evil-maps"
      (dolist (map '(evil-motion-state-map
                     evil-insert-state-map
                     evil-emacs-state-map))
        (define-key (eval map) "\C-f" nil)))

    (eval-after-load "evil-maps"
      (dolist (map '(evil-motion-state-map
                     evil-insert-state-map
                     evil-emacs-state-map))
        (define-key (eval map) "\C-b" nil)))

    (eval-after-load "evil-maps"
      (dolist (map '(evil-motion-state-map
                     evil-insert-state-map
                     evil-emacs-state-map))
        (define-key (eval map) "\C-e" nil)))

    (eval-after-load "evil-maps"
      (dolist (map '(evil-motion-state-map
                     evil-insert-state-map
                     evil-emacs-state-map))
        (define-key (eval map) "\C-w" nil)))
    ;; }}

    (define-key evil-ex-completion-map "\C-a" 'move-beginning-of-line)
    (define-key evil-ex-completion-map "\C-e" 'move-end-of-line)
    (define-key evil-ex-completion-map "\C-b" 'backward-char)
    (define-key evil-ex-completion-map "\C-k" 'kill-line)
    (define-key minibuffer-local-map (kbd "C-w") 'evil-delete-backward-word)

    (define-key evil-visual-state-map (kbd ",/") 'evilnc-comment-or-uncomment-lines)
    (define-key evil-visual-state-map (kbd "C-r") 'hurricane//evil-quick-replace)
    (define-key evil-visual-state-map (kbd "mn") 'mc/mark-next-like-this)
    (define-key evil-visual-state-map (kbd "mp") 'mc/mark-previous-like-this)
    (define-key evil-visual-state-map (kbd "ma") 'mc/mark-all-like-this)
    (define-key evil-visual-state-map (kbd "mf") 'mc/mark-all-like-this-in-defun)

    ;; In spacemacs, we always use evilify miscro state.
    (evil-add-hjkl-bindings package-menu-mode-map 'emacs)

    ;; Don't move back the cursor one position when exiting insert mode.
    (setq evil-move-cursor-back nil)

    (evil-define-key 'emacs term-raw-map (kbd "C-w")
      'evil-delete-backward-word)

    (setq evil-normal-state-tag   (propertize "[N]" 'face '((:background "DarkGoldenrod2" :foreground "black")))
          evil-emacs-state-tag    (propertize "[E]" 'face '((:background "SkyBlue2" :foreground "black")))
          evil-insert-state-tag   (propertize "[I]" 'face '((:background "chartreuse3") :foreground "white"))
          evil-motion-state-tag   (propertize "[M]" 'face '((:background "plum3") :foreground "white"))
          evil-visual-state-tag   (propertize "[V]" 'face '((:background "gray" :foreground "black")))
          evil-operator-state-tag (propertize "[O]" 'face '((:background "purple"))))
    (setq evil-insert-state-cursor '("chartreuse3" box))
    (define-key evil-insert-state-map (kbd "C-z") 'evil-emacs-state)
    ))

(defun hurricane-misc/init-visual-regexp ()
  (use-package visual-regexp
    :commands (vr/replace vr/query-replace)))

(defun hurricane-misc/init-visual-regexp-steroids ()
  (use-package visual-regexp-steroids
    :commands (vr/select-replace vr/select-query-replace)
    :init
    (progn
      (define-key global-map (kbd "C-c r") 'vr/replace)
      (define-key global-map (kbd "C-c q") 'vr/query-replace))))

(defun hurricane-misc/init-multiple-cursors ()
  (use-package multiple-cursors
    :init
    (progn
      (bind-key* "s-." 'mc/mark-next-like-this)
      (bind-key* "s-," 'mc/mark-previous-like-this)
      (bind-key* "s->" 'mc/unmark-next-like-this)
      (bind-key* "s-<" 'mc/unmark-previous-like-this)
      (bind-key* "s-=" 'mc/skip-to-next-like-this)
      (bind-key* "s--" 'mc/skip-to-previous-like-this)
      (bind-key* "s-`" 'mc/mark-all-like-this)

      ;; @see: http://endlessparentheses.com/multiple-cursors-keybinds.html?source=rss
      (define-prefix-command 'endless/mc-map)
      ;; `C-x m' is usually `compose-mail'. Bind it to something.
      ;; Else if you use this command.
      (define-key ctl-x-map "m" 'endless/mc-map)

      ;; Really really nice.
      (define-key endless/mc-map "i" #'mc/insert-numbers)
      (define-key endless/mc-map "h" #'mc-hide-unmatched-lines-mode)
      (define-key endless/mc-map "a" #'mc/mark-all-like-this)
      (define-key endless/mc-map "t" #'set-rectangular-region-anchor)

      ;; Occasionally useful.
      (define-key endless/mc-map "d" #'mc/mark-all-dwim)
      (define-key endless/mc-map "f" #'mc/mark-all-symbols-like-this-in-defun)
      (define-key endless/mc-map "r" #'mc/reverse-regions)
      (define-key endless/mc-map "s" #'mc/sort-regions)
      (define-key endless/mc-map "l" #'mc/edit-lines)
      (define-key endless/mc-map "\C-a" #'mc/edit-beginnings-of-lines)
      (define-key endless/mc-map "\C-e" #'mc/edit-ends-of-lines)
      )
    :config
    (setq mc/cmds-to-run-once
          '(
            counsel-M-x
            hurricane//my-mc-mark-next-like-this))
    (setq mc/cmds-to-run-for-all
          '(
            electric-newline-and-maybe-indent
            hungry-delete-backward
            spacemacs/backward-kill-word-or-region
            spacemacs/smart-move-beginning-of-line
            evil-substitute
            lispy-move-beginning-of-line
            lispy-move-end-of-line
            lispy-space
            lispy-delete-backward
            evil-exit-visual-state
            evil-backward-char
            evil-delete-char
            evil-escape-emacs-state
            evil-escape-insert-state
            mwim-beginning-of-code-or-line
            mwim-end-of-line-or-code
            evil-exit-emacs-state
            evil-previous-visual-line
            evil-next-visual-line
            evil-forward-char
            evil-insert
            evil-next-line
            evil-normal-state
            evil-previous-line
            evil-append
            evil-append-line
            forward-sentence
            kill-sentence
            org-self-insert-command
            sp-backward-delete-char
            sp-delete-char
            sp-remove-active-pair-overlay
            orgtbl-hijacker-command-109))
    ))

(defun hurricane-misc/post-init-evil-escape ()
  (setq evil-escape-delay 0.2))

(defun hurricane-misc/init-find-file-in-project ()
  (use-package find-file-in-project
    :defer t
    :config
    (progn
      ;; If you use other VCS (subversion, for example), enable the following option.
      ;; (setq ffip-project-file ".svn")
      ;; In macOS X, the search file command is `cmd-p'.
      ;; For this project, I'm only interested certain types of files.
      (setq-default ffip-patterns '("*.html" "*.js" "*.css" "*.java" "*.xml" "*.cpp" "*.h" "*.c" "*.mm" "*.m" "*.el"))
      ;; If the full path of current file is under `subproject1' or `subproject2'.
      ;; OR if I'm reading my personal issue track document.
      (defadvice find-file-in-project (before my-find-file-in-project activate compile)
        (when (ffip-current-full-filename-match-pattern-p "\\(/fireball\\)")
          ;; Set the root directory into `~/projs/project_dir'.
          (setq-local ffip-project-root "~/Github/fireball")
          ;; Well, I'm not interested in concatenated big js file or file in dist.
          (setq-local ffip-find-options "-not -size +64k -not -iwholename '*/bin/*'")
          ;; Do not search files in below directories, the default value is better.
          (dolist (item '("*/docs/html/*" "*.meta" "*/cocos2d-x/*" "*.asset" "*/visual-tests/res/*"))
            (push item  ffip-prune-patterns)))
        (when (ffip-current-full-filename-match-pattern-p "\\(/cocos2d-x\\)")
          ;; Set the root directory into `~/projs/project_dir'
          (setq-local ffip-project-root "~/cocos2d-x")
          ;; Well, I'm not interested in concatenated big js file or file in dist.
          (setq-local ffip-find-options "-not -size +64k -not -iwholename '*/bin/*'")
          ;; Do not search files in below directories, the default value is better.
          ;; (setq-default ffip-prune-patterns '(".git" ".hg" "*.svn" "node_modules" "bower_components" "obj"))
          ))
      (ad-activate 'find-file-in-project))))

(defun hurricane-misc/post-init-projectile ()
  (progn
    (with-eval-after-load 'projectile
      (progn
        (setq projectile-completion-system 'ivy)
        (add-to-list 'projectile-other-file-alist '("html" "js"))
        (add-to-list 'projectile-other-file-alist '("js" "html"))))

    (defvar my-simple-todo-regex "\\<\\(FIXME\\|TODO\\|BUG\\):")

    (defun hurricane/simple-todo ()
      "When in a project, create a `multi-occur' buffer matching the
    regex in `my-simple-todo-regex' across all buffers in the
    current project. Otherwise do `occur' in the current file."
      (interactive)
      (if (projectile-project-p)
          (multi-occur (projectile-project-buffer-files) my-simple-todo-regex)
        (occur my-simple-todo-regex)))))

(defun hurricane-misc/post-init-prodigy ()
  (progn
    (prodigy-define-tag
      :name 'jekyll
      :env '(("LANG" "en_US.UTF-8")
             ("LC_ALL" "en_US.UTF-8")))

    ;; Define service.
    (prodigy-define-service
      :name "Blog index"
      :command "grunt"
      :args '("search-index")
      :cwd blog-dir
      :tags '(blog index)
      :kill-signal 'sigkill
      :kill-process-buffer-on-stop t)

    (prodigy-define-service
      :name "Blog push"
      :command "rsync"
      :args '("-avzt" "--exclude=\"./.DS_Store\"" "." "c@182.61.145.178:/home/c/site/public/")
      :cwd blog-dir
      :tags '(blog push)
      :kill-signal 'sigkill
      :kill-process-buffer-on-stop t)

    (prodigy-define-service
      :name "Blog preview"
      :command "hugo"
      :args '("serve")
      :cwd deft-dir
      :tags '(blog preview)
      :init (lambda () (browse-url "http://localhost:1313"))
      :kill-signal 'sigkill
      :kill-process-buffer-on-stop t)

    (defun hurricane//refresh-chrome-current-tab (beg end length-before)
      (call-interactively 'hurricane//browser-refresh--chrome-applescript))
      ;; Add watch for prodigy-view-mode buffer change event.
      (add-hook 'prodigy-view-mode-hook
                #'(lambda() (set (make-local-variable 'after-change-functions) #'hurricane//refresh-chrome-current-tab)))))

(defun hurricane-misc/init-moz-controller ()
  (use-package moz-controller
    :defer t
    :init
    (progn
      (moz-controller-global-mode t)
      (spacemacs|hide-lighter moz-controller-mode))))

(defun hurricane-misc/init-ag ()
  (use-package ag))

(defun hurricane-misc/post-init-erc ()
  (progn
    (add-hook 'erc-text-matched-hook 'hurricane//erc-hook)
    (spaceline-toggle-erc-track-off)))

(defun hurricane-misc/init-wrap-region ()
  (use-package wrap-region
    :init
    (progn
      (wrap-region-global-mode t)
      (wrap-region-add-wrappers
       '(("$" "$")
         ("{-" "-}" "#")
         ("/" "/" nil ruby-mode)
         ("/* " " */" "#" (java-mode javascript-mode css-mode js2-mode))
         ("`" "`" nil (markdown-mode ruby-mode))))
      (add-to-list 'wrap-region-except-modes 'dired-mode)
      (add-to-list 'wrap-region-except-modes 'web-mode)
      )
    :defer t
    :config
    (spacemacs|hide-lighter wrap-region-mode)))

(defun hurricane-misc/init-keyfreq ()
  (use-package keyfreq
    :init
    (progn
      (keyfreq-mode t)
      (keyfreq-autosave-mode 1))))

(defun hurricane-misc/post-init-swiper ()
  "Initialize `swiper' package"
  (progn
    (setq ivy-use-virtual-buffers t)
    (setq ivy-display-style 'fancy)))

(defun hurricane-misc/post-init-magit ()
  (progn
    (with-eval-after-load 'magit
      (progn
        (add-to-list 'magit-no-confirm 'stage-all-changes)
        (define-key magit-log-mode-map (kbd "W") 'magit-copy-section-value)
        (define-key magit-status-mode-map (kbd "s-1") 'magit-jump-to-unstaged)
        (define-key magit-status-mode-map (kbd "s-2") 'magit-jump-to-untracked)
        (define-key magit-status-mode-map (kbd "s-3") 'magit-jump-to-staged)
        (define-key magit-status-mode-map (kbd "s-4") 'magit-jump-to-stashes)
        (setq magit-completing-read-function 'magit-builtin-completing-read)

        ;; (magit-define-popup-switch 'magit-push-popup ?u
        ;;   "Set upstream" "--set-upstream")
        ))

    ;; Prefer two way ediff.
    (setq magit-ediff-dwim-show-on-hunks t)

    ;; (setq magit-repository-directories '("~/Python/"))
    (setq magit-push-always-verify nil)

    (eval-after-load 'magit
      '(define-key magit-mode-map (kbd "C-c g")
         #'hurricane//magit-visit-pull-request))

    (setq magit-process-popup-time 10)))

(defun hurricane-misc/post-init-git-messenger ()
  (use-package git-messenger
    :defer t
    :config
    (progn
      (define-key git-messenger-map (kbd "f") 'hurricane/github-browse-commit))))

;; {{
;; Fix: markdown failed with exit code 127.
;; @see: [[file:~/.emacs.d/elpa/markdown-mode-20180904.1601/markdown-mode.el::(markdown-standalone%20(or%20output-buffer-name%20markdown-output-buffer-name))))]]
;; @see: https://github.com/jrblevin/markdown-mode/issues/177
(defun hurricane-misc/post-init-markdown-mode ()
  (progn
    (add-to-list 'auto-mode-alist '("\\.mdown\\'" . markdown-mode))
    (with-eval-after-load 'markdown-mode
      (progn
        (setq markdown-command "pandoc -s -f markdown -t html5 --mathjax --highlight-style pygments --standalone")
        (spacemacs/set-leader-keys-for-major-mode 'gfm-mode-map
          "p" 'hurricane//markdown-to-html)
        (spacemacs/set-leader-keys-for-major-mode 'markdown-mode
          "p" 'hurricane//markdown-to-html)
        (evil-define-key 'normal markdown-mode-map (kbd "TAB") 'markdown-cycle)
        ))))
;; }}

;; {{
;; @see: http://wikemacs.org/wiki/Shell#Search_the_bash.2C_zsh_or_fish_history_with_Ivy-mode
(defun hurricane-misc/post-init-shell-mode ()
  (spacemacs|use-package-add-hook shell-mode
    :post-config
    (progn
      ;; Fix, add colors and highlight text.
      (require 'ansi-color)
      (defun colorize-compilation-buffer ()
      (toggle-read-only)
      (ansi-color-apply-on-region (point-min) (point-max))
      (toggle-read-only))
      (add-hook 'compilation-filter-hook 'colorize-
              compilation-buffer)
      (add-hook 'shell-mode-hook (lambda () (highlight-regexp
                                       "\\[OK\\]" "hi-green-b")))
      ;; Make `URLs' clickable.
      (add-hook 'shell-mode-hook (lambda ()(goto-address-mode)))

      ;; Make file paths clickable.
      (add-hook 'shell-mode-hook 'compilation-shell-minor-mode)

      ;; Shell completion with a nice menu.
      (add-hook 'shell-mode-hook #'company-mode)
      (define-key shell-mode-map (kbd "TAB" #'company-manual-begin))

      ;; @see: https://stackoverflow.com/questions/20952995/emacs-shell-change-directory-with-ido
      ;; Change directory with ido.
      ;; Ido will keep asking for subdirectory after selecting directory with `RET', to finish selection press `C-RET'.
      (require 'ido)
      (require 'cl-lib)
      (require 'shell)

      (defvar my-dir-selected nil "Flag to indicate that user has selected the directory")

      (defun my-filter-cd-input (current-input)
        "Takes current user input for `cd' the a list
        whose car is the 'maximum possible directory path'
        and cdr is remaining string.

        Examples:
        '~/.emacs.d/in => ('~./emacs.d/' 'in')
        '/home/gue' => ('/home/' 'gue')
        '~/../' => ('~/../' '')"
        (let* ((unquoted-input (shell-unquote-argument current-input))
               (components (split-string unquoted-input "/"))
               (directory-parts (butlast components))
               (possible-prefix (car (last components))))
          (list (if (string= possible-prefix "")
                    unquoted-input
                  (concat (mapconcat 'identity directory-parts "/")
                          (when directory-parts "/")))
                possible-prefix)))

      (defun my-complete-directory-name (directory current-input)
        "Prompts user for directories in `directory', `current-input'
        is the string entered by the user till now."
        (let* ((filtered-input (my-filter-cd-input current-input))
               (directory-path (car filtered-input))
               (partial-input (cadr filtered-input))
               (directory-choices (mapcar 'file-name-nondirectory
                                          (condition-case nil
                                              (cl-remove-if-not 'file-directory-p
                                                                (directory-files (concat directory directory-path) t))
                                            ('file-error (list)))))
               (selected-name (ido-completing-read "Directory: "
                                                   directory-choices
                                                   nil nil partial-input)))
          (comint-delete-input)
          (insert (concat "cd "
                          (shell-quote-argument (concat directory-path selected-name "/"))))))

      (defun my-prompt-for-dir-or-fallback ()
        "If current shell command is `cd' prompt for directory
        using ido otherwise fallback to normal completion."
        (interactive)
        (let* ((user-input (buffer-substring-no-properties (comint-line-beginning-position)
                                                           (point-max))))
          (if (and (>= (length user-input) 3)
                   (string= (substring user-input 0 3) "cd "))
              (progn
                (setq my-dir-selected nil)
                (while (not my-dir-selected)
                  (my-complete-directory-name default-directory
                                              (buffer-substring-no-properties (+ (comint-line-beginning-position) 3)
                                                                              (point-max))))
                (comint-send-input))
            (call-interactively 'completion-at-point))))

      (define-key shell-mode-map (kbd "<tab>") 'my-prompt-for-dir-or-fallback)

      (add-hook 'ido-setup-hook 'ido-my-keys)

      (defun ido-my-keys ()
        "Add my keybindings for ido."
        (define-key ido-completion-map (kbd "<C-return>") (lambda ()
                                                            (interactive)
                                                            (setq my-dir-selected t)
                                                            (ido-exit-minibuffer))))
      ;; Shared and persistent history.
      (add-hook 'shell-mode-hook 'my-shell-mode-hook)
      (defun my-shell-mode-hook ()
        (setq comint-input-ring-file-name "~/.zsh_history")  ;; or bash_history
        (comint-read-input-ring t)))))
;; }}

(defun hurricane-misc/init-pandoc-mode ()
  (use-package pandoc-mode
    :defer t
    :config
    (progn
      (add-hook 'markdown-mode-hook 'pandoc-mode))))

(defun hurricane-misc/post-init-expand-region ()
  (with-eval-after-load 'expand-region
    (when (configuration-layer/package-usedp 'helm-ag)
      (defadvice er/prepare-for-more-expansions-internal
          (around helm-ag/prepare-for-more-expansions-internal activate)
        ad-do-it
        (let ((new-msg (concat (car ad-return-value)
                               ", H to highlight in buffers"
                               ", / to search in project, "
                               "f to search in files, "
                               "b to search in opened buffers"))
              (new-bindings (cdr ad-return-value)))
          (cl-pushnew
           '("H" (lambda ()
                   (call-interactively
                    'symbol-highlight)))
           new-bindings)
          (cl-pushnew
           '("/" (lambda ()
                   (call-interactively
                    'spacemacs/helm-project-smart-do-search-region-or-symbol)))
           new-bindings)
          (cl-pushnew
           '("f" (lambda ()
                   (call-interactively
                    'spacemacs/helm-files-smart-do-search-region-or-symbol)))
           new-bindings)
          (cl-pushnew
           '("b" (lambda ()
                   (call-interactively
                    'spacemacs/helm-buffers-smart-do-search-region-or-symbol)))
           new-bindings)
          (setq ad-return-value (cons new-msg new-bindings)))))))

(defun hurricane-misc/init-autoinsert ()
  (use-package autoinsert
    :config
    ;; Don't want to be prompted before insertion.
    (auto-insert-mode 1)
    (setq auto-insert-query nil)
    (add-hook 'find-file-hook 'auto-insert)
    (setq auto-insert-directory auto-insert-dir)

    (defun hurricane//autoinsert-yas-expand ()
      "Replace text in yasnippet template."
      (yas-expand-snippet (buffer-string) (point-min) (point-max)))

    (define-auto-insert "\\.html?$" ["default-html.html" hurricane//autoinsert-yas-expand])
    (define-auto-insert "\\.el?$" ["default-lisp.el" hurricane//autoinsert-yas-expand])
    (define-auto-insert "\\.org?$" ["default-org.org" hurricane//autoinsert-yas-expand])))

(defun hurricane-misc/init-use-package-ensure-system-package ()
  (use-package use-package-ensure-system-package
    :ensure t))

(defun hurricane-misc/init-atomic-chrome ()
  (use-package atomic-chrome
    :ensure t
    :defer 5                            ; since the entry of this
                                        ; package is from Chrome.
    :config
    (setq atomic-chrome-url-major-mode-alist
          '(("github\\.com"        . gfm-mode)
            ("emacs-china\\.org"   . gfm-mode)
            ("stackexchange\\.com" . gfm-mode)
            ("stackoverflow\\.com" . gfm-mode)))

    (defun hurricane-atomic-chrome-mode-setup ()
      (setq header-line-format
            (substitute-command-keys
             "Edit Chrome text area.  Finish \
`\\[atomic-chrome-close-current-buffer]'.")))

    (add-hook 'atomic-chrome-edit-mode-hook #'hurricane-atomic-chrome-mode-setup)

    (atomic-chrome-start-server)))

(defun hurricane-misc/init-dired-rsync ()
  (use-package dired-rsync
    :config
    (bind-key "C-c C-r" 'dired-rsync dired-mode-map)))

(defun hurricane-misc/init-rime ()
  (use-package rime
    :ensure t
    :requires core-themes-support
    :init
    (setq default-input-method "rime")
    (setq rime-show-candidate 'posframe)
    (setq rime-posframe-style 'simple)
    (setq rime-user-data-dir rime-dir)
    (setq rime-disable-predicates
     '(rime-predicate-evil-mode-p
       rime-predicate-prog-in-code-p
       rime-predicate-ace-window-p
       rime-predicate-hydra-p
       rime-predicate-which-key-activate-p
       rime-predicate-local-map-p))

    (defun +rime-force-enable ()
      "强制 `rime' 使用中文输入状态。
  如果当前不是 `rime' 输入法，则先激活 `rime' 输入法。如果当前是
  `evil' 的非编辑状态，则转为 `evil-insert-state'。"
      (interactive)
      (let ((input-method "rime"))
        (unless (string= current-input-method input-method)
          (activate-input-method input-method))
        (when (rime-predicate-evil-mode-p)
          (if (= (+ 1 (point)) (line-end-position))
              (evil-append 1)
            (evil-insert 1)))
        (when (rime--ascii-mode-p)
          (rime-inline-ascii))
        (rime-force-enable)))

    (defun +rime-convert-string-at-point (&optional return-cregexp)
      "将光标前的字符串转换为中文。"
      (interactive "P")
      (+rime-force-enable)
      (let ((string (if mark-active
                        (buffer-substring-no-properties
                         (region-beginning) (region-end))
                      (buffer-substring-no-properties
                       (line-beginning-position) (point))))
            code
            length)
        (cond ((string-match "\\([a-z'-]+\\|[[:punct:]]\\) *$" string)
               (setq code (replace-regexp-in-string
                           "^[-']" ""
                           (match-string 0 string)))
               (setq length (length code))
               (setq code (replace-regexp-in-string " +" "" code))
               (if mark-active
                   (delete-region (region-beginning) (region-end))
                 (when (> length 0)
                   (delete-char (- 0 length))))
               (when (> length 0)
                 (setq unread-command-events
                       (append (listify-key-sequence code)
                               unread-command-events))))
              (t (message "`+rime-convert-string-at-point' did nothing.")))))

    (defun rime-predicate-which-key-activate-p ()
      which-key--automatic-display)

    (defun rime-predicate-local-map-p ()
      (get-text-property (point) 'local-map))

    (defun dispatching-enchancer (origin-func)
      (defadvice origin-func (before rime-disable-predicate-maybe
                                     () activate)
        "Disable predicate if Ivy dispatch action is activated."
        (rime-inline-ascii)))

    (mapc #'dispatching-enchancer '(ivy-dispatching-done
                                        ivy-dispatching-call
                                        hydra-ivy/body))

    :custom
    (rime-librime-root "~/.emacs.d/librime/dist")
    :bind
    ("C-\\" . #'+rime-force-enable)
    ("M-g" . #'+rime-convert-string-at-point)
    (:map rime-mode-map
          ("C-M-g" . #'rime-inline-ascii)
          ("C-M-`" . #'rime-send-keybinding)
          :map rime-active-mode-map
          ("C-M-g" . #'rime-inline-ascii)
          :map ivy-minibuffer-map
          ("C-M-g" . #'rime-inline-ascii)
          :map blink-search-mode-map
          ("C-M-g" . #'rime-inline-ascii)
          )))

(defun hurricane-misc/init-with-proxy ()
  (use-package with-proxy
    :config
    (setq with-proxy-http-server (format "%s:%s" provixy-host provixy-port))))

(defun hurricane-misc/init-emacsql ()
  (use-package emacsql))

(defun hurricane-misc/init-mybigword ()
  (use-package mybigword
    :ensure t))

(defun hurricane-misc/init-emacs-everywhere ()
  (use-package emacs-everywhere
    :requires org-roam
    :init
    (setq emacs-everywhere-markdown-apps '("MarginNote 3"))))

(defun hurricane-misc/init-command-log-mode ()
  (use-package command-log-mode))

(defun hurricane-misc/post-init-fasd ()
  (progn
    (spacemacs/set-leader-keys
      "fad" 'hurricane/counsel-goto-recent-directory
      "faf" 'hurricane/counsel-find-file-recent-directory
      "fah" 'hurricane/counsel-yank-bash-history
      "fai" 'hurricane/counsel-imenu)

    (when (configuration-layer/layer-used-p 'ivy)
      (ivy-set-actions
       'hurricane/counsel-goto-recent-directory
       '(("o" fasd-find-file-action "find-file")
         ("s" ivy-search-from-action "search-from")
         ("e" eaf-open-in-file-manager "@ Open file in eaf file manager"))))))

(defun hurricane-misc/init-auto-save ()
  (use-package auto-save
    :config
    (auto-save-enable)
    (setq auto-save-silent t)
    (setq auto-save-delete-trailing-whitespace t)))

(defun hurricane-misc/post-init-eaf ()
  (with-eval-after-load 'eaf
    (setq eaf-screenshot-args (list "-i" "-x"))

    ;; /eaf/eaf.py
    ;; @PostGui
    ;; def ocr_area(self, buffer_id)
    (defun eaf-ocr-area ()
      (interactive)
      (eaf-call-async "ocr_area" eaf--buffer-id)))

  (with-eval-after-load 'eaf-browser
    (defun eaf-send-merriam-webster-liju-to-Anki (link)
      (interactive)
      (setq payload (split-string link "::" t))
      (python-bridge-call-async "tts" (nth 2 payload))
      ;; (setq liju-mp3 (format "mw_%s_%s.mp3" (format-time-string "%-I_%M_%p") (nth 0 payload)))
      ;; (let* ((final-cmd (format "aspeak --profile \"%s\" text \"%s\" -O \"%s%s\" -c mp3" aspeak-profile-file (nth 2 payload) (expand-file-name Anki-media-dir) liju-mp3 ))
      ;;        (proc
      ;;         (start-process-shell-command
      ;;          "eaf-send-merriam-webster-liju-to-Anki"
      ;;          nil
      ;;          final-cmd)))
      ;;   (set-process-sentinel
      ;;    proc
      ;;    (lambda (proc event)
      ;;      (print proc)
      ;;      (when (equal event "finished\n")
      ;;        (anki-add-card Anki-deck-name (format "[sound:%s]" liju-mp3) (format "%s" (nth 2 payload)) (format "%s" "") "subs2srs")
      ;;        )))
      ;;   t)
      )

    (defun eaf-copy-merriam-webster-phonetic (link)
      (interactive)
      (setq payload (split-string link "::" t))
      (print (nth 3 payload))
      (kill-new (nth 4 payload)))

    (eaf-bind-key eaf-ocr-buffer "z" eaf-browser-keybinding)
    (eaf-bind-key eaf-ocr-area "Z" eaf-browser-keybinding)
    (eaf-bind-key insert_or_export_text "n" eaf-browser-keybinding)
    (eaf-bind-key insert_or_render_by_eww "N" eaf-browser-keybinding)
    (eaf-bind-key insert_or_switch_to_reader_mode "," eaf-browser-keybinding)
    (eaf-bind-key insert_or_translate_text "." eaf-browser-keybinding)
    (eaf-bind-key insert_or_translate_page ";" eaf-browser-keybinding)
    ;; /eaf/core/webengine.py
    ;; @interactive
    ;; def send_merriam_webster_liju(self):
    ;;     self.buffer_widget.get_link_markers()
    ;;     self.send_input_message("Copy link: ", "send_merriam_webster_liju", "marker");
    (eaf-bind-key send_merriam_webster_liju "C-M-s" eaf-browser-keybinding)
    (eaf-bind-key copy_merriam_webster_phonetic "C-M-p" eaf-browser-keybinding)
    (eaf-bind-key insert_or_download_youtube_video "y" eaf-browser-keybinding)
    (eaf-bind-key insert_or_copy_text "M-w" eaf-browser-keybinding))

  (with-eval-after-load 'eaf-pdf-viewer
    (defun eaf-pdf-viewer-open-with-Adobe-Acrobat ()
      (interactive)
      (let* ((page-number (or (eaf-call-sync "execute_function" eaf--buffer-id "current_page") "1")))
        (do-applescript
         (concat "tell application \"Adobe Acrobat\"\n"
                 "	try\n"
         (format "		open \"%s\" options \"page=%s\"\n" eaf--buffer-url page-number)
                 "	on error\n"
         (format "		display alert \"Cannot open the file\" & \"%s\" \n" eaf--buffer-url)
                 "		return false\n"
                 "	end try\n"
                 "end tell\n"
                 "\n"
                 "tell application \"Adobe Acrobat\"\n"
                 "	tell PDF Window 1\n"
         (format "		goto page %s \n" page-number)
                 "	end tell\n"
                 "end tell\n"
                 ))))
    ;; pdf-viewer -> eaf_pdf_buffer.py
    ;; def popweb_dict_search_select(self):
    ;;     if self.buffer_widget.is_select_mode:
    ;;         content = self.buffer_widget.parse_select_char_list()
    ;;         eval_in_emacs('popweb-dict-eudic-dicts-input', [content])
    ;;         message_to_emacs(content)
    ;;         self.buffer_widget_cleanup_select()
    ;; 没有 @interactive @PostGui 装饰器，无法像 ocr_area 调用。
    ;; 没有在 webengine.py 或 eaf.py 中定义，build_all_methods 无法获取。
    ;; 无法在 evil-define-key 中调用。
    ;; 只能先在 (setq eaf-pdf-viewer-keybinding) 定义，再调用。
    ;; /layers/+tools/eaf/packages.el
    ;; (setq eaf-pdf-viewer-keybinding
    ;;  '(("y" . "popweb_dict_search_select")))
    ;; 以下的写法，无法运行。
    ;; (defun eaf-pdf-viewer-popweb-dict-translate-select ()
    ;;   (interactive)
    ;;   (eaf-call-async "popweb_dict_search_select" eaf--buffer-id))

    (evil-define-key 'normal eaf-pdf-outline-edit-mode-map (kbd "RET") #'eaf-pdf-outline-edit-jump)
    ;; (eaf-bind-key extract_page_images "e" eaf-pdf-viewer-keybinding)

    (eaf-bind-key eaf-ocr-buffer "z" eaf-pdf-viewer-keybinding)
    (eaf-bind-key eaf-ocr-area "Z" eaf-pdf-viewer-keybinding)
    (eaf-bind-key eaf-pdf-viewer-open-with-Adobe-Acrobat "E" eaf-pdf-viewer-keybinding)
    (eaf-bind-key eaf-pdf-outline-edit "O" eaf-pdf-viewer-keybinding)
    (eaf-bind-key select_left_tab "J" eaf-pdf-viewer-keybinding)
    (eaf-bind-key select_right_tab "K" eaf-pdf-viewer-keybinding)
    (eaf-bind-key popweb-dict-eudic-dicts-input "C-c y" eaf-pdf-viewer-keybinding))

  (with-eval-after-load 'eaf-image-viewer
    (eaf-bind-key eaf-ocr-buffer "z" eaf-image-viewer-keybinding)
    (eaf-bind-key eaf-ocr-area "Z" eaf-image-viewer-keybinding)))

(defun hurricane-misc/init-dupan ()
  (use-package dupan
    :ensure t))

;; | Key       | Command                                  | Description                                             |
;; |-----------+------------------------------------------+---------------------------------------------------------|
;; | C + n     | blink-search-candidate-select-next       | Select next candidate item                              |
;; | C + p     | blink-search-candidate-select-prev       | Select previous candidate item                          |
;; | Shift + M + n     | blink-search-backend-select-next         | Select next backend item                                |
;; | Shift + M + p     | blink-search-backend-select-prev         | Select previous backend item                            |
;; | Shift + M + j     | blink-search-candidate-group-select-next | Select next candidate group item                        |
;; | Shift + M + k     | blink-search-candidate-group-select-prev | Select previous candidate group item                    |
;; | C + j     | blink-search-parent                      | Jump to parent directory of select candidate item       |
;; | C + l     | blink-search-continue                    | Continue search new directory of current candidate item |
;; | C + m     | blink-search-do                          | Execute action for selected candidate item              |
;; | C + M + m | blink-search-preview                     | Preview selected candidate item                         |
;; | C + M + n | blink-search-preview-next                | Preview next candidate item                             |
;; | C + M + p | blink-search-preview-next                | Preview previous candidate item                         |
;; | M + w     | blink-search-copy                        | Copy text of selected candidate item                    |
;; | C + g     | blink-search-quit                        | Quit                                                    |
(defun hurricane-misc/init-blink-search ()
  (use-package blink-search
    :ensure t
    :init
    (setq blink-search-quick-keys
          '("h" "l" "u" "i" "y"
            "," "." ";" "'"
            "r" "v" "t" "c"
            "7" "8" "9" "0"
            "H" "L" "U" "I" "Y"
            "s" "a" "e" "q"
            "1" "2" "3" "4"
            "[" "]"))
    :config
    (setq blink-search-search-backends '("Grep PDF" "PDF" "Buffer List" "Find File" "Common Directory" "Recent File" "Grep File" "IMenu" "EAF Browser")
          blink-search-grep-pdf-backend 'pdf-tools
          blink-search-grep-pdf-search-paths '("/Users/c/Library/Mobile Documents/iCloud~QReader~MarginStudy/Documents/WebDownloads/" "/Users/c/Downloads/" "/Users/c/Documents/论文期刊/"))

    (defun hurricane//blink-search-grep-pdf-do (file page submatches)
      ;;highlight the matches
      (cond
       ((and (eq blink-search-grep-pdf-backend 'pdf-tools) (featurep 'pdf-tools) (not (string-empty-p submatches)))
        (let ((eaf-pdf-extension-list '("xps" "oxps" "cbz" "epub" "fb2" "fbz")))
          (blink-search-grep-pdf-pdftool-goto file page submatches)))
      ((and (eq blink-search-grep-pdf-backend 'eaf-pdf-viewer) (featurep 'eaf-pdf-viewer))
        (eaf-pdf-jump-to-page file page))
       ;; TODO support other pdf backends
       (t (message "Unknown backend %s" blink-search-grep-pdf-backend))))

    (defun hurricane//blink-search-pdf-do (file page submatches)
      ;;highlight the matches
      (cond
       ((and (eq blink-search-pdf-backend 'pdf-tools) (featurep 'pdf-tools) (not (string-empty-p submatches)))
        (let ((eaf-pdf-extension-list '("xps" "oxps" "cbz" "epub" "fb2" "fbz")))
          (blink-search-pdf-pdftool-goto file page submatches)))
       ((and (eq blink-search-pdf-backend 'eaf-pdf-viewer) (featurep 'eaf-pdf-viewer))
        (eaf-pdf-jump-to-page file page))
       ;; TODO support other pdf backends
       (t (message "Unknown backend %s" blink-search-pdf-backend))))

    (advice-add #'blink-search-grep-pdf-do :override #'hurricane//blink-search-grep-pdf-do)
    (advice-add #'blink-search-pdf-do :override #'hurricane//blink-search-pdf-do)
    ))

(defun hurricane-misc/init-plisty ()
  (use-package plisty))

(defun hurricane-misc/init-org-tags-filter ()
  (use-package org-tags-filter))

(defun hurricane-misc/init-org-roam-dblocks ()
  (use-package org-roam-dblocks
    :hook (org-mode . org-roam-dblocks-autoupdate-mode)))

(defun hurricane-misc/init-subed ()
  (use-package subed
    :ensure t
    :load-path "~/emacs-config/default/elpa/28.3/develop/subed-20230321.225327/subed"
    :init
    (require 'subed-autoloads)

    (defun hurricane/subed--send-sentence-to-Anki (source-path)
      (setq subed-sentence (substring-no-properties (subed-subtitle-text)))
      (setq subed-mp3 (format "subed_%s.mp3" (format-time-string "%Y_%m_%d_%-I_%M_%p")))
      (setq subed-screenshot (format "subed_%s.png" (format-time-string "%Y_%m_%d__%-I_%M_%p")))
      (let* ((timestamp-start (replace-regexp-in-string "," "." (subed-msecs-to-timestamp (subed-subtitle-msecs-start))))
             (timestamp-stop (replace-regexp-in-string "," "." (subed-msecs-to-timestamp (subed-subtitle-msecs-stop))))
             (final-cmd (format "ffmpeg -hide_banner -nostdin -y -loglevel quiet -sn -vn  -ss %s -to %s -i \"%s\" -map_metadata -1 -map 0:1 -ac 1 -codec:a libmp3lame -vbr on -compression_level 10 -application voip -b:a 24k \"%s%s\" && ffmpeg -hide_banner -nostdin -y -loglevel quiet -sn -an -ss %s -i \"%s\" -map_metadata -1 -vcodec mjpeg -lossless 0 -compression_level 6 -qscale:v 15 -vf scale=-2:200 -vframes 1 \"%s%s\"" timestamp-start timestamp-stop source-path (expand-file-name Anki-media-dir) subed-mp3 timestamp-stop source-path (expand-file-name Anki-media-dir) subed-screenshot))
             (proc
              (start-process-shell-command
               "hurricane/subed-send-sentence-to-Anki"
               nil
               final-cmd)))
        (message "%s" final-cmd)
        (set-process-sentinel
         proc
         (lambda (proc event)
           (when (equal event "finished\n")
             (anki-add-card Anki-deck-name (format "[sound:%s]" subed-mp3) subed-sentence (format "<img src=\"%s\">" subed-screenshot) "subs2srs")
             )))
        t))

    (defun hurricane/subed-send-sentence-to-Anki ()
      (interactive)
      (python-bridge-call-async "mpv" (list (subed-mpv--socket) "get_property" "path")))

    :config
    ;; (add-to-list 'subed-mpv-arguments "--no-sub-visibility")
    ;; 如果是观看 B 站解析的视频链接，就不需要代理。
    ;; (add-to-list 'subed-mpv-arguments (format "--stream-lavf-o-append=%s_proxy=%s://%s:%s" provixy-type provixy-type provixy-host provixy-port))
    ;; Remember cursor position between sessions
	  (add-hook 'subed-mode-hook 'save-place-local-mode)
	  ;; Break lines automatically while typing
	  (add-hook 'subed-mode-hook 'turn-on-auto-fill)
	  ;; Break lines at 40 characters
	  (add-hook 'subed-mode-hook (lambda () (setq-local fill-column 40)))
	  ;; Some reasonable defaults
	  (add-hook 'subed-mode-hook 'subed-enable-pause-while-typing)
	  ;; As the player moves, update the point to show the current subtitle
	  (add-hook 'subed-mode-hook 'subed-enable-sync-point-to-player)
	  ;; As your point moves in Emacs, update the player to start at the current subtitle
	  (add-hook 'subed-mode-hook 'subed-enable-sync-player-to-point)
	  ;; Replay subtitles as you adjust their start or stop time with M-[, M-], M-{, or M-}
	  (add-hook 'subed-mode-hook 'subed-enable-replay-adjusted-subtitle)
	  ;; Loop over subtitles
	  (add-hook 'subed-mode-hook 'subed-enable-loop-over-current-subtitle)
	  ;; Show characters per second
	  (add-hook 'subed-mode-hook 'subed-enable-show-cps)
    (evil-define-key '(normal insert emacs motion) subed-mode-map (kbd "M-n") #'subed-forward-subtitle-text)
    (evil-define-key '(normal insert emacs motion) subed-mode-map (kbd "M-p") #'subed-backward-subtitle-text)
    (evil-define-key '(normal insert emacs motion) subed-mode-map (kbd "C-M-a") #'subed-jump-to-subtitle-text)
    (evil-define-key '(normal insert emacs motion) subed-mode-map (kbd "C-M-e") #'subed-jump-to-subtitle-end)
    (evil-define-key '(normal insert emacs motion) subed-mode-map (kbd "M-[") #'subed-decrease-start-time)
    (evil-define-key '(normal insert emacs motion) subed-mode-map (kbd "M-]") #'subed-increase-start-time)
    (evil-define-key '(normal insert emacs motion) subed-mode-map (kbd "M-{") #'subed-decrease-stop-time)
    (evil-define-key '(normal insert emacs motion) subed-mode-map (kbd "M-}") #'subed-increase-stop-time)
    (evil-define-key '(normal insert emacs motion) subed-mode-map (kbd "C-M-n") #'subed-move-subtitle-forward)
    (evil-define-key '(normal insert emacs motion) subed-mode-map (kbd "C-M-p") #'subed-move-subtitle-backward)
    (evil-define-key '(normal insert emacs motion) subed-mode-map (kbd "C-M-f") #'subed-shift-subtitle-forward)
    (evil-define-key '(normal insert emacs motion) subed-mode-map (kbd "C-M-b") #'subed-shift-subtitle-backward)
    (evil-define-key '(normal insert emacs motion) subed-mode-map (kbd "C-M-x") #'subed-scale-subtitles-forward)
    (evil-define-key '(normal insert emacs motion) subed-mode-map (kbd "C-M-S-x") #'subed-scale-subtitles-backward)
    (evil-define-key '(normal insert emacs motion) subed-mode-map (kbd "M-i") #'subed-insert-subtitle)
    (evil-define-key '(normal insert emacs motion) subed-mode-map (kbd "C-M-i") #'subed-insert-subtitle-adjacent)
    (evil-define-key '(normal insert emacs motion) subed-mode-map (kbd "M-k") #'subed-kill-subtitle)
    (evil-define-key '(normal insert emacs motion) subed-mode-map (kbd "M-m") #'subed-merge-dwim)
    (evil-define-key '(normal insert emacs motion) subed-mode-map (kbd "M-M") #'subed-merge-with-previous)
    (evil-define-key '(normal insert emacs motion) subed-mode-map (kbd "M-.") #'subed-split-subtitle)
    (evil-define-key '(normal insert emacs motion) subed-mode-map (kbd "M-s") #'subed-sort)
    (evil-define-key '(normal insert emacs motion) subed-mode-map (kbd "M-SPC") #'subed-mpv-toggle-pause)
    (evil-define-key '(normal insert emacs motion) subed-mode-map (kbd "M-j") #'subed-mpv-jump-to-current-subtitle)
    (evil-define-key '(normal insert emacs motion) subed-mode-map (kbd "C-c C-d") #'subed-toggle-debugging)
    (evil-define-key '(normal insert emacs motion) subed-mode-map (kbd "C-c C-v") #'subed-mpv-play-from-file)
    (evil-define-key '(normal insert emacs motion) subed-mode-map (kbd "C-c C-u") #'subed-mpv-play-from-url)
    (evil-define-key '(normal insert emacs motion) subed-mode-map (kbd "C-c C-f") subed-mpv-frame-step-map)
    (evil-define-key '(normal insert emacs motion) subed-mode-map (kbd "C-c C-p") #'subed-toggle-pause-while-typing)
    (evil-define-key '(normal insert emacs motion) subed-mode-map (kbd "C-c C-l") #'subed-toggle-loop-over-current-subtitle)
    (evil-define-key '(normal insert emacs motion) subed-mode-map (kbd "C-c C-r") #'subed-toggle-replay-adjusted-subtitle)
    (evil-define-key '(normal insert emacs motion) subed-mode-map (kbd "C-c [") #'subed-copy-player-pos-to-start-time)
    (evil-define-key '(normal insert emacs motion) subed-mode-map (kbd "C-c ]") #'subed-copy-player-pos-to-stop-time)
    (evil-define-key '(normal insert emacs motion) subed-mode-map (kbd "C-c .") #'subed-toggle-sync-point-to-player)
    (evil-define-key '(normal insert emacs motion) subed-mode-map (kbd "C-c ,") #'subed-toggle-sync-player-to-point)
    (evil-define-key '(normal insert emacs motion) subed-mode-map (kbd "C-c C-t") (let ((html-tag-keymap (make-sparse-keymap)))
                                                                             (evil-define-key '(normal insert emacs motion) html-tag-keymap (kbd "C-t") #'subed-insert-html-tag)
                                                                             (evil-define-key '(normal insert emacs motion) html-tag-keymap (kbd "C-i") #'subed-insert-html-tag-italic)
                                                                             (evil-define-key '(normal insert emacs motion) html-tag-keymap (kbd "C-b") #'subed-insert-html-tag-bold)
                                                                             html-tag-keymap))
    (evil-define-key '(normal insert emacs motion) subed-mode-map (kbd "C-M-s") #'hurricane/subed-send-sentence-to-Anki)

    ;; 该部分定义用于和软件 Enjoyable 配合，使用游戏手柄控制。
    (evil-define-key '(normal) subed-mode-map (kbd "RET") #'subed-forward-subtitle-text)
    (evil-define-key '(normal) subed-mode-map (kbd "6") #'subed-backward-subtitle-text)
    (evil-define-key '(normal) subed-mode-map (kbd "3") #'subed-merge-dwim)
    (evil-define-key '(normal) subed-mode-map (kbd "1") #'subed-merge-with-previous)
    (evil-define-key '(normal) subed-mode-map (kbd "7") #'hurricane/subed-send-sentence-to-Anki)
    (evil-define-key '(normal) subed-mode-map (kbd "r") #'subed-mpv-jump-to-current-subtitle)
    (evil-define-key '(normal) subed-mode-map (kbd "p") #'subed-mpv-toggle-pause)
    ;; 因为系统默认版本 python3.11 无法安装 aeneas，只能使用 python3.7 安装成功。
    ;; @see：https://github.com/readbeyond/aeneas/issues/285#issuecomment-1237726917
    (setq subed-align-command '("python3.7" "-m" "aeneas.tools.execute_task"))
    ))

(defun hurricane-misc/init-youtube-sub-extractor ()
  (use-package youtube-sub-extractor
    :ensure t
    :config
    (defun hurricane//youtube-sub-extractor-copy-ts-link ()
      "Get timestamp.
Works only in youtube-sub-extractor-mode buffer."
      (interactive)
      (let ((ts (plist-get (text-properties-at (point)) 'timestamp)))
        (when (and (boundp 'video-url) ts
                   (bound-and-true-p youtube-sub-extractor-subtitles-mode)
                   (string-match "\\([^?]+\\)\\(\\?\\|\\)\\(.*\\)" video-url))
          (let* (
                 (args (url-parse-query-string (match-string 3 video-url)))
                 (tp (floor (youtube-sub-extractor--seconds ts)))
                 (tpoint (when (numberp tp) (number-to-string tp)))
                 (t-item (alist-get "t" args nil nil #'string-equal))
                 (_ (if t-item (setf (car t-item) tpoint)
                      (cl-pushnew (list "t" tpoint) args)))
                 )
            (message tpoint)
            tpoint))))
    :commands (youtube-sub-extractor-extract-subs)
    :custom
    (youtube-sub-extractor-timestamps 'left-side-text)))

;; python3 -m pip install pyobjc
(defun hurricane-misc/init-dictionary-overlay ()
  (use-package dictionary-overlay
    :ensure t
    :init
    (require 'eww)
    (with-eval-after-load 'eww
      (evil-define-key '(normal insert emacs motion) eww-mode-map (kbd "y") #'hurricane/popweb-translate-and-mark-unknown-word)
      (evil-define-key '(normal insert emacs motion) eww-mode-map (kbd "<down-mouse-1>") #'hurricane/popweb-translate-and-mark-unknown-word)
      (evil-define-key '(normal insert emacs motion) eww-mode-map (kbd "u") #'dictionary-overlay-mark-word-known)
      (evil-define-key '(normal insert emacs motion) eww-mode-map (kbd ".") #'dictionary-overlay-render-buffer)
      (define-key evil-normal-state-map (kbd "<down-mouse-1>") #'hurricane/popweb-translate-and-mark-unknown-word)
      (evilified-state-evilify-map eww-mode-map
        :mode eww-mode
        :bindings
        "y" 'hurricane/popweb-translate-and-mark-unknown-word
        "<down-mouse-1>" 'hurricane/popweb-translate-and-mark-unknown-word
        "u" 'dictionary-overlay-mark-word-known
        "." 'dictionary-overlay-render-buffer)
      )
    :custom-face
    (dictionary-overlay-unknownword ((t :inherit font-lock-keyword-face)))
    (dictionary-overlay-translation ((t :inherit font-lock-comment-face)))
    :config
    (require 'websocket-bridge)

    (setq dictionary-overlay-inihibit-keymap nil
          dictionary-overlay-position 'after
          dictionary-overlay-translation-format "(%s)"
          dictionary-overlay-user-data-directory dictionary-overlay-dir
          dictionary-overlay-auto-jump-after
          '(mark-word-known         ; recommended
            ;; mark-word-unknown ; not recommended
            render-buffer           ; opinionated, but turn it on, why not
            )
          dictionary-overlay-recenter-after-mark-and-jump nil)

    ;; 该函数可以通过anki-editor-find-notes 替代，但是保留是为了获取文章中的句子至ANKI。
    (defun hurricane/popweb-translate-and-mark-unknown-word (&optional x)
      (interactive)
      (if (display-graphic-p)
          (popweb-dict-eudic-dicts-input nil (lc-corpus--sentence)))
      (ignore-errors
        (dictionary-overlay-mark-word-unknown)))

    (defun hurricane/dictionary-overlay-jump-next-unknown-word-and-current-subtitle ()
        (interactive)
        (if (derived-mode-p 'subed-srt-mode)
            (progn
             (dictionary-overlay-jump-next-unknown-word)
             (sleep-for 2)
             (subed-mpv-jump-to-current-subtitle))
          (dictionary-overlay-jump-next-unknown-word)))

    (defun hurricane/dictionary-overlay-jump-previous-unknown-word-and-current-subtitle ()
        (interactive)
        (if (derived-mode-p 'subed-srt-mode)
            (progn
             (dictionary-overlay-jump-prev-unknown-word)
             (sleep-for 2)
             (subed-mpv-jump-to-current-subtitle))
          (dictionary-overlay-jump-prev-unknown-word)))
    :bind
    (("C-c d" . dictionary-overlay-render-buffer)
     ("C-c y" . hurricane/popweb-translate-and-mark-unknown-word))
    (:map dictionary-overlay-map
                ("y" . hurricane/popweb-translate-and-mark-unknown-word)
                ("u" . dictionary-overlay-mark-word-known)
                ("s" . hurricane/youdao-search-at-point)
                ("n" . hurricane/dictionary-overlay-jump-next-unknown-word-and-current-subtitle)
                ("p" . hurricane/dictionary-overlay-jump-previous-unknown-word-and-current-subtitle)
                )))

(defun hurricane-misc/init-websocket ()
  (use-package websocket
    :ensure t
    :defer t))

(defun hurricane-misc/init-websocket-bridge ()
  (use-package websocket-bridge
    :ensure t
    :defer t))

(defun hurricane-misc/init-ffmpeg-utils ()
  (use-package ffmpeg-utils
    :ensure t))

(defun hurricane-misc/init-emacs-azure-tts ()
  (use-package emacs-azure-tts
    :ensure t
    :init
    (require 'anki-editor)

    (with-eval-after-load 'anki-editor
     (defun emacs-azure-tts-add-card (deck front back translation)
      (let ((bytes (with-temp-buffer
                     (insert-file-contents-literally front)
                     (buffer-string))))
       (thread-last
        (anki-editor-api-with-multi
         (anki-editor-api-enqueue 'storeMediaFile
                                  :filename (file-name-nondirectory front)
                                  :data (base64-encode-string bytes))
         (anki-editor-api-enqueue 'addNote
                                  :note (list `("deckName" . ,deck)
                                               '("modelName" . "Antimoon without expression")
                                               `("fields" . ,(list `("audio" . ,(format "[sound:%s]" (file-name-nondirectory front)))
                                                                   `("sentence" . ,back)
                                                                   `("translation" . ,translation)))
                                               `("options" . ,(list
                                                               '("allowDuplicate" . t))))))))))
    :config
    (add-to-list 'emacs-azure-tts-after-speak-functions (apply-partially #'emacs-azure-tts-add-card Anki-deck-name))
    (add-to-list 'emacs-azure-tts-after-speak-functions #'(lambda (front back translation) (ignore-errors (if (get-buffer-window goldendict-buffer-name) (and (winum-select-window-2) (evil-exit-visual-state))))))
    ))

(defun hurricane-misc/init-reverso ()
  (use-package reverso))

(defun hurricane-misc/init-nov ()
  (use-package nov
    :init
    (add-hook 'nov-mode-hook #'shrface-mode)
    :config
    (require 'shrface)
    (setq nov-shr-rendering-functions '((img . nov-render-img) (title . nov-render-title)))
    (setq nov-shr-rendering-functions (append nov-shr-rendering-functions shr-external-rendering-functions))))

(defun hurricane-misc/init-goldendict-ng ()
  (use-package goldendict-ng
    :demand t
    :config
    (setq goldendict-ng-executable "/Applications/GoldenDict.app/Contents/MacOS/GoldenDict")
    (setq goldendict-ng-narrow-groups-to-matching-languages t)
    ))
