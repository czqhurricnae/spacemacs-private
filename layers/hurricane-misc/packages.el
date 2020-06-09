(setq hurricane-misc-packages
      '(
        helm-github-stars
        helm
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
        4clojure
        tiny
        ;; smartparens
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
        dired-rsync))

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
          ("s" spacemacs/swiper-region-or-symbol)
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
;; }}

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

(spacemacs/set-leader-keys "ar" 'my-ranger))

;; Copy from spacemacs `helm' layer.
(defun hurricane-misc/init-helm-ag ()
  (use-package helm-ag
    :defer t
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

      ;; Evilify the helm-grep buffer.
      (evilified-state-evilify helm-grep-mode helm-grep-mode-map
        (kbd "RET") 'helm-grep-mode-jump-other-window
        (kbd "q") 'quit-window)

      (spacemacs/set-leader-keys
        ;; Helm-ag marks.
        "s`"  'helm-ag-pop-stack
        ;; Opened buffers scope.
        "sb"  'spacemacs/helm-buffers-smart-do-search
        "sB"  'spacemacs/helm-buffers-smart-do-search-region-or-symbol
        "sab" 'helm-do-ag-buffers
        "saB" 'spacemacs/helm-buffers-do-ag-region-or-symbol
        "skb" 'spacemacs/helm-buffers-do-ack
        "skB" 'spacemacs/helm-buffers-do-ack-region-or-symbol
        "stb" 'spacemacs/helm-buffers-do-pt
        "stB" 'spacemacs/helm-buffers-do-pt-region-or-symbol
        ;; Current file scope.
        "ss"  'spacemacs/helm-file-smart-do-search
        "sS"  'spacemacs/helm-file-smart-do-search-region-or-symbol
        "saa" 'helm-ag-this-file
        "saA" 'spacemacs/helm-file-do-ag-region-or-symbol
        ;; Files scope.
        "sf"  'spacemacs/helm-files-smart-do-search
        "sF"  'spacemacs/helm-files-smart-do-search-region-or-symbol
        "saf" 'helm-do-ag
        "saF" 'spacemacs/helm-files-do-ag-region-or-symbol
        "skf" 'spacemacs/helm-files-do-ack
        "skF" 'spacemacs/helm-files-do-ack-region-or-symbol
        "stf" 'spacemacs/helm-files-do-pt
        "stF" 'spacemacs/helm-files-do-pt-region-or-symbol
        ;; Current project scope.
        "/"   'spacemacs/helm-project-smart-do-search
        "*"   'spacemacs/helm-project-smart-do-search-region-or-symbol
        "sp"  'spacemacs/helm-project-smart-do-search
        "sP"  'spacemacs/helm-project-smart-do-search-region-or-symbol
        "sap" 'spacemacs/helm-project-do-ag
        "saP" 'spacemacs/helm-project-do-ag-region-or-symbol
        "skp" 'spacemacs/helm-project-do-ack
        "skP" 'spacemacs/helm-project-do-ack-region-or-symbol
        "stp" 'spacemacs/helm-project-do-pt
        "stP" 'spacemacs/helm-project-do-pt-region-or-symbol))
    :config
    (progn
      (advice-add 'helm-ag--save-results :after 'spacemacs//gne-init-helm-ag)
      (evil-define-key 'normal helm-ag-map "SPC" spacemacs-default-map)
      (evilified-state-evilify helm-ag-mode helm-ag-mode-map
        (kbd "RET") 'helm-ag-mode-jump-other-window
        (kbd "gr") 'helm-ag--update-save-results
        (kbd "q") 'quit-window))))

(defun hurricane-misc/post-init-hydra ()
  (progn
    (defhydra hydra-hotspots (:color blue)
      "Hotspots."
      ("b" blog-admin-start "blog")
      ("g" helm-github-stars "helm github stars")
      ("r" hurricane/run-current-file "run current file"))

    (defhydra multiple-cursors-hydra (:hint nil)
      "
       ^Up^            ^Down^        ^Other^
             ----------------------------------------------
         [_p_]   Next    [_n_]   Next    [_l_] Edit lines
         [_P_]   Skip    [_N_]   Skip    [_a_] Mark all
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

    (define-key global-map (kbd "<f1>") 'hydra-hotspots/body)
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
  (use-package smartparens
    :defer t
    :init
    (smartparens-global-mode t)
    (global-set-key (kbd "C-(") 'wrap-sexp-with-new-round-parens)
    :config
    ;; 写 lisp 时不成对补全 "'" 和 "`".
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

(defun hurricane-misc/post-init-helm ()
  (with-eval-after-load 'helm
    (progn
      ;; Limit max number of matches displayed for speed.
      (setq helm-candidate-number-limit 100)
      ;; Ignore boring files like .o and .a.
      (setq helm-ff-skip-boring-files t)
      ;; Replace locate with spotlight on macOS.
      (setq helm-locate-command "mdfind -name %s %s")
      (push "\\.emlx$" helm-boring-file-regexp-list))))

(defun hurricane-misc/init-helm-github-stars ()
  (use-package helm-github-stars
    :commands (helm-github-stars)
    :init
    (setq helm-github-stars-username "hurricane")))

(defun hurricane-misc/post-init-command-log ()
  (with-eval-after-load 'global-command-log-mode
    (setq clm/log-command-exceptions* (append clm/log-command-exceptions*
                                              '(evil-next-visual-line
                                                evil-previous-visual-line)))))

(defun hurricane-misc/init-litable ()
  (use-package litable
    :init
    :defer t))

(defun hurricane-misc/init-osx-dictionary ()
  (use-package osx-dictionary
    :init
    (progn
      (evilified-state-evilify osx-dictionary-mode osx-dictionary-mode-map)
      (setq osx-dictionary-use-chinese-text-segmentation t)
      (global-set-key (kbd "C-c d") 'osx-dictionary-search-pointer))))

(defun hurricane-misc/init-4clojure ()
  (use-package 4clojure
    :init
    (progn
      (spacemacs/declare-prefix "o4" "4clojure")
      (spacemacs/set-leader-keys "o4q" '4clojure-open-question)
      (spacemacs/set-leader-keys "o4n" '4clojure-next-question)
      (spacemacs/set-leader-keys "o4p" '4clojure-previous-question)
      (spacemacs/set-leader-keys "o4c" '4clojure-check-answers))))

(defun hurricane-misc/post-init-avy ()
  (progn
    (global-set-key (kbd "C-s-'") 'avy-goto-char-2)
    (global-set-key (kbd "M-'") 'avy-goto-char-2)))

(defun hurricane-misc/post-init-ace-window ()
  (global-set-key (kbd "C-x C-o") #'ace-window))

(defun hurricane-misc/init-discover-my-major ()
  (use-package discover-my-major
    :defer t
    :init
    (progn
      (spacemacs/set-leader-keys (kbd "mhm") 'discover-my-major)
      (evilified-state-evilify makey-key-mode makey-key-mode-get-key-map))))

(defun hurricane-misc/post-init-elfeed ()
  (use-package elfeed
    :init
    (global-set-key (kbd "C-x w") 'elfeed)
    :defer t
    :config
    (progn
      (setq elfeed-feeds
            '("http://nullprogram.com/feed/"
              "http://z.caudate.me/rss/"
              "http://irreal.org/blog/?feed=rss2"
              "http://feeds.feedburner.com/LostInTheTriangles"
              "http://tonybai.com/feed/"
              "http://planet.emacsen.org/atom.xml"
              "http://feeds.feedburner.com/emacsblog"
              "http://blog.binchen.org/rss.xml"
              "http://oremacs.com/atom.xml"
              "http://blog.gemserk.com/feed/"
              "http://www.masteringemacs.org/feed/"
              "http://t-machine.org/index.php/feed/"
              "http://gameenginebook.blogspot.com/feeds/posts/default"
              "http://feeds.feedburner.com/ruanyifeng"
              "http://coolshell.cn/feed"
              "http://blog.devtang.com/atom.xml"
              "http://emacsist.com/rss"
              "http://puntoblogspot.blogspot.com/feeds/2507074905876002529/comments/default"
              "http://angelic-sedition.github.io/atom.xml"))

      ;; (Evilify elfeed-search-mode elfeed-search-mode-map)
      (evilified-state-evilify-map elfeed-search-mode-map
        :mode elfeed-search-mode
        :bindings
        "G" 'elfeed-update
        "g" 'elfeed-search-update--force)

      (defun hurricane/elfeed-mark-all-as-read ()
        (interactive)
        (mark-whole-buffer)
        (elfeed-search-untag-all-unread))

      (define-key elfeed-search-mode-map (kbd "R") 'hurricane/elfeed-mark-all-as-read)

      (defadvice elfeed-show-yank (after elfeed-show-yank-to-kill-ring activate compile)
        "Insert the yanked text from x-selection to kill ring."
        (kill-new (x-get-selection)))

      (ad-activate 'elfeed-show-yank))))

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

    (define-key evil-visual-state-map "p" 'evil-paste-after-from-0)
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

    (defun my-evil-yank ()
      (interactive)
      (save-excursion
        (call-interactively 'evil-yank))
      (backward-char))

    (define-key evil-visual-state-map (kbd "y") 'my-evil-yank)

    (define-key evil-normal-state-map
      (kbd "Y") 'hurricane/yank-to-end-of-line)

    (define-key evil-normal-state-map (kbd "[ SPC") (lambda () (interactive) (evil-insert-newline-above) (forward-line)))
    (define-key evil-normal-state-map (kbd "] SPC") (lambda () (interactive) (evil-insert-newline-below) (forward-line -1)))

    (define-key evil-normal-state-map (kbd "[ b") 'previous-buffer)
    (define-key evil-normal-state-map (kbd "] b") 'next-buffer)
    (define-key evil-normal-state-map (kbd "M-y") 'counsel-yank-pop)

    ;; {{ Unbinding Evil's mappings.
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
    ;; (define-key evil-motion-state-map "\C-e" 'evil-end-of-line)
    ;; (define-key evil-insert-state-map "\C-e" 'evil-end-of-line)
    ;; (define-key evil-emacs-state-map "\C-e" 'evil-end-of-line)

    (define-key evil-visual-state-map (kbd ",/") 'evilnc-comment-or-uncomment-lines)
    (define-key evil-visual-state-map (kbd "C-r") 'hurricane/evil-quick-replace)
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
            hurricane/my-mc-mark-next-like-this))
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

    (defun my-simple-todo ()
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
      :name "Preview cocos2d-x web"
      :command "python"
      :args '("-m" "SimpleHTTPServer" "6001")
      :cwd "~/cocos2d-x/web"
      :tags '(work)
      :kill-signal 'sigkill
      :kill-process-buffer-on-stop t)

    (prodigy-define-service
      :name "Preview creator engine"
      :command "python"
      :args '("-m" "SimpleHTTPServer" "6004")
      :cwd "~/Github/fireball/engine"
      :tags '(work)
      :kill-signal 'sigkill
      :kill-process-buffer-on-stop t)

    (prodigy-define-service
      :name "Hexo Server"
      :command "hexo"
      :args '("server")
      :cwd blog-admin-dir
      :tags '(hexo server)
      :kill-signal 'sigkill
      :kill-process-buffer-on-stop t)

    (prodigy-define-service
      :name "Hexo Deploy"
      :command "hexo"
      :args '("deploy" "--generate")
      :cwd blog-admin-dir
      :tags '(hexo deploy)
      :kill-signal 'sigkill
      :kill-process-buffer-on-stop t)

    (prodigy-define-service
      :name "Debug Fireball"
      :command "npm"
      :args '("start" "--" "--nologin" "/Users/hurricane/Github/example-cases")
      :cwd "~/Github/fireball/"
      :tags '(work)
      :kill-signal 'sigkill
      :kill-process-buffer-on-stop t)

    (prodigy-define-service
      :name "Org wiki preview"
      :command "python"
      :args '("-m" "SimpleHTTPServer" "8088")
      :cwd "~/org-notes/public_html"
      :tags '(org-mode)
      :init (lambda () (browse-url "http://localhost:8088"))
      :kill-signal 'sigkill
      :kill-process-buffer-on-stop t)

    (defun refresh-chrome-current-tab (beg end length-before)
      (call-interactively 'hurricane/browser-refresh--chrome-applescript))
      ;; Add watch for prodigy-view-mode buffer change event.
      (add-hook 'prodigy-view-mode-hook
                #'(lambda() (set (make-local-variable 'after-change-functions) #'refresh-chrome-current-tab)))))

(defun hurricane-misc/init-moz-controller ()
  (use-package moz-controller
    :init
    (progn
      (moz-controller-global-mode t)
      (spacemacs|hide-lighter moz-controller-mode))))

(defun hurricane-misc/init-ag ()
  (use-package ag
    :init))

(defun hurricane-misc/post-init-erc ()
  (progn
    (add-hook 'erc-text-matched-hook 'my-erc-hook)
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
    (setq ivy-display-style 'fancy)

    (evilified-state-evilify ivy-occur-mode ivy-occur-mode-map)

    (use-package ivy
      :defer t
      :config
      (progn
        (spacemacs|hide-lighter ivy-mode)

        (ivy-set-actions
         t
         '(("f" my-find-file-in-git-repo "find files")
           ("!" my-open-file-in-external-app "Open file in external app")
           ("I" ivy-insert-action "insert")
           ("C" ivy-kill-new-action "copy")
           ("S" ivy-ff-checksum-action "Checksum")))

        (spacemacs/set-leader-keys "fad" 'counsel-goto-recent-directory)
        (spacemacs/set-leader-keys "faf" 'counsel-find-file-recent-directory)

        (setq ivy-initial-inputs-alist nil)
        (setq ivy-wrap t)
        (setq confirm-nonexistent-file-or-buffer t)

        (define-key ivy-minibuffer-map (kbd "C-c o") 'ivy-occur)
        (define-key ivy-minibuffer-map (kbd "TAB") 'ivy-call)
        (define-key ivy-minibuffer-map (kbd "C-s-m") 'ivy-partial-or-done)
        (define-key ivy-minibuffer-map (kbd "C-c s") 'ivy-ff-checksum)
        (define-key ivy-minibuffer-map (kbd "s-o") 'ivy-dispatching-done-hydra)
        (define-key ivy-minibuffer-map (kbd "C-o") 'hydra-ivy/body)
        (define-key ivy-minibuffer-map (kbd "C-c C-e") 'spacemacs//counsel-edit)
        (define-key ivy-minibuffer-map (kbd "<f3>") 'ivy-occur)
        (define-key ivy-minibuffer-map (kbd "C-s-j") 'ivy-immediate-done)
        (define-key ivy-minibuffer-map (kbd "C-j") 'ivy-next-line)
        (define-key ivy-minibuffer-map (kbd "C-k") 'ivy-previous-line)))

    ;; (define-key global-map (kbd "C-s") 'my-swiper-search)))
    (define-key global-map (kbd "C-s") 'color-rg-search-symbol)))

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

        (magit-define-popup-switch 'magit-push-popup ?u
          "Set upstream" "--set-upstream")
        ))

    ;; Prefer two way ediff.
    (setq magit-ediff-dwim-show-on-hunks t)

    (setq magit-repository-directories '("~/Python/"))
    (setq magit-push-always-verify nil)

    (eval-after-load 'magit
      '(define-key magit-mode-map (kbd "C-c g")
         #'hurricane/magit-visit-pull-request))

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
          "p" 'hurricane/markdown-to-html)
        (spacemacs/set-leader-keys-for-major-mode 'markdown-mode
          "p" 'hurricane/markdown-to-html)
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
                    'hurricane/highlight-dwim)))
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
    (define-auto-insert "\\.html?$" ["default-html.html" hurricane/autoinsert-yas-expand])
    (define-auto-insert "\\.el?$" ["default-lisp.el" hurricane/autoinsert-yas-expand])
    (define-auto-insert "\\.org?$" ["default-org.org" hurricane/autoinsert-yas-expand])))

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
    :init
    (when (functionp 'module-load)
      (progn
        (require 'posframe)
        (setq rime-show-candidate 'posframe)
        (setq rime-posframe-properties
              (list :font "sarasa ui sc"
                    :internal-border-width 10))))
    (setq default-input-method "rime")
    (setq rime-user-data-dir rime-dir)
    (setq rime-disable-predicates
     '(rime-predicate-evil-mode-p
       rime-predicate-after-alphabet-char-p
       rime-predicate-prog-in-code-p))

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

    :custom
    (rime-librime-root "~/.emacs.d/librime/dist")
    (rime-posframe-properties
          (list :background-color "#333333"
                :foreground-color "#dcdccc"
                :internal-border-width 10))
    :bind
    ("C-\\" . #'+rime-force-enable)
    ("M-g" . #'+rime-convert-string-at-point)
    (:map rime-mode-map
          ("M-n" . #'rime-inline-ascii)
          ("C-`" . rime-send-keybinding))))
