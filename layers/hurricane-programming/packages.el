(defconst hurricane-programming-packages
  '(
    flycheck
    dash-at-point
    virtualenvwrapper
    web-mode
    slime
    dumb-jump
    yasnippet
    (standardfmt :location (recipe :fetcher github :repo "jimeh/standardfmt.el"))
    (eslintfmt :location (recipe :fetcher github :repo "czqhurricnae/eslintfmt.el"))
    (pythonfmt :location (recipe :fetcher github :repo "czqhurricnae/pythonfmt.el"))
    ;; prettier-js
    (psearch :location (recipe :fetcher github :repo "twlz0ne/psearch.el" :files ("psearch.el")))
))

(defun hurricane-programming/post-init-yasnippet ()
  (progn
    (yas-global-mode 1)
    (set-face-background 'secondary-selection "gray")
    (setq-default yas-prompt-functions '(yas-ido-prompt yas-dropdown-prompt))
    (mapc #'(lambda (hook) (remove-hook hook 'spacemacs/load-yasnippet)) '(prog-mode-hook
                                                                      org-mode-hook
                                                                      markdown-mode-hook))
    (spacemacs/add-to-hooks 'hurricane//load-yasnippet '(prog-mode-hook
                                                        org-mode-hook
                                                        markdown-mode-hook))))

(defun hurricane-programming/init-virtualenvwrapper ()
  (use-package virtualenvwrapper
    :config
    (venv-initialize-interactive-shells)
    (venv-initialize-eshell)
    (setq venv-location virtualenv-dir)))

(defun hurricane-programming/post-init-web-mode ()
  (use-package web-mode
    :config
    (add-to-list 'auto-mode-alist '("\\.html$" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
    (add-to-list 'auto-mode-alist '("\\.jsx$" . rjsx-mode))
    (add-hook 'web-mode-hook  'web-mode-init-hook)
    ;; Remove the annoying underline in flycheck.
    (add-hook 'js2-mode-hook (lambda () (setq js2-strict-missing-semi-warning nil)))
    (web-mode-toggle-current-element-highlight)
    (web-mode-dom-errors-show)))
    (setq company-backends-web-mode '((company-dabbrev-code
                                       company-keywords
                                       company-etags)
                                       company-files
                                       company-dabbrev))

(defun hurricane-programming/post-init-dumb-jump ()
  (setq dumb-jump-selector 'ivy))

(defun hurricane/dumb-jump ()
  (interactive)
  (evil-set-jump)
  (dumb-jump-go-other-window))

(defun hurricane-programming/init-ycmd ()
  (use-package ycmd
    :init
    (set-variable 'ycmd-global-config "~/.ycm_extra_conf.py")
    (set-variable 'ycmd-server-command `("python" ,(expand-file-name "~/YouCompleteMe/third_party/ycmd/ycmd/")))
    (setq ycmd-tag-files 'auto)
    (setq ycmd-force-semantic-completion t)
    (setq ycmd-request-message-level -1)
    (add-hook 'c++-mode-hook 'ycmd-mode)
    (add-hook 'python-mode-hook 'ycmd-mode)
    (setq company-backends-c-mode-common '((company-c-headers
                                            company-dabbrev-code
                                            company-keywords
                                            company-gtags :with company-yasnippet)
                                            company-files company-dabbrev ))
    (hurricane|toggle-company-backends company-ycmd)
    :config
    (eval-after-load 'ycmd
      '(spacemacs|hide-lighter ycmd-mode))))

;; {{
;; @see: https://github.com/slime/slime
;; @see: https://github.com/syl20bnr/spacemacs/tree/master/layers/%2Blang/common-lisp
;; This layer defaults to using sbcl.
;; $ brew install sbcl
;; Set your lisp system and, optionally, some contribs.
(defun hurricane-programming/post-init-slime ()
  (progn
    (setq inferior-lisp-program "/usr/local/opt/sbcl/bin/sbcl")
    (setq slime-contribs '(slime-fancy))))
;; }}

(defun hurricane-programming/init-dash-at-point ()
  (use-package dash-at-point
    :config
    (autoload 'dash-at-point "dash-at-point"
      "Search the word at point with Dash." t nil)
    (add-to-list 'dash-at-point-mode-alist '(c-mode . "C"))))

;; {{
;; @see: https://gist.github.com/CodyReichert/9dbc8bd2a104780b64891d8736682cea
;; @see: https://github.com/flycheck/flycheck/issues/997
;; $ npm install -g eslint babel-eslint eslint-plugin-react js-beautify prettier
;; $ cd XXX
;; $ eslint --init
(defun hurricane-programming/post-init-flycheck ()
  (with-eval-after-load 'flycheck
    (progn
      (setq flycheck-display-errors-delay 0.9)
      (setq flycheck-idle-change-delay 2.0)
      ;; (setq-default flycheck-disabled-checkers
      ;;   (append flycheck-disabled-checkers
      ;;     '(javascript-jshint json-jsonlint)))
      ;; (add-hook 'js2-mode-hook (lambda ()
      ;;                            (setq exec-path (cons "/usr/local/bin/node" exec-path))
      ;;                            (setq exec-path (cons "/usr/local/bin/eslint" exec-path))
      ;;                            (flycheck-mode)
      ;;                            (flycheck-add-mode 'javascript-eslint 'js2-mode)
      ;;                            (flycheck-select-checker 'javascript-eslint)))
      ;; (add-hook 'rjsx-mode-hook  'web-mode-init-hook)
      (add-hook 'rjsx-mode-hook (lambda ()
        (setq exec-path (cons "/usr/local/bin/node" exec-path))
        (setq exec-path (cons "/usr/local/bin/eslint" exec-path))
        (setq sgml-basic-offset 2)
        (setq js-indent-level 2)
        (web-mode-init-hook)
        (flycheck-select-checker 'javascript-standard)))
      )))
;; }}

(defun hurricane-programming/init-flycheck-package ()
  (use-package flycheck-package))

;; {{
;; $ npm install -g prettier
;; (defun hurricane-programming/init-prettier-js ()
;;   (use-package prettier-js
;;     :after web-mode
;;     :init
;;     (add-hook 'js2-mode-hook 'prettier-js-mode)
;;     (add-hook 'web-mode-hook 'prettier-js-mode)
;;     (add-hook 'react-mode-hook 'prettier-js-mode)
;;     :config
;;     (setq prettier-js-args '("--trailing-comma" "none"
;;                              "--bracket-spacing" "true"
;;                              "--no-semi" "false"
;;                              "--single-quote" "true"
;;                              "--jsx-single-quote" "true"
;;                              "--jsx-bracket-same-line" "true"
;;                              "--arrow-parens" "always"
;;                              "--insert-pragma true"))
;;     (defun enable-minor-mode (my-pair)
;;       "Enable minor mode if file-name match the regexp.  `my-pair' is a cons cell (regexp . minor-mode)."
;;       (if (buffer-file-name)
;;           (if (string-match (car my-pair) buffer-file-name)
;;               (funcall (cdr my-pair)))))
;;     (add-hook 'web-mode-hook #'(lambda ()
;;                                  (enable-minor-mode
;;                                   '("\\.js?\\'" . prettier-js-mode))))
;;     (add-hook 'web-mode-hook #'(lambda ()
;;                                  (enable-minor-mode
;;                                   '("\\.jsx?\\'" . prettier-js-mode))))))
;; }}

(defun hurricane-programming/init-standardfmt ()
  (use-package standardfmt
    :config
    (add-hook 'rjsx-mode-hook #'standardfmt-mode)))

(defun hurricane-programming/init-eslintfmt ()
  (use-package eslintfmt
    :config
    (setq eslintfmt-command-args (list "--config" eslintfmt-configuration-file))
    (add-hook 'js-mode-hook #'eslintfmt-mode)
    (add-hook 'js2-mode-hook #'eslintfmt-mode)))

;; (defun hurricane-programming/init-pythonfmt ()
;;   (use-package pythonfmt
;;     :config
;;     (setq pythonfmt-command "yapf")
;;     (setq pythonfmt-command-args "-i")
;;     (add-hook 'python-mode-hook #'pythonfmt-mode)))

(defun hurricane-programming/init-psearch ()
  (use-package psearch
    :ensure t))
