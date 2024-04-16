(defconst hurricane-programming-packages
  '(
    flycheck
    dash-at-point
    virtualenvwrapper
    web-mode
    slime
    (standardfmt :location (recipe :fetcher github :repo "jimeh/standardfmt.el"))
    (eslintfmt :location (recipe :fetcher github :repo "czqhurricnae/eslintfmt.el"))
    ;; (pythonfmt :location (recipe :fetcher github :repo "czqhurricnae/pythonfmt.el"))
    ;; prettier-js
    (psearch :location (recipe :fetcher github :repo "twlz0ne/psearch.el" :files ("psearch.el")))
    (lsp-bridge :location (recipe
                           :fetcher github
                           :repo "manateelazycat/lsp-bridge"
                           :branch "master"
                           :files ("*.el" "*.py" "acm" "core" "langserver" "multiserver" "resources")
                           ;; do not perform byte compilation or native compilation for lsp-bridge
                           :build (:not compile)))
    ;; (lsp-bridge :location local)
    (color-rg :location (recipe :fetcher github
                              :repo "manateelazycat/color-rg"))
    (stm32 :location (recipe :fetcher github
                             :repo "SL-RU/stm32-emacs"))
    friendly-shell-command
    (dap-mode :location (recipe
                         :fetcher github
                         :repo "emacs-lsp/dap-mode"
                         :files ("*.el" "docs" "features" "icons")))
    exec-path-from-shell))

(defun hurricane-programming/init-virtualenvwrapper ()
  (use-package virtualenvwrapper
    :config
    (venv-initialize-interactive-shells)
    (venv-initialize-eshell)
    (setq venv-location virtualenv-dir)))

(defun hurricane-programming/post-init-web-mode ()
  (with-eval-after-load 'web-mode
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

;; {{
;; @See: https://github.com/slime/slime
;; @See: https://github.com/syl20bnr/spacemacs/tree/master/layers/%2Blang/common-lisp
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
;; @See: https://gist.github.com/CodyReichert/9dbc8bd2a104780b64891d8736682cea
;; @See: https://github.com/flycheck/flycheck/issues/997
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

(defun hurricane-programming/init-lsp-bridge ()
  (use-package lsp-bridge
    :config
    (global-lsp-bridge-mode)

    ;; @See: https://tuhdo.github.io/emacs-frame-peek.html
    (defun make-peek-frame (func filename filehost position)
      "Make a new frame for peeking definition"
      (let (summary
            doc-frame
            x y
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
            ;; 1. Find the absolute position of the current beginning of the symbol at point, ;;
            ;; in pixels.                                                                     ;;
          ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
            (abs-pixel-pos (save-excursion
                             (beginning-of-thing 'symbol)
                             (window-absolute-pixel-position))))
        (setq x (car abs-pixel-pos))
        ;; (setq y (cdr abs-pixel-pos))
        (setq y (+ (cdr abs-pixel-pos) (frame-char-height)))

      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;; 2. Create a new invisible frame, with the current buffer in it. ;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        (setq doc-frame (make-frame '((minibuffer . t)
                                      (name . "*Lsp-bridge Peek*")
                                      (width . 80)
                                      (visibility . nil)
                                      (height . 15))))

      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;; 3. Position the new frame right under the beginning of the symbol at point. ;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        (set-frame-position doc-frame x y)

      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;; 4. Jump to the symbol at point. ;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        (with-selected-frame doc-frame
          (funcall func filename filehost position)
          ;; (read-only-mode)
          (when global-semantic-stickyfunc-mode (semantic-stickyfunc-mode -1))
          (recenter-top-bottom 0))

      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;; 5. Make frame visible again ;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        (make-frame-visible doc-frame)
        (select-frame-set-input-focus doc-frame)))

    (advice-add #'lsp-bridge-define--jump :around #'make-peek-frame)

    (add-hook 'prog-mode-hook #'(lambda () (local-set-key (kbd "<f3>") #'lsp-bridge-find-def)))
    :custom
    (lsp-bridge-enable-search-words nil)
    (lsp-bridge-enable-hover-diagnostic t)
    ;; brew 升级 python3 为 python3.12，该版本没有 lsp-bridge 的依赖，所以使用3.11。
    (lsp-bridge-python-command "/usr/local/bin/python3.11")
    (lsp-bridge-enable-org-babel t)))

(defun hurricane-programming/init-color-rg ()
 (use-package color-rg
   :ensure t
   :load-path ("~/.emacs.d/elpa/29.3/develop/color-rg-20240331.104519")
   :config
   (define-key isearch-mode-map (kbd "M-s M-s") 'isearch-toggle-color-rg)
    ;; https://emacs.stackexchange.com/a/10588/22102
   (eval-after-load 'color-rg
     '(progn
        (evil-make-overriding-map color-rg-mode-map 'normal)
        (add-hook 'color-rg-mode-hook #'evil-normalize-keymaps)))))

(defun hurricane-programming/init-stm32 ()
  (use-package stm32
    :ensure t))

(defun hurricane-programming/init-friendly-shell-command ()
  (use-package friendly-shell-command
    :ensure t))

(defun hurricane-programming/init-dap-mode ()
  (use-package dap-mode
    :ensure t
    :config
    (dap-ui-mode 1)
    (dap-tooltip-mode 1)
    (tooltip-mode 1)
    (dap-ui-controls-mode 1)
    ;;@See: https://zhuanlan.zhihu.com/p/467681146
    (use-package dap-lldb
      :after dap-mode
      ;; 配置中明确指定了 lldb-vscode 的路径。这里的路径是 macOS 上通过 Homebrew 安装 llvm 的路径，其它平台的路径需要自行确定。
      :custom
      (dap-lldb-debug-program '("/usr/local/opt/llvm/bin/lldb-vscode"))
      ;; ask user for executable to debug if not specified explicitly (c++)
      (dap-lldb-debugged-program-function
       (lambda () (read-file-name "Select file to debug: "))))
    (require 'dap-gdb-lldb)
    (dap-gdb-lldb-setup)))

(defun hurricane-programming/init-exec-path-from-shell ()
  (use-package exec-path-from-shell
    :ensure t
    :init (exec-path-from-shell-initialize)))
