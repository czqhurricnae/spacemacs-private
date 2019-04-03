;;; packages.el --- czqhurricane layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: c <c@ubuntu>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;; See the Spacemacs documentation and FAQs for instructions on how to implement
;; a new layer:
;;
;;   SPC h SPC layers RET
;;
;;
;; Briefly, each package to be installed or configured by this layer should be
;; added to `czqhurricane-packages'. n, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `czqhurricane/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `czqhurricane/pre-init-PACKAGE' and/or
;;   `czqhurricane/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:
(defconst czqhurricane-programming-packages
  '(
    web-mode
    slime
    (exec-path-from-shell :location elpa)
    dumb-jump
    (color-rg :location (recipe :fetcher github :repo "manateelazycat/color-rg"))
;;  " list of Lisp packages required by the czqhurricane-programming layer.

;; Each entry is either:

;; 1. A symbol, which is interpreted as a package to be installed, or

;; 2. A list of the form (PACKAGE KEYS...), where PACKAGE is the
;;     name of the package to be installed or loaded, and KEYS are
;;     any number of keyword-value-pairs.
;;
;;      following keys are accepted:
;;
;;     - :excluded (t or nil): Prevent the package from being loaded
;;       if value is non-nil
;;
;;     - :location: Specify a custom installation location.
;;        following values are legal:
;;
;;       -  symbol `elpa' (default) means PACKAGE will be
;;         installed using the Emacs package manager.
;;
;;       -  symbol `local' directs Spacemacs to load the file at
;;         `./local/PACKAGE/PACKAGE.el'
;;
;;       - A list beginning with the symbol `recipe' is a melpa
;;         recipe.  See: https://github.com/milkypostman/melpa#recipe-format"
))

(defun czqhurricane-programming/post-init-yasnippet ()
    (progn
      (set-face-background 'secondary-selection "gray")
      (setq-default yas-prompt-functions '(yas-ido-prompt yas-dropdown-prompt))
      (mapc #'(lambda (hook) (remove-hook hook 'spacemacs/load-yasnippet)) '(prog-mode-hook
                                                                        org-mode-hook
                                                                        markdown-mode-hook))
      (spacemacs/add-to-hooks 'czqhurricane/load-yasnippet '(prog-mode-hook
                                                             org-mode-hook
                                                             markdown-mode-hook))))

(defun czqhurricane-programming/post-init-exec-path-from-shell ()
  (progn
    (when (memq window-system '(mac ns x))
      (exec-path-from-shell-initialize))
    (exec-path-from-shell-copy-env "PATH")))

(defun czqhurricane-programming/init-virtualenvwrapper ()
  (use-package virtualenvwrapper
    :config
    (progn
      (venv-initialize-interactive-shells)
      (venv-initialize-eshell)
      (setq venv-location virtualenv-dir))))

(defun czqhurricane-programming/post-init-web-mode ()
  (with-eval-after-load "web-mode"
    (web-mode-toggle-current-element-highlight)
    (web-mode-dom-errors-show))
  (setq company-backends-web-mode '((company-dabbrev-code
                                     company-keywords
                                     company-etags)
                                    company-files company-dabbrev)))

(defun czqhurricane-programming/post-init-dumb-jump ()
  (setq dumb-jump-selector 'ivy)
  (defun my-dumb-jump ()
    (interactive)
    (evil-set-jump)
    (dumb-jump-go-other-window))
  (global-set-key (kbd "C-x g") 'my-dumb-jump))

(defun czqhurricane-programming/init-ycmd ()
  (use-package ycmd
    :init
    (progn
      (set-variable 'ycmd-global-config "/Users/c/.ycm_extra_conf.py")
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
      (czqhurricane|toggle-company-backends company-ycmd)
      :config
      (eval-after-load 'ycmd
        '(spacemacs|hide-lighter ycmd-mode)))))

;; {{
;; @see: https://github.com/slime/slime
;; @see: https://github.com/syl20bnr/spacemacs/tree/master/layers/%2Blang/common-lisp
;; This layer defaults to using sbcl.
;; $ brew install sbcl
;; Set your lisp system and, optionally, some contribs.
(defun czqhurricane-programming/post-init-slime ()
  (setq inferior-lisp-program "/usr/local/opt/sbcl/bin/sbcl")
  (setq slime-contribs '(slime-fancy))
)
;; }}

;; {{
;; @see: https://github.com/manateelazycat/color-rg
;; $ brew install rg
(defun czqhurricane-programming/init-color-rg ()
  (use-package color-rg
    :config
    (add-to-list 'evil-emacs-state-modes 'color-rg-mode))
)
;; }}
