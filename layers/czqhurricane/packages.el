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

(defconst czqhurricane-packages
  '(youdao-dictionary
    mic-paren
    )
  (occur-mode :location build-in)
;;  " list of Lisp packages required by the czqhurricane layer.

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
)

(defun czqhurricane/init-youdao-dictionary()
  (use-package youdao-dictionay
    :defer
    :init
    (spacemacs/set-leader-keys "hy" 'youdao-dictionary-search-at-point+)
    )
)

(defun czqhurricane/init-mic-paren()
  (use-package mic-paren
    :config
    (setq blink-matching-paren nil)
    (paren-activate)
    (setq paren-match-face 'mode-line)
  )
)

(defun czqhurricane/post-init-projectile ()
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
          (multi-occur (projectile-project-buffers) my-simple-todo-regex)
        (occur my-simple-todo-regex)))
    (spacemacs/set-leader-keys "pf" 'czqhurricane/open-file-with-projectile-or-counsel-git)
    (spacemacs/set-leader-keys "pt" 'my-simple-todo)))

;;; packages.el ends here
