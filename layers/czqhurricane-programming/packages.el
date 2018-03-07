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
    yasnippet
    (exec-path-from-shell :location elpa)
;;  " list of Lisp packages required by the czqhurricane-better-defaults layer.

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
  (use-package yasnippet
    :ensure t
    :load-path "/Users/c/.spacemacs.d/snippets")
    :defer t
    :init
    (setq yas-snippet-dirs '("/Users/c/.spacemacs.d/snippets")
          yas-indent-line 'fixed
          yas-verbosity 0)
    (add-hook 'prog-mode-hook #'yas-minor-mode)
    :config
    (progn
      (yas-global-mode 1)
      (spacemacs|diminish yas-minor-mode)
      (set-face-background 'secondary-selection "gray")
      (setq-default yas-prompt-functions '(yas-ido-prompt yas-dropdown-prompt))
      (mapc #'(lambda (hook) (remove-hook hook 'spacemacs/load-yasnippet)) '(prog-mode-hook
                                                                             org-mode-hook
                                                                             markdown-mode-hook))

      (spacemacs/add-to-hooks 'czqhurricane/load-yasnippet '(prog-mode-hook
                                                             markdown-mode-hook
                                                             org-mode-hook))
      ))

(defun czqhurricane-programming/post-init-exec-path-from-shell ()
  (use-package exec-path-from-shell
    :config
    (progn
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))
(exec-path-from-shell-copy-env "PATH")
)))
;;; packages.el ends here

