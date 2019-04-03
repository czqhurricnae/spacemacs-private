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
(defconst czqhurricane-ui-packages
  '(
    (color-theme-solarized :location elpa)
    which-func
    pangu-spacing
    (awesome-tab :location (recipe :fetcher github :repo "manateelazycat/awesome-tab"))
;;  " list of Lisp packages required by the czqhurricane-ui layer.

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

(defun czqhurricane-ui/init-which-func ()
  (use-package which-func
  :init
  (progn
    ;; http://emacsredux.com/blog/2014/04/05/which-function-mode/
    ;; Show the current function name in the Power line.
    (which-function-mode 1)
    (if (not (member '(which-func-mode ("" which-func-format " ")) mode-line-format))
      (progn
      (push '(which-func-mode ("" which-func-format " ")) mode-line-format)))
  )
  :defer t
  :config
    (setq mode-line-format (remove '(which-func-mode ("" which-func-format " ")) mode-line-format))
  ))

(defun czqhurricane-ui/init-color-theme-solarized ()
  (use-package color-theme-solarized
  :init
  (spacemacs/load-theme 'solarized)
  :defer t))

(defun czqhurricane-ui/init-pangu-spacing ()
  (use-package pangu-spacing
  :init
  (progn
    ;; add toggle options
    (spacemacs|add-toggle toggle-pangu-spaceing
      :status pangu-spacing-mode
      :on (global-pangu-spacing-mode)
      :off (global-pangu-spacing-mode -1)
      :documentation "Toggle pangu spacing mode"
      :evil-leader "ots")
    (add-hook 'org-mode-hook
              #'(lambda ()
                  (set (make-local-variable 'pangu-spacing-real-insert-separtor) t)
                  (pangu-spacing-space-current-buffer))))))

;; {{
;; @see: https://github.com/manateelazycat/awesome-tab/blob/master/README.md
(defun czqhurricane-ui/init-awesome-tab ()
  (use-package awesome-tab
    :load-path "~/.emacs.d/elpa/awesome-tab"
    :config
    (awesome-tab-mode t)
    (setq awesome-tab-background-color "#002b36")
    (setq awesome-tab-style "bar")
))
;; }}
;;; packages.el ends here
