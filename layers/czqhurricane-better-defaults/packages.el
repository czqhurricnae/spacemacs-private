;;; -*- lexical-binding: t -*-
;;; packages.el --- czqhurricane Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: c <c@ubuntu>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defconst czqhurricane-better-defaults-packages
  '(
    (youdao-dictionary :location elpa)
    (mic-paren :location elpa)
    (recentf :location elpa)
    (occur-mode :location local)
    (dired-mode :location local)))

(defun czqhurricane-better-defaults/init-youdao-dictionary ()
  (use-package youdao-dictionay
    :defer
    :init
    (spacemacs/set-leader-keys "hy" 'youdao-dictionary-search-at-point+)))

(defun czqhurricane-better-defaults/init-mic-paren ()
  (use-package mic-paren
    :config
    (setq blink-matching-paren nil)
    (paren-activate)
    (setq paren-match-face 'mode-line)))

(defun czqhurricane-better-defaults/post-init-recentf ()
  (progn
    (setq recentf-exclude
      '("COMMIT_MSG"
        "COMMIT_EDITMSG"
        "github.*txt$"
        "/tmp/"
        "/ssh:"
        "/sudo:"
        "/TAGS$"
        "/GTAGS$"
        "/GRAGS$"
        "/GPATH$"
        "\\.mkv$"
        "\\.mp[34]$"
        "\\.avi$"
        "\\.pdf$"
        "\\.sub$"
        "\\.srt$"
        "\\.ass$"
        ".*png$"))
    (setq recentf-max-saved-items 2048)))

(defun czqhurricane-better-defaults/init-dired-mode ()
  (use-package dired-mode
    :defer t
    :init
    (progn
      (require 'dired-x)
      (require 'dired-aux)
      (setq dired-dwin-target 1)
      (setq dired-listing-switches "-alh")
      (setq dired-guess-shell-alist-user
            '(("\\.pdf\\'" "open")
              ("\\.docx\\'" "open")
              ("\\.\\(?:djvu\\|eps\\)\\'" "open")
              ("\\.\\(?:jpg\\|jpeg\\|png\\|gif\\|xpm\\)\\'" "open")
              ("\\.\\(?:xcf\\)\\'" "open")
              ("\\.csv\\'" "open")
              ("\\.tex\\'" "open")
              ("\\.\\(?:mp4\\|mkv\\|avi\\|flv\\|ogv\\)\\(?:\\.part\\)?\\'"
               "open")
              ("\\.\\(?:mp3\\|flac\\)\\'" "open")
              ("\\.html?\\'" "open")
              ("\\.md\\'" "open")))

      (setq dired-omit-files
            (concat dired-omit-files "\\|^.DS_Store$\\|^.projectile$\\|\\.js\\.meta$\\|\\.meta$"))

      ;; Always delete and copy recursively.
      (setq dired-recursive-deletes 'always)
      (setq dired-recursive-copies 'always)

      ;; {{
      ;; @see: https://oremacs.com/2017/03/18/dired-ediff/
      (defun ora-ediff-files ()
        (interactive)
        (let ((files (dired-get-marked-files))
              (wnd (current-window-configuration)))
          (if (<= (length files) 2)
              (let ((file1 (car files))
                    (file2 (if (cdr files)
                               (cadr files)
                             (read-file-name
                              "file: "
                              (dired-dwim-target-directory)))))
                (if (file-newer-than-file-p file1 file2)
                    (ediff-files file2 file1)
                  (ediff-files file1 file2))
                (add-hook 'ediff-after-quit-hook-internal
                          (lambda ()
                            (setq ediff-after-quit-hook-internal nil)
                            (set-window-configuration wnd))))
            (error "no more than 2 files should be marked"))))
      ;; }}

      (defvar dired-filelist-cmd
        '(("vlc" "-L")))

      ;; FIXME: Evilify dired mode will lead to startup warnings.
      (evilified-state-evilify-map dired-mode-map
        :mode dired-mode
        :bindings
        (kbd "C-k") 'czqhurricane/dired-up-directory
        "<RET>" 'dired-find-alternate-file
        "E" 'dired-toggle-read-only
        "C" 'dired-do-copy
        "<mouse-2>" 'my-dired-find-file
        "`" 'dired-open-terminal
        "p" 'peep-dired-prev-file
        "n" 'peep-dired-next-file
        "z" 'dired-get-size
        "c" 'dired-copy-file-here
        "J" 'counsel-find-file
        "f" 'czqhurricane/open-file-with-projectile-or-counsel-git
        ")" 'dired-omit-mode)
      )))

(defun czqhurricane-better-defaults/init-profiler ()
  (use-package profiler
    :defer t
    :init
    (evilified-state-evilify profiler-report-mode profiler-report-mode-map)))

(defun czqhurricane-better-defaults/post-init-occur-mode ()
  "Auto switch to 'occur buffer'"
    (add-hook 'occur-hook
              '(lambda ()
                 (switch-to-buffer-other-window "*Occur*"))))
;;; packages.el ends here
