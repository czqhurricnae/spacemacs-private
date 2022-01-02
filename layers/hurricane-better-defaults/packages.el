(defconst hurricane-better-defaults-packages
  '(
    (youdao-dictionary :location elpa)
    (mic-paren :location elpa)
    (recentf :location elpa)
    (occur-mode :location local)
    (dired-mode :location local)
    counsel))

(defun hurricane-better-defaults/pre-init-youdao-dictionary ()
  (use-package youdao-dictionary
    :commands youdao-dictionary-play-voice-of-current-word
    :bind (("C-c y" . my-youdao-search-at-point)
           ("C-c Y" . youdao-dictionary-search-at-point)
           :map youdao-dictionary-mode-map
           ("h" . youdao-dictionary-hydra/body)
           ("?" . youdao-dictionary-hydra/body))
    :init
    (setq url-automatic-caching t
          youdao-dictionary-use-chinese-word-segmentation t)

    (defun my-youdao-search-at-point ()
      (interactive)
      (if (display-graphic-p)
          (if emacs/>=26p
              (youdao-dictionary-search-at-point-posframe)
            (youdao-dictionary-search-at-point-tooltip))
        (youdao-dictionary-search-at-point)))
    :config
    (with-eval-after-load 'hydra
      (defhydra youdao-dictionary-hydra (:color blue)
        ("p" youdao-dictionary-play-voice-of-current-word "play voice of current word")
        ("y" youdao-dictionary-play-voice-at-point "play voice at point")
        ("q" quit-window "quit")
        ("C-g" quit-window "quit")
        ("h" nil nil)
        ("?" nil nil)))
    (add-hook 'youdao-dictionary-mode-hook
              (lambda ()
                (define-key evil-normal-state-local-map
                  (kbd "h") #'youdao-dictionary-hydra/body)))))

(defun hurricane-better-defaults/init-mic-paren ()
  (use-package mic-paren
    :config
    (setq blink-matching-paren nil)
    (paren-activate)
    (setq paren-match-face 'mode-line)))

(defun hurricane-better-defaults/post-init-recentf ()
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

(defun hurricane-better-defaults/init-dired-mode ()
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
                            (if wnd
                                (set-window-configuration wnd)))))
            (error "No more than 2 files should be marked"))))
      ;; }}

      (defvar dired-filelist-cmd
        '(("vlc" "-L")))

      ;; FIXME: Evilify dired mode will lead to startup warnings.
      (evilified-state-evilify-map dired-mode-map
        :mode dired-mode
        :bindings
        "E" 'dired-toggle-read-only
        "C" 'dired-do-copy
        "<mouse-2>" #'hurricane//dired-find-file
        "`" 'hurricane/dired-open-terminal
        "p" 'peep-dired-prev-file
        "n" 'peep-dired-next-file
        "z" 'dired-get-size
        "c" 'hurricane/dired-copy-file-here
        "J" 'counsel-find-file
        "f" 'hurricane/open-file-with-projectile-or-counsel-git
        ")" 'dired-omit-mode)
      )))

(defun hurricane-better-defaults/init-profiler ()
  (use-package profiler
    :defer t
    :init
    (evilified-state-evilify profiler-report-mode profiler-report-mode-map)))

(defun hurricane-better-defaults/post-init-occur-mode ()
  "Auto switch to `occur buffer'."
    (add-hook 'occur-hook
              '(lambda ()
                 (switch-to-buffer-other-window "*Occur*"))))


(defun hurricane-better-defaults/post-init-counsel ()
  (with-eval-after-load 'counsel
    (ivy-add-actions
     'counsel-find-file
     '(("!" hurricane//open-file-in-external-app "@ Open file in external app")
       ("g" hurricane//find-file-in-git-repo "@ Find file in git repo")
       ("S" hurricane//ivy-ff-checksum-action "@ Checksum")
       ))
    (setq ivy-initial-inputs-alist nil)))
