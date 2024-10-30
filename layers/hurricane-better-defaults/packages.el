(defconst hurricane-better-defaults-packages
  '(
    (youdao-dictionary :location elpa)
    (mic-paren :location elpa)
    (recentf :location elpa)
    (occur-mode :location local)
    (dired :location local)
    counsel
    (image-mode :location built-in)
    (dired+ :location (recipe :fetcher github :repo "emacsmirror/dired-plus"))))

(defun hurricane-better-defaults/pre-init-youdao-dictionary ()
  (use-package youdao-dictionary
    :commands youdao-dictionary-play-voice-of-current-word
    :bind (("C-c S" . hurricane/youdao-search-at-point)
           ;; ("C-c Y" . youdao-dictionary-search-at-point)
           :map youdao-dictionary-mode-map
           ("h" . youdao-dictionary-hydra/body)
           ("?" . youdao-dictionary-hydra/body))
    :init
    (setq url-automatic-caching t
          youdao-dictionary-use-chinese-word-segmentation t)

    (defun hurricane/youdao-search-at-point ()
      (interactive)
      (if (display-graphic-p)
          (if emacs/>=26p
              (progn
                (youdao-dictionary-play-voice-at-point)
                (youdao-dictionary-search-at-point-posframe))
            (youdao-dictionary-search-at-point-tooltip))
        (youdao-dictionary-search-at-point)))

    :config
    (setq youdao-dictionary-app-key "622ac768f5eb280a")
    (setq youdao-dictionary-secret-key "Wz5qQqTxQ8yYtcZhKMhyCpkfQ1yPtVKZ")
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
                            (kbd "h") #'youdao-dictionary-hydra/body)))
    (setq url-proxy-services
          '(("http"     . "localhost:7890")
            ("https"     . "localhost:7890")
            ("ftp"     . "localhost:7890")
            ("no_proxy" . "\\(localhost\\|127.0.0.1\\|.?baidu.com\\|.?youdao.com\\)")))))

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

(defun hurricane-better-defaults/post-init-dired ()
  (use-package dired
    :config
    (require 'dired-x)
    (require 'dired-aux)
    (when sys/macp
      (setq dired-use-ls-dired nil))
    (setq dired-dwin-target 1)
    ;; 对于 Mac 需要先安装 gls
    ;; brew install coreutils
    (setq insert-directory-program "gls")
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
    ;; @See: https://oremacs.com/2017/03/18/dired-ediff/
    ;; (defun ora-ediff-files ()
    ;;   (interactive)
    ;;   (let ((files (dired-get-marked-files))
    ;;         (wnd (current-window-configuration)))
    ;;     (if (<= (length files) 2)
    ;;         (let ((file1 (car files))
    ;;               (file2 (if (cdr files)
    ;;                          (cadr files)
    ;;                        (read-file-name
    ;;                         "file: "
    ;;                         (dired-dwim-target-directory)))))
    ;;           (if (file-newer-than-file-p file1 file2)
    ;;               (ediff-files file2 file1)
    ;;             (ediff-files file1 file2))
    ;;           (add-hook 'ediff-after-quit-hook-internal
    ;;                     (lambda ()
    ;;                       (setq ediff-after-quit-hook-internal nil)
    ;;                       (if wnd
    ;;                           (set-window-configuration wnd)))))
    ;;       (error "No more than 2 files should be marked"))))
    ;; }}

    (defvar dired-filelist-cmd
      '(("vlc" "-L")))

    ;; (evil-define-key 'normal dired-mode-map (kbd "W") #'hurricane//dired-copy-filename-as-kill)
    (evil-define-key 'normal dired-mode-map (kbd "/") #'hurricane/open-file-with-projectile-or-counsel-git)

    ;; FIXME: Evilify dired mode will lead to startup warnings.
    (evilified-state-evilify-map dired-mode-map
      :mode dired-mode
      :bindings
      "E" 'dired-toggle-read-only
      "C" 'dired-do-copy
      "<mouse-2>" #'hurricane//dired-find-file
      "`" #'eaf-open-in-file-manager
      "p" #'peep-dired-prev-file
      "n" #'peep-dired-next-file
      "z" #'dired-get-size
      "c" #'hurricane/dired-copy-file-here
      "/" #'hurricane/open-file-with-projectile-or-counsel-git
      ")" #'dired-omit-mode
      ;; "W" #'hurricane//dired-copy-filename-as-kill
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
       ("d" hurricane//find-file-delete-file "@ Delete file")
       ("g" hurricane//find-file-in-git-repo "@ Find file in git repo")
       ("S" hurricane//ivy-ff-checksum-action "@ Checksum")
       ("e" eaf-open-in-file-manager "@ Open file in eaf file manager")
       ("w" hurricane//find-file-copy-filename-as-kill "@ Copy filename")
       ("W" hurricane//find-file-copy-abs-filename-as-kill "@ Copy absolute filename")
       ("C" hurricane//find-file-copy-file-to "@ Copy to")
       ("m" hurricane//find-file-move-file-to "@ Move to")
       ("E" hurricane/find-file-html-or-markdown-to-org "@ Convert to org")
       ("p" hurricane//find-file-org-pandoc-import-to-org "@ Import to org")
       ("t" find-file-other-tab "@ find-file-other-tab")
       ("c" gptel-context-add-file "@ gptel-context-add-file")))

    (ivy-add-actions
     'counsel-recentf
     '(("t" find-file-other-tab "@ find-file-other-tab")
       ("c" gptel-context-add-file "@ gptel-context-add-file")))

    (ivy-add-actions
     'spacemacs/counsel-recentf
     '(("t" find-file-other-tab "@ find-file-other-tab")
       ("c" gptel-context-add-file "@ gptel-context-add-file")))

    (ivy-add-actions
     'counsel-bookmark
     '(("s" hurricane//bookmark-search-from-action "@search-from")
       ("e" hurricane//bookmark-open-in-file-manager-action "@ Open file in eaf file manager")
       ("b" hurricane//browse-url "@browse url")))

    (ivy-add-actions
     'counsel-file-jump
     '(("w" hurricane//file-jump-copy-filename-as-kill "@ Copy filename")
       ("W" hurricane//file-jump-copy-abs-filename-as-kill "@ Copy absolute filename")
       ("!" hurricane//file-jump-open-file-in-external-app "@ Open file in external app")
       ("d" hurricane//file-jump-delete-file "@ Delete file")
       ("C" hurricane//dired-copy-file-to "@ Copy to")
       ("m" hurricane//dired-move-file-to "@ Move to")
       ("c" gptel-context-add-file "@ gptel-context-add-file")))

    (ivy-add-actions
     'counsel-git
     '(("w" hurricane//file-jump-copy-filename-as-kill "@ Copy filename")
       ("W" hurricane//file-jump-copy-abs-filename-as-kill "@ Copy absolute filename")
       ("!" hurricane//file-jump-open-file-in-external-app "@ Open file in external app")
       ("d" hurricane//file-jump-delete-file "@ Delete file")
       ("C" hurricane//dired-copy-file-to "@ Copy to")
       ("m" hurricane//dired-move-file-to "@ Move to")
       ("c" gptel-context-add-file "@ gptel-context-add-file")))

    (dolist (action '(spacemacs/counsel-search counsel-rg counsel-ag))
      (ivy-add-actions
       action
       '(("t" hurricane//find-file-other-tab "@ find-file-other-tab")
         ("c" gptel-context-add-file "@ gptel-context-add-file"))))

    (setq ivy-initial-inputs-alist nil)))

(defun hurricane-better-defaults/post-init-image-mode ()
  (use-package image-mode
    :defer t
    :config
    (evilified-state-evilify-map image-mode-map
      :mode image-mode
      :bindings
      "n" #'image-next-file)))

(defun hurricane-better-defaults/init-dired+ ()
  (use-package dired+
    :defer t
    :init
    (progn
      (setq diredp-hide-details-initially-flag t)
      (setq diredp-hide-details-propagate-flag t)
      ;; use single buffer for all dired navigation
      ;; disable font themeing from dired+
      (setq font-lock-maximum-decoration (quote ((dired-mode . 1) (t . t))))
      (toggle-diredp-find-file-reuse-dir 1))
    :config
    (evil-define-key 'normal dired-mode-map (kbd "W") #'diredp-copy-abs-filenames-as-kill)

    (evilified-state-evilify-map dired-mode-map
      :mode dired-mode
      :bindings
      "W" #'diredp-copy-abs-filenames-as-kill
      )))
