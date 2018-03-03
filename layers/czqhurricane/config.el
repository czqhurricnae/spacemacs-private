(global-linum-mode t)
(defvar org-agenda-dir ""
  "gtd org files location")

;; 自动切换到occur buffer
(add-hook 'occur-hook
          '(lambda ()
          (switch-to-buffer-other-window "*Occur*")))
(defvar deft-dir ""
  "deft org files locaiton")

(defvar blog-admin-dir ""
  "blog-admin files location")

(setq-default
 org-agenda-dir "~/org-notes"
 deft-dir "~/org-notes"
 blog-admin-dir "~/czqhurricane.com")
