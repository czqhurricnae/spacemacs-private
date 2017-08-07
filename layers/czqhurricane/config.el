(global-linum-mode t)

;; 自动切换到occur buffer
(add-hook 'occur-hook
          '(lambda ()
          (switch-to-buffer-other-window "*Occur*")))
