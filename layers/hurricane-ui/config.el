(display-time-mode 1)
(setq my-flycheck-mode-line
        '(:eval
          (when
              (and (bound-and-true-p flycheck-mode)
                   (or flycheck-current-errors
                       (eq 'running flycheck-last-status-change)))
            (pcase flycheck-last-status-change
              ((\` not-checked) nil)
              ((\` no-checker) (propertize " -" 'face 'warning))
              ((\` running) (propertize " ✷" 'face 'success))
              ((\` errored) (propertize " !" 'face 'error))
              ((\` finished)
               (let* ((error-counts (flycheck-count-errors flycheck-current-errors))
                      (no-errors (cdr (assq 'error error-counts)))
                      (no-warnings (cdr (assq 'warning error-counts)))
                      (face (cond (no-errors 'error)
                                  (no-warnings 'warning)
                                  (t 'success))))
                 (propertize (format "[%s/%s]" (or no-errors 0) (or no-warnings 0))
                             'face face)))
              ((\` interrupted) " -")
              ((\` suspicious) '(propertize " ?" 'face 'warning))))))

(setq hurricane-icon t)

(setq tab-bar-separator ""
      tab-bar-new-tab-choice "*scratch*"
      tab-bar-tab-name-truncated-max 20
      tab-bar-auto-width nil
      tab-bar-close-button-show nil
      tab-bar-tab-hints t)
