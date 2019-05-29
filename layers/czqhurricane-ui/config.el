;; {{
;; @see: http://philipdaniels.com/blog/2017/02/spacemacs---configuring-the-solarized-theme/
;; Get color-theme-solarized working. It is specified as an additional package
;; above. First we setup some theme modifications - we must do this *before*
;; we load the theme. Note that the color-theme-solarized package appears in
;; the list of themes as plain old 'solarized'.
(setq theming-modifications
    '((solarized
        ;; Provide a sort of "on-off" modeline whereby the current buffer has a nice
        ;; bright blue background, and all the others are in cream.
        ;; TODO: Change to use variables here. However, got error:
        ;; (Spacemacs) Error in dotspacemacs/user-config: Wrong type argument: stringp, pd-blue
        (mode-line :foreground "#e9e2cb" :background "#2075c7"  :distant-foreground "#e9e2cb" :foreground "#e9e2cb" :inverse-video nil)
        (mode-line-inactive :foreground "#2075c7" :background "#e9e2cb" :inverse-video nil)
        (powerline-active1 :foreground "#e9e2cb" :background "#2075c7" :inverse-video nil)
        (powerline-active2 :foreground "#e9e2cb" :background "#2075c7" :inverse-video nil)
        (mode-line-inactive :foreground "#2075c7" :background "#e9e2cb" :inverse-video nil)
        (powerline-inactive1 :foreground "#2075c7" :background "#e9e2cb" :inverse-video nil)
        (powerline-inactive2 :foreground "#2075c7" :background "#e9e2cb" :inverse-video nil)
        (company-scrollbar-fg :background "alternateSelectedControlColor")
        (cursor :background "#b58900")
        (ivy-current-match :inherit region)
        (helm-selection-line :inherit isearch)
        (ivy-minibuffer-match-face-2 :inherit isearch)
        (ivy-minibuffer-match-face-3 :inherit isearch)
        (org-format-latex-options :foreground default :background default :scale 3.0 :html-foreground "Black" :html-background "Transparent" :html-scale 1.0 :matchers ("begin" "$1" "$" "$$" "\\(" "\\["))
        ;; Turn this off to stop it interfering with mic-paren.
        (set-face-attribute 'sp-show-pair-match-face nil :foreground 'unspecified :background 'unspecified)
        (set-face-attribute 'sp-show-pair-mismatch-face nil :foreground 'unspecified :background 'unspecified)
        )))
;; }}
(set-terminal-parameter nil 'background-mode 'dark)
(set-frame-parameter nil 'background-mode 'dark)
(display-time-mode 1)
