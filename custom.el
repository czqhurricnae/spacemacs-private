;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#0a0814" "#f2241f" "#67b11d" "#b1951d" "#4f97d7" "#a31db1" "#28def0" "#b2b2b2"])
 '(debug-on-error t)
 '(doom-modeline-buffer-file-name-style (quote truncate-with-project))
 '(doom-modeline-icon t)
 '(doom-modeline-major-mode-icon nil)
 '(doom-modeline-minor-modes nil)
 '(doom-themes-enable-bold t)
 '(doom-themes-enable-italic t)
 '(evil-want-Y-yank-to-eol t)
 '(evil-want-y-yank-to-eol t)
 '(fci-rule-color "#D6D6D6")
 '(hl-todo-keyword-faces
   (quote
    (("TODO" . "#dc752f")
     ("NEXT" . "#dc752f")
     ("THEM" . "#2d9574")
     ("PROG" . "#4f97d7")
     ("OKAY" . "#4f97d7")
     ("DONT" . "#f2241f")
     ("FAIL" . "#f2241f")
     ("DONE" . "#86dc2f")
     ("NOTE" . "#b1951d")
     ("KLUDGE" . "#b1951d")
     ("HACK" . "#b1951d")
     ("TEMP" . "#b1951d")
     ("FIXME" . "#dc752f")
     ("XXX+" . "#dc752f")
     ("\\?\\?\\?+" . "#dc752f"))))
 '(jdee-db-active-breakpoint-face-colors (cons "#FFFBF0" "#268bd2"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#FFFBF0" "#859900"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#FFFBF0" "#E1DBCD"))
 '(notmuch-search-line-faces
   (quote
    (("unread" :foreground "#aeee00")
     ("flagged" :foreground "#0a9dff")
     ("deleted" :foreground "#ff2c4b" :bold t))))
 '(objed-cursor-color "#dc322f")
 '(org-pomodoro-length 25)
 '(org-roam-capture-templates
   (quote
    (("d" "default" plain
      (function org-roam-capture--get-point)
      "%?" :file-name "${slug}" :head "#+SETUPFILE: ../theme-rose.setup
#+DATE: %T
#+TITLE: ${title}
" :unnarrowed t))))
 '(org-roam-directory
   "~/Library/Mobile Documents/iCloud~com~appsonthemove~beorg/Documents/org-notes/notes/")
 '(org2ctex-latex-commands
   (quote
    ("xelatex -shell-escape -interaction nonstopmode -output-directory %o %f" "bibtex %b" "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f" "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f")))
 '(org2ctex-latex-packages-alist
   (quote
    ("
%%% 默认使用的latex宏包 %%%
\\usepackage{tikz}
\\usepackage{CJKulem}
\\usepackage{graphicx}

%%% 设置页面边距 %%%
\\usepackage[top=2.54cm, bottom=2.54cm, left=3.17cm, right=3.17cm]{geometry}")))
 '(package-selected-packages
   (quote
    (hide-mode-line pythonfmt doom-modeline shrink-path latex-preview-pane org2ctex color-rg doom-todo-ivy pangu-spacing org-protocol-capture-html pandoc-mode elpy find-file-in-project dash-at-point org-plus-contrib ghub org-mime nodejs-repl slime ob-ipython virtualenvwrapper ox-reveal ox-gfm ein websocket flycheck-ycmd company-ycmd ycmd request-deferred let-alist deferred company-quickhelp vimish-fold origami web-beautify livid-mode skewer-mode simple-httpd json-mode json-snatcher json-reformat js2-refactor multiple-cursors js2-mode js-doc company-tern dash-functional tern coffee-mode web-mode tagedit slim-mode scss-mode sass-mode pug-mode less-css-mode haml-mode emmet-mode company-web web-completion-data reveal-in-osx-finder pbcopy osx-trash osx-dictionary launchctl color-theme-solarized color-theme mic-paren pyim pyim-basedict fasd youdao-dictionary names chinese-word-at-point wgrep smex pdf-tools tablist org-category-capture ivy-hydra flyspell-correct-ivy disaster counsel-projectile counsel swiper company-c-headers cmake-mode clang-format ivy unfill smeargle orgit org-projectile org-present org-pomodoro alert log4e gntp org-download mwim mmm-mode markdown-toc markdown-mode magit-gitflow htmlize helm-gitignore helm-company helm-c-yasnippet gnuplot gitignore-mode gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link gh-md fuzzy flyspell-correct-helm flyspell-correct flycheck-pos-tip pos-tip flycheck evil-magit magit magit-popup git-commit with-editor company-statistics company-anaconda company auto-yasnippet yasnippet auto-dictionary ac-ispell auto-complete yapfify pyvenv pytest pyenv-mode py-isort pip-requirements live-py-mode hy-mode helm-pydoc cython-mode anaconda-mode pythonic ws-butler winum which-key volatile-highlights vi-tilde-fringe uuidgen use-package toc-org spaceline powerline restart-emacs request rainbow-delimiters popwin persp-mode pcre2el paradox spinner org-bullets open-junk-file neotree move-text macrostep lorem-ipsum linum-relative link-hint info+ indent-guide hydra hungry-delete hl-todo highlight-parentheses highlight-numbers parent-mode highlight-indentation hide-comnt help-fns+ helm-themes helm-swoop helm-projectile helm-mode-manager helm-make projectile pkg-info epl helm-flx helm-descbinds helm-ag google-translate golden-ratio flx-ido flx fill-column-indicator fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-lisp-state smartparens evil-indent-plus evil-iedit-state iedit evil-exchange evil-escape evil-ediff evil-args evil-anzu anzu evil goto-chg undo-tree eval-sexp-fu highlight elisp-slime-nav dumb-jump f s diminish define-word column-enforce-mode clean-aindent-mode bind-map bind-key auto-highlight-symbol auto-compile packed dash aggressive-indent adaptive-wrap ace-window ace-link ace-jump-helm-line helm avy helm-core popup async)))
 '(pangu-spacing-inhibit-mode-alist
   (quote
    (eshell-mode shell-mode term-mode dired-mode fundamental-mode text-mode)))
 '(paradox-github-token t)
 '(pdf-view-midnight-colors (quote ("#b2b2b2" . "#292b2e")))
 '(pyfmt-command "black")
 '(request-backend (quote curl))
 '(rime-librime-root "~/.emacs.d/librime/dist" t)
 '(rustic-ansi-faces
   ["#FDF6E3" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#556b72"])
 '(safe-local-variable-values
   (quote
    ((eval setq org-download-image-dir
           (concat default-directory "screenshotImg/自动油门电门组件/"))
     (eval setq org-download-image-dir
           (concat default-directory "screenshotImg/737NG 飞机 APU 启动发电机基本原理浅析/"))
     (eval setq org-download-image-dir
           (concat default-directory "screenshotImg/737NG APU 滑油滤压差指示器弹出/"))
     (eval setq org-download-image-dir
           (concat default-directory "screenshotImg/737NG 的 APU 爆炸帽灯点亮的故障分析/"))
     (eval setq org-download-image-dir
           (concat default-directory "screenshotImg/When_to_use_weak_references_in_python/"))
     (eval setq org-download-image-dir
           (concat default-directory "screenshotImg/Accessibility: sr-only or aria-label/"))
     (eval setq org-download-image-dir
           (concat default-directory "screenshotImg/Flask-0.1/"))
     (eval setq org-download-image-dir
           (concat default-directory "screenshotImg/filter_python_list/"))
     (eval setq org-download-image-dir
           (concat default-directory "screenshotImg/Using_org_babel_to_enable_virtualenv/"))
     (eval setq org-download-image-dir
           (concat default-directory "screenshotImg/what_does_sudo_h_do/"))
     (eval setq org-download-image-dir
           (concat default-directory "screenshotImg/container document/"))
     (eval setq org-download-image-dir
           (concat default-directory "screenshotImg/Custom_Flask-Admin_list_action"))
     (eval setq org-download-image-dir
           (concat default-directory "screenshotImg/get_content_of_a_buffer/"))
     (eval setq org-download-image-dir
           (concat default-directory "screenshotImg/how_to_reference_named_table_or_code_block_in_org_mode/"))
     (eval setq org-download-image-dir
           (concat default-directory "screenshotImg/维 a 酸使用注意事项/"))
     (eval setq org-download-image-dir
           (concat default-directory "screenshotImg/fix_columns_in_horizontal_scrolling/"))
     (eval setq org-download-image-dir
           (concat default-directory "/screenshotImg/Flask-SQLAlchemy_db_create_all_not_creating_database/"))
     (eval setq org-download-image-dir
           (concat default-directory "screenshotImg/sae 部署/"))
     (eval setq org-download-image-dir
           (concat default-directory "screenshotImg/Python 2-MRO_and_super/"))
     (eval setq org-download-image-dir
           (concat default-directory "screenshotImg/Python-asyncio_gather_vs_asyncio_wait/"))
     (eval setq org-download-image-dir
           (concat default-directory "screenshotImg/Python-协程/"))
     (eval setq org-download-image-dir
           (concat default-directory "screenshotImg/Nginx 怎么转发/"))
     (eval setq org-download-image-dir
           (concat default-directory "screenshotImg/PyCharm 激活/"))
     (eval setq org-download-image-dir
           (concat default-directory "screenshotImg/evil-guide/"))
     (eval setq org-download-image-dir
           (concat default-directory "screenshotImg/MacOS 无法登录 App Store 修复/"))
     (eval setq org-download-image-dir
           (concat default-directory "screenshotImg/使用 lunr 实现静态博客的前端搜索/"))
     (eval setq org-download-image-dir
           (concat default-directory "screenshotImg/Graphviz/"))
     (eval setq org-download-image-dir
           (concat default-directory "screenshotImg/对于植发的看法，以及一些疑问的解答/"))
     (eval setq org-download-image-dir
           (concat default-directory "screenshotImg/CSS3 常用选择器一览表/"))
     (eval setq org-download-image-dir
           (concat default-directory "screenshotImg/CSS-position/"))
     (eval setq org-download-image-dir
           (concat default-directory "screenshotImg/CSS 基础/"))
     (eval setq org-download-image-dir
           (concat default-directory "screenshotImg/Taro/"))
     (eval setq org-download-image-dir
           (concat default-directory "screenshotImg/小助手使用说明/"))
     (eval setq org-download-image-dir
           (concat default-directory "screenshotImg/Add_annotation_in_PDF_src_block/"))
     (eval setq org-download-image-dir
           (concat default-directory "screenshotImg/org 文件树生成/"))
     (eval setq org-download-image-dir
           (concat default-directory "screenshotImg/org 表格/"))
     (eval setq org-download-image-dir
           (concat default-directory "screenshotImg/org-mode 转 latex ，设置 block 的 latex 选项/"))
     (eval setq org-download-image-dir
           (concat default-directory "screenshotImg/Latex 实现图文混排/"))
     (eval setq org-download-image-dir
           (concat default-directory "screenshotImg/react-dates 支持对过去时间的选择/"))
     (eval setq org-download-image-dir
           (concat default-directory "screenshotImg/React.js 小书/"))
     (eval setq org-download-image-dir
           (concat default-directory "screenshotImg/Python WSGI/"))
     (eval setq org-download-image-dir
           (concat default-directory "screenshotImg/电源准备/"))
     (eval setq org-download-image-dir
           (concat default-directory "screenshotImg/Python 2-wraps/"))
     (eval setq org-download-image-dir
           (concat default-directory "screenshotImg/航线线路施工及检查/"))
     (eval setq org-download-image-dir
           (concat default-directory "screenshotImg/How_to_compute_options_of_dropdown_for_column_filter_in_Flask_Adminn/"))
     (eval setq org-download-image-dir
           (concat default-directory "screenshotImg/Ubuntu 查看用户和用户组/"))
     (eval setq org-download-image-dir
           (concat default-directory "screenshotImg/Flask 子域名/"))
     (eval setq org-download-image-dir
           (concat default-directory "screenshotImg/Flask-Admin/"))
     (eval setq org-download-image-dir
           (concat default-directory "screenshotImg/flask-0.1/"))
     (eval setq org-download-image-dir
           (concat default-directory "screenshotImg/Let's_build_a_web_server/"))
     (eval setq org-download-image-dir
           (concat default-directory "screenshotImg/Flask + Docker 部署/"))
     (eval setq org-download-image-dir
           (concat default-directory "screenshotImg/Flask-SQLAlchemy 外键多对多关系/"))
     (eval setq org-download-image-dir
           (concat default-directory "screenshotImg/JavaScript-继承/"))
     (eval setq org-download-image-dir
           (concat default-directory "screenshotImg/React 构造函数中为什么要将类方法绑定到 this/"))
     (eval setq org-download-image-dir
           (concat default-directory "screenshotImg/JavaScript-原型链/"))
     (eval setq org-download-image-dir
           (concat default-directory "screenshotImg/原生 JavaScript 实现滑动进度条/"))
     (eval setq org-download-image-dir
           (concat default-directory "screenshotImg/Python-how_do_coroutines_work/"))
     (eval setq org-download-image-dir
           (concat default-directory "screenshotImg/Python 线程基础/"))
     (eval setq org-download-image-dir
           (concat default-directory "screenshotImg/Copy_with_JSONDecodeError_in_requests.get().json()_in_Python_2_and_3/"))
     (eval setq org-download-image-dir
           (concat default-directory "screenshotImg/Python 2-attribute_and_property/"))
     (eval setq org-download-image-dir
           (concat default-directory "screenshotImg/Python-MRO/"))
     (eval setq org-download-image-dir
           (concat default-directory "screenshotImg/Python 2-bound_method_and_unbound_method_and_classmethod_and_staticmethod/"))
     (eval setq org-download-image-dir
           (concat default-directory "screenshotImg/Git --force-with-lease/"))
     (eval setq org-download-image-dir
           (concat default-directory "screenshotImg/printf() 函数转换说明/"))
     (eval setq org-download-image-dir
           (concat default-directory "screenshotImg/数据类型和流程控制/"))
     (eval setq org-download-image-dir
           (concat default-directory "screenshotImg/设置断点的原理/"))
     (eval setq org-download-image-dir
           (concat default-directory "screenshotImg/SAP 章节号使用 BeautifulSoup 抓取/"))
     (eval setq org-download-image-dir
           (concat default-directory "screenshotImg/飞机部件电气测量设备参考/"))
     (eval setq org-download-image-dir
           (concat default-directory "screenshotImg/短跑道构型/"))
     (eval setq org-download-image-dir
           (concat default-directory "screenshotImg/短跑道构型自动减速板未预位灯亮/"))
     (eval setq org-download-image-dir
           (concat default-directory "screenshotImg/WDM 符号说明/"))
     (eval setq org-download-image-dir
           (concat default-directory "screenshotImg/通过插头来查找插钉件号/"))
     (eval setq org-download-image-dir
           (concat default-directory "screenshotImg/WDM 使用说明/"))
     (eval setq org-download-image-dir
           (concat default-directory "screenshotImg/737NG 飞机面板/"))
     (eval setq org-download-image-dir
           (concat default-directory "screenshotImg/737NG 按压式烧水杯电门更换/"))
     (eval setq org-download-image-dir
           (concat default-directory "screenshotImg/桥式整流器/"))
     (eval setq org-download-image-dir
           (concat default-directory "screenshotImg/APU 用电瓶无法起动，外电源起动成功故障特例/"))
     (eval setq org-download-image-dir
           (concat default-directory "screenshotImg/Flask 源码/"))
     (eval setq org-download-image-dir
           (concat default-directory "screenshotImg/emacs org 参考文献工具 org-ref 简介/"))
     (eval setq org-download-image-dir
           (concat default-directory "screenshotImg/JavaScript-basic/"))
     (eval setq org-download-image-dir
           (concat default-directory "screenshotImg/JavaScript-call_stack_callback_queue_event_loop/"))
     (eval setq org-download-image-dir
           (concat default-directory "screenshotImg/Custom_Flask-Admin_list_action/"))
     (eval setq org-download-image-dir
           (concat default-directory "screenshotImg/如何做到像使用 LaTex 那样优雅地使用 Word/"))
     (eval setq org-download-image-dir
           (concat default-directory "screenshotImg/Python 异常处理/"))
     (eval setq org-download-image-dir
           (concat default-directory "screenshotImg/反推工作原理/"))
     (eval setq org-download-image-dir
           (concat default-directory "screenshotImg/737NG 反推保留 M 程序/"))
     (eval setq org-download-image-dir
           (concat default-directory "screenshotImg/737-800 空调温度控制/"))
     (eval setq org-download-image-dir
           (concat default-directory "screenshotImg/737-800 空调平衡和不平衡/"))
     (typescript-backend . tide)
     (typescript-backend . lsp)
     (javascript-backend . tide)
     (javascript-backend . tern)
     (javascript-backend . lsp))))
 '(vc-annotate-background "#FDF6E3")
 '(vc-annotate-color-map
   (list
    (cons 20 "#859900")
    (cons 40 "#959300")
    (cons 60 "#a58e00")
    (cons 80 "#b58900")
    (cons 100 "#bc7407")
    (cons 120 "#c35f0e")
    (cons 140 "#cb4b16")
    (cons 160 "#cd4439")
    (cons 180 "#d03d5d")
    (cons 200 "#d33682")
    (cons 220 "#d63466")
    (cons 240 "#d9334a")
    (cons 260 "#dc322f")
    (cons 280 "#dd5c56")
    (cons 300 "#de867e")
    (cons 320 "#dfb0a5")
    (cons 340 "#D6D6D6")
    (cons 360 "#D6D6D6")))
 '(vc-annotate-very-old-color nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(awesome-tab-selected ((t (:background "#002b36" :foreground "white"))))
 '(company-scrollbar-fg ((t (:background "alternateSelectedControlColor"))))
 '(cursor ((t (:background "#b58900"))))
 '(doom-modeline-bar ((t (:backgroud "#6272a4"))))
 '(expand-region-contract-fast-key "V")
 '(expand-region-exclude-text-mode-expansions (quote (html-mode nxml-mode web-mode)))
 '(expand-region-reset-fast-key "r")
 '(helm-move-to-line-cycle-in-source t)
 '(set-face-attribute ((t ((quote sp-show-pair-match-face) nil :foreground (quote unspecified) :background (quote unspecified)))))
 '(set-frame-parameter ((t (nil (quote background-mode) (quote dark)))))
 '(set-terminal-parameter ((t (nil (quote background-mode) (quote dark)))))
 '(spacemacs-iedit-face ((t (:background "firebrick1" :foreground "#2075c7" :inherit (quote mode-line)))))
 '(spacemacs-iedit-insert-face ((t (:background "firebrick1" :foreground "#2075c7" :inherit (quote mode-line)))))
 '(which-func ((t (:inherit modeline)))))
