;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#0a0814" "#f2241f" "#67b11d" "#b1951d" "#4f97d7" "#a31db1" "#28def0" "#b2b2b2"])
 '(custom-safe-themes
   '("fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" default))
 '(debug-on-error t)
 '(doom-modeline-buffer-file-name-style 'truncate-with-project)
 '(doom-modeline-icon t)
 '(doom-modeline-major-mode-icon nil)
 '(doom-modeline-minor-modes nil)
 '(evil-want-Y-yank-to-eol nil)
 '(evil-want-y-yank-to-eol t)
 '(helm-mode nil)
 '(hl-todo-keyword-faces
   '(("TODO" . "#dc752f")
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
     ("\\?\\?\\?+" . "#dc752f")))
 '(jdee-db-active-breakpoint-face-colors (cons "#FFFBF0" "#268bd2"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#FFFBF0" "#859900"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#FFFBF0" "#E1DBCD"))
 '(notmuch-search-line-faces
   '(("unread" :foreground "#aeee00")
     ("flagged" :foreground "#0a9dff")
     ("deleted" :foreground "#ff2c4b" :bold t)))
 '(objed-cursor-color "#dc322f")
 '(org-html-table-default-attributes
   '(:border "2" :cellspacing "0" :cellpadding "6" :rules "groups" :frame "border"))
 '(org-pomodoro-length 45)
 '(org-roam-link-popup-window-width-scale 0.8)
 '(org2ctex-latex-commands
   '("xelatex -shell-escape -interaction nonstopmode -output-directory %o %f" "bibtex %b" "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f" "xelatex -shell-escape -interaction nonstopmode -output-directory %o %f"))
 '(org2ctex-latex-packages-alist
   '("
%%% 默认使用的latex宏包 %%%
\\usepackage{tikz}
\\usepackage{CJKulem}
\\usepackage{graphicx}

%%% 设置页面边距 %%%
\\usepackage[top=2.54cm, bottom=2.54cm, left=3.17cm, right=3.17cm]{geometry}"))
 '(package-selected-packages
   '(helm-org-rifle helm-gtags company-lua lua-mode wrap-region visual-regexp-steroids visual-regexp use-package-ensure-system-package system-packages cfrs pfuture tiny string-edit standardfmt sphinx-doc spaceline-all-the-icons memoize language-detection shell-pop shackle rjsx-mode rainbow-identifiers quickrun pydoc prodigy prettier-js poetry pippel pipenv peep-dired password-generator overseer osx-clipboard orgit-forge org-rich-yank pretty-hydra org-contrib org org-cliplink ob-restclient ob-http npm-mode nose nameless multi-term multi-line shut-up mpv json-navigator hierarchy ivy-xref ivy-rtags ivy-purpose window-purpose imenu-list ivy-avy inspector importmagic epc ctable concurrent highlight-global helm-github-stars graphviz-dot-mode google-c-style good-scroll gitignore-templates git-modes ggtags gendoxy forge yaml closql treepy flycheck-rtags flycheck-package flycheck-elsa find-by-pinyin-dired evil-textobj-line evil-lion evil-goggles evil-easymotion evil-collection annalist evil-cleverparens eslintfmt eshell-z eshell-prompt-extras esh-help emr paredit list-utils emacsql emacs-everywhere e2ansi face-explorer drag-stuff discover-my-major makey dired-rsync dired-quick-sort cpp-auto-include counsel-css company-rtags company-restclient know-your-http-well company-reftex company-math math-symbol-lists chinese-conv centered-cursor-mode ht auctex-latexmk atomic-chrome ace-pinyin 4clojure org-media-note org-xournal emacsql-sqlite shrface anki-editor thing-edit with-proxy yasnippet-snippets xterm-color writeroom-mode visual-fill-column vterm treemacs-projectile treemacs-persp treemacs-magit treemacs-icons-dired treemacs-evil treemacs tide typescript-mode terminal-here symbol-overlay string-inflection rime ranger rainbow-mode org-tree-slide org-superstar org-roam org-re-reveal org-brain magit-section ivy-yasnippet ivy-posframe posframe impatient-mode ibuffer-projectile evil-org transient engine-mode polymode editorconfig doom-themes counsel-gtags company-auctex color-identifiers-mode browse-at-remote blacken awesome-tab auctex pinyinlib restclient rtags package-lint all-the-icons hide-mode-line pythonfmt doom-modeline shrink-path latex-preview-pane org2ctex color-rg doom-todo-ivy pangu-spacing org-protocol-capture-html pandoc-mode elpy find-file-in-project dash-at-point org-plus-contrib ghub org-mime nodejs-repl slime ob-ipython virtualenvwrapper ox-reveal ox-gfm ein websocket flycheck-ycmd company-ycmd ycmd request-deferred let-alist deferred company-quickhelp vimish-fold origami web-beautify livid-mode skewer-mode simple-httpd json-mode json-snatcher json-reformat js2-refactor multiple-cursors js2-mode js-doc company-tern dash-functional tern coffee-mode web-mode tagedit slim-mode scss-mode sass-mode pug-mode less-css-mode haml-mode emmet-mode company-web web-completion-data reveal-in-osx-finder pbcopy osx-trash osx-dictionary launchctl color-theme-solarized color-theme mic-paren pyim pyim-basedict fasd youdao-dictionary names chinese-word-at-point wgrep smex pdf-tools tablist org-category-capture ivy-hydra flyspell-correct-ivy disaster counsel-projectile counsel swiper company-c-headers cmake-mode clang-format ivy unfill smeargle orgit org-projectile org-present org-pomodoro alert log4e gntp org-download mwim mmm-mode markdown-toc markdown-mode magit-gitflow htmlize helm-gitignore helm-company helm-c-yasnippet gnuplot gitignore-mode gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link gh-md fuzzy flyspell-correct-helm flyspell-correct flycheck-pos-tip pos-tip flycheck evil-magit magit git-commit with-editor company-statistics company-anaconda company auto-yasnippet yasnippet auto-dictionary ac-ispell auto-complete yapfify pyvenv pytest pyenv-mode py-isort pip-requirements live-py-mode hy-mode helm-pydoc cython-mode anaconda-mode pythonic ws-butler winum which-key volatile-highlights vi-tilde-fringe uuidgen use-package toc-org spaceline powerline restart-emacs request rainbow-delimiters popwin persp-mode pcre2el paradox spinner org-bullets open-junk-file neotree move-text macrostep lorem-ipsum linum-relative link-hint info+ indent-guide hydra hungry-delete hl-todo highlight-parentheses highlight-numbers parent-mode highlight-indentation hide-comnt help-fns+ helm-themes helm-swoop helm-projectile helm-mode-manager helm-make projectile pkg-info epl helm-flx helm-descbinds helm-ag google-translate golden-ratio flx-ido flx fill-column-indicator fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-lisp-state smartparens evil-indent-plus evil-iedit-state iedit evil-exchange evil-escape evil-ediff evil-args evil-anzu anzu evil goto-chg undo-tree eval-sexp-fu highlight elisp-slime-nav dumb-jump f s diminish define-word column-enforce-mode clean-aindent-mode bind-map bind-key auto-highlight-symbol auto-compile packed dash aggressive-indent adaptive-wrap ace-window ace-link ace-jump-helm-line helm avy helm-core popup async))
 '(pangu-spacing-inhibit-mode-alist
   '(eshell-mode shell-mode term-mode dired-mode fundamental-mode text-mode))
 '(paradox-github-token t)
 '(pdf-view-midnight-colors '("#b2b2b2" . "#292b2e"))
 '(pyfmt-command "black")
 '(request-backend 'curl)
 '(rustic-ansi-faces
   ["#FDF6E3" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#556b72"])
 '(safe-local-variable-values
   '((eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/AO/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/Evil key bindings with evil-define-key do opposite of what I want/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/通过盖板号得到盖板图号，搜索 IPC 得到螺钉号/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/3M 8673/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/FP-200/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/BMS10-21/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/737NG 垂尾高频天线前缘风蚀的预防与修理/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/JavaScript-self-invoking_function/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/疫情影响下“客改货”国际航线空防安全风险识别及应对措施/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/bbbb/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/Tool tips on text in Emacs/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/737-800 再循环系统/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/逗号表达式/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/反引用表达式/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/identity 函数/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/format 函数/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/assoc/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/Provide some metadata for completing-read function/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/If-Let and When-Let/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/基于 AHP 的机场商业特许经营绩效综合评价/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/org_mode_how_to_automatically_generate_nice_file_hierarchy_trees_with_well_form/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/make_completing_read_respect_sorting_order_of_a_collection/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/HFEC/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/Emacs: Rename File, Space to Hyphen or Lowline/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/中文 PDF 电子书自动添加书签指南/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/Git submodule 的使用/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/Hugo 将 Org-mode 文件使用 Github Action 自动部署静态博客到 Github Pages/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/Cycle Hyphen or Lowline or Space/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/表单验证, 如果用户没有选择任何书籍, 则提交按钮无效, 并弹出警告/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/翼上应急门窗户灯/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/组件嵌套/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/管理维修人员执照/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/硅树脂胶带/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/特氟龙胶带/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/找出哪个表单没有暂存/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/建立人员技术档案/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/居中, 覆盖颜色/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/尾白灯不亮/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/办理维修许可项目的变更/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/使用 new 实现托管类/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/上报例行飞机发动机数据/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/上报 SDR/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/react-dates/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/react-dates 日期时间格式化/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/org-mode-how-to-automatically-generate-nice-file-hierarchy-trees-with-well-form/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/org-element-context & org-element-at-point/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/TypeError: <flask_restful.marshal_with object at 0x11bb91fd0> is not JSON serializable/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/React 默认值/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/React 路由 Redirect 组件/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/React 类型检查/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/React 登录页面和用户面板的设计/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/React 在 componentDidMount 中使用 JQuery 获取数据并显示/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/React 单选框/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/React 使用 connect/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/React 事件对象/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/React “Spell it as lowercase”/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/React spread attributes/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/React bookList 小的示例/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/Namespaced components/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/JavaScript spread attributes/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/Flask-restful/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/Flask 404 405 500 503 错误/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/厕所马桶不能冲刷/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/React 使用 combineReducers/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/20220701122254-使用 Task 类来和生成器协作/"))
     (eval setq org-download-image-dir
           (concat default-directory "/screenshotImg/"))
     (eval setq org-download-image-dir
           (concat default-directory "screenshotImg/"))
     (eval setq org-download-image-dir
           (concat default-directory "screenshotImg/returning_list_from_dolist_loop_instead_return_nil/"))
     (eval setq org-download-image-dir
           (concat default-directory "/screenshotImg"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "screenshotImg/为什么设置 display:inline 后，padding-bottom 仍然起作用/"))
     (eval progn
           (setq org-media-note-screenshot-image-dir
                 (concat default-directory "screenshotImg/spacemacs 出现 “starts with non-prefix key” 错误/"))
           (setq org-download-image-dir
                 (concat default-directory "screenshotImg/spacemacs 出现 “starts with non-prefix key” 错误/")))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "screenshotImg/使用 mkvirtualenv 创建新的 Python 3 虚拟环境时报错 TypeError: stat: path should be string, bytes, os.PathLike or integer, not NoneType/"))
     (eval setq org-download-image-dir
           (concat default-directory "screenshotImg/Accessibility: sr-only or aria-label/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "screenshotImg/MF:0301-安全管理体系/"))
     (eval progn
           (setq-local org-download-heading-lvl nil)
           (setq-local org-media-note-screenshot-image-dir
                       (concat default-directory "screenshotImg/"
                               (file-name-sans-extension
                                (buffer-name))
                               "/")))
     (eval progn
           (setq-local org-download-heading-lvl nil)
           (setq-local org-download-image-dir
                       (concat default-directory "screenshotImg/"
                               (file-name-sans-extension
                                (buffer-name))
                               "/")))
     (eval setq org-download-image-dir
           (concat default-directory "static/737NG 盖板紧固件件号确认/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/737NG 中文/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/test/"))
     (org-hugo-base-dir . "..")
     (eval setq org-download-image-dir
           (concat default-directory "./static/737NG 雷击检查要点/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/737NG 机身雷击损伤放行标准查找案例/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/一个 Emacs 程序员的一天/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/一个Emacs程序员的一天/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/7703-2A 定义及人员分类/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/浅谈民航维修体系之“安全积分卡管理”系统/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/公司级风险管理项目/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/77 维修工程系统培训大纲/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/97 维修工程系统课程大纲/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/0710-6 各类人员资格要求/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/航空安全信息分类/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/MRB/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/MPD/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/CPM/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/AWL/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/9713-9 人员资格评估课程大纲/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/0701-1 手册体系说明/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/IPM/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/IOC/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/ICAO /"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/IAH/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/公司安全管理体系文件的结构/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/MF:0301-安全管理体系/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/CDCCL/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/制度合规性和执行有效性自查表/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/危险源数据库/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/CMR/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/航空不安全事件知识图谱构建与事件类型分析/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/民航维修工作作风管理规范/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/ISARPs/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/03 航空安全管理手册/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/民航维修单位管理体系整合及一体化审核实施/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/IOSA/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/厦航手册体系/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/IATA 运行安全审计及在我国的实践/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/人员疏失归责图/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/ISM/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/正互反矩阵/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/厦福机务维修助手/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/冰雪霜与民用航空器安全/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/一致性矩阵/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/AHP/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/737 电门保护帽/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/特征值、特征向量/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/附件：货航本部安全过程考核细则/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/737 发动机碳封严/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/Git 分支的衍合/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/隐患/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/737NG APU 引气系统/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/基于离散优化的安全绩效指标预警规则设计方法/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/安全管理系统评估工具/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/疲劳管理/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat defaut-directory "./static/737NG APU 引气系统/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/风险/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/质量调查/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat defau-t-directory "./static/737NG APU 引气系统/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/737-800 PACK 非指令关闭/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/SAMPLE OF SAFETY PERFORMANCE INDICATORS/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/民航不安全事件/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/航空维修系统危险源识别和风险分析方法/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/人为差错分析模型/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/737-300 与 737-800 设备冷却系统对比/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/737NG 自动刹车不预位灯亮且无故障信息/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/737ng apu 引气系统/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/737-800 pack 非指令关闭/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/维修差错调查方法：MEDA/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/以结构化的方法，分级分解目标和需求/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/管理违章/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/doc 9859 安全管理手册/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/stdev 和 stdevp 的区别/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/危险源/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/航空维修工程管理之 MSG-3 /"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/bar/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/foo/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/安全管理体系/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/安全绩效管理/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/ceshi/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/ceshi 00 测试/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/returning_list_from_dolist_loop_instead_return_nil/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/测试/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/MF8201-29 超出持续适航文件的的放行管理程序/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/JavaScript 的原型链到底是什么/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/使用 mkvirtualenv 创建新的 Python 3 虚拟环境时报错 TypeError: stat: path should be string, bytes, os.PathLike or integer, not NoneType/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/Python threading.local 是全局变量但是它的值却在当前调用它的线程当中/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/Python 如何复制一个 class/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/Wireless QAR/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/单词量子速读方法论/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/AVM/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/MF8201-26 未超标损伤及外部修理记录管理程序/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/紧固件图库/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/AWG and CAU conversion/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/JavaScript 的 new 到底是干什么的/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/JavaScript 的 this 的值到底是什么/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/N1IMB 和 LPTIMB/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/737NG 减速板手柄自动伸出慢或者伸出不完全/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/工程师培养项目/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/index/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/resolving_keybinding_conflicts_with_evil_mode_troubleshooting_image_dired_bindi/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/热风枪使用注意事项/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/减速板离合器/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/Python 线程之定位与销毁/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/Python 3 编码原理/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/Python UserDict、UserString、UserList 存在的意义/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/737NG 减速板手柄位置传感器/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/Anki 基本操作/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/journal/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/三角函数和反三角函数图像、导数、积分、等式关系/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/JavaScript-call, apply, bind/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/Javascript-call, apply, bind/"))
     (org-refile-targets)
     (org-download-delete-image-after-download)
     (org-download-method . directory)
     (org-download-heading-lvl . 0)
     (org-download-image-dir . "~/Dropbox/journals/images/anki/")
     (eval setq org-download-image-dir
           (concat default-directory "./static/737NG 1 号主油箱后燃油泵低压灯亮/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/R648 SMOKE CONTROL RELAY/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/737NG 除烟模式构型导致设备冷却供气 OFF 灯亮/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/737NG 设备冷却/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/R949 EXHAUST SYSTEM CONTROL INTERRUPT RELAY/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/一号风挡结构图/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/WHCU/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/737NG 风挡加温故障/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/737NG 风扇配平方法/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/splice 接线管的安装/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/适航性限制项目/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/飞机的几种重量/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/时控与时限/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/发动机振动指示单位/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/737NG 发动机振动值高/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/07 维修工程管理手册/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/8208-1 培训大纲管理程序/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/0709-1 维修工程系统培训大纲的制定/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/737NG transfer bus off 灯亮处置预案/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/splice 接线管的查找/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/波音系列飞机常用接线管/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/扎绳/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/terminal lug 接线片的安装/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/Assembly of Terminals and Splices under special conditions/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/TB 编码规则和更换/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/AMM TASK 20-50-11/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/AMM TASK 20-10-51/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/737NG 液压管安装力矩的确定/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/Nginx url 配置/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/Python-super/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/使用 Python 进行并发编程 asyncio/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/Python 线程进阶/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/紧固件/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/737NG 自动油门电门组件更换调节测试/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/737NG 空调制冷效果差故障处置预案/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/使用 mkvirtualenv 创建新的虚拟环境时报错：bad interpreter/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/Python Interpreter on macOS/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/使用 mkvirtualenv 创建新的 Python3 虚拟环境时报错 TypeError: stat: path should be string, bytes, os.PathLike or integer, not NoneType/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/Spacemacs 报错 code for hash md5 was not found/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/李正元/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/2022 考研数学武忠祥体系说明.org/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/737NG 翼尖小翼烧蚀/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/737NG 飞机结构损伤处理/"))
     (eval setq org-\./static/波音标准件-note-screenshot-image-dir
           (concat default-directory "./static/波音标准件/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/飞机管路拆装/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/前退式与后退式插头/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/插钉压接/"))
     (eval setq org-\./static/飞机管路拆装-note-screenshot-image-dir
           (concat default-directory "./static/飞机管路拆装/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/Plastic Installing and Removal Tool Part Number Guide/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/757 中央液压油箱油量指示跳变/"))
     (eval setq org-\./static/同轴电缆接头件号查询与施工-note-screenshot-image-dir
           (concat default-directory "./static/同轴电缆接头件号查询与施工/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/M81969 Tool Part Number Guide/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/屏蔽地线/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/SWPM 主要章节目录/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/737NG 雷击/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/737NG 蒙皮厚度的确定/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/Redux middleware/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/Linux netstat 命令/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/Nginx root 和 alias 指令/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/737NG 机身凹坑放行标准查找/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/737NG 机身外部损伤手册快速参考指南/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/Python mock/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/Emacs org-mode examples and cookbook/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/737NG 前轮转弯手轮偶尔卡阻怎么回事/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/737NG 飞机查件常用 4 种方法/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/737NG EFLOW 构型失效的分析和处置/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/考研信息查询/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/Nginx 限制静态文件访问/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/显影剂使用规范/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/737NG 发动机故障代码解读/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/Taro 微信小程序实现 echarts 点击获取动态数据/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/React 实现甘特图/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/737NG 起落架指示不一致/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/737NG 飞机冲压门全开灯指示与实际不一致/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/Creating a Basic Auth. WSGI Middleware in Python/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/EMDP 地面故障保护系统测试不通过/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/737NG 冲压控制/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/为什么设置 display:inline 后，padding-bottom 仍然起作用/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/align-content 和 align-items 之间的区别/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/保险丝/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/Initial Server Setup with Ubuntu 14/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/插钉说明/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/CSS inline 与 block 与 inline-block/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/Padding for inline elements/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/padding_for_inline_elements/"))
     (checkdoc-minor-mode . t)
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/Setting locale failed/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/Flask 重建数据库索引/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/Flask 项目集成富文本编辑器 UEditor/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/what_does_double_star_asterisk_and_star_asterisk_do_for_parameters/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/机组氧气瓶工艺孔漏气检查/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/百度云服务器上运行 oss2 错误/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/微信小程序路由/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/requires a peer of webpack@^3.0.0 || ^4.0.0/"))
     (conding . utf-8)
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/前后端分离，nginx 解决跨域问题/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/sigint sigterm 的区别/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/在修改运行文件后如何重新载入 uWSGI/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/Python 模块导入与包构建最佳实践/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/站位/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/737-800 应急灯/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/`(file-name-nondirectory (file-name-base (buffer-file-name)))`/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/紧固件松动/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/紧固件分类/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/补充型号合格证/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/紧固件的力矩/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/紧固件的替代件/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/紧固件的拆装标准施工程序/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/AMOC/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/波音系列飞机结构修理的批准/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/737NG 飞机区域划分/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/737 系列飞机结构修理类型分类方法/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/737NG 标准力矩值查询指南/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/737NG 盖板螺钉力矩查询/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/737NG 盖板图号查找/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/从开源项目中学习 Python 高级编程/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/Python 2 使用描述符实现实例间属性共享和属性分隔存储/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/Python 上下文管理器/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/波音工程图纸-图纸页（DWG）/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/Python 多核并行运算/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/737-800 MEL21-32 组件温度控制系统/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/备用液压系统压力组件/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/波音工程图纸-图纸页资料清单（PSDL）/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/737NG 盖板紧固件件号确认/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/737NG 盖板螺钉件号查找/"))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/塑料扎带/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/737NG 配平空气 PRSOV/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/737NG 起落架选择活门/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/气象雷达自测试/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/仪表转换组件/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/737NG 起落架转换活门/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/时间关键宣告/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/天线补漆/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/IR 工作模式/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/AP 宣告牌/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/ATC 与 ADS-B OUT/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/ATC 应答机/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/手册有效性确认/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/Electronic Flight Instrument System/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/SSM SYMBOLS/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/737NG 组件活门故障导致左右发 N1 目标值显示不一致/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/1/"))
     (eval setq org-download-image-dir
           (concat default-directory "/./static/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/Insert Configurations/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/737NG 起动电门无法保持/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/OUTER DIAMETER OF WIRE/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/高压涡轮叶片/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/737NG PZTC/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/邦迪块/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/737NG EEC BITE 故障等级/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/SPAR/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/游离态水/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/stringer/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/737NG gasket 检查/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/Power Plant Test Reference Table/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/737NG 放泄燃油沉淀/"))
     (eval setq org-ownload-image-dir
           (concat default-directory "./static/737NG FIM 三个厕所马桶不工作/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/DEU 输出模拟离散数据/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/737NG 起动活门打开灯 START VALVE OPEN 亮/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/AIPC 手册简介/"))
     (eval progn
           (setq-local org-download-heading-lvl nil)
           (setq-local org-media-note-screenshot-image-dir
                       (concat default-directory "./static/"
                               (file-name-sans-extension
                                (buffer-name))
                               "/")
                       org-download-image-dir
                       (concat default-directory "./static/"
                               (file-name-sans-extension
                                (buffer-name))
                               "/")))
     (eval setq org-download-image-dir
           (concat default-directory "./static/737NG 自动刹车压力控制组件/"))
     (eval progn
           (setq-local org-download-heading-lvl nil)
           (setq-local org-media-note-screenshot-image-dir
                       (concat default-directory "./static/"
                               (file-name-sans-extension
                                (buffer-name))
                               "/")
                       (setq-local org-download-image-dir
                                   (concat default-directory "./static/"
                                           (file-name-sans-extension
                                            (buffer-name))
                                           "/"))))
     (eval progn
           (setq-local org-download-heading-lvl nil)
           (setq-local org-media-note-screenshot-image-dir
                       (concat default-directory "./static/"
                               (file-name-sans-extension
                                (buffer-name))
                               "/")))
     (eval setq org-download-image-dir
           (concat default-directory "./static/stiffener/"))
     (eval progn
           (setq-local org-download-heading-lvl nil)
           (setq-local org-download-image-dir
                       (concat default-directory "./static/"
                               (file-name-sans-extension
                                (buffer-name))
                               "/")
                       (setq-local org-media-note-screenshot-image-dir
                                   (concat default-directory "./static/"
                                           (file-name-sans-extension
                                            (buffer-name))
                                           "/"))))
     (eval progn
           (setq org-media-note-screenshot-image-dir
                 (concat default-directory "./static/spacemacs 出现 “starts with non-prefix key” 错误/"))
           (setq org-download-image-dir
                 (concat default-directory "./static/spacemacs 出现 “starts with non-prefix key” 错误/")))
     (eval setq org-download-image-dir
           (concat default-directory "./static/737NG AUTO BRAKE DISARM 灯亮/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/`(file-name-nondirectory (file-name-base (buffer-file-name)))`/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/737NG 减速板手柄位置电门/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/737NG AACU/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/737NG 减速板预位电门/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/737NG 减速板中断起飞电门/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/737NG RTO 提升摇臂机构顶块与减速板手柄凸耳间隙过小引发起飞警告/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/fay seal/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/737NG 前登机门门警电门 S1147/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/737NG 飞机结构的详细检查方法及实例/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/737NG-组件活门故障导致左右发-N1-目标值显示不一致/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/737NG 水平安定面前缘损伤/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/如何判断起动悬挂属于热起动保护/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/地继电器（系统 2）/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/飞行控制电门打到 STBY RUB 会发生什么情况/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/STBY RUB ON 灯何时点亮/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/备用关断活门指示继电器（R625）/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/737NG 组件活门（文氏管构型）/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/MaoXian web clipper/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/737NG APU 进气门/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/737NG FIM 三个厕所马桶不工作/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/how_to_get_colored_syntax_highlighting_of_code_blocks_in_asynchronous_org_mode_exports_to_HTML/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/how_to_get_colored_syntax_highlighting_of_code_blocks_in_asynchronous_org_mode_e/"))
     (eval progn
           (setq org-download-image-dir
                 (concat default-directory "./static/燃油控制面板、液压控制面板、门指示面板、交流系统发电机和 APU 面板/"))
           (setq org-media-note-screenshot-image-dir
                 (concat default-directory "./static/燃油控制面板、液压控制面板、门指示面板、交流系统发电机和 APU 面板/")))
     (eval setq org-media-note-screenshot-image-dir
           (concat default-directory "./static/燃油控制面板、液压控制面板、门指示面板、交流系统发电机和 APU 面板/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/737NG 发动机燃油滤旁通灯亮/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/737NG 起动刹车组件/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/737NG 发动机构型/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/DDPG/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/737NG 各种航段的定义/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/737NG CFM56-7B 发动机识别销的原理与调节/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/737NG EEC 双通道交替使用/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/737NG CFM56-7B 发动机推力控制模式/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/737NG 服务内话系统/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/QAR 译码参数选择/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/737NG REU/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/737NG EEC 三种慢车控制模式/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/737NG 发动机识别销/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/737NG/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/II 类 第六阶段/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/各种速度的定义/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/II 类 第五阶段/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/737NG 导航显示方式/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/调幅和调频/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/UTF-8/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/737NG APU 启动电门位置信号/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/737NG ACP/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/737NG EEC 导致的右点火故障/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/737NG VHF 穿云时通讯噪音/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/737NG 前登机门下部凹坑/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/Flask Docker 部署/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/737NG SRM Structural Classification/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/737NG VHF 只能接收不能发射/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/天空内饰飞机客舱滤波器引发的内话故障/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/737-SL-36-024-B/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/卫星通讯系统构型/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/波音工程图纸-零件清单（PL）/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/通过波音工程图纸查询放电刷基座以及铆钉件号方法/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/波音工程图纸-图页资料清单（PSDL）/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/波音工程图纸-图页（DWG）/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/BACT12AR 紧口接线片/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/Git rebase/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/737NG 更换一号风挡固定拖把耗材工程图纸查询/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/II 类 第三阶段/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/II 类 第四阶段/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/暗亮入口灯/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/环境控制系统/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/SSM 33-21-12/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/Mitmproxy/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/R645 SUPPLY SYSTEM CONTROL INTERRUPT RELAY/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/SSM 33-21-11/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/SSM 33-22-11_(sh_1)/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/737NG 天空内饰窗户灯/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/公众号 __biz 值/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/左右空速不一致故障总结/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/甚高频通讯噪音的故障总结/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/飞机 APU 起动超温故障/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/EEC 导致的右点火故障/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/737NG 偏航阻尼故障分析/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/JavaScript、jQuery、HTML、CSS 构建 Web IM 远程及时聊天通信程序/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/燃油、液压控制面板、门指示面板、交流系统发电机和 APU 面板/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/燃油控制面板、液压控制面板、门指示面板、交流系统发电机和 APU 面板/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/737-800 应急灯分布/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/控制电门保险丝/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/737NG 天花板上通道照明灯不亮/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/737NG 天空内饰厕所灯不亮/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/737NG 应急灯光系统学习笔记/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/中立位移位置/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/RNP/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/ACMS/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/II 类 第二阶段/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/黑苹果更新系统/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/升降舵调整片控制机构/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/升降舵系统概述/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/升降舵平衡板/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/二号风挡结构图/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/II 类 第一阶段/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/SRM 51 章（chapter）分节（section）/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/重着陆检查/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/SRM 52-57 5X-XX-XX 中最后两位数字代表含义/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/因为应急门上这个特殊的窗灯,我们平时测试天空內饰应急灯的方法错啦/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/飞行操纵关断活门关闭通往哪里的液压/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/Git/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/pipenv/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/Python 元类/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/使用 Portal 优雅实现漂浮在页面上的组件/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/org_to_do/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/babel 的 plugins 和 presets 解析"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/require 和 import/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/II 类/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/unwind-protect"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/Python __all__ 变量/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/Git --force-with-lease/Git --force-with-lease/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/Python 的弱引用/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/Python 2-Queue/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/Python-absolute-import/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/737NG 发动机火警探测导线束接线片的更换/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/hasattr 函数/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/自动减速板/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/Python 元类"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/Boeing Standard Wire Part Number Data/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/Python 属性的查找顺序/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/PyCharm 快捷键/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/Python 3 Text Vs. Data Instead Of Unicode Vs. 8-bit"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/线路施工工具/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/Python-asyncio/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/RESTful/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/保留工作项目/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/机务 AirFase B4 版本客户端安装手册/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/Python 类的 __get__() 调用/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/React-dates 支持对过去时间的选择/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/Python 实现在类中动态添加属性和生成对象/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/辨别断桥铝/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/在 React 中使用 jQuery 插件/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/how_to_reference_named_table_or_code_block_in_org_mode/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/错误处理程序中 return"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/jQuery/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/Python WSGI middleware/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/Python __package__ 变量/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/Filter_python_list/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/Python 内置函数 type()/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/空客电传往事/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/get_content_of_a_buffer/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/CMM/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/diff 命令和 patch 命令/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/旋转磁极式发电机"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/使用 Python 和 Flask 设计 RESTful API"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/有哪些让人欲罢不能的学习方法/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/Python-socket/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/What's_the_canonical_way_to_check_for_type_in_Python"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/旋转电枢式发电机/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/Python 面向切面编程"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/航化品的存储时限"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/Python yield/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/Python-datetime.timedelta/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/保留故障/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/Python 实现线程安全队列/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/Nginx 简介/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/Graphviz-节点"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/Python 包，模块，类以及代码文件和目录的一种管理方案/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/terminal lug 接线片的查找/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/737 R317 继电器上接线片和导线的更换/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/飞机编号及飞机维修手册的有效性标识/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/Python 反射机制及实际应用场景/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/Python __import_/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/PEP 328: Absolute and Relative Imports"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/Python-from-import"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/Python-import xx.xx.xx/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/fix_columns_in_horizontal_scrolling/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/8203-40 非例行工卡管理工作程序/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/NRC 方案制定与处置/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/0705-12 必检项目/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/AJAX 的出现与跨域处理/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/MEL/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/8201-8 必检项目管理程序/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/82 维修工作程序手册/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/驾驶舱的空气如何排出机外/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/检验/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/发动机火警过热探测元件/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/737NG 发动机火警核心机导线束更换/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/核心机导线 MW0325 MW0326 与风扇导线 MW0315 MW0316 相连的支架/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/发动机火警线束/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/Using_directory_local_variables_to_make_drag-and-drop_image_save_in_the_same_name_folder_as_org_file/"))
     (eval progn
           (setq-local org-download-heading-lvl nil)
           (setq-local org-download-image-dir
                       (concat default-directory "./static/"
                               (file-name-sans-extension
                                (buffer-name))
                               "/")))
     (eval progn
           (setq-local org-download-image-dir
                       (concat default-directory "./static/"
                               (file-name-sans-extension
                                (buffer-name))
                               "/")))
     (eval setq org-download-image-dir
           (concat default-directory "./static/737NG 发动机火警探测环路故障电阻测量/"))
     (eval setq org-download-image-dir
           (concat default-directory "/./static"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/雄性脱发可能解释"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/737NG 发动机火警探测环路故障/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/737NG 火警故障不工作测试/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/737NG 火警过热探测系统逻辑表/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/737NG 反推内壁冷却孔/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/Can_a_jinja_variables_scope_extend_beyond_in_an_inner_block/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/双发 N1 相同时油门杆不齐，人工对齐油门杆双发 N1 有差值/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/双发N1相同时油门杆不齐，人工对齐油门杆双发N1有差值/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/更换备用刹车选择活门管路和接头的标准力矩查找/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/液压系统维护时管路接头堵头的件号和使用/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/Multicolumn_cells_in_org_mode_tables/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/List_comprehension_vs_lambda_filter/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/How_does_pythons_super_work_with_multiple_inheritance/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/Converting_unicode_string_to_chinese_characters/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/provixy 代理/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/自动油门电门组件/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/737NG 飞机 APU 启动发电机基本原理浅析/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/737NG APU 滑油滤压差指示器弹出/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/737NG 的 APU 爆炸帽灯点亮的故障分析/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/When_to_use_weak_references_in_python/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/Accessibility: sr-only or aria-label/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/Flask-0.1/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/filter_python_list/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/Using_org_babel_to_enable_virtualenv/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/what_does_sudo_h_do/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/container document/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/Custom_Flask-Admin_list_action"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/维 a 酸使用注意事项/"))
     (eval setq org-download-image-dir
           (concat default-directory "/./static/Flask-SQLAlchemy_db_create_all_not_creating_database/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/sae 部署/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/Python 2-MRO_and_super/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/Python-asyncio_gather_vs_asyncio_wait/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/Python-协程/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/Nginx 怎么转发/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/PyCharm 激活/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/evil-guide/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/MacOS 无法登录 App Store 修复/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/使用 lunr 实现静态博客的前端搜索/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/Graphviz/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/对于植发的看法，以及一些疑问的解答/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/CSS3 常用选择器一览表/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/CSS-position/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/CSS 基础/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/Taro/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/小助手使用说明/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/Add_annotation_in_PDF_src_block/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/org 文件树生成/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/org 表格/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/org-mode 转 latex ，设置 block 的 latex 选项/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/Latex 实现图文混排/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/react-dates 支持对过去时间的选择/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/React.js 小书/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/Python WSGI/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/电源准备/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/Python 2-wraps/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/航线线路施工及检查/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/How_to_compute_options_of_dropdown_for_column_filter_in_Flask_Adminn/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/Ubuntu 查看用户和用户组/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/Flask 子域名/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/Flask-Admin/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/flask-0.1/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/Let's_build_a_web_server/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/Flask + Docker 部署/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/Flask-SQLAlchemy 外键多对多关系/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/JavaScript-继承/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/React 构造函数中为什么要将类方法绑定到 this/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/JavaScript-原型链/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/原生 JavaScript 实现滑动进度条/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/Python-how_do_coroutines_work/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/Python 线程基础/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/Copy_with_JSONDecodeError_in_requests.get().json()_in_Python_2_and_3/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/Python 2-attribute_and_property/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/Python-MRO/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/Python 2-bound_method_and_unbound_method_and_classmethod_and_staticmethod/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/Git --force-with-lease/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/printf() 函数转换说明/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/数据类型和流程控制/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/设置断点的原理/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/SAP 章节号使用 BeautifulSoup 抓取/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/飞机部件电气测量设备参考/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/短跑道构型/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/短跑道构型自动减速板未预位灯亮/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/WDM 符号说明/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/通过插头来查找插钉件号/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/WDM 使用说明/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/737NG 飞机面板/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/737NG 按压式烧水杯电门更换/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/桥式整流器/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/APU 用电瓶无法起动，外电源起动成功故障特例/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/Flask 源码/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/emacs org 参考文献工具 org-ref 简介/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/JavaScript-basic/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/JavaScript-call_stack_callback_queue_event_loop/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/Custom_Flask-Admin_list_action/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/如何做到像使用 LaTex 那样优雅地使用 Word/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/Python 异常处理/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/反推工作原理/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/737NG 反推保留 M 程序/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/737-800 空调温度控制/"))
     (eval setq org-download-image-dir
           (concat default-directory "./static/737-800 空调平衡和不平衡/"))
     (typescript-backend . tide)
     (typescript-backend . lsp)
     (javascript-backend . tide)
     (javascript-backend . tern)
     (javascript-backend . lsp)))
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
 '(company-scrollbar-fg ((t (:background "alternateSelectedControlColor"))) t)
 '(company-tooltip-scrollbar-thumb ((t (:background "alternateSelectedControlColor"))))
 '(cursor ((t (:background "#b58900"))))
 '(doom-modeline-bar ((t (:backgroud "#6272a4"))))
 '(expand-region-contract-fast-key "V")
 '(expand-region-exclude-text-mode-expansions '(html-mode nxml-mode web-mode))
 '(expand-region-reset-fast-key "r")
 '(helm-move-to-line-cycle-in-source t)
 '(set-face-attribute ((t ('sp-show-pair-match-face nil :foreground 'unspecified :background 'unspecified))))
 '(set-frame-parameter ((t (nil 'background-mode 'dark))))
 '(set-terminal-parameter ((t (nil 'background-mode 'dark))))
 '(spacemacs-iedit-face ((t (:background "firebrick1" :foreground "#2075c7" :inherit 'mode-line))))
 '(spacemacs-iedit-insert-face ((t (:background "firebrick1" :foreground "#2075c7" :inherit 'mode-line))))
 '(which-func ((t (:inherit modeline)))))
