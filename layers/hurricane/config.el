(defconst sys/win32p
  (eq system-type 'windows-nt)
  "Are we running on a WinTel system?")

(defconst sys/linuxp
  (eq system-type 'gnu/linux)
  "Are we running on a GNU/Linux system?")

(defconst sys/macp
  (eq system-type 'darwin)
  "Are we running on a Mac system?")

(defconst sys/mac-x-p
  (and (display-graphic-p) sys/macp)
  "Are we running under X on a Mac system?")

(defconst sys/mac-cocoa-p
  (featurep 'cocoa)
  "Are we running with Cocoa on a Mac system?")

(defconst sys/linux-x-p
  (and (display-graphic-p) sys/linuxp)
  "Are we running under X on a GNU/Linux system?")

(defconst sys/cygwinp
  (eq system-type 'cygwin)
  "Are we running on a Cygwin system?")

(defconst sys/rootp
  (string-equal "root" (getenv "USER"))
  "Are you using ROOT user?")

(defconst emacs/>=25p
  (>= emacs-major-version 25)
  "Emacs is 25 or above.")

(defconst emacs/>=26p
  (>= emacs-major-version 26)
  "Emacs is 26 or above.")

(defconst emacs/>=27p
  (>= emacs-major-version 27)
  "Emacs is 27 or above.")

(defconst emacs/>=25.3p
  (or emacs/>=26p
      (and (= emacs-major-version 25) (>= emacs-minor-version 3)))
  "Emacs is 25.3 or above.")

(defconst emacs/>=25.2p
  (or emacs/>=26p
      (and (= emacs-major-version 25) (>= emacs-minor-version 2)))
  "Emacs is 25.2 or above.")

(defvar org-agenda-dir ""
  "Gtd org files location.")

(defvar deft-dir ""
  "Deft org files locaiton.")

(defvar blog-dir ""
  "Blog files locaiton.")

(defvar snippet-dir ""
  "Yasnippet files location.")

(defvar virtualenv-dir ""
  "Virtualenvs location.")

(defvar auto-insert-dir ""
  "Auto insert templates location.")

(defvar rime-dir ""
  "Emacs-rime configuration location.")

(defvar Anki-media-dir ""
  "Anki media loaction.")

(defvar dictionary-overlay-dir "")

(defvar jupyter-bin ""
  "Jupyter location.")

(defvar eslintfmt-configuration-file ""
  "Eslintfmt global configuration.")

(defvar Baidu-OCR-Python-file ""
  "Baidu OCR Python file location.")

(defvar scripts-file ""
  "scripts can be used in org mode.")

(defvar provixy-type "http"
  "proxy type for EAF and popweb.")

(defvar provixy-host "0.0.0.0"
  "Provixy listen host.")

(defvar provixy-port "8118"
  "Provixy listen port.")

(defcustom eaf-screenshot-command (if (memq system-type '(cygwin windows-nt ms-dos)) "" "/usr/sbin/screencapture")
  "The screenshot command used by eaf.py.

Absolute path is best."
  :type 'string)

(defcustom eaf-screenshot-args nil
  "Arguments passed to 'screenshot.'

For example, (setq eaf-screenshot-args (list \"-i\" \"-x\"))"
  :type '(repeat string))

(setq-default
 org-agenda-dir "~/Library/Mobile Documents/iCloud~com~appsonthemove~beorg/Documents/braindump/"
 deft-dir (cond
           (sys/macp "~/Library/Mobile Documents/iCloud~com~appsonthemove~beorg/Documents/braindump/")
           (sys/win32p "~\braindump\\"))
 blog-dir (cond
           (sys/macp "~/Library/Mobile Documents/iCloud~com~appsonthemove~beorg/Documents/braindump/public/")
           (sys/win32p "~\public\\"))
 snippet-dir "~/.spacemacs.d/snippets/"
 virtualenv-dir "~/.virtualenvs/"
 auto-insert-dir "~/.spacemacs.d/templates/"
 rime-dir "~/.spacemacs.d/rime/"
 Anki-media-dir (cond
                 (sys/mac-x-p "~/Library/Application Support/Anki2/用户1/collection.media/")
                 t "")
 dictionary-overlay-dir "~/.spacemacs.d/dictionary-overlay-user-data-directory/"
 jupyter-bin "~/.virtualenvs/ipy/bin/jupyter"
 eslintfmt-configuration-file "~/.spacemacs.d/.eslintrc.js"
 Baidu-OCR-Python-file "~/.spacemacs.d/BaiduOcr.py"
 scripts-file "~/.spacemacs.d/scripts.org"
)
