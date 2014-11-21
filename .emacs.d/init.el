(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))

(setq-default inhibit-splash-screen t)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default js-indent-level 2)
(setq-default show-trailing-whitespace t)
(setq-default truncate-lines t)

;; prevent backup files from being littered throughout the file system
(setq backup-directory-alist `(("." . "~/.emacs.d/backups")))

;; chromebook has no delete key, so make C-w
;; backward-kill-word if there is no region.
(defun unix-werase-or-kill (arg)
      (interactive "*p")
      (if (and transient-mark-mode mark-active)
          (kill-region (region-beginning) (region-end))
        (backward-kill-word arg)))
(global-set-key (kbd "C-w") 'unix-werase-or-kill)
(add-hook 'ido-setup-hook
          (lambda ()
            (define-key ido-file-completion-map "\C-w" 'ido-delete-backward-word-updir)))

;; shell
(global-set-key (kbd "C-M-f") 'find-file-at-point)
(global-set-key (kbd "<f12>") 'comint-get-next-from-history)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(setenv "NODE_NO_READLINE" "1")
(setenv "PAGER" "cat")
(setq eshell-prompt-function
      (lambda ()
        (concat (car (last (split-string (eshell/pwd) "/"))) " $ ")))
(setq eshell-aliases-file "~/.emacs.d/eshell/alias")
(setq comint-prompt-read-only t)
(defun allow-trailing-whitespace ()
  "Allow trailing whitespace in shell buffers."
  (set-variable 'show-trailing-whitespace nil))
(add-hook 'shell-mode-hook 'allow-trailing-whitespace)
(add-hook 'eshell-mode-hook 'allow-trailing-whitespace)
(add-hook 'term-mode-hook 'allow-trailing-whitespace)
(eshell)

;; open in current window
(add-to-list 'same-window-buffer-names "*Buffer List*")
(custom-set-variables
 '(magit-status-buffer-switch-function (quote switch-to-buffer)))

(add-to-list 'load-path "~/.emacs.d")

;; package
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(package-initialize)

(when (null package-archive-contents)
  (package-refresh-contents))

(defvar my-packages '(cider
                      clojure-mode
                      coffee-mode
                      color-theme-solarized
                      flymake-cursor
                      flymake-jshint
                      ido-ubiquitous
                      magit
                      markdown-mode
                      smex
                      yasnippet))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; ido
(ido-mode t)
(ido-ubiquitous-mode)

;; smex
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; y or n instead of "yes" or "no" to prompts
(defalias 'yes-or-no-p 'y-or-n-p)

;; markdown
(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(setq auto-mode-alist
      (cons '("\\.md" . markdown-mode) auto-mode-alist))
(custom-set-variables
 '(markdown-command "markdown_py-2.7")
 '(markdown-command-needs-filename t))

;; magit
(require 'magit)
(global-set-key (kbd "C-c l") 'magit-log)
(global-set-key (kbd "C-c s") 'magit-status)
(set-face-foreground 'magit-diff-add "green")
(set-face-foreground 'magit-diff-del "red")
(defun magit-highlight-section () nil)

;; jshint
;; - add flymake-cursor, flymake-jshint to my-packages
;; - sudo apt-get install nodejs
;; - sudo apt-get install npm
;; - sudo npm install -g jshint
(require 'flymake-cursor)
(require 'flymake-jshint)
(setq jshint-configuration-path "~/.emacs.d/jshint.json")
(add-hook 'javascript-mode-hook
     (lambda () (flymake-mode t)))

;; allows for M-x winner-undo to undo window changes
(winner-mode t)

;; i miss modal editing sometimes. (kbd "C-c i") puts me
;; into vi-mode. (kbd "i") puts me into insert mode, which
;; is just the previous major mode.
(global-set-key (kbd "C-c i") 'vi-mode)

;; toggle comment current line if region is not active.
;; based on http://www.emacswiki.org/emacs/CommentingCode
(defun comment-dwim-line (&optional arg)
  (interactive "*P")
  (if (region-active-p)
      (comment-dwim arg)
    (comment-or-uncomment-region (line-beginning-position) (line-end-position))))
(global-set-key (kbd "M-;") 'comment-dwim-line)

;; replace kill-sentence with kill-whole-line
(global-set-key (kbd "M-k") 'kill-whole-line)

;; enable narrow-to-region by default
(put 'narrow-to-region 'disabled nil)

;; clojurescript
(add-to-list 'auto-mode-alist '("\.cljs$" . clojure-mode))
;; node/ejs
(add-to-list 'auto-mode-alist '("\.ejs$" . html-mode))

;; yasnippet
(require 'yasnippet)
(yas/global-mode 1)

;; enable change case within a region
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(load-theme 'solarized-light t)
