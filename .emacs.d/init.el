(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))

(setq-default inhibit-splash-screen t)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq-default js-indent-level 2)
(setq-default show-trailing-whitespace t)
(setq-default truncate-lines t)
(setq-default ring-bell-function 'ignore)

;; prevent backup files from being littered throughout the file system
(setq backup-directory-alist `(("." . "~/.emacs.d/backups")))

;; shell
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(setenv "NODE_NO_READLINE" "1")
(setenv "PAGER" "cat")
(setq eshell-prompt-function
      (lambda ()
        (concat (car (last (split-string (eshell/pwd) "/"))) " $ ")))
(setq eshell-prompt-regexp "^[^#$\n]*[#$] ")
(setq comint-prompt-read-only t)
(defun allow-trailing-whitespace ()
  "Allow trailing whitespace in shell buffers."
  (set-variable 'show-trailing-whitespace nil))
(add-hook 'shell-mode-hook 'allow-trailing-whitespace)
(add-hook 'eshell-mode-hook 'allow-trailing-whitespace)
(add-hook 'term-mode-hook 'allow-trailing-whitespace)
(eshell)

(add-to-list 'load-path ".")

;; package
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(when (null package-archive-contents)
  (package-refresh-contents))

(defvar my-packages '(cider
                      clojure-mode
                      coffee-mode
                      color-theme-solarized
                      elixir-mode
                      exec-path-from-shell
                      ido-ubiquitous
                      magit
                      markdown-mode
                      paredit
                      pbcopy
                      projectile
                      projectile-rails
                      scss-mode
                      slim-mode
                      smex
                      web-mode
                      yasnippet
                      zenburn-theme))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; Copy $PATH from shell for windowed Emacs
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;; ido
(ido-mode t)
(ido-ubiquitous-mode)
(setq ido-enable-prefix nil
      ido-enable-flex-matching t
      ido-use-filename-at-point 'guess
      ido-use-virtual-buffers t)

;; smex
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; projectile
(projectile-global-mode)

;; y or n instead of "yes" or "no" to prompts
(defalias 'yes-or-no-p 'y-or-n-p)

;; highlight matching parens
(show-paren-mode 1)

;; markdown
(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(setq auto-mode-alist
      (cons '("\\.md" . markdown-mode) auto-mode-alist))
(custom-set-variables
 '(markdown-command "markdown")
 '(markdown-command-needs-filename t))
(add-hook 'markdown-mode-hook 'turn-on-auto-fill)

;; magit
(require 'magit)
(global-set-key (kbd "C-c l") 'magit-log-buffer-file)
(global-set-key (kbd "C-c s") 'magit-status)
(setq magit-last-seen-setup-instructions "1.4.0")
(setq magit-log-show-margin nil)
(defun magit-log-mode-config ()
  (local-set-key (kbd "h") 'magit-toggle-margin))
(add-hook 'magit-log-mode-hook 'magit-log-mode-config)

;; org-mode
(org-babel-do-load-languages
 'org-babel-load-languages
 '((ruby . t)))

;; clojure
(add-hook 'clojure-mode-hook #'paredit-mode)
(setq cider-repl-history-file "~/.emacs.d/cider-history")

;; ruby
(add-hook 'projectile-mode-hook 'projectile-rails-on)
(setq ruby-deep-indent-paren nil)

;; allows for M-x winner-undo to undo window changes
(winner-mode t)

;; i miss modal editing sometimes. (kbd "C-c i") puts me into
;; viper-mode. (kbd "C-z") puts me back into emacs mode.
;; M-x viper-go-away disables it entirely.
(global-set-key (kbd "C-c i") 'viper-mode)
(setq viper-expert-level  '3)
(setq viper-inhibit-startup-message 't)
(setq viper-always nil)

;; C-w.
;; Chromebook has no delete key so I can't M-DEL to kill the previous
;; word. Bash uses C-w to cut up to the last space, so why not do
;; that. Only problem: C-w kills the active region by default, so
;; keep that functionality.
(defun unix-werase-or-kill (arg)
  (interactive "*p")
  (if (and transient-mark-mode mark-active)
      (kill-region (region-beginning) (region-end))
    (backward-kill-word arg)))
(global-set-key (kbd "C-w") 'unix-werase-or-kill)
;; Use C-w in ido minibuffers too.
(add-hook 'ido-setup-hook
          (lambda ()
            (define-key ido-file-completion-map "\C-w" 'ido-delete-backward-word-updir)))

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

;; pbcopy - integrate with OS copy / paste
(require 'pbcopy)
(turn-on-pbcopy)

;; dired-x
(autoload 'dired-jump "dired-x"
  "Jump to Dired buffer corresponding to current buffer." t)
(global-set-key (kbd "C-x C-j") 'dired-jump)

;; enable change case within a region
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; color theme
(load-theme 'zenburn t)

;; web-mode
(defun my-web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-enable-auto-pairing t)
  (setq web-mode-enable-auto-closing t))
(add-hook 'web-mode-hook  'my-web-mode-hook)
(add-to-list 'auto-mode-alist '("\.html$" . web-mode))
;; JSX syntax highlighting on .js files in web-mode.
(setq web-mode-content-types-alist
      '(("jsx" . "\.js[x]?$")))

;; scss mode
(setq scss-compile-at-save nil)
