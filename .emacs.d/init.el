(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))

(setq-default tab-width 4)
(setq-default show-trailing-whitespace t)

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
(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(eshell)

;; open in current window
(add-to-list 'same-window-buffer-names "*Buffer List*")
(custom-set-variables
 '(magit-status-buffer-switch-function (quote switch-to-buffer)))

(add-to-list 'load-path "~/.emacs.d")

;; package
(when (not (require 'package nil t))
  (require 'package "package-23.el"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

(when (null package-archive-contents)
  (package-refresh-contents))

(defvar my-packages '(starter-kit clojure-mode markdown-mode))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; markdown
(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(setq auto-mode-alist
      (cons '("\\.md" . markdown-mode) auto-mode-alist))
(custom-set-variables
 '(markdown-command "markdown-2.7")
 '(markdown-command-needs-filename t))

;; magit
(require 'magit)
(global-set-key (kbd "C-c l") 'magit-log)
(global-set-key (kbd "C-c s") 'magit-status)
(set-face-foreground 'magit-diff-add "green")
(set-face-foreground 'magit-diff-del "red")
(defun magit-highlight-section () nil)