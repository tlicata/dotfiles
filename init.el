;; init.el

;; turn on font-lock mode
(when (fboundp 'global-font-lock-mode)
  (global-font-lock-mode t))

;; default to better frame titles
(setq frame-title-format
      (concat  "%b - emacs@" system-name))

;; default tab-width is 8
(setq-default tab-width 4)

;; put my emacs settings in load-path
(add-to-list 'load-path "~/.emacs.d")
(add-to-list 'load-path "~/.emacs.d/ac-slime")
(add-to-list 'load-path "~/.emacs.d/auto-complete")
(add-to-list 'load-path "~/.emacs.d/color-theme-6.6.0")
(add-to-list 'load-path "~/.emacs.d/espresso")
(add-to-list 'load-path "~/.emacs.d/las3r")
(add-to-list 'load-path "~/.emacs.d/magit")
(add-to-list 'load-path "~/.emacs.d/sanityinc")
(add-to-list 'load-path "~/.emacs.d/undo-tree")

;; open in current window
(add-to-list 'same-window-buffer-names "*Buffer List*")

;; disable menu bar 
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

;; color-theme
(require 'color-theme)
(eval-after-load "color-theme"
  '(progn
     (color-theme-initialize)
     (color-theme-hober)))

;; sanity inc color-theme
(require 'color-theme-autoloads)
(autoload 'color-theme-sanityinc-light "color-theme-sanityinc" "A light color theme" t)

;; shell properly colors directories
(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; auto-complete
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/auto-complete/dict")
(ac-config-default)

;; slime auto-complete & docs
(require 'ac-slime)
(add-hook 'slime-mode-hook 'set-up-slime-ac)
;; prevents warning at slime startup
(eval-after-load 'slime '(setq slime-protocol-version 'ignore))

;; undo-tree
(require 'undo-tree)
(global-undo-tree-mode)
(global-set-key (kbd "C-c u") 'undo-tree-undo)
(global-set-key (kbd "C-c r") 'undo-tree-redo)

;; magit.el
(require 'magit)
(global-set-key (kbd "C-c l") 'magit-log)
(global-set-key (kbd "C-c s") 'magit-status)
(set-face-background 'magit-item-highlight "DarkBlue")

;; Actionscript
(require 'ecmascript-mode)
(require 'las3r-mode)
(add-to-list 'auto-mode-alist '("\\.as$" . ecmascript-mode))
(add-to-list 'auto-mode-alist '("\\.lsr$" . las3r-mode))

;; Javascript
(add-to-list 'auto-mode-alist '("\\.js$" . ecmascript-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . ecmascript-mode))

;;; This was installed by package-install.el.
;;; This provides support for the package system and
;;; interfacing with ELPA, the package archive.
;;; Move this code earlier if you want to reference
;;; packages in your .emacs.
(when
    (load
     (expand-file-name "~/.emacs.d/elpa/package.el"))
  (package-initialize))
