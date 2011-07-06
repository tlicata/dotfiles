;; init.el

;; turn on font-lock mode
(when (fboundp 'global-font-lock-mode)
  (global-font-lock-mode t))

;; default to better frame titles
(setq frame-title-format
      (concat  "%b - emacs@" system-name))

;; default tab-width is 8
(setq-default tab-width 4)
;; no tabs
(setq indent-tabs-mode nil)

;; put my emacs settings in load-path
(add-to-list 'load-path "~/.emacs.d")
(add-to-list 'load-path "~/.emacs.d/ac-slime")
(add-to-list 'load-path "~/.emacs.d/auto-complete")
(add-to-list 'load-path "~/.emacs.d/clojure")
(add-to-list 'load-path "~/.emacs.d/clojure/durendal")
(add-to-list 'load-path "~/.emacs.d/coffee")
(add-to-list 'load-path "~/.emacs.d/color-theme-6.6.0")
(add-to-list 'load-path "~/.emacs.d/elpa")
(add-to-list 'load-path "~/.emacs.d/js")
(add-to-list 'load-path "~/.emacs.d/las3r")
(add-to-list 'load-path "~/.emacs.d/magit")
(add-to-list 'load-path "~/.emacs.d/sanityinc")
(add-to-list 'load-path "~/.emacs.d/undo-tree")

;; ELPA
(load "package")
(package-initialize)

;; open in current window
(add-to-list 'same-window-buffer-names "*Buffer List*")

;; disable bars 
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))

;; color-theme
(require 'color-theme)
(eval-after-load "color-theme"
  '(progn
     (color-theme-initialize)
     (color-theme-hober)))

;; sanity inc color-theme
(require 'color-theme-autoloads)
(autoload 'color-theme-sanityinc-light "color-theme-sanityinc" "A light color theme" t)

;; org mode
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

;; auto-complete
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/auto-complete/dict")
(ac-config-default)

;; erc
(setq erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"))
(setq erc-hide-list '("JOIN" "NICK" "PART" "QUIT"))

;; slime auto-complete & docs
(require 'ac-slime)
(add-hook 'slime-mode-hook 'set-up-slime-ac)
;; prevents warning at slime startup
(eval-after-load 'slime '(setq slime-protocol-version 'ignore))

;; desktop
;(desktop-save-mode 1)
;; revive
(autoload 'save-current-configuration "revive" "Save status" t)
(autoload 'resume "revive" "Resume Emacs" t)
(autoload 'wipe "revive" "Wipe Emacs" t)
(global-set-key (kbd "C-x S") 'save-current-configuration)
(global-set-key (kbd "C-x F") 'resume)
(global-set-key (kbd "C-x K") 'wipe)

;; parenthesis highlighting
(require 'highlight-parentheses)
(add-hook 'clojure-mode-hook 'clojure-mode-setup)
(defun clojure-mode-setup ()
  (highlight-parentheses-mode t))

;; undo-tree
(require 'undo-tree)
(global-undo-tree-mode)
(global-set-key (kbd "C-/") 'undo-tree-undo)
(global-set-key (kbd "C-_") 'undo-tree-redo)

;; direct window movement
(global-set-key (kbd "M-<right>") 'windmove-right)
(global-set-key (kbd "M-<left>") 'windmove-left)
(global-set-key (kbd "M-<up>") 'windmove-up)
(global-set-key (kbd "M-<down>") 'windmove-down)
;; window history
(when (fboundp 'winner-mode)
  (winner-mode 1))

;; chromebook has no delete key, so make C-w
;; backward-kill-word if there is no region.
(defun unix-werase-or-kill (arg)
      (interactive "*p")
      (if (and transient-mark-mode mark-active)
		  (kill-region (region-beginning) (region-end))
		(backward-kill-word arg)))
(global-set-key (kbd "C-w") 'unix-werase-or-kill)

;; magit.el
(require 'magit)
(global-set-key (kbd "C-c l") 'magit-log)
(global-set-key (kbd "C-c s") 'magit-status)
(set-face-foreground 'magit-diff-add "green")
(set-face-foreground 'magit-diff-del "red")
(set-face-foreground 'magit-header "orange")
(set-face-background 'magit-item-highlight nil)
; open status is current buffer
(custom-set-variables
 '(magit-status-buffer-switch-function 'switch-to-buffer))

(require 'durendal)
(defun durendal-jack-in-with-coffee (&optional port-prompt)
  "Does durendal-jack-in plus watches & autocompiles coffeescript changes."
  (interactive "P")
  (durendal-jack-in port-prompt)
  (let ((root (locate-dominating-file default-directory "project.clj")))
	(shell-command (format "cd %s && coffee -w -o ./war/js/bin -c ./src/tl/coffee &" root))))

;; Actionscript
(require 'ecmascript-mode)
(require 'las3r-mode)
(add-to-list 'auto-mode-alist '("\\.as$" . ecmascript-mode))
(add-to-list 'auto-mode-alist '("\\.lsr$" . las3r-mode))

;; Javascript
(require 'coffee-mode)
(add-to-list 'auto-mode-alist '("\\.coffee$" . coffee-mode))
(add-to-list 'auto-mode-alist '("Cakefile" . coffee-mode))
(add-to-list 'auto-mode-alist '("\\.js$" . espresso-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . espresso-mode))
(autoload 'espresso-mode "espresso" nil t)
; mozrepl
(autoload 'moz-minor-mode "moz" "Mozilla Minor and Inferior Mozilla Modes" t)
(add-hook 'espresso-mode-hook 'espresso-custom-setup)
(defun espresso-custom-setup ()
  (moz-minor-mode 1))

;; Dired
;; http://stackoverflow.com/questions/1110118/in-emacs-dired-how-to-find-visit-multiple-files
(eval-after-load "dired"
  '(progn
     (define-key dired-mode-map "F" 'my-dired-find-file)
     (defun my-dired-find-file (&optional arg)
       "Open each of the marked files, or the file under the point, or when prefix arg, the next N files "
       (interactive "P")
       (let* ((fn-list (dired-get-marked-files nil arg)))
         (mapc 'find-file fn-list)))))

;;; shell

;; open the file under point
(global-set-key (kbd "C-M-f") 'find-file-at-point)
;; properly colors directories
(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
;; prevent welcome screen from displaying
;; and open a shell buffer in its place.
(setq inhibit-splash-screen t)
(shell)
