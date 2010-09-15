(require 'color-theme)

(defun color-theme-sanityinc-light ()
  "Based on color-theme-pierson"
  (interactive)
  (color-theme-install
   (let ((background-white "AntiqueWhite1")
         (foreground-black "Grey15")
         (green "DarkOliveGreen4")
         (orange "DarkOrange3")
         (purple "Purple")
         (dark-purple "MediumPurple3")
         (blue "RoyalBlue3")
         (light-blue "CadetBlue")
         (grey "grey")
         (red "Maroon")
         (brown "DarkGoldenrod4")
         (orchid "Orchid")
         (dark-rose "MistyRose4")
         (light-rose "MistyRose2")
         (pale-pink "PaleVioletRed")
         (bright-white "ivory1")
         (yellow "#edd400"))
     `(color-theme-sanityinc-light
       ((background-color . ,background-white)
        (background-mode . light)
        (border-color . ,foreground-black)
        (cursor-color . ,orchid)
        (foreground-color . ,foreground-black)
        (mouse-color . ,orchid))

       ;; Standard font lock faces
       (default ((t (nil))))
       (bold ((t (:bold t))))
       (bold-italic ((t (:italic t :bold t))))
       (underline ((t (:underline t))))
       (italic ((t (:italic t))))
       (font-lock-builtin-face ((t (:foreground ,dark-purple))))
       (font-lock-comment-delimiter-face ((t (:foreground ,dark-rose))))
       (font-lock-comment-face ((t (:foreground ,dark-rose))))
       (font-lock-constant-face ((t (:foreground ,light-blue))))
       (font-lock-doc-face ((t (:foreground ,pale-pink))))
       (font-lock-doc-string-face ((t (:foreground ,pale-pink))))
       (font-lock-function-name-face ((t (:foreground ,green))))
       (font-lock-keyword-face ((t (:foreground ,blue))))
       (font-lock-preprocessor-face ((t (:foreground ,orange))))
       (font-lock-string-face ((t (:foreground ,orange))))
       (font-lock-type-face ((t (:foreground ,purple))))
       (font-lock-variable-name-face ((t (:foreground ,green))))
       (font-lock-warning-face ((t (:bold t :foreground ,red))))

       ;; Search
       (isearch ((t (:foreground ,background-white :background ,green))))
       (isearch-lazy-highlight-face ((t (:foreground ,foreground-black :background ,yellow))))

       ;; IDO
       (ido-subdir ((t (:foreground ,purple))))
       (ido-first-match ((t (:foreground ,orange))))
       (ido-only-match ((t (:foreground ,green))))

       ;; Emacs interface
       (fringe ((t (:background ,bright-white))))
       (border ((t (:background ,bright-white))))
       (border-glyph ((t (nil))))
       (highlight ((t (:background "darkseagreen2"))))
       (gui-element ((t (:background "#0f0f0f" :foreground ,foreground-black))))
       (mode-line ((t (:foreground ,background-white :background ,foreground-black))))
       (mode-line-buffer-id ((t (:foreground ,background-white :background nil :bold t))))
       (mode-line-inactive ((t (:foreground ,background-white :background ,dark-rose))))
       (minibuffer-prompt ((t (:foreground ,blue))))
       (region ((t (:background ,grey))))
       (secondary-selection ((t (:background "paleturquoise"))))

       ;; Parenthesis matching
       (show-paren-match-face ((t (:background "turquoise"))))
       (show-paren-mismatch-face ((t (:background ,purple :foreground ,background-white))))

       (slime-highlight-edits-face ((t (:background ,light-rose))))

       (link ((t (:foreground ,blue :underline t))))
       (org-link ((t (:foreground ,blue :underline t))))
       (org-date ((t (:foreground ,blue :underline t))))
       (org-done ((t (:foreground ,green))))
       (org-todo ((t (:foreground ,red))))
       (org-special-keyword ((t (:foreground ,orange))))
       (org-level-1 ((t (:foreground ,brown))))
       (org-level-2 ((t (:foreground ,foreground-black))))
       (org-level-3 ((t (:foreground ,dark-purple))))
       (org-column ((t (:background "palegoldenrod"))))
       (org-warning ((t (:bold t :foreground ,red))))
       (org-scheduled-previously ((t (:foreground ,orange))))

       (highlight-80+ ((t (:background "palegoldenrod"))))
       ))))

(provide 'color-theme-sanityinc)