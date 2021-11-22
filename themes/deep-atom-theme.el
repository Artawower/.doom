;;; deep-atom-theme.el --- inspired by Deep Atom One Dark -*- lexical-binding: t; no-byte-compile: t; -*-
;;
;; Copyright (C) 2021 Artur Yaroshenko
;;
;; Keywords: custom themes, faces
;; Homepage: https://github.com/Artawower/.doom
;; Package-Requires: ((emacs "25.1") (cl-lib "0.5") (doom-themes "2.2.1"))
;;
;;; Commentary:
;;
;; Inspired by Atom's One Dark color scheme.
;; Stolen from https://github.com/navarasu/onedark.nvim/blob/master/lua/onedark/colors.lua
;;; Code:

(require 'doom-themes)


;;
;;; Variables

(defgroup deep-atom-theme nil
  "Options for the `deep-atom' theme."
  :group 'doom-themes)

(defcustom deep-atom-brighter-modeline nil
  "If non-nil, more vivid colors will be used to style the mode-line."
  :group 'deep-atom-theme
  :type 'boolean)

(defcustom deep-atom-brighter-comments nil
  "If non-nil, comments will be highlighted in more vivid colors."
  :group 'deep-atom-theme
  :type 'boolean)

(defcustom deep-atom-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line.
Can be an integer to determine the exact padding."
  :group 'deep-atom-theme
  :type '(choice integer boolean))


;;; Theme definition

(def-doom-theme deep-atom
  "A dark theme inspired by Atom One Dark."

  ;; name        default   256           16
  ((bg         '("#1a212e" "#21283b"      "black"  ))
   (fg         '("#a5b0c5" "#a5b0c5"     "brightwhite"  ))

   (bg-alt     '("#141b24" "black"       "black"        ))
   (fg-alt     '("#5B6268" "#2d2d2d"     "white"        ))

   (base0      '("#1B2229" "black"       "black"        ))
   (base1      '("#1c1f24" "#1e1e1e"     "brightblack"  ))
   (base2      '("#202328" "#2e2e2e"     "brightblack"  ))
   (base3      '("#23272e" "#262626"     "brightblack"  ))
   (base4      '("#3f444a" "#3f3f3f"     "brightblack"  ))
   (base5      '("#5B6268" "#525252"     "brightblack"  ))
   (base6      '("#73797e" "#6b6b6b"     "brightblack"  ))
   (base7      '("#9ca0a4" "#979797"     "brightblack"  ))
   (base8      '("#DFDFDF" "#dfdfdf"     "white"        ))

   (grey       base4)
   (red        '("#f65866" "#f65866" "red"          ))
   (orange     '("#dd9046" "#dd9046" "brightred"    ))
   (green      '("#8bcd5b" "#8bcd5b" "green"        ))
   (teal       '("#4db5bd" "#44b9b1" "brightgreen"  ))
   (yellow     '("#ECBE7B" "#ECBE7B" "yellow"       ))
   (blue       '("#41a7fc" "#41a7fc" "brightblue"   ))
   (dark-blue  '("#1c4a6e" "#1c4a6e" "blue"         ))
   (magenta    '("#c75ae8" "#c75ae8" "brightmagenta"))
   (violet     '("#c75ae8" "#a9a1e1" "magenta"      ))
   (cyan       '("#34bfd0" "#34bfd0" "brightcyan"   ))
   (dark-cyan  '("#5699AF" "#5699AF" "cyan"         ))

   ;; These are the "universal syntax classes" that doom-themes establishes.
   ;; These *must* be included in every doom themes, or your theme will throw an
   ;; error, as they are used in the base theme defined in doom-themes-base.
   (highlight      blue)
   (vertical-bar   (doom-darken base1 0.1))
   (selection      dark-blue)
   (builtin        blue)
   (comments       (if deep-atom-brighter-comments dark-cyan base5))
   (doc-comments   (doom-lighten (if deep-atom-brighter-comments dark-cyan base5) 0.25))
   (constants      red)
   (functions      blue)
   (keywords       violet)
   (methods        red)
   (operators      blue)
   (type           yellow)
   (strings        green)
   (variables      (doom-lighten red 0.2))
   (numbers        orange)
   (region         `(,(doom-lighten (car bg-alt) 0.15) ,@(doom-lighten (cdr base1) 0.35)))
   (error          red)
   (warning        yellow)
   (success        green)
   (vc-modified    orange)
   (vc-added       green)
   (vc-deleted     red)

   ;; These are extra color variables used only in this theme; i.e. they aren't
   ;; mandatory for derived themes.
   (modeline-fg              fg)
   (modeline-fg-alt          base5)
   (modeline-bg              (if deep-atom-brighter-modeline
                                 (doom-darken blue 0.45)
                               (doom-darken bg-alt 0.1)))
   (modeline-bg-alt          (if deep-atom-brighter-modeline
                                 (doom-darken blue 0.475)
                               `(,(doom-darken (car bg-alt) 0.15) ,@(cdr bg))))
   (modeline-bg-inactive     `(,(car bg-alt) ,@(cdr base1)))
   (modeline-bg-inactive-alt `(,(doom-darken (car bg-alt) 0.1) ,@(cdr bg)))

   (-modeline-pad
    (when deep-atom-padded-modeline
      (if (integerp deep-atom-padded-modeline) doom-deep-atom-one-dark-padded-modeline 4))))


  ;;;; Base theme face overrides
  (((line-number &override) :foreground base4)
   ((line-number-current-line &override) :foreground fg)
   ((font-lock-comment-face &override)
    :background (if deep-atom-brighter-comments (doom-lighten bg 0.05)))
   (mode-line
    :background modeline-bg :foreground modeline-fg
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
   (mode-line-inactive
    :background modeline-bg-inactive :foreground modeline-fg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive)))
   (mode-line-emphasis :foreground (if deep-atom-brighter-modeline base8 highlight))

   ;;;; css-mode <built-in> / scss-mode
   (css-proprietary-property :foreground orange)
   (css-property             :foreground green)
   (css-selector             :foreground blue)
   ;;;; doom-modeline
   (doom-modeline-bar :background (if deep-atom-brighter-modeline modeline-bg highlight))
   (doom-modeline-buffer-file :inherit 'mode-line-buffer-id :weight 'bold)
   (doom-modeline-buffer-path :inherit 'mode-line-emphasis :weight 'bold)
   (doom-modeline-buffer-project-root :foreground green :weight 'bold)
   ;;;; elscreen
   (elscreen-tab-other-screen-face :background "#353a42" :foreground "#1e2022")
   ;;;; ivy
   (ivy-current-match :background red :foreground base0 :distant-foreground base0 :weight 'bold)
   (ivy-minibuffer-match-face-2 :background green :foreground base0)

   ;;;; LaTeX-mode
   (font-latex-math-face :foreground green)
;;; Treemacs
(font-lock-doc-face :foreground (doom-lighten red 0.1))
   ;;;; markdown-mode
   (markdown-markup-face :foreground base5)
   (markdown-header-face :inherit 'bold :foreground red)
   ((markdown-code-face &override) :background (doom-lighten base3 0.05))
   ;;;; rjsx-mode
   (rjsx-tag :foreground red)
   (rjsx-attr :foreground orange)
   ;;;; solaire-mode
   (solaire-mode-line-face
    :inherit 'mode-line
    :background modeline-bg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-alt)))
   (solaire-mode-line-inactive-face
    :inherit 'mode-line-inactive
    :background modeline-bg-inactive-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive-alt))))

  ;;;; Base theme variable overrides-
  ())

;;; deep-atom-theme.el ends here
