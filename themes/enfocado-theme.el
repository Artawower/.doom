;;; enfocado-theme.el --- Theme stolen from codeart -*- lexical-binding: t; no-byte-compile: t; -*-
;;
;; Copyright (C) 2021 Artur Yaroshenko
;;
;; Keywords: custom themes, faces
;; Homepage: https://github.com/Artawower/.doom
;; Package-Requires: ((emacs "25.1") (cl-lib "0.5") (doom-themes "2.2.1"))
;;
;;; Commentary:
;;
;; Vim copied theme
;;; Code:

(require 'doom-themes)


;;; Variables

(defgroup enfocado-theme nil
  "Options for the `enfocado' theme."
  :group 'doom-themes)

(defcustom enfocado-brighter-modeline nil
  "If non-nil, more vivid colors will be used to style the mode-line."
  :group 'enfocado-theme
  :type 'boolean)

(defcustom enfocado-brighter-comments nil
  "If non-nil, comments will be highlighted in more vivid colors."
  :group 'enfocado-theme
  :type 'boolean)

(defcustom enfocado-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line.
Can be an integer to determine the exact padding."
  :group 'enfocado-theme
  :type '(choice integer boolean))


;;; Theme definition

(def-doom-theme enfocado
  "A dark theme inspired by Atom One Dark."

  ;; name        default   256           16
  ((bg         '("#181818" "#181818"      "black"  ))
   (fg         '("#B9B9B9" "#B9B9B9"     "brightwhite"  ))

   (bg-alt     '("#252525" "#252525"       "black"        ))
   (fg-alt     '("#DEDEDE" "#DEDEDE"     "white"        ))

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
   (red        '("#ED4A46" "#ED4A46" "red"          ))
   (orange     '("#E67F43" "#E67F43" "brightred"    ))
   (green      '("#70B433" "#70B433" "green"        ))
   (teal       '("#4db5bd" "#44b9b1" "brightgreen"  ))
   (yellow     '("#DBB32D" "#DBB32D" "yellow"       ))
   (blue       '("#368AEB" "#368AEB" "brightblue"   ))
   (dark-blue  '("#1c4a6e" "#1c4a6e" "blue"         ))
   (magenta    '("#EB6EB7" "#EB6EB7" "brightmagenta"))
   (violet     '("#A580E2" "#A580E2" "magenta"      ))
   (cyan       '("#3FC5B7" "#3FC5B7" "brightcyan"   ))
   (dark-cyan  '("#5699AF" "#5699AF" "cyan"         ))
   (dark-red '("#FF5E56" "#FF5E56" "red"))

   ;; These are the "universal syntax classes" that doom-themes establishes.
   ;; These *must* be included in every doom themes, or your theme will throw an
   ;; error, as they are used in the base theme defined in doom-themes-base.
   (highlight      blue)
   (vertical-bar   (doom-darken base1 0.1))
   (selection      dark-blue)
   (builtin        blue)
   (comments       (if enfocado-brighter-comments dark-cyan base5))
   (doc-comments   (doom-lighten (if enfocado-brighter-comments dark-cyan base5) 0.25))
   (constants      violet)
   (functions      green)
   (keywords       blue)
   (methods        violet)
   (operators      blue)
   (type           (doom-lighten blue 0.5))
   (strings        cyan)
   (variables      (doom-lighten blue 0.2))
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
   (modeline-bg              (if enfocado-brighter-modeline
                                 (doom-darken blue 0.45)
                               (doom-darken bg-alt 0.1)))
   (modeline-bg-alt          (if enfocado-brighter-modeline
                                 (doom-darken blue 0.475)
                               `(,(doom-darken (car bg-alt) 0.15) ,@(cdr bg))))
   (modeline-bg-inactive     `(,(car bg-alt) ,@(cdr base1)))
   (modeline-bg-inactive-alt `(,(doom-darken (car bg-alt) 0.1) ,@(cdr bg)))

   (enfocado-modeline-pad
    (when enfocado-padded-modeline
      (if (integerp enfocado-padded-modeline) doom-deep-atom-one-dark-padded-modeline 4))))


  ;;;; Base theme face overrides
  (((line-number &override) :foreground base4)
   ((line-number-current-line &override) :foreground fg)
   ((font-lock-comment-face &override)
    :background (if enfocado-brighter-comments (doom-lighten bg 0.05)))
   (mode-line
    :background modeline-bg :foreground modeline-fg
    :box (if enfocado-modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
   (mode-line-inactive
    :background modeline-bg-inactive :foreground modeline-fg-alt
    :box (if enfocado-modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive)))
   (mode-line-emphasis :foreground (if enfocado-brighter-modeline base8 highlight))

   ;;;; css-mode <built-in> / scss-mode
   (css-proprietary-property :foreground orange)
   (css-property             :foreground green)
   (css-selector             :foreground blue)
   ;;;; doom-modeline
   (doom-modeline-bar :background (if enfocado-brighter-modeline modeline-bg highlight))
   (doom-modeline-buffer-file :inherit 'mode-line-buffer-id :weight 'bold)
   (doom-modeline-buffer-path :inherit 'mode-line-emphasis :weight 'bold)
   (doom-modeline-buffer-project-root :foreground green :weight 'bold)
   ;;;; elscreen
   (elscreen-tab-other-screen-face :background "#353a42" :foreground "#1e2022")
   ;;;; ivy
   (ivy-current-match :background dark-blue :distant-foreground base0 :weight 'normal)
   ;;;; LaTeX-mode
   (font-latex-math-face :foreground green)
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
    :box (if enfocado-modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-alt)))
   (solaire-mode-line-inactive-face
    :inherit 'mode-line-inactive
    :background modeline-bg-inactive-alt
    :box (if enfocado-modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive-alt))))

  ;;;; Base theme variable overrides-
  ())

;;; enfocado-theme.el ends here
