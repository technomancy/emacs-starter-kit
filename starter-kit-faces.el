;;; starter-kit-faces.el --- Facelifts for the modes

(eval-after-load 'diff-mode
  '(progn
     (set-face-foreground 'diff-added "green4")
     (set-face-foreground 'diff-removed "red3")))

(eval-after-load 'ido-mode
  '(progn
     (set-face-background 'ido-first-match "blue3")
     (set-face-foreground 'ido-subdir "SteelBlue1")
     (set-face-foreground 'ido-only-match "blue3")
     (set-face-background 'ido-only-match "white")
     (set-face-foreground 'ido-incomplete-regexp "cornsilk1")
     (set-face-background 'ido-incomplete-regexp "green")))

(eval-after-load 'magit
  '(progn
     (set-face-foreground 'magit-diff-add "green3")
     (set-face-foreground 'magit-diff-del "red3")))

(eval-after-load 'nxhtml
  '(eval-after-load 'zenburn
     '(set-face-background 'mumamo-background-chunk-submode "gray22")))
