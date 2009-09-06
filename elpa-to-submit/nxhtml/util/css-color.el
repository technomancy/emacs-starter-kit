;;; css-color.el --- Highlight and edit CSS colors

(defconst css-color:version "0.03")
;; Copyright (C) 2008  Niels Giesen

;; Author: Niels Giesen
;; Keywords: processes, css, extensions, tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Edit css-colors in hex, rgb or hsl notation in-place, with
;; immediate feedback by font-locking. Cycle between color-spaces.

;; Usage:

;; (autoload 'css-color-mode "css-color" "" t)
;; (add-hook 'css-mode-hook 'css-color-mode-turn-on)

;; Css-Css-color.el propertizes colours in a CSS stylesheet found by
;; font-locking code with a keymap. From that keymap, you can easily
;; adjust values such as red green and blue, hue, saturation and
;; value, or switch between different color (space) notations.

;; It supports all 'css-colors', so hex, rgb(), hsl() and even HTML
;; color names (although I wouldn't use them myself, it is nice to be
;; able to quickly convert those), can be used and switched between.

;; The rgb() notation can be expressed either in percentages or in
;; values between 0-255.

;; You can cycle between the different formats (with SPACE), so that
;; it is possible to edit the color in hsl mode (which is more
;; intuitive than hsv, although hsv has its merits too), and switch
;; back to rgb or hex if so desired.

;; With point on a color, the keys - and = to are bound to the down
;; and up functions for channels (or 'fields'). Toggling percentage
;; in rgb() is done with the % key (not sure if that is wise
;; though). The TAB key is bound to go to the next channel, cycling
;; when at the end. color.el propertizes the longhand hexcolours
;; found by the

;; Caveats:

;; Notation cycling can often introduce small errors inherent to
;; switching color spaces. Currently there is no check nor a warning
;; for that.

;; ToDo:

;; Try and fix those conversion inaccuracies. This cannot be done
;; completely I guess. But maybe we can check whether this has
;; occured, and then warn.

;;; Change log:

;; 2009-01-11 Lennart Borgman
;;   - Minor code clean up.
;; 2009-05-23 Lennart Borgman
;;   - Let bound m1 and m2.

;;; Code:
(eval-when-compile (require 'cl))

;;;###autoload
(defgroup css-color ()
  "Customization group for library `css-color'."
  :group 'css
  :group 'nxhtml)

(defun css-color-turn-on-in-buffer ()
  "Turn on `css-color-mode' in `css-mode'."
  (when (derived-mode-p 'css-mode)
    (css-color-mode 1)))

;;;###autoload
(define-globalized-minor-mode css-color-global-mode css-color-mode
  css-color-turn-on-in-buffer
  :group 'css-color)

(defconst css-color-hex-chars "0123456789abcdefABCDEF"
  "Composing chars in hexadecimal notation, save for the hash (#) sign.")

(defconst css-color-hex-re
  "#\\([a-fA-F[:digit:]]\\{6\\}\\|[a-fA-F[:digit:]]\\{3\\}\\)")

(defconst css-color-hsl-re
  "hsla?(\\([[:digit:]]\\{1,3\\}\\),[[:space:]]*\\([[:digit:]]\\{1,3\\}\\(?:\.?[[:digit:]]*\\)\\)%,[[:space:]]*\\([[:digit:]]\\{1,3\\}\\)\\(?:\.?[[:digit:]]*\\)%)")

(defconst css-color-rgb-re
  "rgba?(\\([[:digit:]]\\{1,3\\}\\(?:\.?[[:digit:]]*%\\)?\\),[[:space:]]*\\([[:digit:]]\\{1,3\\}\\(?:\.?[[:digit:]]*%\\)?\\),[[:space:]]*\\([[:digit:]]\\{1,3\\}\\(?:\.?[[:digit:]]*%\\)?\\)\\(:?,[[:space:]]*\\(0\.[0-9]+\\|1\\)\\)?)")

(defconst css-color-html-colors
  '(("AliceBlue" "#F0F8FF")
    ("AntiqueWhite" "#FAEBD7")
    ("Aqua" "#00FFFF")
    ("Aquamarine" "#7FFFD4")
    ("Azure" "#F0FFFF")
    ("Beige" "#F5F5DC")
    ("Bisque" "#FFE4C4")
    ("Black" "#000000")
    ("BlanchedAlmond" "#FFEBCD")
    ("Blue" "#0000FF")
    ("BlueViolet" "#8A2BE2")
    ("Brown" "#A52A2A")
    ("BurlyWood" "#DEB887")
    ("CadetBlue" "#5F9EA0")
    ("Chartreuse" "#7FFF00")
    ("Chocolate" "#D2691E")
    ("Coral" "#FF7F50")
    ("CornflowerBlue" "#6495ED")
    ("Cornsilk" "#FFF8DC")
    ("Crimson" "#DC143C")
    ("Cyan" "#00FFFF")
    ("DarkBlue" "#00008B")
    ("DarkCyan" "#008B8B")
    ("DarkGoldenRod" "#B8860B")
    ("DarkGray" "#A9A9A9")
    ("DarkGrey" "#A9A9A9")
    ("DarkGreen" "#006400")
    ("DarkKhaki" "#BDB76B")
    ("DarkMagenta" "#8B008B")
    ("DarkOliveGreen" "#556B2F")
    ("Darkorange" "#FF8C00")
    ("DarkOrchid" "#9932CC")
    ("DarkRed" "#8B0000")
    ("DarkSalmon" "#E9967A")
    ("DarkSeaGreen" "#8FBC8F")
    ("DarkSlateBlue" "#483D8B")
    ("DarkSlateGray" "#2F4F4F")
    ("DarkSlateGrey" "#2F4F4F")
    ("DarkTurquoise" "#00CED1")
    ("DarkViolet" "#9400D3")
    ("DeepPink" "#FF1493")
    ("DeepSkyBlue" "#00BFFF")
    ("DimGray" "#696969")
    ("DimGrey" "#696969")
    ("DodgerBlue" "#1E90FF")
    ("FireBrick" "#B22222")
    ("FloralWhite" "#FFFAF0")
    ("ForestGreen" "#228B22")
    ("Fuchsia" "#FF00FF")
    ("Gainsboro" "#DCDCDC")
    ("GhostWhite" "#F8F8FF")
    ("Gold" "#FFD700")
    ("GoldenRod" "#DAA520")
    ("Gray" "#808080")
    ("Grey" "#808080")
    ("Green" "#008000")
    ("GreenYellow" "#ADFF2F")
    ("HoneyDew" "#F0FFF0")
    ("HotPink" "#FF69B4")
    ("IndianRed" "#CD5C5C")
    ("Indigo" "#4B0082")
    ("Ivory" "#FFFFF0")
    ("Khaki" "#F0E68C")
    ("Lavender" "#E6E6FA")
    ("LavenderBlush" "#FFF0F5")
    ("LawnGreen" "#7CFC00")
    ("LemonChiffon" "#FFFACD")
    ("LightBlue" "#ADD8E6")
    ("LightCoral" "#F08080")
    ("LightCyan" "#E0FFFF")
    ("LightGoldenRodYellow" "#FAFAD2")
    ("LightGray" "#D3D3D3")
    ("LightGrey" "#D3D3D3")
    ("LightGreen" "#90EE90")
    ("LightPink" "#FFB6C1")
    ("LightSalmon" "#FFA07A")
    ("LightSeaGreen" "#20B2AA")
    ("LightSkyBlue" "#87CEFA")
    ("LightSlateGray" "#778899")
    ("LightSlateGrey" "#778899")
    ("LightSteelBlue" "#B0C4DE")
    ("LightYellow" "#FFFFE0")
    ("Lime" "#00FF00")
    ("LimeGreen" "#32CD32")
    ("Linen" "#FAF0E6")
    ("Magenta" "#FF00FF")
    ("Maroon" "#800000")
    ("MediumAquaMarine" "#66CDAA")
    ("MediumBlue" "#0000CD")
    ("MediumOrchid" "#BA55D3")
    ("MediumPurple" "#9370D8")
    ("MediumSeaGreen" "#3CB371")
    ("MediumSlateBlue" "#7B68EE")
    ("MediumSpringGreen" "#00FA9A")
    ("MediumTurquoise" "#48D1CC")
    ("MediumVioletRed" "#C71585")
    ("MidnightBlue" "#191970")
    ("MintCream" "#F5FFFA")
    ("MistyRose" "#FFE4E1")
    ("Moccasin" "#FFE4B5")
    ("NavajoWhite" "#FFDEAD")
    ("Navy" "#000080")
    ("OldLace" "#FDF5E6")
    ("Olive" "#808000")
    ("OliveDrab" "#6B8E23")
    ("Orange" "#FFA500")
    ("OrangeRed" "#FF4500")
    ("Orchid" "#DA70D6")
    ("PaleGoldenRod" "#EEE8AA")
    ("PaleGreen" "#98FB98")
    ("PaleTurquoise" "#AFEEEE")
    ("PaleVioletRed" "#D87093")
    ("PapayaWhip" "#FFEFD5")
    ("PeachPuff" "#FFDAB9")
    ("Peru" "#CD853F")
    ("Pink" "#FFC0CB")
    ("Plum" "#DDA0DD")
    ("PowderBlue" "#B0E0E6")
    ("Purple" "#800080")
    ("Red" "#FF0000")
    ("RosyBrown" "#BC8F8F")
    ("RoyalBlue" "#4169E1")
    ("SaddleBrown" "#8B4513")
    ("Salmon" "#FA8072")
    ("SandyBrown" "#F4A460")
    ("SeaGreen" "#2E8B57")
    ("SeaShell" "#FFF5EE")
    ("Sienna" "#A0522D")
    ("Silver" "#C0C0C0")
    ("SkyBlue" "#87CEEB")
    ("SlateBlue" "#6A5ACD")
    ("SlateGray" "#708090")
    ("SlateGrey" "#708090")
    ("Snow" "#FFFAFA")
    ("SpringGreen" "#00FF7F")
    ("SteelBlue" "#4682B4")
    ("Tan" "#D2B48C")
    ("Teal" "#008080")
    ("Thistle" "#D8BFD8")
    ("Tomato" "#FF6347")
    ("Turquoise" "#40E0D0")
    ("Violet" "#EE82EE")
    ("Wheat" "#F5DEB3")
    ("White" "#FFFFFF")
    ("WhiteSmoke" "#F5F5F5")
    ("Yellow" "#FFFF00")
    ("YellowGreen" "#9ACD32")))

(defvar css-color-html-re
  (concat "\\<\\("
	  (funcall 'regexp-opt
		   (mapcar 'car css-color-html-colors))
	  "\\)\\>"))

(defconst
  css-color-color-re
  "\\(?:#\\(?:[a-fA-F[:digit:]]\\{6\\}\\|[a-fA-F[:digit:]]\\{3\\}\\)\\|hsl(\\(?:[[:digit:]]\\{1,3\\}\\),[[:space:]]*\\(?:[[:digit:]]\\{1,3\\}\\)%,[[:space:]]*\\(?:[[:digit:]]\\{1,3\\}\\)%)\\|rgba?(\\(?:[[:digit:]]\\{1,3\\}%?\\),[[:space:]]*\\(?:[[:digit:]]\\{1,3\\}%?\\),[[:space:]]*\\(?:[[:digit:]]\\{1,3\\}%?\\)\\(?:,[[:space:]]*\\(?:0.[0-9]+\\|1\\)\\)?)\\)"
  "Regular expression containing only shy groups matching any type of CSS color")

;; (defconst css-color-color-re
;;   (concat "\\(?1:"
;;   (mapconcat
;;    'identity
;;    (list css-color-hex-re
;; 	 css-color-hsl-re
;; 	 css-color-rgb-re) "\\|")
;;   "\\)"))

(defvar css-color-keywords
  `((,css-color-hex-re
     (0
      (progn
	(when (= 7 (- (match-end 0)
		      (match-beginning 0)))
	  (put-text-property (match-beginning 0)
			     (match-end 0)
			     'keymap css-color-map))
	(put-text-property (match-beginning 0)
			   (match-end 0)
			   'color-type 'hex)
	(put-text-property (match-beginning 0)
			   (match-end 0)
			   'rear-nonsticky t)
	(put-text-property (match-beginning 0)
			   (match-end 0)
			   'face (list :background
				       (match-string-no-properties 0)
				       :foreground
				       (css-color-foreground-color
                                     (match-string-no-properties 0)))))))
  (,css-color-html-re
   (0
     (let ((color
           (css-color-string-name-to-hex (match-string-no-properties 0))))
     (put-text-property (match-beginning 0)
                        (match-end 0)
                        'keymap css-color-generic-map)
	(put-text-property (match-beginning 0)
			   (match-end 0)
			   'color-type 'name)
	(put-text-property (match-beginning 0)
			   (match-end 0)
			   'rear-nonsticky t)
	(put-text-property (match-beginning 0)
			   (match-end 0)
			   'face (list :background
				       color
				       :foreground
				       (css-color-foreground-color
					color))))))
    (,css-color-hsl-re
     (0
      (let ((color (concat "#" (apply 'css-color-hsl-to-hex
				      (mapcar 'string-to-number
					      (list
					       (match-string-no-properties 1)
					       (match-string-no-properties 2)
					       (match-string-no-properties 3)))))))
	(put-text-property (match-beginning 0)
			   (match-end 0)
			   'keymap css-color-generic-map)
	(put-text-property (match-beginning 0)
			   (match-end 0)
			   'color-type 'hsl)
	(put-text-property (match-beginning 0)
			   (match-end 0)
			   'rear-nonsticky t)
	(put-text-property (match-beginning 0)
			   (match-end 0)
			   'face (list :background
				       color
				       :foreground
				       (css-color-foreground-color
					color))))))
    (,css-color-rgb-re
     (0
      (let ((color (css-color-string-rgb-to-hex (match-string-no-properties 0))))
	(put-text-property (match-beginning 0)
			   (match-end 0)
			   'keymap css-color-generic-map)
	(put-text-property (match-beginning 0)
			   (match-end 0)
			   'color-type 'rgb)
	(put-text-property (match-beginning 0)
			   (match-end 0)
			   'rear-nonsticky t)
	(put-text-property (match-beginning 0)
			   (match-end 0)
			   'face (list :background
				       color
				       :foreground
				       (css-color-foreground-color
					color))))))))
;;;###autoload
(define-minor-mode css-color-mode
  "Show hex color literals with the given color as background.
In this mode hexadecimal colour specifications like #3253ff are
displayed with the specified colour as background.

Certain keys are bound to special colour editing commands when
point is at a hexadecimal colour:

\\{css-color-map}"
  :initial-value nil
  :group 'css-color
  (unless font-lock-defaults
    (error "Can't use css-color-mode for this major mode"))
  (if css-color-mode
      (progn
	(unless font-lock-mode (font-lock-mode 1))
        (css-color-font-lock-hook-fun)
        (add-hook 'font-lock-mode-hook 'css-color-font-lock-hook-fun nil t))
    (remove-hook 'font-lock-mode-hook 'css-color-font-lock-hook-fun t)
    (font-lock-remove-keywords nil css-color-keywords))
  ;;(font-lock-fontify-buffer)
  (save-restriction
    (widen)
    (mumamo-mark-for-refontification (point-min) (point-max))))

(put 'css-color-mode 'permanent-local t)

(defun css-color-font-lock-hook-fun ()
  "Add css-color pattern to font-lock's."
  (if font-lock-mode
      (font-lock-add-keywords nil css-color-keywords t)
    (css-color-mode -1)))

(defvar css-color-map
  (let ((m (make-sparse-keymap)))
    (define-key m "=" 'css-color-up)
    (define-key m "-" 'css-color-down)
    (define-key m "h" 'css-color-hue-up)
    (define-key m "H" 'css-color-hue-down)
    (define-key m "s" 'css-color-saturation-up)
    (define-key m "S" 'css-color-saturation-down)
    (define-key m "v" 'css-color-value-up)
    (define-key m "V" 'css-color-value-down)
    (define-key m "\t" 'css-color-next-channel)
    (define-key m " " 'css-color-cycle-type)
    m)
  "Mode map for `css-color-minor-mode'")

(defvar css-color-generic-map
  (let ((m (make-sparse-keymap)))
    (define-key m "=" 'css-color-num-up)
    (define-key m "-" 'css-color-num-down)
    (define-key m " " 'css-color-cycle-type)
    (define-key m "%" 'css-color-toggle-percentage)
    (define-key m "\t" 'css-color-next-channel)
    m)
  "Mode map for simple numbers in `css-color-minor-mode'")

(defun css-color-pal-lumsig (r g b)
  "Return PAL luminance signal, but in range 0-255."
  (+
   (* 0.3 r)
   (* 0.59 g)
   (* 0.11 b)))

(defun css-color-foreground-color (hex-color)
  (multiple-value-bind (r g b) (css-color-hex-to-rgb hex-color)
    (if (< (css-color-pal-lumsig r g b) 128)
	"#fff"
      "#000")))

;; Normalizing funs
(defun css-color-normalize-hue (h)
  (mod (+ (mod h 360) 360) 360))

(defun css-color-within-bounds (num min max)
  (min (max min num) max))

;; Source: hex
(defun css-color-hex-to-rgb (str)
  (cond
   ((not (string-match "^#?[a-fA-F[:digit:]]*$" str))
    (error "No valid hexadecimal: %s" str))
   ((= 0 (length str))
    nil)
   ((= (aref str 0) 35)
    (css-color-hex-to-rgb (substring str 1)))
   (;;(oddp (length str))
    (= (mod (length str) 2) 1)
    (css-color-hex-to-rgb (mapconcat (lambda (c)
				  (make-string 2 c))
				(string-to-list str) "")))
   (t (cons (string-to-number (substring str 0 2) 16)
	    (css-color-hex-to-rgb (substring str 2))))))

(defun css-color-hex-to-hsv (hex)
  (multiple-value-bind (r g b) (css-color-hex-to-rgb hex)
    (css-color-rgb-to-hsv r g b)))

;; Source: rgb
(defun css-color-rgb-to-hex (r g b)
  "Return r g b as #rrggbb in hexadecimal, propertized to have
the keymap `css-color-map'"
   (format "%02x%02x%02x" r g b))			     ;val

(defun css-color-rgb-to-hsv (r g b)
  "Return list of (hue saturation value).
Arguments are: R = red; G = green; B = blue.
Measure saturation and value on a scale from 0 - 100.
GIMP-style, that is."
  (let* ((r (float r))
	 (g (float g))
	 (b (float b))
	 (max (max r g b))
	 (min (min r g b)))
    (values
	  (round
	   (cond ((and (= r g) (= g b)) 0)
		 ((and (= r max)
		       (>= g b))
		  (* 60 (/ (- g b) (- max min))))
		 ((and (= r max)
		       (< g b))
		  (+ 360 (* 60 (/ (- g b) (- max min)))))
		 ((= max g)
		  (+ 120 (* 60 (/ (- b r) (- max min)))))
		 ((= max b)
		  (+ 240 (* 60 (/ (- r g) (- max min)))))))  ;hue
	  (round (* 100 (if (= max 0) 0 (- 1 (/ min max))))) ;sat
	  (round (/ max 2.55)))))

(defun css-color-rgb-to-hsl (r g b)
  "Return R G B (in range 0-255) converted to HSL (0-360 for hue, rest in %)"
  (let* ((r (/ r 255.0))
	 (g (/ g 255.0))
	 (b (/ b 255.0))
	 (h 0)
	 (s 0)
	 (l 0)
	 (v (max r g b))
	 (m (min r g b))
	 (l (/ (+ m v) 2.0))
	 (vm 0)
	 (r2 0)
	 (g2 0)
	 (b2 0))
    (multiple-value-bind (h s v)
	(if (<= l 0)
	    (values h s l)
	  (setq vm (- v m)
		s vm)
	  (if (>= 0 s)
	      (values h s l)
	    (setq s (/ s (if (<= l 0.5)
			     (+ v m)
			   (- 2.0 v m))))
	    (if (not (= 0 vm))
		(setq r2 (/ (- v r) vm)
		      g2 (/ (- v g) vm)
		      b2 (/ (- v b) vm)))
	    (cond ((= r v)
		   (setq h (if (= g m)
			       (+ 5.0 b2)
			     (- 1.0 g2))))
		  ((= g v)
		   (setq h (if (= b m)
			       (+ 1.0 r2)
			     (- 3.0 b2))))
		  (t
		   (setq h (if (= r m)
			       (+ 3.0 g2)
			     (- 5.0 r2)))))
	    (values (/ h 6.0) s l)))
      (list (round(* 360 h))
	    (* 100 s)
	    (* 100 l)))))

;; Source: hsv
(defun css-color-hsv-to-hsl (h s v)
  (multiple-value-bind (r g b) (css-color-hsv-to-rgb h s v)
    (css-color-rgb-to-hsl r g b)))

(defun css-color-hsv-to-hex (h s v)
   (apply 'css-color-rgb-to-hex (css-color-hsv-to-rgb h s v)))

(defun css-color-hsv-to-rgb (h s v)
  "Convert a point in the Hue, Saturation, Value (aka Brightness)
color space to list of normalized Red, Green, Blue values.

HUE is an angle in the range of 0 degrees inclusive to 360
exclusive.  The remainder of division by 360 is used for
out-of-range values.
SATURATION is in the range of 0 to 100.
VALUE is in the range of 0 to 100.
Returns a list of values in the range of 0 to 255.
"
  ;; Coerce to float and get hue into range.
  (setq h (mod h 360.0)
        s (/ (float s) 100)
        v (/ (float v) 100))
  (let* ((hi (floor h 60.0))
         (f (- (/ h 60.0) hi))
         (p (* v (- 1.0 s)))
         (q (* v (- 1.0 (* f s))))
         ;; cannot use variable t, obviously.
         (u (* v (- 1.0 (* (- 1.0 f) s))))
         r g b)
    (case hi
      (0 (setq r v g u b p))
      (1 (setq r q g v b p))
      (2 (setq r p g v b u))
      (3 (setq r p g q b v))
      (4 (setq r u g p b v))
      (5 (setq r v g p b q)))
    (mapcar (lambda (color) (round (* 255 color))) (list r g b))))

(defun css-color-hsv-to-prop-hexstring (color-data)
  (propertize
   (apply 'css-color-hsv-to-hex color-data)
   'keymap css-color-map
   'color color-data))

;; Source: hsl
(defun css-color-hsl-to-rgb-fractions (h s l)
  (let (m1 m2)
    (if (<= l 0.5)
	(setq m2 (* l (+ s 1)))
      (setq m2 (- (+ l s) (* l s))))
    (setq m1 (- (* l 2) m2))
    (values (css-color-hue-to-rgb m1 m2 (+ h (/ 1 3.0)))
	    (css-color-hue-to-rgb m1 m2 h)
	    (css-color-hue-to-rgb m1 m2 (- h (/ 1 3.0))))))

(defun css-color-hsl-to-rgb (h s l)
  (multiple-value-bind (r g b)
      (css-color-hsl-to-rgb-fractions
       (/ h;; (css-color-normalize-hue h)
	  360.0)
       (/ s 100.0)
       (/ l 100.0))
    (values (css-color-within-bounds (* 256 r) 0 255)
	    (css-color-within-bounds (* 256 g) 0 255)
	    (css-color-within-bounds (* 256 b) 0 255))))

(defun css-color-hsl-to-hex (h s l)
  (apply 'css-color-rgb-to-hex
	 (css-color-hsl-to-rgb h s l)))

(defun css-color-hue-to-rgb (x y h)
  (when (< h 0) (incf h))
  (when (> h 1) (decf h))
   (cond ((< h (/ 1 6.0))
	  (+ x (* (- y x) h 6)))
	 ((< h 0.5) y)
	 ((< h (/ 2.0 3.0))
	  (+ x (* (- y x) (- (/ 2.0 3.0) h) 6)))
	 (t x)))

(defun css-color-parse-hsl (str)
  (string-match
   css-color-hsl-re
   str)
  (mapcar 'string-to-number
	  (list
	   (match-string 1 str)
	   (match-string 2 str)
	   (match-string 3 str))))

(defun css-color-inchue (color incr)
  (multiple-value-bind (h s v) color
    (css-color-hsv-to-prop-hexstring
     (list (+ incr h) s v))))

(defun css-color-incsat (color incr)
  (multiple-value-bind (h s v) color
    (css-color-hsv-to-prop-hexstring
     (list h (css-color-within-bounds (+ incr s) 0 100) v))))

(defun css-color-incval (color incr)
  (multiple-value-bind (h s v) color
    (css-color-hsv-to-prop-hexstring
     (list h s (css-color-within-bounds (+ incr v) 0 100)))))

(defun css-color-hexval-beginning ()
  (skip-chars-backward css-color-hex-chars)
  (if (= (char-after) 35)
      (forward-char 1)))

(defun css-color-replcolor-at-p (fun increment)
  (let ((pos (point)))
    (css-color-hexval-beginning)
    (insert
     (funcall fun
	      (css-color-get-color-at-point)
	      increment))
    (delete-region (point) (+ (point) 6))
    (goto-char pos)))

(defun css-color-get-color-at-point ()
  (save-excursion
    (css-color-hexval-beginning)
    (let ((saved-color (get-text-property (point) 'color)))
      (or saved-color
	  (css-color-hex-to-hsv
	   (buffer-substring-no-properties (point) (+ (point) 6)))))))

(defun css-color-adj-hue-at-p (increment)
  (interactive "p")
  (css-color-replcolor-at-p 'css-color-inchue increment))

(defun css-color-adj-saturation-at-p (increment)
  (interactive "p")
  (css-color-replcolor-at-p 'css-color-incsat increment))

(defun css-color-adj-value-at-p (increment)
  (interactive "p")
  (css-color-replcolor-at-p 'css-color-incval increment))

(defun css-color-what-channel ()
  (let ((pos (point)))
    (prog1
	(/ (skip-chars-backward css-color-hex-chars) -2)
      (goto-char pos))))

(defun css-color-adjust-hex-at-p (incr)
  (interactive "p")
  (let ((pos (point))
	(channel (css-color-what-channel)))
    (css-color-hexval-beginning)
    (let ((rgb
	   (css-color-hex-to-rgb
	    (buffer-substring-no-properties (point)
					    (+ 6 (point))))))
      (setf (nth channel rgb)
	    (css-color-within-bounds
	     (+ incr (nth channel rgb))
	     0 255))
      (delete-region (point) (+ 6 (point)))
      (insert
       (propertize
	(apply 'format "%02x%02x%02x" rgb)
	'keymap css-color-map
	'color nil
	'rear-nonsticky t)))
    (goto-char pos)))

;; channels (r, g, b)
(defun css-color-up (val)
  (interactive "p")
  (css-color-adjust-hex-at-p val))

(defun css-color-down (val)
  (interactive "p")
  (css-color-adjust-hex-at-p (- val)))
;; hue
(defun css-color-hue-up (val)
  (interactive "p")
  (css-color-adj-hue-at-p val))

(defun css-color-hue-down (val)
  (interactive "p")
  (css-color-adj-hue-at-p (- val)))
;; saturation
(defun css-color-saturation-up (val)
  (interactive "p")
  (css-color-adj-saturation-at-p val))

(defun css-color-saturation-down (val)
  (interactive "p")
  (css-color-adj-saturation-at-p (- val)))
;; value
(defun css-color-value-up (val)
  (interactive "p")
  (css-color-adj-value-at-p val))

(defun css-color-value-down (val)
  (interactive "p")
  (css-color-adj-value-at-p (- val)))

(defun css-color-num-up (arg)
  (interactive "p")
  (save-excursion
    (let ((digits "1234567890"))
      (skip-chars-backward digits)
      (when
	  (looking-at "[[:digit:]]+")
	(replace-match
	 (propertize
	  (let ((num (+ (string-to-number (match-string 0)) arg)))
					;max = 100 when at percentage
	    (save-match-data
	      (cond ((looking-at "[[:digit:]]+%")
		     (setq num (min num 100)))
		    ((looking-back "hsla?(")
		     (setq num (css-color-normalize-hue num)))
		    ((memq 'color-type (text-properties-at (point)))
		     (setq num (min num 255)))))
	    (number-to-string num))
	  'keymap
	  css-color-generic-map))))))

(defun css-color-num-down (arg)
  (interactive "p")
  (save-excursion
    (let ((digits "1234567890"))
      (skip-chars-backward digits)
      (when
	  (looking-at "[[:digit:]]+")
	(replace-match
	 (propertize
	  (let ((num (- (string-to-number (match-string 0)) arg)))
					;max = 100 when at percentage
	    (save-match-data
	      (cond ((looking-back "hsla?(")
		     (setq num (css-color-normalize-hue num)))
		    (t (setq num (max 0 num)))))
	    (number-to-string num))
	  'keymap css-color-generic-map))))))


(defun css-color-beginning-of-color ()
  "Skip to beginning of color.

Return list of point and color-type."
  (while (memq 'color-type (text-properties-at (point)))
    (backward-char 1))
  (forward-char 1)
  (cons (point) (plist-get (text-properties-at (point)) 'color-type)))

(defun css-color-end-of-color ()
  "Skip to beginning of color.

Return list of point and color-type."
  (while (plist-get (text-properties-at (point)) 'color-type)
    (forward-char 1))
  (cons (point) (plist-get (text-properties-at (1- (point))) 'color-type)))

(defun css-color-color-info ()
  (destructuring-bind ((beg . type)
		       (end . type))
      (list
       (css-color-beginning-of-color)
       (css-color-end-of-color))
    (list beg end type (buffer-substring-no-properties beg end))))

(defconst css-color-type-circle '#1=(hex hsl rgb name . #1#))

(defun css-color-next-type (sym)
  (cadr (member sym css-color-type-circle)))

(defun css-color-cycle-type ()
  (interactive)
  (destructuring-bind (beg end type color) (css-color-color-info)
    (if (or (= 0 (length color)) (null type))
	(error "Not at color"))
    (delete-region beg end)
    (insert
     (propertize
     (funcall
      (intern-soft
     (format "css-color-string-%s-to-%s"
            type
            (css-color-next-type type)))
      color)
   'keymap (if (eq (css-color-next-type type) 'hex)
              css-color-map
              css-color-generic-map)     'rear-nonsticky t))
    (goto-char beg)))

(defun css-color-string-hex-to-hsl (str)
  (multiple-value-bind (h s l)
      (apply 'css-color-rgb-to-hsl
	     (css-color-hex-to-rgb str))
    (format "hsl(%d,%d%%,%d%%)"
	    h s l)))

(defun css-color-string-hsl-to-rgb (str)
  (multiple-value-bind (h s l)
      (css-color-parse-hsl str)
    (apply 'format
	     "rgb(%d,%d,%d)"
	     (mapcar 'round (css-color-hsl-to-rgb h s l)))))

(defun css-color-string-rgb-to-name (str)
  (let ((color (css-color-string-rgb-to-hex str)))
    (or (car (rassoc (list (upcase color)) css-color-html-colors)) ;if name ok
      color)))                                  ;else return hex
 (defun css-color-string-name-to-hex (str)
  (let ((str (downcase str)))
    (cadr (assoc-if
	   (lambda (a)
	     (string=
	      (downcase a)
	      str))
	   css-color-html-colors))))
 (defun css-color-string-rgb-to-hex (str)
 (save-match-data
    (string-match css-color-rgb-re str)
    (concat "#"
	    (apply 'css-color-rgb-to-hex
		   (mapcar
		    ;;'string-to-number
		    (lambda (s)
		      (if (= (aref s (1- (length s))) ?\%)
			  (round (* (string-to-number s) 2.55))
			(string-to-number s)))
		    (list
		     (match-string-no-properties 1 str)
		     (match-string-no-properties 2 str)
		     (match-string-no-properties 3 str)))))))

(defun css-color-string-hsl-to-hex (str)
  (concat "#" (apply 'css-color-hsl-to-hex (css-color-parse-hsl str))))

(defun css-color-next-channel ()
  (interactive)
  (multiple-value-bind (beg end type color)
      (save-excursion (css-color-color-info))
    (case type
      ((hsl rgb)
       (if (not (re-search-forward ",\\|(" end t))
	   (goto-char (+ beg 4))))
      (hex
       (cond ((> (point) (- end 3))
	      (goto-char (+ 1 beg)))
	     ((= (char-after) 35)
	      (forward-char 1))
	     ((evenp (- (point) beg))
	      (forward-char 1))
	     (t (forward-char 2)))))))

(defun css-color-hexify-anystring (str)
  (cond ((string-match "^hsl" str)
	 (css-color-string-hsl-to-hex str))
	((string-match "^rgb" str)
	 (css-color-string-rgb-to-hex str))
	(t str)))

(defun css-color-toggle-percentage ()
  (interactive)
  (let ((pos (point)))
    (if (eq (nth 2 (save-excursion (css-color-color-info))) 'rgb)
	(let ((chars "%1234567890."))
	  (skip-chars-backward chars)
	  (when
	      (looking-at "[[:digit:]]+\\(?:\.?[[:digit:]]*%\\)?%?")
	    (let ((s (match-string 0)))
	      (replace-match
	       (propertize
		(if (= (aref s (1- (length s))) ?\%)
		    (number-to-string (round (* (string-to-number s) 2.55)))
		  (format "%d%%" (/ (string-to-number s) 2.55)))
		'keymap css-color-generic-map
		'rear-nonsticky t)))
	    ;;(goto-char pos)
	    ))
      (message "No toggling at point."))))

;; provide some backwards-compatibility to hexcolor.el:
(defvar css-color-fg-history nil)
(defvar css-color-bg-history nil)
(defun css-color-test (fg-color bg-color)
  "Test colors interactively.
The colors are displayed in the echo area. You can specify the
colors as any viable css color.  Example:

  red
  #f00
  #0C0
  #b0ff00
  hsla(100, 50%, 25%)
  rgb(255,100,120)"
  (interactive (list (completing-read "Foreground color: "
				      css-color-html-colors
				      nil nil nil nil css-color-fg-history)
		     (completing-read "Background color: "
				      css-color-html-colors
				      nil nil nil nil css-color-bg-history)))
  (let* ((s (concat " Foreground: " fg-color ", Background: " bg-color " ")))
    (put-text-property 0 (length s)
                       'face (list
                              :foreground (css-color-hexify-anystring fg-color)
                              :background (css-color-hexify-anystring bg-color))
                       s)
    (message "Here are the colors: %s" s)))

(defun css-color-run-tests ()
  (interactive)
  (unless
      (progn
	(assert
	 (string= (css-color-string-hex-to-hsl "#ffff00") "hsl(60,100%,50%)"))
	(assert
	 (string= (css-color-string-rgb-to-hex "rgb(255, 50%,   0)")"#ff7f00"))
	(assert
	 (string= (css-color-string-hsl-to-rgb "hsl(60, 100%, 50%)") "rgb(255,255,0)"))
	(assert
	 (string= (css-color-string-hsl-to-hex "hsl(60, 100%, 50%)") "#ffff00")))
    (message "All tests passed")))

(provide 'css-color)
;;; css-color.el ends here
