;;; n-back.el --- n-back game
;;
;; Author: Lennart Borgman (lennart O borgman A gmail O com)
;; Created: 2009-05-23 Sat
(defconst n-back:version "0.5");; Version:
;; Last-Updated: 2009-08-04 Tue
;; URL:
;; Keywords:
;; Compatibility:
;;
;; Features that might be required by this library:
;;
  ;; `winsize'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; n-back game for brain training.  See `n-back-game' for more
;; information.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change log:
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

;; (setq n-back-trials 2)

(require 'winsize) ;; Ehum...
;;(require 'new-key-seq-widget)

(defvar n-back-game-window nil)
(defvar n-back-game-buffer nil)

(defvar n-back-ctrl-window nil)
(defvar n-back-ctrl-buffer nil)

(defvar n-back-info-window nil)
(defvar n-back-info-buffer nil)

(defvar n-back-trials-left nil)
(defvar n-back-timer nil)
(defvar n-back-clear-timer nil)

(defvar n-back-result nil)
(defvar n-back-this-result nil)

(defvar n-back-ring nil)

(defvar n-back-num-active nil)


(defgroup n-back nil
  "Customizations for `n-back-game' game."
  :group 'games)

(defgroup n-back-feel nil
  "Customizations for `n-back-game' game keys, faces etc."
  :group 'n-back)

(defface n-back-ok
  '((t (:foreground "black" :background "green")))
  "Face for OK answer."
  :group 'n-back-feel)

(defface n-back-bad
  '((t (:foreground "black" :background "OrangeRed1")))
  "Face for bad answer."
  :group 'n-back-feel)

(defface n-back-hint
  '((t (:foreground "black" :background "gold")))
  "Face for bad answer."
  :group 'n-back-feel)

(defface n-back-do-now
  '((((background dark)) (:foreground "yellow"))
    (t (:foreground "blue")))
  "Face for start and stop hints."
  :group 'n-back-feel)

(defface n-back-game-word
  '((t (:foreground "black")))
  "Face for word displayed in game."
  :group 'n-back-feel)

(defface n-back-header
  '((((background dark)) (:background "OrangeRed4"))
    (t (:background "gold")))
  "Face for headers."
  :group 'n-back-feel)

(defface n-back-keybinding
  '((((background dark)) (:background "purple4"))
    (t (:background "OliveDrab1")))
  "Face for key bindings."
  :group 'n-back-feel)

(defface n-back-last-result
  '((((background dark)) (:background "OliveDrab4"))
    (t (:background "yellow")))
  "Face for last game result header."
  :group 'n-back-feel)

(defface n-back-welcome
  '((((background dark)) (:foreground "OliveDrab3"))
    (t (:foreground "OliveDrab4")))
  "Face for welcome string"
  :group 'n-back-feel)

(defface n-back-welcome-header
  '((t (:height 2.0)))
  "Face for welcome header."
  :group 'n-back-feel)

(defcustom n-back-level 1
  "The n-Back level."
  :type '(radio (const 1)
                 (const 2)
                 (const 3)
                 (const 4))
  :set (lambda (sym val)
         (set-default sym val)
         (when (featurep 'n-back)
           (n-back-update-control-buffer)
           (n-back-update-info)))
  :group 'n-back)

(defcustom n-back-active-match-types '(position color sound)
  "Active match types."
  :type '(set (const position)
              (const color)
              (const sound)
              (const word))
  :set (lambda (sym val)
         (set-default sym val)
         (setq n-back-num-active (length val))
         (when (featurep 'n-back)
           (n-back-init-control-status)
           (n-back-update-control-buffer)
           (n-back-update-info)))
  :group 'n-back)

(defcustom n-back-allowed-match-types '(position color sound word)
  "Match types allowed in auto challenging."
  :type '(set (const position)
              (const color)
              (const sound)
              (const word))
  :set (lambda (sym val)
         (set-default sym val)
         (when (featurep 'n-back)
           (n-back-set-random-match-types (length n-back-active-match-types) nil)
           (n-back-init-control-status)
           (n-back-update-control-buffer)
           (n-back-update-info)))
  :group 'n-back)

(defcustom n-back-auto-challenge t
  "Automatic challenge decrease/increase."
  :type 'boolean
  :group 'n-back)

(defun n-back-toggle-auto-challenge ()
  "Toggle `n-back-auto-challenge'."
  (interactive)
  (let ((val (not n-back-auto-challenge)))
    (customize-set-variable 'n-back-auto-challenge val)
    (customize-set-value 'n-back-auto-challenge val)))

(defcustom n-back-colors
  '("gold" "orange red" "lawn green" "peru" "pink" "gray" "light blue")
  "Random colors to display."
  :type '(repeat color)
  :group 'n-back)

(defcustom n-back-words "you cat going me forest crying brown"
  "Random words to display."
  :type 'string
  :group 'n-back)

(defcustom n-back-sound-volume 0.2
  "Sound volume 0-1."
  :type 'float
  :group 'n-back-feel)

(defcustom n-back-sounds '("c:/program files/brain workshop/res" "piano-")
  "Random sounds location."
  :type '(list (directory :tag "Directory")
               (regexp :tag "File name regexp"))
  :group 'n-back)

(defvar n-back-control-mode-map nil)

(defun n-back-key-binding (what)
  "Return key binding used for WHAT match answers."
  (nth
   (case what
    (position 0)
    (color    1)
    (sound    2)
    (word     3))
   n-back-keys))

(defun n-back-make-keymap ()
  "Make keymap for the game."
  (let ((map (make-sparse-keymap)))
    (define-key map [?1] 'n-back-change-level)
    (define-key map [?2] 'n-back-change-level)
    (define-key map [?3] 'n-back-change-level)
    (define-key map [?4] 'n-back-change-level)
    (define-key map [?5] 'n-back-change-level)
    (define-key map [?6] 'n-back-change-level)
    (define-key map [??] 'n-back-help)
    (define-key map [?\ ] 'n-back-play)
    (define-key map [(control ?g)] 'n-back-stop)
    (define-key map [?-] 'n-back-decrease-speed)
    (define-key map [?+] 'n-back-increase-speed)

    (define-key map [(control ?r)] 'n-back-reset-game-to-saved)
    (define-key map [(control ?s)] 'n-back-save-game-settings)

    (define-key map [?t ?p] 'n-back-toggle-position)
    (define-key map [?t ?c] 'n-back-toggle-color)
    (define-key map [?t ?s] 'n-back-toggle-sound)
    (define-key map [?t ?w] 'n-back-toggle-word)

    (define-key map [?T ?a] 'n-back-toggle-auto-challenge)
    (define-key map [up]    'n-back-challenge-up)
    (define-key map [down]  'n-back-challenge-down)

    (define-key map [?T ?p] 'n-back-toggle-allowed-position)
    (define-key map [?T ?c] 'n-back-toggle-allowed-color)
    (define-key map [?T ?s] 'n-back-toggle-allowed-sound)
    (define-key map [?T ?w] 'n-back-toggle-allowed-word)

    (define-key map (n-back-key-binding 'position) 'n-back-position-answer)
    (define-key map (n-back-key-binding 'color)    'n-back-color-answer)
    (define-key map (n-back-key-binding 'sound)    'n-back-sound-answer)
    (define-key map (n-back-key-binding 'word)     'n-back-word-answer)
    ;;(define-key map [t] 'ignore)
    (setq n-back-control-mode-map map)))

(defcustom n-back-keys
  '(
    [?p]
    [?c]
    [?s]
    [?w]
    )
  "Key bindings for answering."
  :type '(list
          (key-sequence :tag "position key")
          (key-sequence :tag "color key")
          (key-sequence :tag "sound key")
          (key-sequence :tag "word key")
          )
  :set (lambda (sym val)
         (set-default sym val)
         (n-back-make-keymap))
  :group 'n-back-feel)

(defvar n-back-display-hint nil)
(defcustom n-back-hint t
  "Display hints - learning mode."
  :type 'boolean
  :group 'n-back)



(defvar n-back-sound-files nil)
;;(n-back-get-sound-files)
(defun n-back-get-sound-files ()
  "Get sound file names."
  (let ((dir (nth 0 n-back-sounds))
        (regexp (nth 1 n-back-sounds)))
    (when (file-directory-p dir)
      (setq n-back-sound-files (directory-files dir nil regexp)))))

(defun n-back-toggle-position ()
  "Toggle use of position in `n-back-active-match-types'."
  (interactive)
  (n-back-toggle 'position))

(defun n-back-toggle-color ()
  "Toggle use of color in `n-back-active-match-types'."
  (interactive)
  (n-back-toggle 'color))

(defun n-back-toggle-sound ()
  "Toggle use of sound in `n-back-active-match-types'."
  (interactive)
  (n-back-toggle 'sound))

(defun n-back-toggle-word ()
  "Toggle use of word in `n-back-active-match-types'."
  (interactive)
  (n-back-toggle 'word))

(defun n-back-toggle (match-type)
  "Toggle use of MATCH-TYPE in `n-back-active-match-types'."
  (n-back-toggle-1 match-type 'n-back-active-match-types))

(defun n-back-toggle-allowed-position ()
  "Toggle use of position in `n-back-allowed-match-types'."
  (interactive)
  (n-back-toggle-allowed 'position))

(defun n-back-toggle-allowed-color ()
  "Toggle use of color in `n-back-allowed-match-types'."
  (interactive)
  (n-back-toggle-allowed 'color))

(defun n-back-toggle-allowed-sound ()
  "Toggle use of sound in `n-back-allowed-match-types'."
  (interactive)
  (n-back-toggle-allowed 'sound))

(defun n-back-toggle-allowed-word ()
  "Toggle use of word in `n-back-allowed-match-types'."
  (interactive)
  (n-back-toggle-allowed 'word))

(defun n-back-toggle-allowed (match-type)
  "Toggle use of MATCH-TYPE in `n-back-allowed-match-types'."
  (n-back-toggle-1 match-type 'n-back-allowed-match-types))

(defun n-back-sort-types (types)
  "Sort TYPES to order used in defcustoms here."
  (sort types
        (lambda (a b)
          (let ((all '(position color sound word)))
            (< (length (memq a all))
               (length (memq b all)))))))

(defun n-back-toggle-1 (match-type active-list-sym)
  "Toggle use of MATCH-TYPE in list ACTIVE-LIST-SYM."
  (let (active-types)
    (if (memq match-type (symbol-value active-list-sym))
        (setq active-types (delq match-type (symbol-value active-list-sym)))
      (setq active-types (cons match-type (symbol-value active-list-sym))))
    (setq active-types (n-back-sort-types active-types))
    (customize-set-variable active-list-sym active-types)
    (customize-set-value active-list-sym active-types)))

(defcustom n-back-sec-per-trial 3.0
  "Seconds per trial."
  :type 'float
  :set (lambda (sym val)
         (set-default sym val)
         (when (featurep 'n-back)
           (n-back-update-info)))
  :group 'n-back)

(defun n-back-decrease-speed ()
  "Decrease speed of trials."
  (interactive)
  (setq n-back-sec-per-trial (+ n-back-sec-per-trial 0.25))
  (when (> n-back-sec-per-trial 5.0)
    (setq n-back-sec-per-trial 5.0))
  (n-back-update-info))

(defun n-back-increase-speed ()
  "Increase speed of trials."
  (interactive)
  (let ((sec (- n-back-sec-per-trial 0.25)))
    (when (< sec 1.0)
      (setq sec 1.0))
    (customize-set-variable 'n-back-sec-per-trial sec)
    (customize-set-value 'n-back-sec-per-trial sec)))

(defun n-back-help ()
  "Show help for `n-back-game' game."
  (interactive)
  (save-selected-window
    (describe-function 'n-back-game)))

(defun n-back-change-level (level)
  "Change n-Back level to LEVEL."
  (interactive (progn
                 (if (and (numberp last-input-event)
                          (>= last-input-event ?1)
                          (<= last-input-event ?9))
                     (list (- last-input-event ?0))
                   (list (string-to-number (read-string "n Back: "))))))
  (customize-set-variable 'n-back-level level)
  (customize-set-value 'n-back-level level))

(defvar n-back-frame nil)

;;;###autoload
(defun n-back-game ()
  "Emacs n-Back game.
This game is supposed to increase your working memory and fluid
intelligence.

In this game something is shown for half a second on the screen
and maybe a sound is played.  You should then answer if parts of
it is the same as you have seen or heard before.  This is
repeated for about 20 trials.

You answer with the keys shown in the bottom window.

In the easiest version of the game you should answer if you have
just seen or heard what is shown now.  By default the game gets
harder as you play it with success.  Then first the number of
items presented in a trial grows.  After that it gets harder by
that you have to somehow remember not the last item, but the item
before that \(or even earlier). That is what \"n-Back\" stands
for.

Note that remember does not really mean remember clearly.  The
game is for training your brain getting used to keep those things
in the working memory, maybe as a cross-modal unit.  You are
supposed to just nearly be able to do what you do in the game.
And you are supposed to have fun, that is what your brain like.

You should probably not overdue this. Half an hour a day playing
might be an optimal time according to some people.

The game is shamelessly modeled after Brain Workshop, see URL
`http://brainworkshop.sourceforge.net/' just for the fun of
getting it into Emacs.  The game resembles but it not the same as
that used in the report by Jaeggi mentioned at the above URL.

Not all features in Brain Workshop are implemented here, but some
new are maybe ... - and you have it available here in Emacs."
;; -----
;; Below is a short excerpt from the report by Jaeggi et al which
;; gave the idea to the game:

;; Training task.  For the training task, we used the same material
;; as described by Jaeggi et al.  (33), which was a dual n-Back task
;; where squares at eight different locations were presented
;; sequentially on a computer screen at a rate of 3 s (stimulus
;; length, 500 ms; interstimulus interval, 2,500 ms).
;; Simultaneously with the presentation of the squares, one of eight
;; consonants was presented sequentially through headphones.  A
;; response was required whenever one of the presented stimuli
;; matched the one presented n positions back in the sequence.  The
;; value of n was the same for both streams of stimuli.  There were
;; six auditory and six visual targets per block (four appearing in
;; only one modality, and two appearing in both modalities
;; simultaneously), and their positions were determined randomly.
;; Participants made responses manually by pressing on the letter
;; ‘‘A’’ of a standard keyboard with their left index finger for
;; visual targets, and on the letter ‘‘L’’ with their right index
;; finger for auditory targets.  No responses were required for
;; non-targets.
  (interactive)
  (when window-system
    (unless (frame-live-p n-back-frame)
      (setq n-back-frame (make-frame
                          (list '(name . "n-back game")
                                '(tool-bar-lines . 0)
                                '(menu-bar-lines . 0)
                                (case (frame-parameter nil 'background-mode)
                                  ('light '(background-color . "cornsilk"))
                                  ('dark  '(background-color . "MidnightBlue"))
                                  (t nil))
                                '(height . 45)
                                '(width . 150)))))
    (select-frame n-back-frame)
    (raise-frame n-back-frame))
  (n-back-cancel-timers)
  (n-back-get-sound-files)
  (unless n-back-sound-files
    (when (memq 'sound n-back-allowed-match-types)
      (n-back-toggle-allowed-sound))
    (when (memq 'sound n-back-active-match-types)
      (n-back-toggle-sound)))
  (n-back-init-control-status)
  (n-back-setup-windows)
  )

(defconst n-back-match-types
  '((position ": position match" nil)
    (color    ": color match" nil)
    (sound    ": sound match" nil)
    (word     ": word match" nil)
    ))

(defconst n-back-control-status nil
  "For showing status in control window.")

;;(n-back-set-match-status 'position 'bad)
(defun n-back-set-match-status (match-type status)
  "Set MATCH-TYPE status to STATUS for control window."
  (unless (memq status '(ok bad miss nil)) (error "n-back: Bad status=%s" status))
  (let ((entry (assoc match-type n-back-control-status)))
    (setcar (cddr entry) status)
    ))

;;(n-back-clear-match-status)
(defun n-back-clear-match-status ()
  "Clear match status for control window."
  ;;(dolist (entry n-back-control-status)
  (dolist (entry n-back-match-types)
    (setcar (cddr entry) nil)
    ))

;; (n-back-init-control-status)
(defun n-back-init-control-status ()
  "Init match status for control window."
  (setq n-back-control-status nil)
  (dolist (what n-back-active-match-types)
    (setq n-back-control-status
          (cons (assoc what n-back-match-types)
                n-back-control-status))))

(defsubst n-back-is-playing ()
  "Return non-nil when game is active."
  (timerp n-back-timer))

;;(n-back-update-control-buffer)
(defun n-back-update-control-buffer ()
  "Update content of control buffer."
  (save-match-data ;; runs in timer
    (when (buffer-live-p n-back-ctrl-buffer)
      (with-current-buffer n-back-ctrl-buffer
        (setq buffer-read-only nil)
        (erase-buffer)
        (insert (propertize (format "%s %s-back"
                                    (let ((n (length n-back-active-match-types)))
                                      (cond
                                       ((= 1 n) "Single")
                                       ((= 2 n) "Dual")
                                       ((= 3 n) "Triple")
                                       ))
                                    n-back-level
                                    ) 'face 'n-back-header)
                (propertize
                 (if (n-back-is-playing) "  Press C-g to stop" "  Press SPACE to play")
                 'face 'n-back-do-now)
                (if (n-back-is-playing) (format "  Left %s" n-back-trials-left) "")
                "\n")
        ;;(unless n-back-control-status (n-back-init-control-status))
        (dolist (entry n-back-control-status)
          (let* ((what (nth 0 entry))
                 (msg  (nth 1 entry))
                 (sts  (nth 2 entry))
                 (key (key-description (n-back-key-binding what))))
            ;;(setq msg (concat (key-description (n-back-key-binding what)) msg))
            (cond
             ((eq sts 'bad)
              (setq msg (propertize (concat key msg) 'face 'n-back-bad)))
             ((eq sts 'ok)
              (setq msg (propertize (concat key msg) 'face 'n-back-ok)))
             ((eq sts 'miss)
              (setq msg (concat
                         (if n-back-display-hint
                             (propertize key 'face 'n-back-header)
                           key)
                         msg)))
             ((not sts)
              (setq msg (concat key msg)))
             (t
              (error "n-back:Unknown sts=%s" sts)
              ))
            (insert msg "   "))
          )
        (when n-back-display-hint
          (setq n-back-display-hint nil)
          (run-with-timer 0.1 nil 'n-back-update-control-buffer))
        (setq buffer-read-only t)
        (if (window-live-p n-back-ctrl-window)
            (with-selected-window n-back-ctrl-window
              (goto-char 1))
          (goto-char 1))))))

(defcustom n-back-trials 20
  "Number of trials per session."
  :type 'integer
  :group 'n-back)

;;(n-back-compute-result-values n-back-result)
(defvar n-back-result-values nil)
(defun n-back-compute-single-result-value (entry)
  "Compute result stored in ENTRY."
  (let* ((what (nth 0 entry))
         (good (nth 1 entry))
         (bad  (nth 2 entry))
         (miss (nth 3 entry))
         (err (+ bad miss))
         ;;(tot  (+ good bad miss 0.0))
         ;;(gnum 6)
         ;;(weighted-err (* err (/ gnum tot)))
         )
    (cons what (if (= 0 good)
                   0
                 (/ (- n-back-trials err 0.0)
                    n-back-trials)))))

(defun n-back-compute-result-values (result)
  "Compute result values from game result RESULT."
  (let ((results nil))
    (dolist (entry result)
      (let ((res (n-back-compute-single-result-value entry)))
        (setq results (cons res results))))
    (setq n-back-result-values (reverse results))))

;; Thresholds
(defun n-back-view-threshold-discussion-page ()
  "View some discussion of threshold."
  (interactive)
  (browse-url "http://groups.google.com/group/brain-training/browse_thread/thread/f4bfa452943c2a2d/ba31adfd0b97771c?lnk=gst&q=threshold#ba31adfd0b97771c"))

;;(n-back-set-next-challenge)
(defvar n-back-worst nil)

(defvar n-back-challenge-change nil)

(defun n-back-set-next-challenge ()
  "Set next game difficulty level from last game result."
  (let ((r 2.8)) ;; stay as default
    (setq n-back-worst nil)
    (dolist (res n-back-result-values)
      (when (< (cdr res) r)
        (setq r (cdr res))
        (setq n-back-worst res)))
    (setq n-back-challenge-change (if (< r 0.74)
                                      'down
                                    (if (> r 0.91)
                                        'up
                                      'stay)))
    (n-back-change-challenge n-back-challenge-change)))

(defun n-back-challenge-up ()
  "Make the game harder."
  (interactive)
  (n-back-change-challenge 'up))

(defun n-back-challenge-down ()
  "Make the game easier."
  (interactive)
  (n-back-change-challenge 'down))

(defun n-back-change-challenge (challenge-change)
  "Change game difficulty level by CHALLENGE-CHANGE."
  (let ((new-level n-back-level)
        (new-num-active n-back-num-active)
        (num-allowed (length n-back-allowed-match-types)))
    (case challenge-change
      (down
       (if (= 1 n-back-num-active)
           (unless (= 1 n-back-level)
             (setq new-num-active (min 3 num-allowed))
             (setq new-level (1- n-back-level)))
         (setq new-num-active (1- n-back-num-active))))
      (up
       (if (or (<= 3 n-back-num-active)
               (<= num-allowed n-back-num-active))
           (progn
             (setq new-level (1+ n-back-level))
             (setq new-num-active 1))
         (setq new-num-active (min 3 (1+ n-back-num-active))))))
    ;;(when (= new-level 0) (setq new-level 1))
    ;;(when (= new-num-active 0) (setq new-num-active 1))
    (when (and (= new-level n-back-level)
               (= new-num-active n-back-num-active))
      (setq n-back-challenge-change 'stay))
    (unless (= new-level n-back-level)
      (customize-set-variable 'n-back-level new-level)
      (customize-set-value 'n-back-level new-level))
    (n-back-set-random-match-types new-num-active (car n-back-worst))))

(defun n-back-set-random-match-types (num worst)
  "Select NUM random match types.
If type WORST is non-nil try to include that."
  (let ((alen (length n-back-allowed-match-types))
        (old-types n-back-active-match-types)
        types)
    (unless (<= num alen)
      (error "n-back: Too many match types required = %s" num))
    (when (and worst
               (< 1 num)
               (memq worst n-back-allowed-match-types))
      (add-to-list 'types worst))
    (while (< (length types) num)
      (add-to-list 'types (nth (random alen) n-back-allowed-match-types)))
    (setq types (n-back-sort-types types))
    (unless (equal old-types types)
      (customize-set-variable 'n-back-active-match-types types)
      (customize-set-value 'n-back-active-match-types types))))

;; (defcustom n-back-keybinding-color "OliveDrab1"
;;   "Background color for key binding hints."
;;   :type 'color
;;   :group 'n-back)

(defun n-back-update-info ()
  "Update info buffer."
  (when (buffer-live-p n-back-info-buffer)
    (when (window-live-p n-back-info-window)
      (set-window-buffer n-back-info-window n-back-info-buffer))
    (with-current-buffer n-back-info-buffer
      (setq buffer-read-only nil)
      (erase-buffer)

      (insert (propertize "n-back" 'face 'n-back-header)
              "  "
              (propertize "Help: ?" 'face 'n-back-keybinding))

      ;; Auto challenging
      (insert "\n\nAuto challenging: "
              (if n-back-auto-challenge "on " "off ")
              (propertize "toggle: Ta" 'face 'n-back-keybinding))

      (insert "\n  Manually change challenging: "
              (propertize "up-arrow/down-arrow" 'face 'n-back-keybinding))

      (insert "\n  Allowed match types: ")
      (dolist (type n-back-allowed-match-types)
        (insert (format "%s " type)))
      (insert (propertize "toggle: T" 'face 'n-back-keybinding))

      ;; Current game
      (insert "\n\nCurrent game:")

      (insert (format "\n  n Back: %s " n-back-level)
              (propertize "change: number 1-9" 'face 'n-back-keybinding))
      (insert "\n  Match types: ")
      (dolist (type n-back-active-match-types)
        (insert (format "%s " type)))
      (insert (propertize "toggle: t" 'face 'n-back-keybinding))

      (insert (format "\n  %.2f seconds per trial " n-back-sec-per-trial)
              (propertize "change: +/-" 'face 'n-back-keybinding))

      ;; Save and restore
      (insert "\n\n")
      (insert "Game settings: "
              (propertize "reset: C-r" 'face 'n-back-keybinding)
              " "
              (propertize "save: C-s" 'face 'n-back-keybinding))

      (insert "\n\n")
      (unless (or (n-back-is-playing)
                  (not n-back-result))
        (insert (propertize (format "Last result, %s" n-back-challenge-change)
                            'face 'n-back-last-result)
                "\n  Good-Bad-Miss:")
        (dolist (entry n-back-result)
          (let* ((what (nth 0 entry))
                 (good (nth 1 entry))
                 (bad  (nth 2 entry))
                 (miss (nth 3 entry))
                 (tot  (+ good bad miss 0.0))
                 (res (n-back-compute-single-result-value entry)))
            (insert (format "  %s: %s-%s-%s (%d%%)"
                            (key-description (n-back-key-binding what))
                            good
                            bad
                            miss
                            (floor (* 100 (cdr res))))))))

    (setq buffer-read-only t))))

(defun n-back-show-welcome (msg)
  "Show welcome startup info and message MSG."
  (with-current-buffer n-back-game-buffer
    (let ((src (or (when (boundp 'nxhtml-install-dir)
                     (expand-file-name "nxhtml/doc/img/fun-brain-2.png" nxhtml-install-dir))
                   "c:/program files/brain workshop/res/brain_graphic.png"))
          img
          buffer-read-only)
      (erase-buffer)
      ;;(insert (propertize "\nEmacs n-back game (after Brain Workshop)\n\n" 'face '(:height 2.0)))
      (insert (propertize "\nEmacs n-back game (after Brain Workshop)\n\n" 'face 'n-back-welcome-header))
      (if (file-exists-p src)
          (condition-case err
              (setq img (create-image src nil nil
                                      :relief 0
                                      ;;:margin inlimg-margins
                                      ))
            (error (setq img (error-message-string err))))
        (setq img (concat "Image not found: " src)))
      (if (stringp img)
          (insert img)
        (insert-image img))
      (insert (propertize "\n\nPlay for fun and maybe a somewhat happier brain"
                          'face 'n-back-welcome))
      (when msg (insert "\n\n" msg))
      )))

(defun n-back-setup-windows ()
  "Setup game frame and windows."
  (delete-other-windows)
  ;; Info
  (split-window-horizontally)
  (setq n-back-info-window (next-window (frame-first-window)))
  (setq n-back-info-buffer (get-buffer-create "* n-back info *"))
  (when (< 75 (window-width n-back-info-window))
    (with-selected-window n-back-info-window
      (enlarge-window (- 75 (window-width n-back-info-window)) t)))
  (with-current-buffer n-back-info-buffer
    (n-back-control-mode)
    (setq wrap-prefix "      "))
  (n-back-update-info)
  ;; Control
  (split-window-vertically)
  (setq n-back-ctrl-window (next-window (frame-first-window)))
  (setq n-back-ctrl-buffer (get-buffer-create "* n-back control *"))
  (set-window-buffer n-back-ctrl-window n-back-ctrl-buffer)
  (with-current-buffer n-back-ctrl-buffer (n-back-control-mode))
  (n-back-update-control-buffer)
  (fit-window-to-buffer n-back-ctrl-window)
  (set-window-dedicated-p n-back-ctrl-window t)
  ;; Game
  (setq n-back-game-window (frame-first-window))
  (setq n-back-game-buffer (get-buffer-create "*n-back game*"))
  (set-window-buffer n-back-game-window n-back-game-buffer)
  (set-window-dedicated-p n-back-game-window t)
  (with-current-buffer n-back-game-buffer (n-back-control-mode))
  (n-back-show-welcome nil)
  ;; Position in control window
  (select-window n-back-ctrl-window)
  )

;;(n-back-display "str" 1 0 3 3 6)
(defun n-back-display (str x y cols rows max-strlen color)
  "Display a trial.
Display item with text STR at column X in row Y using COLS
columns and ROWS rows.  Strings to display have max length
MAX-STRLEN.  Display item with background color COLOR."
  (unless (< x cols) (error "n-back: Not x=%s < cols=%s" x cols))
  (unless (< y rows) (error "Not y=%s < rows=%s" y rows))
  (unless str (setq str ""))
  (with-current-buffer n-back-game-buffer
    (let* (buffer-read-only
           (tot-str "")
           ;; Pad spaces left, two right, four between
           (game-w (window-width n-back-game-window))
           (pad-x 0)
           (scale (if (not window-system)
                      1.0
                    (/ (* 1.0 game-w)
                       (+ (* 2 pad-x)
                          (* (1- cols) 4)
                          (* cols max-strlen)))))
           (str-diff (- max-strlen (length str)))
           (str-l-len (/ str-diff 2))
           (str-r-len (- max-strlen (length str) str-l-len))
           (face-spec (if window-system
                          (list :inherit 'n-back-game-word :background color :height scale)
                        (list :inherit 'n-back-game-word :background color)))
           (str-disp (propertize
                      (concat (make-string str-l-len 32) str (make-string str-r-len 32))
                      'face face-spec))
           (col-str (concat
                     (make-string pad-x ?p)
                     (make-string
                      (+ (* x (+ 4 max-strlen)))
                      32
                      ;;?x
                      )))
           ;; Pad lines above and below, two between
           (pad-y 0)
           (game-h (window-body-height n-back-game-window))
           (game-h-scaled (/ game-h scale))
           (lines-between (/ (- game-h-scaled rows (* 2 pad-y))
                             (1- rows)))
           (row-scaled (+ pad-y (* y (1+ lines-between)) (1- y)))
           (row-num (if (= y 0)
                        pad-y
                      (round row-scaled)))
           (row-str (make-string row-num ?\n)))
      (setq show-trailing-whitespace nil)
      ;;(setq cursor-type nil)
      (erase-buffer)
      (setq tot-str row-str)
      (setq tot-str (concat tot-str col-str))
      (insert (propertize tot-str 'face (list :height scale)))
      (insert str-disp)
      )))

;; (setq timer-list nil)
;;(n-back-display-in-timer)
;; (setq n-back-trials-left 3)

(defun n-back-clear-game-window ()
  "Erase game buffer."
  (save-match-data ;; runs in timer
    (with-current-buffer n-back-game-buffer
      (let (buffer-read-only)
        (erase-buffer)))))

(defun n-back-play ()
  "Start playing."
  (interactive)
  (message "  ") ;; For easier reading *Messages*
  (n-back-update-info)
  (if (not n-back-active-match-types)
    (message (propertize "No active match types"
                         'face 'secondary-selection))
    ;;(setq n-back-result nil)
    (n-back-init-control-status)
    (n-back-init-this-result)
    (n-back-cancel-timers)
    (winsize-set-mode-line-colors t)
    (setq n-back-ring (make-ring (1+ n-back-level)))
    (n-back-clear-game-window)
    (setq n-back-trials-left (+ n-back-trials n-back-level))
    (random t)
    (n-back-start-main-timer)
    (n-back-update-control-buffer)))

(defun n-back-start-main-timer ()
  "Start main game timer."
  (setq n-back-timer
        (run-with-timer
         n-back-sec-per-trial
         nil ;;n-back-sec-per-trial
         'n-back-display-in-timer)))

(defun n-back-finish-game ()
  "Finish the game."
  (n-back-cancel-timers)
  (fit-window-to-buffer n-back-ctrl-window)
  (setq n-back-result n-back-this-result)
  (n-back-compute-result-values n-back-result)
  (when n-back-auto-challenge (n-back-set-next-challenge))
  (n-back-update-info)
  (n-back-init-control-status)
  (n-back-clear-match-status)
  (n-back-update-control-buffer)
  (n-back-show-welcome "Game over")
  (with-current-buffer n-back-game-buffer
    ;;(setq n-back-challenge-change 'up)
    (let (buffer-read-only)
      (insert
       "\n\n"
       (case n-back-challenge-change
         (up "Congratulations! I see you need more challenge, raising difficulty!")
         (down "Making it a bit easier for now to make your playing more fun.")
         (t "This game challenges seems the right way for you now.")))
      (let* ((dir (when (boundp 'nxhtml-install-dir)
                    (expand-file-name "nxhtml/doc/img/" nxhtml-install-dir)))
             (up-imgs '("rembrandt-self-portrait.jpg"
                        "bacchante2.jpg"
                        "giraffe.jpg"
                        "Las_Medulas.jpg"
                        ))
             (t-imgs '("continue-play.jpg"
                       "Toco_toucan.jpg"
                       "raindrops2.jpg"
                       "divine2.jpg"
                       ;;"butterflies.png"
                       "volga.jpg"
                       "healthy_feet2.jpg"
                       ))
             ;; (setq n-back-trials 1)
             (pic (when dir (case n-back-challenge-change
                              (up (nth (random (length up-imgs)) up-imgs))
                              (t  (nth (random (length t-imgs))  t-imgs)))))
             (src (when dir (expand-file-name pic dir)))
             img)
           (when (and src (file-exists-p src))
             (condition-case err
                 (setq img (create-image src nil nil
                                         :relief 0
                                         ))
               (error (setq img (error-message-string err)))))
           (if (stringp img)
               img
             (insert "\n\n")
             (insert-image img)))))
  (message "Game over"))

(defun n-back-display-random ()
  "Display a random item."
  (when (current-message) (message ""))
  ;;(message "here start display")
  (let* ((use-position (memq 'position n-back-active-match-types))
         (use-color (memq 'color n-back-active-match-types))
         (use-sound (memq 'sound n-back-active-match-types))
         (use-word  (memq 'word  n-back-active-match-types))
         (old-rec (when (n-back-match-possible)
                    (ring-ref n-back-ring (1- n-back-level))))
         (cols 3)
         (rows 3)
         (x (if use-position (random 3) 1))
         (y (if use-position (random 3) 1))
         (old-x (if use-position (nth 1 old-rec)))
         (old-y (if use-position (nth 2 old-rec)))
         (color (nth (if use-color (random (length n-back-colors)) 0) n-back-colors))
         (old-color (if use-color (nth 3 old-rec)))
         (sound (when use-sound (expand-file-name (nth (random (length n-back-sound-files))
                                                       n-back-sound-files)
                                                  (nth 0 n-back-sounds))))
         (old-sound (if use-sound (nth 4 old-rec)))
         (words (when use-word (split-string n-back-words)))
         (word (when use-word (nth (random (length words)) words)))
         (old-word (when use-word (nth 5 old-rec)))
         (str (if word word "")) ;(format "%s" n-back-trials-left))
         (max-strlen (if words
                         (+ 2 (apply 'max (mapcar (lambda (w) (length w)) words)))
                       5))
         (compensate 24)
         )
    ;; To get more targets make it more plausible that it is the same here.
    ;; (/ (- 6 (/ 20.0 8)) 20)
    (when old-rec
      (when (and use-position
                 (not (and (= x old-x)
                           (= y old-y)))
                 (< (random 100) compensate))
        (setq x (nth 1 old-rec))
        (setq y (nth 2 old-rec)))
      (when (and use-color
                 (not (equal color old-color))
                 (< (random 100) compensate))
        (setq color (nth 3 old-rec)))
      (when (and use-sound
                 (not (equal sound old-sound))
                 (< (random 100) compensate))
        (setq sound (nth 4 old-rec)))
      (when (and use-word
                 (not (equal word old-word))
                 (< (random 100) compensate))
        (setq word (nth 5 old-rec))))
    (setq str word) ;; fix-me
    (ring-insert n-back-ring (list str x y color sound word))
    ;;(message "here before display")
    (n-back-display str x y cols rows max-strlen color)
    ;;(when sound (play-sound (list 'sound :file sound)))
    ;;(message "here before clear-m")
    (n-back-clear-match-status)
    ;;(message "here before position")
    (when (and use-position (n-back-matches 'position)) (n-back-set-match-status 'position 'miss))
    ;;(message "here before color")
    (when (and use-color (n-back-matches 'color)) (n-back-set-match-status 'color 'miss))
    ;;(message "here before sound")
    (when (and use-sound (n-back-matches 'sound)) (n-back-set-match-status 'sound 'miss))
    ;;(message "here before word")
    (when (and use-word (n-back-matches 'word)) (n-back-set-match-status 'word 'miss))
    (setq n-back-display-hint n-back-hint)
    ;;(message "here before control")
    (n-back-update-control-buffer)
    ;;(message "here before clear timer")
    (setq n-back-clear-timer (run-with-timer 0.5 nil 'n-back-clear-game-window))
    ;;(message "here before sound timer")
    (when sound (run-with-timer 0.01 nil 'n-back-play-sound-in-timer sound))
    ;;(message "here exit display")
    ))

(defun n-back-display-in-timer ()
  "Display a trial in a timer."
  (condition-case err
      (save-match-data ;; runs in timer
        (n-back-add-result)
        (if (>= 0 (setq n-back-trials-left (1- n-back-trials-left)))
            (n-back-finish-game)
          (n-back-display-random)
          (n-back-start-main-timer)
          ;;(message "after start-main-timer")
          ))
    (error (message "n-back-display: %s" (error-message-string err))
           (n-back-cancel-timers))))

(defun n-back-play-sound-in-timer (sound-file)
  "Play sound SOUND-FILE in a timer."
  (condition-case err
      (save-match-data ;; runs in timer
        (play-sound (list 'sound :file sound-file :volume n-back-sound-volume)))
    (error (message "n-back-sound: %s" (error-message-string err))
           (n-back-cancel-timers))))


;;; Answers

;;(defvar n-back-answers nil)

(defun n-back-init-this-result ()
  "Init `n-back-this-result'."
  (setq n-back-this-result nil)
  (dolist (sts-entry n-back-control-status)
    (let* ((what (nth 0 sts-entry))
           (res-entry (list what 0 0 0)))
      (setq n-back-this-result (cons res-entry n-back-this-result)))))

(defun n-back-match-possible ()
  "Return t if enouch entries have been shown to match."
  (= (ring-length n-back-ring) (1+ n-back-level)))

(defun n-back-add-result ()
  "Add result of last trial."
  (when (n-back-match-possible)
    (dolist (sts-entry n-back-control-status)
      (let* ((what (nth 0 sts-entry))
             (sts  (nth 2 sts-entry))
             (matches (n-back-matches what))
             (num (cond
                   ((eq sts 'ok) 1)
                   ((eq sts 'bad) 2)
                   ;;((eq sts nil) (when matches 3))
                   ((eq sts 'miss) 3)
                   ((not sts) nil)
                   (t (error "n-back: Bad status=%s" sts))))
             (res-entry (when num (assoc what n-back-this-result)))
             (lst (when num (nthcdr num res-entry))))
        (when num
          (if res-entry
              (setcar lst (1+ (car lst)))
            (setq res-entry (list what 0 0 0))
            ;;(setq lst (nthcdr num res-entry))
            (setq n-back-this-result (cons res-entry n-back-this-result))))))))

(defun n-back-matches-position ()
  "Return non-nil iff last trial position match."
  (when (n-back-match-possible)
    (let* ((comp-item (ring-ref n-back-ring n-back-level))
           (curr-item (ring-ref n-back-ring 0))
           (comp-x (nth 1 comp-item))
           (curr-x (nth 1 curr-item))
           (comp-y (nth 2 comp-item))
           (curr-y (nth 2 curr-item)))
      (and (= comp-y curr-y)
           (= comp-x curr-x)))))

(defun n-back-matches-color ()
  "Return non-nil iff last trial color match."
  (when (n-back-match-possible)
    (let* ((comp-item (ring-ref n-back-ring n-back-level))
           (curr-item (ring-ref n-back-ring 0))
           (comp-color (nth 3 comp-item))
           (curr-color (nth 3 curr-item)))
      (equal comp-color curr-color))))

(defun n-back-matches-sound ()
  "Return non-nil iff last trial sound match."
  (when (n-back-match-possible)
    (let* ((comp-item (ring-ref n-back-ring n-back-level))
           (curr-item (ring-ref n-back-ring 0))
           (comp-sound (nth 4 comp-item))
           (curr-sound (nth 4 curr-item)))
      (equal comp-sound curr-sound))))

(defun n-back-matches-word ()
  "Return non-nil iff last trial word match."
  (when (n-back-match-possible)
    (let* ((comp-item (ring-ref n-back-ring n-back-level))
           (curr-item (ring-ref n-back-ring 0))
           (comp-word (nth 5 comp-item))
           (curr-word (nth 5 curr-item)))
      (equal comp-word curr-word))))

(defun n-back-matches (what)
  "Return non-nil iff last trial part WHAT match."
  (cond
   ((eq what 'position) (n-back-matches-position))
   ((eq what 'color) (n-back-matches-color))
   ((eq what 'sound) (n-back-matches-sound))
   ((eq what 'word)  (n-back-matches-word))
   (t (error "n-back: Unknown match type: %s" what))))

(defun n-back-answer (what)
  "Tell that you think WHAT matched."
  (when (n-back-is-playing)
    (if (memq what n-back-active-match-types)
        (if (n-back-match-possible)
            (let ((sts (if (n-back-matches what) 'ok 'bad)))
              (n-back-set-match-status what sts)
              (n-back-update-control-buffer))
          (message "%s n-back items must be displayed before anything can match"
                   n-back-level))
      (message "%s match is not active" what)
      (ding t))))

(defun n-back-position-answer ()
  "Tell that you think position matched."
  (interactive)
  (n-back-answer 'position))

(defun n-back-color-answer ()
  "Tell that you think color matched."
  (interactive)
  (n-back-answer 'color))

(defun n-back-sound-answer ()
  "Tell that you think sound matched."
  (interactive)
  (n-back-answer 'sound))

(defun n-back-word-answer ()
  "Tell that you think word matched."
  (interactive)
  (n-back-answer 'word))

(defun n-back-stop ()
  "Stop playing."
  (interactive)
  (n-back-cancel-timers)
  (n-back-update-control-buffer)
  (message "Stopped n-back game")
  (n-back-show-welcome "Stopped"))

(define-derived-mode n-back-control-mode nil "N-back"
  "Mode for controlling n-back game."
  (setq cursor-type nil)
  (setq buffer-read-only t)
  (set (make-local-variable 'viper-emacs-state-mode-list) '(n-back-control-mode))
  (set (make-local-variable 'viper-emacs-state-hook) nil) ;; in vis cursor
  (abbrev-mode -1)
  (setq show-trailing-whitespace nil)
  (when (fboundp 'visual-line-mode) (visual-line-mode 1))
  (n-back-make-keymap))

(defun n-back-cancel-timers ()
  "Cancel game timers."
  (when (timerp n-back-timer)
    (cancel-timer n-back-timer))
  (setq n-back-timer nil)
  (when (timerp n-back-clear-timer)
    (cancel-timer n-back-clear-timer))
  (setq n-back-clear-timer nil)
  (winsize-set-mode-line-colors nil))

(defvar n-back-game-settings-symbols
    '(
      ;;n-back-keys
      n-back-level
      n-back-active-match-types
      n-back-allowed-match-types
      n-back-auto-challenge
      ;;n-back-colors
      ;;n-back-words
      ;;n-back-sound-volume
      ;;n-back-sounds
      n-back-sec-per-trial
      ;;n-back-keybinding-color
      ;;n-back-trials
      ))

(defun n-back-save-game-settings ()
  "Save game settings."
  (interactive)
  (dolist (var n-back-game-settings-symbols)
  )
  (custom-save-all))

(defun n-back-reset-game-to-saved ()
  "Reset game playing options to saved values."
  (interactive)
  (dolist (pass '(1 2))
    (dolist (var n-back-game-settings-symbols)
      (if (= pass 1)
          ;; pass 1 is for my lousy programming:
          (condition-case err
              (custom-reevaluate-setting var)
            (error nil))
        (custom-reevaluate-setting var)))))

(provide 'n-back)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; n-back.el ends here
