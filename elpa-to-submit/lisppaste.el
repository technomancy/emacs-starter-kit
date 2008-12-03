;;; lisppaste.el --- Interact with the lisppaste pastebot via XML-RPC.

;; Copyright (C) 2004, 2005, 2006, 2007, 2008 Lawrence Mitchell <wence@gmx.li>
;; File: lisppaste.el
;; Author: Lawrence Mitchell <wence@gmx.li>
;; Created: 2004-04-25
;; Version: 1.4
;; Keywords: IRC xml rpc network
;; URL: http://purl.org/NET/wence/lisppaste.el
;; Package-Requires: ((xml-rpc "1.6.4"))

;;; Commentary:
;; This file provide an Emacs interface to the lisppaste bot running
;; on the Freenode IRC network (http://freenode.net).
;; A number of commands are provided.
;;
;; Pasting a region may be carried out using `lisppaste-region'.
;; A top-level entry point to all of lisppaste's functionality is
;; provided via the `lisppaste' command.  If you want to intercept
;; lisppaste urls and display them in Emacs, you can do so by
;; modifying `browse-url-browser-function' as described in
;; `lisppaste-browse-url'.
;;
;; Interacting with lisppaste requires xml-rpc.el which you can find
;; a link for at <URL: http://www.emacswiki.org/cgi-bin/wiki/XmlRpc>.

;;; Code:

(require 'cl)
(require 'xml-rpc)

(defconst lisppaste-url "http://common-lisp.net:8185/RPC2")

(defun lisppaste-send-command (command &rest stuff)
  "Send COMMAND to the lisppaste bot with STUFF as arguments."
  (let ((xml-entity-alist nil))         ; defeat xml.el encoding of entities
    (apply #'xml-rpc-method-call lisppaste-url command stuff)))

(defun lisppaste-new-paste (channel nick title content &optional annotate)
  "Create a new paste with the specified arguments.
CHANNEL is the channel the paste will appear in.
NICK is the nickname the paste will appear to be from.
TITLE is the paste's title.
CONTENT is the paste content.
If ANNOTATE is non-nil, annotate that paste."
  (lisppaste-check-channel channel)
  (lisppaste-send-command 'newpaste channel nick title content annotate))

(defun lisppaste-get-paste (paste &optional n)
  "Fetch PASTE.

If N is non-nil, fetch the Nth annotation."
  (lisppaste-send-command 'pastedetails paste n))

(defun lisppaste-list-annotations (paste)
  "List the annotations for PASTE."
  (lisppaste-send-command 'pasteannotationheaders paste))

(defun lisppaste-list-pastes (n &optional start channel)
  "Fetch the most recent N pastes.

If START is non-nil return the most recent N pastes from the STARTth
paste.
If CHANNEL is non-nil, only return pastes from that channel."
  (and start (zerop start) (setq start nil))
  (if (and channel
           (not (string= channel "")))
      (progn (lisppaste-check-channel channel)
             (lisppaste-send-command 'pasteheadersbychannel channel n start))
    (lisppaste-send-command 'pasteheaders n start)))

(defun lisppaste-channels ()
  "Return which channels the lisppaste bot runs on."
  (lisppaste-send-command 'listchannels))

(defvar lisppaste-channels nil
  "Cached value of the channels lisppaste is running on.

Initialised using the function `lisppaste-channels'.")

(defsubst lisppaste-check-channel (channel)
  "Check that CHANNEL is supported by lisppaste.

Checks the cached value of the variable `lisppaste-channels' before
requesting a new list."
  (or lisppaste-channels (setq lisppaste-channels (lisppaste-channels)))
  (unless (member channel lisppaste-channels)
    (error "%s not a valid channel.  Try M-: (setq lisppaste-channels nil) RET"
           channel)))

(defsubst lisppaste-all-channels ()
  ;; Retardedness due to completing read requiring an alist.
  (mapcar #'list
          (or lisppaste-channels
              (setq lisppaste-channels (lisppaste-channels)))))

(defvar lisppaste-default-nick nil
  "*The default nick for pastes.

See also the function `lisppaste-default-nick'.")

(defsubst lisppaste-default-nick (channel)
  "Return the default nick for CHANNEL.

If ERC is loaded, try and find a nick by looking for
`erc-current-nick' in CHANNEL's buffer.

If that returns nil, return the value of the variable
`lisppaste-default-nick'."
  (or (when (featurep 'erc)
        (erc-with-buffer ((get-buffer channel))
          (erc-current-nick)))
      lisppaste-default-nick))

(defsubst lisppaste-paste (p)
  (plist-get p 'lisppaste-paste))
(defsubst lisppaste-annotation (p)
  (plist-get p 'lisppaste-annotation))
(defsubst lisppaste-channel (p)
  (plist-get p 'lisppaste-channel))
(defsubst lisppaste-annotations (p)
  (plist-get p 'lisppaste-annotations))

(defsubst lisppaste-read-number (prompt &optional annotation)
  "Read a number prompting with PROMPT.

Default values are picked up from the text-properties around `point'.
If ANNOTATION is non-nil, look for annotation text-properties."
  (let* ((p (text-properties-at (point)))
         (num (lisppaste-paste p))
         (ann (lisppaste-annotation p)))
    (string-to-number
     (if annotation
         (read-from-minibuffer prompt
                               (and ann
                                    (number-to-string ann)))
       (read-from-minibuffer prompt
                             (and num
                                  (number-to-string num)))))))

(defsubst lisppaste-read-channel ()
  "Read a channel name."
  (completing-read "Channel: " (lisppaste-all-channels)))

(defsubst lisppaste-read-nick (c)
  "Read a nick.

C is the default channel to look for a nick in with `lisppaste-default-nick'."
  (read-string "Nick: " (lisppaste-default-nick c)))

(defsubst lisppaste-read-title ()
  "Read a paste title."
  (read-string "Title: "))

(defun lisppaste-clean-returned-paste (paste)
  "Clean PASTE of HTML character entities."
  (with-temp-buffer
    (insert (format "%s" paste))
    (goto-char (point-min))
    ;; Remove spurious ^M's
    (save-excursion (while (search-forward "&#xD;" nil t)
                      (replace-match "")))
    (while (re-search-forward "&\\(#x[^;]+\\);" nil t)
      (insert (read (match-string 1)))
      (replace-match ""))
    (buffer-substring-no-properties (point-min) (point-max))))

(defun lisppaste-clean-time-string (time)
  "Clean an iso8601 TIME string to return YYYY-MM-DD.

Not very robust."
  (if (string-match "^\\(....\\)\\(..\\)\\(..\\)T..:..:..$" time)
      (format "%s-%s-%s" (match-string 1 time)
                         (match-string 2 time)
                         (match-string 3 time))
    (error "Invalid time format `%s'" time)))

(defvar lisppaste-creation-help
  (concat ";; Enter your paste below, and press C-c C-c to send.\n"
          ";; Press C-c C-d to cancel this paste.\n\n")
  "Paste creation help text.")
  
(defsubst lisppaste-buffer-substring (beg end)
  "Return part of the current buffer as a string.

BEG and END delimit the part of the buffer to return.

The string is returned with all tabs replaced by spaces.  See also
`untabify'."
  (let* ((inhibit-read-only t)
         (s (buffer-substring-no-properties beg end))
         (tw tab-width))
    (with-temp-buffer
      (let ((tab-width tw))
        (insert s)
        (untabify (point-min) (point-max))
        (buffer-substring-no-properties (point-min) (point-max))))))

;;;###autoload
(defun lisppaste-paste-region (beg end)
  "Send the region between BEG and END as a paste."
  (interactive "r")
  (let* ((annotate (if (y-or-n-p "Send this region as an annotation? ")
                       (lisppaste-read-number "Paste to annotate: ")))
         (channel (lisppaste-read-channel))
         (nick (lisppaste-read-nick channel))
         (title (lisppaste-read-title))
         (content (lisppaste-buffer-substring beg end)))
    (lisppaste-check-channel channel)
    (lisppaste-new-paste channel nick title content annotate)))


(defun lisppaste-browse-url (url &rest ignore)
  "Display a paste URL as a paste in Emacs.

To use this, modify `browse-url-browser-function' in the following way:

  (setq browse-url-browser-function
        '((\"^http://paste\\\\.lisp\\\\.org/display\" . lisppaste-browse-url)
          (\".\" . whatever-you-want-the-default-browse-url-function-to-be)))"
  (when (string-match
         "http://paste\\.lisp\\.org/display/\\([0-9]+\\)\\(?:#\\([0-9]+\\)\\)?"
         url)
    (let ((paste (string-to-number (match-string 1 url)))
          (ann (match-string 2 url)))
      (lisppaste-display-paste paste (if ann (string-to-number ann))))))

(defun lisppaste-display-paste (paste &optional n)
  "Display PASTE.

If N is non-nil, display PASTE's Nth annotation."
  (interactive
   (list (lisppaste-read-number "Paste number: ")))
  (when current-prefix-arg
    (setq n (lisppaste-read-number "Annotation number: " t)))
  (multiple-value-bind (num time user channel title annotations
                            content) (lisppaste-get-paste paste n)
    (switch-to-buffer (get-buffer-create
                       (format "*Paste %s%s*" paste
                               (if n
                                   (format " annotation %s"
                                           n)
                                 ""))))
    (setq buffer-read-only nil)
    (erase-buffer)
    (insert (format "Paste number: %s%s\nUser: %s\nChannel: %s\nTitle: %s\nDate: %s\nAnnotations: %s\n\n"
                    paste (if n
                              (format "\nAnnotation: %s" n)
                              "")
                    user channel title
                    (lisppaste-clean-time-string time)
                    annotations))
    (insert (lisppaste-clean-returned-paste content))
    (set-text-properties (point-min)
                         (point-max)
                         `(lisppaste-user ,user
                           lisppaste-title ,title
                           lisppaste-time ,time
                           lisppaste-paste ,paste
                           lisppaste-annotation ,n
                           lisppaste-annotations ,annotations
                           lisppaste-channel ,channel)))
  (lisppaste-mode))

(defun lisppaste-list-paste-annotations (paste)
  "List PASTE's annotations."
  (interactive
   (list (lisppaste-read-number
          "List annotations for paste number: ")))
  (let ((result (lisppaste-list-annotations paste))
        (buffer-read-only nil))
    (unless result
      (error "Paste %s has no annotations" paste))
    (switch-to-buffer (get-buffer-create
                       (format "*Paste %s Annotations*" paste)))
    (erase-buffer)
    (loop for (num time user channel title annotations) in result
          do (insert
              (propertize (format
                           "Annotation number: %s\nUser: %s\nchannel: %s\nTitle: %s\nDate: %s\n"
                           num user channel title
                           (lisppaste-clean-time-string time))
                          'lisppaste-user user
                          'lisppaste-time time
                          'lisppaste-paste paste
                          'lisppaste-annotation num
                          'lisppaste-channel channel
                          'lisppaste-annotations annotations)
              (make-string 75 ?=)
              "\n"))
    (lisppaste-mode)))

(defun lisppaste-list-recent-pastes (n &optional start channel)
  "List the most recent N pastes.

If START is non-nil, list the most recent N pastes prior to and
including START.
If CHANNEL is non-nil, only list pastes for that channel."
  (interactive "nNumber of pastes to get: ")
  (if current-prefix-arg
      (setq start (lisppaste-read-number "Start paste: ")
            channel (lisppaste-read-channel)))
  (let ((result (lisppaste-list-pastes n start channel)))
    (unless result
      (error "No pastes returned"))
    (switch-to-buffer (get-buffer-create
                       (format "*Paste list%s*"
                               (if (and channel (not (string= channel "")))
                                   (format " for %s" channel)
                                 ""))))
    (setq buffer-read-only nil)
    (erase-buffer)
    (loop for (num time user channel title annotations) in result
          do (insert
              (propertize (format
                           "Paste number: %s\nUser: %s\nChannel: %s\nTitle: %s\nDate: %s\nAnnotations: %s\n"
                           num user channel title (lisppaste-clean-time-string time)
                           annotations)
                          'lisppaste-user user
                          'lisppaste-title title
                          'lisppaste-time time
                          'lisppaste-paste num
                          'lisppaste-channel channel
                          'lisppaste-annotations annotations)
              (make-string 75 ?=)
              "\n"))
    (lisppaste-mode)))

(defun lisppaste-create-paste (callback-fn)
  "Create a new paste.

CALLBACK-FN should be a function accepting one argument to send the
paste.  See also `lisppaste-send-paste'."
  (switch-to-buffer (get-buffer-create "*paste*"))
  (erase-buffer)
  (insert lisppaste-creation-help)
  (local-set-key (kbd "C-c C-d") #'lisppaste-quit)
  (local-set-key (kbd "C-c C-c") `(lambda ()
                                    (interactive)
                                    (lisppaste-send-paste ,callback-fn))))

(defun lisppaste-send-paste (callback-fn)
  "Send a paste via CALLBACK-FN.

CALLBACK-FN is called with one argument, the contents of the
`current-buffer' from the end of `lisppaste-creation-help' to
`point-max'."
  (goto-char (point-min))
  (search-forward lisppaste-creation-help)
  (funcall callback-fn (buffer-substring-no-properties
                        (match-end 0) (point-max)))
  (kill-this-buffer))

(defun lisppaste-create-new-paste (&optional channel nick title)
  "Interactively create a new paste.

CHANNEL, NICK and TITLE are defaults for the paste's channel, nick
and title arguments respectively."
  (interactive)
  (let* ((channel (or channel (lisppaste-read-channel)))
         (nick    (or nick (lisppaste-read-nick channel)))
         (title   (or title (lisppaste-read-title))))
    (lisppaste-check-channel channel)
    (lisppaste-create-paste `(lambda (x)
                               (lisppaste-new-paste ,channel ,nick
                                                    ,title x)))))

(defun lisppaste-create-new-annotation (&optional channel nick title n)
  "Interactively annotate a paste.

CHANNEL, NICK, TITLE and N are defaults for the annotations's
channel, nick, title, and paste to annotate respectively."
  (interactive)
  (let* ((channel (or channel (lisppaste-read-channel)))
         (nick    (or nick (lisppaste-read-nick channel)))
         (title   (or title (lisppaste-read-title)))
         (n       (or n (lisppaste-read-number "Paste to annotate: "))))
    (lisppaste-check-channel channel)
    (lisppaste-create-paste `(lambda (x)
                               (lisppaste-new-paste ,channel ,nick
                                                    ,title x ,n)))))

(defun lisppaste-dwim ()
  "Display either the paste or annotation at `point'."
  (interactive)
  (let ((props (text-properties-at (point))))
    (unless (lisppaste-paste props)
      (error "No paste at point"))
    (if (lisppaste-annotation props)
        (lisppaste-display-paste (lisppaste-paste props)
                                 (lisppaste-annotation props))
      (lisppaste-display-paste (lisppaste-paste props)))))

(defun lisppaste-quit ()
  "Quit the current paste buffer."
  (interactive)
  (set-buffer-modified-p nil)
  (kill-this-buffer))

(defun lisppaste-annotate ()
  "Annotate the paste at `point'."
  (interactive)
  (let ((props (text-properties-at (point))))
    (lisppaste-create-new-annotation (lisppaste-channel props)
                                     nil
                                     nil
                                     (lisppaste-paste props))))

(defun lisppaste-display-supported-channels ()
  "Display the channels that lisppaste is running in.

As a side-effect, this updates the channel list stored in the
variable `lisppaste-channels'."
  (interactive)
  (switch-to-buffer (get-buffer-create "*Lisppaste channels*"))
  (erase-buffer)
  (insert "Lisppaste is running on the following channels.\n\n")
  (mapc #'(lambda (c) (insert c "\n"))
        (setq lisppaste-channels (lisppaste-channels))))

(defvar lisppaste-help
  (concat
   "Commands:\n"
   "`a' -- lisppaste-annotate\n"
   "       Annotate the paste at point.  With prefix arg, prompt\n"
   "       for a paste number to annotate.\n"
   "`c' -- lisppaste-display-supported-channels\n"
   "       Display channels lisppaste is running on.\n"
   "`d' -- lisppaste-display-paste\n"
   "       Fetch a paste.  With prefix arg, fetch an annotation.\n"
   "`h' -- lisppaste-help\n"
   "       Show this help.\n"
   "`l a' -- lisppaste-list-paste-annotations\n"
   "         List a paste's annotations.\n"
   "`l p' -- lisppaste-list-recent-pastes\n"
   "         List recent pastes.  With prefix arg, prompt for\n"
   "         channel and start paste.\n"
   "`n' -- lisppaste-create-new-paste\n"
   "       Create a new paste.\n"
   "RET -- lisppaste-dwim\n"
   "       Fetch either the paste or the annotation at point.\n"
   "SPC -- scroll-up\n"
   "`q' -- lisppaste-quit\n"
   "       Quit the paste display.\n"))

(defun lisppaste-help ()
  "Show some help for `lisppaste-mode'."
  (interactive)
  (with-output-to-temp-buffer "*Lisppaste Help*"
    (princ lisppaste-help)))

(defun lisppaste ()
  "Top-level interface to lisppaste."
  (interactive)
  (switch-to-buffer (get-buffer-create "*Lisppaste*"))
  (setq buffer-read-only nil)
  (erase-buffer)
  (insert "Top-level interface to lisppaste\n\n"
          lisppaste-help)
  (lisppaste-mode))

(defvar lisppaste-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "a" #'lisppaste-annotate)
    (define-key map "c" #'lisppaste-display-supported-channels)
    (define-key map "d" #'lisppaste-display-paste)
    (define-key map "h" #'lisppaste-help)
    (define-key map (kbd "l a") #'lisppaste-list-paste-annotations)
    (define-key map (kbd "l p") #'lisppaste-list-recent-pastes)
    (define-key map "n" #'lisppaste-create-new-paste)
    (define-key map (kbd "RET") #'lisppaste-dwim)
    (define-key map (kbd "SPC") #'scroll-up)
    (define-key map "q" #'lisppaste-quit)
    map)
  "Keymap for `lisppaste-mode'.")

(define-derived-mode lisppaste-mode fundamental-mode "Lisppaste"
  "Major mode for viewing and creating IRC pastes via the lisppaste pastebot."
  (setq buffer-read-only t)
  (goto-char (point-min)))

(provide 'lisppaste)

;;; lisppaste.el ends here
