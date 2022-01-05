;;; postagger.el --- tag text with part-of-speech information
;;
;; Author: Michael Piotrowski <mxp@cl.uzh.ch>
;; Keywords: editing, part-of-speech tagging, LingURed
;; Time-stamp: <2022-01-05T15:32:23 mpiotrow>
;; Version: $Revision: $

;; Copyright (C) 2009 Michael Piotrowski

;; ### PRELIMINARY GNU EMACS VERSION ###

;; This file is not currently part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program ; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; postagger provides an interface to a part-of-speech tagger,
;; currently MBT (Memory-based tagger generator and tagger)
;; <http://ilk.uvt.nl/mbt/>.  The primary interface is the function
;; `postagger-tag-sentence', which feeds the current sentence to the
;; tagger and attaches the POS tags returned by the tagger as text
;; properties to the word forms of the sentence.
;;
;; Put this in your init.el to make the functions of this library
;; available:
;;
;;  (require 'postagger)
;;
;; The settings files for Mbt to use are (obviously) language-dependent.
;; Specify the settings file for each language you're using in the
;; variable `postagger-settings-files'.  The settings file to use is
;; selected on the basis of the current language environment.  Use the
;; function `set-language-environment' to correctly set the language
;; environment.
;; 
;; postagger provides a customize interface to set all relevant options.
;;
;; postagger is not really useful by itself, but it is intended to
;; provide infrastructure for linguistically supported editing
;; functions.  `postagger-tag-sentence' will then probably be run
;; automatically by some hook.
;;
;; However, `postagger-tag-sentence' can be called interactively.  For
;; testing you may want to bind `postagger-tag-sentence' to a key
;; combination, e.g., C-c p:
;;
;;  (define-key text-mode-map [(control c p)] 'postagger-tag-sentence)
;;
;; If you're using AUCTeX, you may want to add:
;;
;;  (add-hook 'TeX-mode-hook
;;	    (lambda ()
;;	      (define-key LaTeX-mode-map [(control c p)]
;;	        'postagger-tag-sentence)))
;;
;; If you're using Gnus, you may also want to add:
;;
;;  (define-key message-mode-map [(control c p)] 'postagger-tag-sentence)
;;
;; Todo:
;;
;; - Should we use a buffer instead of the global `postagger-output' variable?
;; - Should we specify a sentinel for the process?
;; - One could also use a transaction queue for communicating with the process
;;   (Info-goto-node "(Lispref)Transaction Queues").  Would this be better?

;;; Code:

(require 'cl-lib)
(require 'levents)

(defun beginning-of-word ()
  "Move point to the beginning of the word the point is in or which is
immediately left to the point."
  (skip-syntax-backward "w_"))



;; XEmacs <= 21.4 does not have replace-regexp-in-string, but XEmacs >= 21.5
;; dumps it (it is defined in subr.el).  Therefore, it is either defined
;; regardless of what has been loaded already, or it won't be defined
;; regardless of what is loaded.
(unless (fboundp 'replace-regexp-in-string)
    (defun replace-regexp-in-string (regexp rep string &optional
				     fixedcase literal subexp start)
      "Replace all matches for REGEXP with REP in STRING.

Return a new string containing the replacements.

Optional arguments FIXEDCASE, LITERAL and SUBEXP are like the
arguments with the same names of function `replace-match'.  If START
is non-nil, start replacements at that index in STRING.

REP is either a string used as the NEWTEXT arg of `replace-match' or a
function.  If it is a function it is applied to each match to generate
the replacement passed to `replace-match'; the match-data at this
point are such that match 0 is the function's argument.

To replace only the first match (if any), make REGEXP match up to \\'
and replace a sub-expression, e.g.
  (replace-regexp-in-string \"\\(foo\\).*\\'\" \"bar\" \" foo foo\" nil nil 1)
    => \" bar foo\"
"
      (let ((l (length string))
	    (start (or start 0))
	    matches str mb me)
	(save-match-data
	  (while (and (< start l) (string-match regexp string start))
	    (setq mb (match-beginning 0)
		  me (match-end 0))
	    ;; If we matched the empty string, make sure we advance by one char
	    (when (= me mb) (setq me (min l (1+ mb))))
	    ;; Generate a replacement for the matched substring.
	    ;; Operate only on the substring to minimize string consing.
	    ;; Set up match data for the substring for replacement;
	    ;; presumably this is likely to be faster than munging the
	    ;; match data directly in Lisp.
	    (string-match regexp (setq str (substring string mb me)))
	    (setq matches
		  (cons (replace-match (if (stringp rep)
					   rep
					 (funcall rep (match-string 0 str)))
				       fixedcase literal str subexp)
			(cons (substring string start mb) ; unmatched prefix
			      matches)))
	    (setq start me))
	  ;; Reconstruct a string from the pieces.
	  (setq matches (cons (substring string start l) matches)) ; leftover
	  (apply #'concat (nreverse matches))))))

(unless (fboundp 'set-face-bold)
  (defun set-face-bold (face v &optional f)
    (when v (ignore-errors (make-face-bold face)))))

;; Customization

(defgroup postagger ()
  "Interface to MBT part-of-speech tagger."
  :prefix "postagger-"
  :group 'editing)

(defcustom postagger-command "Mbt"
  "Name of the POS tagger executable.  Currently only MBT is supported,
so this should be something like \"Mbt\" or \"/opt/timbl/bin/Mbt\"."
  :type 'string
  :group 'postagger)

(defcustom postagger-settings-files
  `("German"  ,(expand-file-name "~/lib/mbt/tuebadz-5.0.data.settings")
    "English" ,(expand-file-name "~/lib/mbt/brown.data.settings")
    "French"  nil
    "Dutch"   ,(expand-file-name "~/lib/mbt/eindh.data.settings"))
  "Names of the Mbt settings files for different languages.

This is a property list (plist) keyed by language. The language name
should be a name accepted by the function `set-language-environment'."
  :type '(restricted-sexp :match-alternatives (valid-plist-p))
  :group 'postagger)

(defcustom postagger-sentence-delimiter "<utt>"
  "The delimiter for marking the end of a sentence."
  :type 'string
  :group 'postagger)

(copy-face 'default 'postagger-finite-verb-face)
(copy-face 'default 'postagger-separable-prefix-face)
(set-face-background 'postagger-finite-verb-face "light pink")
(set-face-bold 'postagger-finite-verb-face t)
(set-face-background 'postagger-separable-prefix-face "light pink")

;; Variables

(defvar postagger-output nil
  "Variable which holds the output from the POS tagger.")

(defvar postagger-debug nil
  "When t, print debug messages.")

;; Functions

;; This implementation of `postagger-tokenize' does work for our
;; purposes, but I'm not completely happy with it: First, it traverses
;; the buffer three times.  Second, it contains hardcoded information
;; similar (but not identical) to the syntax table; movement commands
;; (which are syntax-based) thus have a different view of words than
;; the POS tagger.  But perhaps this is unavoidable, since syntax
;; classes do not seem to be sufficient for our purposes.

;; Maybe we could try something like `forward-same-syntax' (see
;; thingatpt) and insert a space where the syntax class changes.

(defun postagger-tokenize (string)
  "Tokenize STRING so that it can be fed to the POS tagger."
  (with-temp-buffer
    (insert string)
    (goto-char 0)
    (while (re-search-forward "\\([^0-9]\\)\\([.,:;]\\)" nil t)
      (replace-match "\\1 \\2 " nil nil))

    (goto-char 0)
    (while (re-search-forward "\\([^0-9]\\)\\([']\\)" nil t)
      (replace-match "\\1 \\2" nil nil))

    (goto-char 0)
    (while (re-search-forward "\\([()!?\"«»]\\)" nil t)
      (replace-match " \\1 " nil nil))

    (buffer-string)))

(defun postagger-process-filter (process output)
  "Preprocess the output of the POS tagger and store it in the
`postagger-output' variable."
  (when postagger-debug
    (message "Tagger output: %s" output))

  (setq postagger-output
	(map 'list
	     (lambda (x) 
	       (let ((tagged (split-string x "/" t)))
		 (cons (car tagged) (intern (cadr tagged)))))
	     (split-string (replace-regexp-in-string
			    postagger-sentence-delimiter "" output)))))

(defun postagger-process-name ()
  "Return the name of the POS tagger process for the current language
environment."
  (concat "postagger-" current-language-environment)
)

(defun postagger-start ()
  "Start the POS tagger process, unless it is already running.

This function is normally called automatically from functions requiring
the POS tagger, e.g., `postagger-tag-sentence'."
  (unless (get-process (postagger-process-name))
    (let ((process-connection-type t))  ; Use a pty (pipe seems to be unreliable)
      (start-process-shell-command (postagger-process-name) "*postagger*"
				   postagger-command "-s"
				   (lax-plist-get postagger-settings-files
						  current-language-environment)
				   "2>" "/dev/null"))
    (message "Starting POS tagger...")
    (set-process-filter (get-process (postagger-process-name))
			'postagger-process-filter)
    (process-kill-without-query (get-process (postagger-process-name)))
    (sleep-for 3)
    (message nil)))

(defun postagger-stop ()
  "Stop the POS tagger process for the current language environment,
if it exists."
  (interactive)
  (when (get-process (postagger-process-name))
    (delete-process (get-process (postagger-process-name)))))

(defun postagger-attach-properties (tags)
  "This is an internal function; it is intended to be called
only from `postagger-tag-sentence'.

TAGS is a postagger result list, i.e., a list of pairs, where each
pair consists of the surface and the assigned tag.  This function
searches the current buffer for the surfaces and assigns a text
property with the tag to the matching token."
  (when tags
    (let ((surf (caar tags))
	  (tag  (cdar tags))
	  (postagger-temp-overlay))
      (search-forward surf)
      (when postagger-debug
	(message "%d %s %s" (match-beginning 0) surf tag))
      (put-text-property (match-beginning 0) (point) 'pos tag)

      ;; This is just an example: Highlight finite verbs (STTS tagset)
      (when (or (eq tag 'VVFIN) (eq tag 'VMFIN) (eq tag 'VAFIN))
	(setq postagger-temp-overlay (make-overlay (match-beginning 0) (point)))
	(overlay-put postagger-temp-overlay 'face 'postagger-finite-verb-face))
      ;; And separable prefixes.
      (when (or (eq tag 'PTKVZ))
	(setq postagger-temp-overlay (make-overlay (match-beginning 0) (point)))
	(overlay-put postagger-temp-overlay 'face 'postagger-separable-prefix-face))
      )

    (postagger-attach-properties (cdr tags))))

(defun postagger-tag-sentence ()
  "Run the POS tagger on the current sentence and tag the word forms of
the sentence with their parts of speech.  POS tags are attached to the
word forms as text properties."
  (interactive)
  (let ((sentence (thing-at-point 'sentence)))
    (unless (null sentence)
      (unless (get-process (postagger-process-name))
	(postagger-start))

      (process-send-string (postagger-process-name)
			   (concat (postagger-tokenize sentence)
				   "\n" postagger-sentence-delimiter "\n"))
      (accept-process-output (get-process (postagger-process-name)) 2)

      (let ((tags postagger-output)
	    (end-of-sentence (cdr (bounds-of-thing-at-point 'sentence))))
	(save-excursion
	  (beginning-of-thing 'sentence)
	  (postagger-attach-properties tags))))))

(defun postagger-tag-buffer (&optional buffer)
  "Run the POS tagger on BUFFER.  BUFFER defaults to the current buffer.
This function calls `postagger-tag-sentence'."
  (interactive "*b")
  (with-current-buffer (or buffer (current-buffer))
    (save-excursion
      (goto-char (point-min))
      ;; Iterate over all sentences
      (while (not (eobp))
	(forward-sentence)
	(postagger-tag-sentence)))))

;; Utility functions for using POS tags
(defun pos-at-point ()
  "Return the value of the `pos' text property at point."
  (plist-get (text-properties-at (point)) 'pos))

(defun pos-of-word-at-point ()
  "Return the part of speech of the word at point."
  (let ((start (car (bounds-of-thing-at-point 'word))))
    (when start
      (plist-get (text-properties-at start) 'pos))))

(defun pos-of-word-before ()
  "Return the part of speech of the preceding word."
  (save-excursion
    (beginning-of-word)
    (cond ((eq (point) (point-min))
	   nil)
	  (t
	   (backward-word)
	   (pos-at-point)))))

;; Usage examples
(defun goto-first-finite-verb ()
  "This is an example function which shows how POS information stored
in text properties (as added by `postagger-tag-sentence') could be
used."
  (interactive)
  (beginning-of-thing 'sentence)
  (let* ((start (car (bounds-of-thing-at-point 'sentence)))
	 (end   (cdr (bounds-of-thing-at-point 'sentence)))
	 (verb  (text-property-any start end 'pos 'VVFIN)))
    (when verb
      (goto-char verb))))

(defface postagger-sentence-highlight-face
  '((t (:background "orchid1")))
  "Face used for highlighting sentences without a finite verb.")

(defun postagger-sentences-without-finverb ()
  ""
  (interactive)

  (postagger-tag-buffer)

  (save-excursion
    (let ((sentence-bounds (bounds-of-thing-at-point 'sentence))
	  (overlays)
	  )
      (goto-char (cdr sentence-bounds))

      (while (< (point) (point-max))
	(setq sentence-bounds (bounds-of-thing-at-point 'sentence))

	(unless
	    (or
	     (text-property-any (car sentence-bounds) (cdr sentence-bounds)
				'pos 'VVFIN)
	     (text-property-any (car sentence-bounds) (cdr sentence-bounds)
				'pos 'VAFIN)
	     (text-property-any (car sentence-bounds) (cdr sentence-bounds)
				'pos 'VMFIN))
	  (push (make-overlay (car sentence-bounds) (cdr sentence-bounds))
		overlays)

	  (overlay-put
	   (car overlays)
	   'face
	   'postagger-sentence-highlight-face)

	  )
	(forward-sentence)
	)

      )
    )
  )


(defun postagger-sentence-pos-to-string ()
  "Return the POS tags of the sentence at point as a string."

  (let ((sentence-bounds (bounds-of-thing-at-point 'sentence))
	(string))
    (save-excursion
      (goto-char (car sentence-bounds))

      (while (< (point) (cdr sentence-bounds))
	(if (pos-at-point)
	    (setq string (concat string (symbol-name (pos-at-point))))
	  (setq string (concat string " ")))

	(goto-char (next-single-property-change (point) 'pos))))

    string))

(defun postagger-highlight-matching-sentences (regex)
  ""
  (interactive "sRegex over POS: ")

  (postagger-tag-buffer)

  (let ((sentence-bounds (bounds-of-thing-at-point 'sentence))
	(overlays)
	(event))
    (save-excursion
      (goto-char (cdr sentence-bounds))

      (while (< (point) (point-max))
    	(setq sentence-bounds (bounds-of-thing-at-point 'sentence))

    	(when (string-match regex (postagger-sentence-pos-to-string))
    	  (push (make-overlay (car sentence-bounds) (cdr sentence-bounds))
    		overlays)
    	  (overlay-put (car overlays) 'face 'postagger-sentence-highlight-face)
    	  )
    	(forward-sentence)
    	)
      )

    ;; What corresponds to events in FSF Emacs!?
    ;; Remove the highlighting at the next event
    ;; (setq event (next-command-event))
    ;; (when event
    ;;   (mapcar 'delete-overlay overlays)
    ;;   (dispatch-event event))
    ;(setq event (read-command-event))
    (when (read-command-event)
      (mapcar 'delete-overlay overlays)
      )

    ))


;;

(provide 'postagger)

;;; end of postagger.el
