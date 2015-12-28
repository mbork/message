;;; mbork-message.el --- mbork's helper functions for message-mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Marcin Borkowski

;; Author: Marcin Borkowski <mbork@amu.edu.pl>
;; Keywords: mail

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 2 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This small library is a set of helper functions to write emails
;; more effectively.  Currently it is WiP, but quite a few functions
;; here are already useful.  Note: many things are assumed, like that
;; ">" is the quotation prefix.  Comments welcome.

;;; Code:

(require 'cl-lib)


(defcustom message-quotation-regex
  "> \\|>$\\|On.*wrote:$"
  "A regular expression matching at the beginning of a quotation line.
Most probably should be an alternative of a quotation prefix (usually
\"> \"), an empty quotation line (usually \">$\") and a citation line
(e.g., \"On.*wrote:$\").")

(defun message-in-quotation-p ()
  "Return t if the point is within a quotation, including the
citation line.  Warning: this function is extremely ugly and
probably buggy."
  ;; We can't use `message-yank-prefix', since the quotation line may
  ;; be just the single ">", and the default value of
  ;; `message-yank-prefix' is "> ".
  (save-excursion
    (beginning-of-line)
    (looking-at-p message-quotation-regex)))

(defun mbork/message-add-newline ()
  "Add a newline at the beginning of the message."
  (save-excursion
    (search-forward (concat "\n" mail-header-separator "\n") nil t)
    (newline)))

(add-hook 'mu4e-compose-mode-hook #'mbork/message-add-newline)

(defun mbork/message-count-sentences (&optional print-message)
  "Count the sentences in the current message.
Exclude headers, signature and quotation lines nil.  Print the
resulting number if PRINT-MESSAGE is non-nil."
  (interactive "p")
  (save-excursion
    (save-restriction
      (narrow-to-region
       (progn
	 (goto-char (point-min))
	 (search-forward (concat "\n" mail-header-separator "\n") nil t)
	 (point))
       (progn
	 (goto-char (point-max))
	 (re-search-backward message-signature-separator nil t)
	 (skip-chars-backward " \t\n")
	 (point)))
      (goto-char (point-min))
      (let ((sentences 0))
	(while (not (eobp))
	  (save-restriction
	    (narrow-to-region
	     (point)
	     (save-excursion
	       (while (not (or (message-in-quotation-p)
			       (eobp)))
		 (forward-line 1))
	       (skip-chars-backward " \t\n")
	       (point)))
	    (while (not (eobp))
	      (forward-sentence 1)
	      (setq sentences (1+ sentences))))
	  (unless (eobp)
	    (forward-char))
	  (while (and (not (eobp))
		      (or (message-in-quotation-p)
			  (looking-at-p "^[ \t]*$")))
	    (forward-line 1)))	
	(if print-message
	    (message
	     "%s sentence%s in this message."
	     sentences
	     (if (= 1 sentences) "" "s"))
	  sentences)))))

(defun mbork/message-goto-end-body ()
  "Go to end of message body (before signature and attachments)."
  (let (before-attachments)
    (setq before-attachments (goto-char (point-max)))
    (while (and (search-backward "<#part " nil t)
		(eq (get-text-property (point) 'face)
		    'message-mml))
      (setq before-attachments (point)))
    (goto-char before-attachments)
    (re-search-backward message-signature-separator nil t)))

(defun mbork/message-signature-delete (&optional force)
  "Delete the signature of the email."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward message-signature-separator nil t)
      (delete-region
       (1- (match-beginning 0))
       (let ((pos (point)))
	 (while (and pos
		     (not (eq (get-text-property pos 'face)
			      'message-mml)))
	   (setq pos (next-single-property-change pos 'face)))
	 (or pos (point-max)))))))

(defun mbork/message-insert-signature-advice (fun &rest r)
  "A function for avising `message-insert-signature'."
  (save-excursion
    (save-restriction
      (mbork/message-signature-delete)
      (goto-char (point-max))
      (when (and (search-backward "<#part " nil t)
		 (eq (get-text-property (point) 'face) 'message-mml))
	(narrow-to-region (point-min) (point)))
      (apply fun r))))

(advice-add 'message-insert-signature :around 'mbork/message-insert-signature-advice)

(setq message-signature-insert-empty-line t)

(defcustom mbork/message-language-list
  '(pl en)
  "A list of possible langauges of the message.")

(defvar mbork/message-language (car mbork/message-language-list)
  "The symbol of the language of the current message.")
(make-variable-buffer-local 'mbork/message-language)

(defun mbork/message-select-language (&optional number)
  "Select the language for the message.
If called with numeric argument N, select Nth language from the
`mbork/message-languages' list (counting from zero)."
  (interactive "P")
  (setq mbork/message-language
	(if (numberp number)
	    (nth number mbork/message-language-list)
	  (intern (completing-read "Language for this buffer: "
				   mbork/message-language-list
				   nil t)))))

;; TODO: code duplication!
(defun mbork/message-sentenc-es-signature-pl (sentences)
  "Return a signature (in Polish) with a sentence count, but only
if there are no more than five sentences in the message."
  (when (<= 2 sentences 5)
    (format "- Dlaczego ta wiadomość jest tak krótka?
- Bo szkoda naszego czasu na zbędne rzeczy.
http://%s.sentenc.es" (alist-get sentences '((2 . "two")
					     (3 . "three")
					     (4 . "four")
					     (5 . "five"))))))

(defun mbork/message-sentenc-es-signature-en (sentences)
  "Return a signature (in English) with a sentence count, but only
if there are no more than five sentences in the message."
  (when (<= 2 sentences 5)
    (format "Q: Why is this message so short?
A: Because our time is valuable.
http://%s.sentenc.es" (alist-get sentences '((2 . "two")
					     (3 . "three")
					     (4 . "four")
					     (5 . "five"))))))

(defcustom mbork/message-signature-alist
  '()
  "Alist of lists of signature-generating functions.
Each entry is a cons whose car is the language symbol and cdr is
a list of functions taking no arguments.  Each of these functions
should return either nil (if they are not applicable) or a string
with the signature.  Instead of a function, an Elisp form may be
used; it's then evalled each time the signature is changed into
it.  (Useful ones include `(shell-command-to-string \"some shell
command\")' Also, a string literal may be used, and it is then
used as the signature.")

(defcustom mbork/message-language-recognizers-alist
  '(("the\\|are\\|is\\|there" . en)
    ("[ąćęłńóśźż]" . pl))
  "Alist of pairs of regexen and languages.
Used by `mbork/buffer-determine-language': the language of the
message is determined as the one whose regex had most hits.")

(defun mbork/buffer-determine-language ()
  "Determine the language of the buffer and return as a symbol.
Currently only `pl' and `en' are supported.  The determination is
a very crude, regex-based one, but for two langauges it seems to
work just fine."
  ;; When I have time, I'll probably change this to letter/n-gram
  ;; frequency approach.
  (mapcar (lambda (lang)
	    (cons (how-many (car lang) (point-min) (point-max))
		  (cdr lang)))
	  mbork/message-language-recognizers-alist))

(defun mbork/message-insert-custom-signature (signature)
  "Insert SIGNATURE at the bottom of the message."
  (let ((message-signature signature))
    (message-insert-signature)))

(defun mbork/message-sentences-count-signature ()
  "Return a signature containing a sentence count."
  (cl-case (message-count-sentences)
    (2 "Marcin Borkowski
This message contains no greetings etc. and consists of
only two sentences because I value your time.  See
http://two.sentenc.es/ for more info.")
    (t "")))

(defun mbork/message-blank-line-p (include-quotations)
  "Return non-nil if the current line is blank.
More precisely, an empty string is returned if the line is blank,
and a string of \">\" signs and spaces (beginning and ending with
\">\") if INCLUDE-QUOTATIONS is non-nil and the line begins with
\">\" and contains nothing but these signs and whitespace."
  (save-excursion
    (beginning-of-line)
    (cond ((looking-at-p "[ \t]*$")
	   "")
	  ((and include-quotations
		(eq (char-after) ?>)
		(looking-at "^\\([> \t]*>\\)[ \t]*$"))
	   (match-string-no-properties 1)))))

(defun mbork/message-back-up-blank-lines ()
  "Go backward to the beginning of the first blank line in the
current stretch.  Don't move point if not on a blank line."
  (when (and (mbork/message-blank-line-p t)
	     (save-excursion
	       (beginning-of-line)
	       (not (bobp))))
    (while (progn
	     (forward-line -1)
	     (mbork/message-blank-line-p t)))
    (forward-line 1)))

(defun mbork/message-compress-blank-lines-here ()
  "Delete a run of blank lines here.
A line is considered blank if it contains nothing but whitespace
and comment chars.  In the latter case it must begin with
a comment char.  Leave the first shortest prefix."
  (when (mbork/message-blank-line-p t)
    (mbork/message-back-up-blank-lines)
    (let (shortest-quotation-prefix current-quotation-prefix)
      (while (and (not (eobp))
		  (setq current-quotation-prefix (mbork/message-blank-line-p t)))
	(delete-region (line-beginning-position) (line-end-position))
	(unless (eobp) (delete-char 1))
	(if (or (null shortest-quotation-prefix)
		(< (length current-quotation-prefix) (length shortest-quotation-prefix)))
	    (setq shortest-quotation-prefix current-quotation-prefix)))
      (insert shortest-quotation-prefix ?\n))))

(defun mbork/message-compress-blank-lines ()
  "Delete runs of blank lines, leaving just one.
In case of blank lines with various amount of quotation signs,
leave the first shortest one."
  (interactive)
  (save-excursion
    (save-restriction
      (mbork/message-narrow-to-body)
      (while (not (eobp))
	(while (not (mbork/message-blank-line-p t))
	  (forward-line))
	(mbork/message-compress-blank-lines-here)))))

(defcustom mbork/message-salutations
  '("Czołem"
    "Cześć"
    "Hej"
    "Szanowny Panie Profesorze"
    "Szanowna Pani Profesor"
    "Szanowny Panie Doktorze"
    "Szanowna Pani Doktor"
    "Szanowny Kolego"
    "Szanowna Koleżanko"
    "Szanowni Koledzy"
    "Szanowny Panie"
    "Szanowna Pani"
    "Szanowni Państwo"
    "Szanowni Panowie"
    "Szanowne Panie"
    "Dzień dobry"
    "Dobry wieczór"
    "Hi all"
    "Hi there"
    "Hi list"
    "Hi"
    "Dear Sir"
    "Dear Madam"
    "Dear Sirs"
    "Dear Doctor"
    "Dear Professor")
  "A list of standard salutations for the beginning of the email,
sans punctuation (a comma will be added automatically at the
end).")

(defvar mbork/message-salutation-history nil
  "History of message salutations.")

(defcustom mbork/message-closings
  '("Pozdrawiam"
    "Łączę pozdrowienia"
    "Pozostaję z szacunkiem"
    "Bywaj"
    "Best,"
    "Regards,"
    "Sincerely yours,"
    "Yours sincerely,"
    "Take care,"
    "TIA,"
    "HTH,")
  "A list of standard message closings.")

(defvar mbork/message-closing-history
  "History of message closings.")

(defun mbork/message-goto-body ()
  "A non-interactive version of `message-goto-body'.
It is much simpler and does not touch the mark ring."
  (goto-char (point-min))
  (search-forward (concat "\n" mail-header-separator "\n") nil t))

(defun mbork/message-narrow-to-body ()
  "Narrow to message body and leave point at its beginning."
  (interactive)
  (widen)
  (mbork/message-goto-body)
  (narrow-to-region
   (point)
   (save-excursion
     (mbork/message-goto-end-body)
     (point))))

(defmacro conditional-save-excursion (arg &rest body)
  "Wrap BODY in `save-excursion', but only if ARG is non-nil." 
 (declare (indent 1) (debug t))
  `(if ,arg
       (save-excursion ,@body)
     (push-mark)
     ,@body))

(defun mbork/message-delete-salutation ()
  "Delete the salutation from the first line of the message.
Assume that the first line is the salutation if it begins with the same
text as one of the elements of `mbork/message-salutations', sans the
comma at the end."
  (save-excursion
    (mbork/message-goto-body)
    (when (cl-some
	   (lambda (salutation)
	     (looking-at-p (regexp-quote salutation)))
	   mbork/message-salutations)
      (delete-region (point) (1+ (line-end-position))))))

(defun mbork/message-insert-salutation (&optional move-point)
  "Insert a salutation at the beginning of the message.
If there's already one there, delete it first.  When called with
a prefix argument, leave the point right before the comma at the end
of the newly-inserted salutation."
  (interactive "P")
  (let ((salutation (completing-read
		   "Salutation: "
		   mbork/message-salutations
		   nil
		   nil
		   nil
		   mbork/message-salutation-history)))
    (mbork/message-delete-salutation)
    (conditional-save-excursion (null move-point)
      (mbork/message-goto-body)
      (insert salutation ",\n")
      (backward-char 2))))

(defun mbork/message-delete-closing ()
  "Delete the closing from the message.
Assume that the closing is the last nonempty line before the
sigdashes."
  (save-excursion
    (re-search-forward message-signature-separator nil t)
    (goto-char (1- (match-beginning 0)))
    (while (looking-at-p "^[ \t]*$")
      (forward-line -1))
    (when (cl-some
	   (lambda (closing)
	     (looking-at-p
	      (regexp-quote closing)))
	   mbork/message-closings)
      (delete-region
       (point)
       (1+ (line-end-position))))))

(defun mbork/message-insert-closing (&optional move-point)
  "Insert a closing right before the sigdashes.
If there is one already, delete it first.  When called with
a prefix argument, leave the point right after the
newly-inserted closing."
  (interactive "P")
  (let ((closing (completing-read
		  "Closing: "
		  mbork/message-closings
		  nil nil nil
		  mbork/message-closing-history)))
    (mbork/message-delete-closing)
    (conditional-save-excursion (null move-point)
      (mbork/message-goto-end-body)
      (when (looking-at-p message-signature-separator)
	(forward-line -1))
      (mbork/message-compress-blank-lines-here)
      (insert closing "\n\n"))))

(defun mbork/message-open-line ()
  "If point is on a blank line (possibly with quotation signs),
run `mbork/message-compress-blank-lines-here', delete the
quotation signs and insert an empty line above and below point."
  (interactive)
  (when (mbork/message-blank-line-p t)
    (mbork/message-compress-blank-lines-here)
    (forward-line -1)
    (insert ?\n)
    (delete-region
     (point)
     (line-end-position))
    (forward-line -1)
    (insert ?\n)))

(defun mbork/message-next-paragraph (count)
  "Move COUNT message paragraphs forward.
Here, a message paragraph is a stretch of blank lines (in the
sense of `mbork/message-blank-line-p')."
  (interactive "p")
  (if (zerop count)
      (mbork/message-back-up-blank-lines)
    (let ((dir (signum count))
	  (check (if (> count 0) #'eobp #'bobp)))
      (dotimes (_ (abs count))
	(while (and (mbork/message-blank-line-p t)
		    (not (funcall check)))
	  (forward-line dir))
	(while (not (or (mbork/message-blank-line-p t)
			(funcall check)))
	  (forward-line dir))))))

(defun mbork/message-previous-paragraph (count)
  "Move COUNT message paragraphs backward.
Here, a message paragraph is a stretch of blank lines (in the
sense of `mbork/message-blank-line-p')."
  (interactive "p")
  (mbork/message-next-paragraph (- count)))

(provide 'mbork-message)

;;; mbork-message.el ends here
