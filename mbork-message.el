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


;;; Quotation-related stuff

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


;;; Counting sentences

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


;;; Simple signatures

(defun mbork/message-insert-custom-signature (signature)
  "Insert SIGNATURE at the bottom of the message."
  (let ((message-signature signature))
    (message-insert-signature)))

(defun mbork/message-insert-signature ()
  "Insert a signature below the message.
If there is one already, delete it first.  Don't move point."
  (interactive)
  (let ((signature (completing-read
		    "Signature: "
		    mbork/message-signatures
		    nil nil)))
    (mbork/message-signature-delete)
    (mbork/message-insert-custom-signature signature)))

(defun mbork/message-signature-delete (&optional force)
  "Delete the signature of the email if present.
If no signature was found, insert a newline.  This is a really
nasty hack so that a signature gets a newline before itself when
composing a new message."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward message-signature-separator nil t)
	(delete-region
	 (1- (match-beginning 0))
	 (progn
	   (goto-char (point-max))
	   (while (search-backward "<#part " nil t))
	   (point)))
      (goto-char (point-max))
      (while (search-backward "<#part " nil t))
      (insert "\n"))))

(defun mbork/message-insert-signature-advice (fun &rest r)
  "A function for advising `message-insert-signature'."
  (save-excursion
    (save-restriction
      (mbork/message-signature-delete)
      (goto-char (point-max))
      (when (search-backward "<#part " nil t)
	(narrow-to-region (point-min) (point)))
      (apply fun r))))

(advice-add 'message-insert-signature :around 'mbork/message-insert-signature-advice)

(setq message-signature-insert-empty-line t)

(defcustom mbork/message-signatures
  '()
  "List of signatures.")


;;; Identities

(defcustom mbork/message-identity-list
  '((pl-formal
     (salutations . ("Szanowny Panie Profesorze"
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
		     "Dobry wieczór"))
     (closings . ("Pozdrawiam"
		  "Łączę pozdrowienia"
		  "Pozostaję z szacunkiem"))
     (signatures . ("Marcin Borkowski
http://octd.wmi.amu.edu.pl/pl/Marcin_Borkowski
Wydział Matematyki i Informatyki
Uniwersytet im. Adama Mickiewicza")))
    (pl-informal
     (salutations . ("Czołem"
		     "Cześć"
		     "Hej"))
     (closings . ("Pozdrawiam"
		  "Na razie"
		  "Bywaj"))
     (signatures . ("Marcin Borkowski
http://mbork.pl")))
    (en-formal
     (salutations . ("Dear Sir"
		     "Dear Madam"
		     "Dear Sirs"
		     "Dear Doctor"
		     "Dear Professor"))
     (closings . ("Best regards,"
		  "Yours sincerely,"
		  "Yours faithfully,"))
     (signatures . ("Marcin Borkowski
http://octd.wmi.amu.edu.pl/en/Marcin_Borkowski
Faculty of Mathematics and Computer Science
Adam Mickiewicz University")))
    (en-informal
     (salutations . ("Hi all"
		     "Hi there"
		     "Hi list"
		     "Hi"))
     (closings . ("Best,"
		  "Take care,"
		  "TIA,"
		  "HTH,"))
     (signatures . ("Marcin Borkowski
http://mbork.pl/en"))))
  "A list of \"identities\".
An identity consists of a list of possible salutations, closings
and signatures.  Examples might be formal and informal messages,
or messages in different languages.

An identity is represented by a list whose car is the identity's
symbol and cdr is an alist with the keys `salutations', `closings' and
`signatures'.")

(defvar mbork/message-identity (car mbork/message-identity-list)
  "The symbol of the identity of the current message.")
(make-variable-buffer-local 'mbork/message-identity)

(defun mbork/message-select-identity (&optional number)
  "Select the identity for the message.
If called with numeric argument N, select Nth identity from the
`mbork/message-identity-list' (counting from zero)."
  (interactive "P")
  (mbork/message-set-identity
   (setq mbork/message-identity
	 (if (numberp number)
	     (nth number mbork/message-identity-list)
	   (intern (completing-read "Identity for this message: "
				    mbork/message-identity-list
				    nil t))))))

(defun mbork/message-set-identity (identity)
  "Set `mbork/message-signatures', `mbork/message-salutations'
and `mbork/message-closings' according to IDENTITY."
  (setq mbork/message-signatures (alist-get 'signatures
						 (alist-get identity mbork/message-identity-list)))
  (setq mbork/message-salutations (alist-get 'salutations
						 (alist-get identity mbork/message-identity-list)))
  (setq mbork/message-closings (alist-get 'closings (alist-get
							  identity mbork/message-identity-list))))

;; TODO: remove code duplication!
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


;;; Blank lines

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

(defun mbork/message-add-newline-at-the-beginning ()
  "Add a newline at the beginning of the message."
  (save-excursion
    (search-forward (concat "\n" mail-header-separator "\n") nil t)
    (insert "\n")))

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


;;; Salutations and closings

(defvar mbork/message-salutations
  '()
  "A list of standard salutations for the beginning of the email,
sans punctuation (a comma will be added automatically at the
end).  Initially empty, this will be populated by the
identity-setting functions.")

(defvar mbork/message-salutation-history nil
  "History of message salutations.")

(defcustom mbork/message-closings
  '()
  "A list of standard message closings.  Initially empty (see
`mbork/message-salutations' for the reason).")

(defvar mbork/message-closing-history
  "History of message closings.")

(defun mbork/message-goto-body ()
  "A non-interactive version of `message-goto-body'.
It is much simpler and does not touch the mark ring."
  (goto-char (point-min))
  (search-forward (concat "\n" mail-header-separator "\n") nil t))

(defun mbork/message-goto-end-body ()
  "Go to end of message body (before signature and attachments)."
  (let (before-attachments)
    (setq before-attachments (goto-char (point-max)))
    (while (search-backward "<#part " nil t)
      (setq before-attachments (point)))
    (goto-char before-attachments)
    (re-search-backward message-signature-separator nil t)))

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


;;; Main entry point
(defun mbork/message-prepare-message ()
  "Prepare the message for sending.
This means inserting a salutation, a closing and a signature,
compressing the blank lines, and if we reply to a message, positioning
the point after the first paragraph."
  (interactive)
  (call-interactively #'mbork/message-select-identity)
  (mbork/message-insert-salutation)
  (mbork/message-insert-closing)
  (mbork/message-insert-signature)
  (mbork/message-goto-body)
  (mbork/message-compress-blank-lines)
  (when mu4e-compose-parent-message
    (forward-paragraph 1)))


;;; That's all, folks

(provide 'mbork-message)

;;; mbork-message.el ends here
