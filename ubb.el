;;; ubb.el --- Unicode block browser      -*-coding:utf-8; lexical-binding:t-*-

;; Copyright (C) 2016 Free Software Foundation, Inc.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This is a mode for browsing the characters in Unicode Blocks or
;; other collections of characters, like Emacs' "charsets".
;;
;; Start the browser with "M-x ubb-browse".  That displays the
;; characters of the "Basic Latin" block.  From there it's probably
;; the easiest to use the menu to explore the available commands.
;;
;; The browser displays the current "set" of the current "group".  A
;; set is a collection of codepoints, e.g. the Unicode Block "Box
;; Drawing" is such a set.  Groups are collections of sets, e.g. all
;; Unicode Blocks form a group.
;;
;; Displaying large sets can be slow.  Don't hesitate to press C-g if
;; it takes too long.

;;; Code

(require 'cl-lib)


;;; Some type definitions

;; A ubb--set represents a set of codepoints.
(cl-defstruct (ubb--set (:constructor ubb--make-set% (name ranges))
			(:constructor nil)
			(:predicate nil) (:copier nil))
  ;; NAME is a string for display purposes.
  (name "" :type string :read-only t)
  ;; RANGES is a list or a function.
  ;; If a list then it looks like ((START . END) ...)
  ;; START and END denote a range of codepoints.
  ;; START is inclusive; END is exclusive.
  ;; If a function, it returns a list of ranges of the above form.
  (ranges () :type (or list function) :read-only t))

(defun ubb--check-range (x)
  (cl-assert (consp x))
  (let ((start (car x)) (end (cdr x)))
    (cl-assert (integerp start))
    (cl-assert (integerp end))
    (cl-assert (<= 0 start))
    (cl-assert (< start end))))

(defun ubb--sort-ranges (ranges)
  (sort (copy-sequence ranges)
	(lambda (r1 r2)
	  (< (car r1) (car r2)))))

(defun ubb--ranges-to-set (ranges)
  (cond ((functionp ranges) (ubb--ranges-to-set (funcall ranges)))
	((null ranges) '())
	((null (cdr ranges)) ranges)
	(t
	 (let ((sorted (ubb--sort-ranges ranges)))
	   (nreverse
	    (cl-reduce (lambda (left+m right)
			 (cl-destructuring-bind ((s1 &rest e1) &rest m) left+m
			   (cl-destructuring-bind (s2 &rest e2) right
			     (cl-assert (<= s1 s2))
			     (cond ((<= e1 s2) ; no overlap
				    (cons right left+m))
				   (t
				    (cons (cons (min s1 s2)
						(max e1 e2))
					  m))))))
		       (cdr sorted) :initial-value (list (car sorted))))))))

(defun ubb--make-set (name ranges)
  (cl-check-type name string) (cl-check-type ranges (or list function))
  (ubb--make-set% name ranges))

(defun ubb--set-size (set)
  (cl-loop for (start . end) in (ubb--ranges-to-set (ubb--set-ranges set))
	   sum (- end start)))

(defun ubb--set-foreach (fun set)
  (cl-loop for (start . end) in (ubb--ranges-to-set (ubb--set-ranges set))
	   do (cl-loop for codepoint from start below end
		       do (funcall fun codepoint))))

(defun ubb--set-fold (fun init set)
  (let ((state init))
    (ubb--set-foreach (lambda (codepoint)
			(setq state (funcall fun codepoint state)))
		      set)
    state))

(defun ubb--set-count (set test)
  (let ((sum 0))
    (ubb--set-foreach (lambda (codepoint)
			(when (funcall test codepoint)
			  (cl-incf sum)))
		      set)
    sum))

(defun ubb--set-member (set codepoint)
  (cl-loop for (start . end) in (ubb--set-ranges set)
	   thereis (and (<= start codepoint) (< codepoint end))))

;; Return a list of ranges corresponding to set of codepoints in the
;; string STRING.
(defun ubb--string-ranges (string)
  (let* ((sorted (cl-sort (copy-sequence string) #'<))
	 (ranges '())
	 (start nil)
	 (end nil))
    (cl-loop for c across sorted do
	     (cond ((not start)
		    (setq start c)
		    (setq end (1+ c)))
		   ((= c (1- end)))
		   ((= c end) (setq end (1+ c)))
		   (t (push (cons start end) ranges)
		      (setq start c)
		      (setq end (1+ c)))))
    (when start (push (cons start end) ranges))
    (reverse ranges)))

;; A ubb--group is used to represent a set of ubb--sets.
(cl-defstruct (ubb--group (:constructor ubb--make-group)
			  (:copier nil) (:predicate nil))
  ;; NAME is a string for display purposes
  (name "" :type string :read-only t)
  ;; SETS%% is either a function (with no arguments) that should return a
  ;; sequence of ubb--sets that belong to this group, or the cached result
  ;; of calling the function.
  (sets%% (error "Arg missing") :type (or function sequence))
  ;; HEADER is a function which receives a set as argument and
  ;; should the value for `header-line-format'.
  (header (error "Arg missing") :type function :read-only t))

(defun ubb--group-sets (group)
  (let ((sets%% (ubb--group-sets%% group)))
    (cl-etypecase sets%%
      (sequence sets%%)
      (function (setf (ubb--group-sets%% group) (funcall sets%%))))))


;;; Unicode blocks

;; Local copy.
(defvar ubb--blocks-file-name
  (expand-file-name "../admin/unidata/Blocks.txt" data-directory))

;; Fallback if no local copy.
(defvar ubb--blocks-url "http://www.unicode.org/Public/UNIDATA/Blocks.txt")

;; Parse blocks in the format used by the Unicode Character Database.
(defun ubb--parse-blocks ()
  (goto-char (point-min))
  (let ((result '()))
    (while (re-search-forward
	    "^\\([0-9A-F]+\\)\\.\\.\\([0-9A-F]+\\);[ ]*\\([^ ].*\\)$"
	    nil t)
      (let* ((start (string-to-number (match-string 1) 16))
	     (end (1+ (string-to-number (match-string 2) 16)))
	     (name (match-string 3)))
	(push (ubb--make-set name (list (cons start end))) result)))
    (let ((r (cl-coerce (nreverse result) 'vector)))
      (cl-assert (let ((s (elt r 0)))
		   (and (equal (ubb--set-name s)  "Basic Latin")
			(equal (ubb--set-ranges s) '((0 . #x80))))))
      r)))

;; Load block information from a file or if the file isn't present
;; download it from unicode.org.
(defun ubb--load-blocks ()
  (with-temp-buffer
    (cond ((file-exists-p ubb--blocks-file-name)
	   (let ((coding-system-for-read 'binary))
	     (insert-file-contents-literally ubb--blocks-file-name)))
	  (t
	   (insert
	    (with-current-buffer (url-retrieve-synchronously ubb--blocks-url)
	      (re-search-forward "\n\n")
	      (buffer-substring (point) (point-max))))))
    (ubb--parse-blocks)))

(defvar ubb--all-blocks-cache nil)

(defun ubb--all-blocks ()
  "Return a sequence of all blocks."
  (or ubb--all-blocks-cache
      (setq ubb--all-blocks-cache (ubb--load-blocks))))

(defun ubb--find-block-by-codepoint (codepoint)
  (cl-find-if (lambda (block)
		(ubb--set-member block codepoint))
	      (ubb--all-blocks)))

(defun ubb--unicode-block-header (set)
  (cl-destructuring-bind ((start &rest end)) (ubb--set-ranges set)
    (format "Block: %s  %04X..%04X" (ubb--set-name set) start end)))

(defvar ubb--unicode-blocks-group
  (ubb--make-group :name "Unicode blocks"
		   :sets%% #'ubb--all-blocks
		   :header #'ubb--unicode-block-header))


;;; Charsets

;; NOTE: `map-charset-chars' can call the function with overlapping
;; ranges.  Also the cons cell for the range argument is updated so
;; it's a good idea to copy the contents instead of using the cons
;; cell.
(defun ubb--charset-ranges (charset)
  (let ((ranges '()))
    (map-charset-chars (lambda (from+to _)
			 (cl-destructuring-bind (from &rest to) from+to
			   (cl-assert (characterp from))
			   (cl-assert (characterp to))
			   (push (cons from (1+ to)) ranges)))
		       charset)
    ranges))

(defun ubb--charset-to-set (charset)
  (ubb--make-set (or (get-charset-property charset :long-name)
		     (get-charset-property charset :short-name)
		     (format "%s" charset))
		 (lambda () (ubb--charset-ranges charset))))

(defun ubb--charsets-without-aliases ()
  (reverse ; ascii first, please
   (cl-remove-duplicates charset-list :key #'charset-plist)))

(defun ubb--all-charsets ()
  (mapcar #'ubb--charset-to-set (ubb--charsets-without-aliases)))

(defun ubb--charset-header (set)
  (format "Charset: %s" (ubb--set-name set)))

(defvar ubb--charsets-group
  (ubb--make-group :name "Charsets"
		   :sets%% #'ubb--all-charsets
		   :header #'ubb--charset-header))


;;; Scripts

(defun ubb--all-scripts ()
  (let ((script2ranges (make-hash-table))
	(sets '()))
    (map-char-table (lambda (key script)
		      (setf (gethash script script2ranges)
			    (cons (cl-etypecase key
				    (character (cons key (1+ key)))
				    (cons (cons (car key) (1+ (cdr key)))))
				  (gethash script script2ranges))))
		    char-script-table)
    (maphash (lambda (script ranges)
	       (push (ubb--make-set (symbol-name script) ranges)
		     sets))
	     script2ranges)
    (cl-sort sets #'string< :key #'ubb--set-name)))

(defvar ubb--scripts-group
  (ubb--make-group :name "Scripts"
		   :sets%% #'ubb--all-scripts
		   :header (lambda (set)
			     (format "Script: %s" (ubb--set-name set)))))


;;; Unicode categories

(defun ubb--all-unicode-categories ()
  (let ((cat2ranges (make-hash-table))
	(sets '()))
    (map-char-table (lambda (key cat)
		      (setf (gethash cat cat2ranges)
			    (cons (cl-etypecase key
				    (character (cons key (1+ key)))
				    (cons (cons (car key) (1+ (cdr key)))))
				  (gethash cat cat2ranges))))
		    unicode-category-table)
    (maphash (lambda (cat ranges)
	       (let* ((desc (char-code-property-description
			     'general-category cat))
		      (name (format "%s (%s)" cat desc)))
		 (push (ubb--make-set name ranges)
		       sets)))
	     cat2ranges)
    (cl-sort sets #'string< :key #'ubb--set-name)))

(defvar ubb--unicode-categories-group
  (ubb--make-group :name "Unicode categories"
		   :sets%% #'ubb--all-unicode-categories
		   :header (lambda (set)
			     (format "Unicode general category: %s"
				     (ubb--set-name set)))))


;;; Languages

(defun ubb--all-languages ()
  (list (ubb--make-set "English" (ubb--string-ranges "“”‘’"))
	(ubb--make-set "French" (ubb--string-ranges "\
éàèùâêîôûëïüÿçœæ\
ÉÀÈÙÂÊÎÔÛËÏÜŸÇŒÆ\
«»‹›“”‘’€"))
	(ubb--make-set "German" (ubb--string-ranges "äöüßÄÖÜ„“‚‘’»«›‹€"))
	(ubb--make-set "Italian" (ubb--string-ranges "\
àèìòùéóî\
ÀÈÌÒÙÉÓÎ\
“”‘’«»‹›€"))
	(ubb--make-set "Spanish" (ubb--string-ranges "\
ñáéíóúü\
ÑÁÉÍÓÚÜ\
“”‘’«»¿¡"))
	(ubb--make-set "Turkish" (ubb--string-ranges "\
çşğıöü\
ÇŞĞİÖÜ\
“”‘’«»‹›"))
	(ubb--make-set "APL" (append (ubb--string-ranges "\
?⌈⌊⍴∼∣⍳⋆−+×÷,⌹○⍟⌽⊖⍋⍒⍎⍕⍉!−×÷⋆○?∈⌈⌊⍴↑↓⊥⊤∣,\/⍳⌹⌽⊖⍟⍕⍉!¨<≤=≥>≠∨∧⍱⍲/⌿\⍀.∘.")
				     `((,?⌶ . ,(1+ ?⍺)))))))

(defvar ubb--languages-group
  (ubb--make-group :name "Languages"
		   :sets%% #'ubb--all-languages
		   :header (lambda (set)
			     (format "Language: %s" (ubb--set-name set)))))



;;; Games (important)

(defun ubb--all-game-sets ()
  (list (ubb--make-set "Chess" (ubb--string-ranges "♚♛♜♝♞♟♙♘♗♔♖♕"))
	(ubb--make-set "Domino" `((#x1f030 . #x1f0a0)))
	(ubb--make-set "Poker" `(,@(ubb--string-ranges "♠♤♥♡♦♢♣♧")
				 (#x1f0a0 . #x1f100)))))

(defun ubb--game-header (set)
  (format "Game: %s" (ubb--set-name set)))

(defvar ubb--games-group
  (ubb--make-group :name "Games"
		   :sets%% #'ubb--all-game-sets
		   :header #'ubb--game-header))


;;; Groups

(defun ubb--all-groups ()
  (list ubb--unicode-blocks-group
	ubb--charsets-group
	ubb--scripts-group
	ubb--unicode-categories-group
	ubb--languages-group
	ubb--games-group))

(defvar ubb--read-group-name-history (list "Unicode blocks"))

(defun ubb--read-group-name (prompt)
  (let ((hist 'ubb--read-group-name-history)
	(completion-ignore-case t))
    (completing-read prompt
		     (mapcar #'ubb--group-name (ubb--all-groups))
		     nil t nil hist)))

(defun ubb--find-group-by-name (name)
  (cl-find name (ubb--all-groups) :key #'ubb--group-name :test #'equal))

(defvar ubb--read-set-name-history (list))

(defun ubb--read-set-name (group prompt)
  (let ((hist 'ubb--read-set-name-history)
	(completion-ignore-case t)
	(completion-styles (cl-adjoin 'substring completion-styles)))
    (completing-read prompt
		     (mapcar #'ubb--set-name (ubb--group-sets group))
		     nil t nil hist)))

(defun ubb--find-set-by-name (group name)
  (cl-find name (ubb--group-sets group)
	   :key #'ubb--set-name :test #'equal))


;;; Codepoint filter

(defvar ubb--categories-to-hide '(Cn Cs Co))

(defmacro ubb--define-hide-category-toggle (name category)
  `(defun ,name ()
     (interactive)
     (cond ((memq ',category ubb--categories-to-hide)
	    (setq ubb--categories-to-hide
		  (remove ',category ubb--categories-to-hide)))
	   (t
	    (push ',category ubb--categories-to-hide)))))

(ubb--define-hide-category-toggle ubb-toggle-hide-not-assigned-codepoints Cn)
(ubb--define-hide-category-toggle ubb-toggle-hide-surrogates Cs)
(ubb--define-hide-category-toggle ubb-toggle-hide-private-use-codepoints Co)

(defun ubb--hide-codepoint-p (codepoint)
  (and ubb--categories-to-hide
       (let ((cat (get-char-code-property codepoint 'general-category)))
	 (memq cat ubb--categories-to-hide))))


;;; Display

(defconst ubb--space " ")
(defconst ubb--thin-space (string #x2009))

;; Insert space around the string from START to END so that the region
;; occupies approximately 2*FONT-WIDTH pixels.  Inserting space is
;; generally a good idea to "neutralize" combining marks.
(defun ubb--insert-space (win start end font-width)
  (let ((pixel-width (car (window-text-pixel-size win start end))))
    (cond ((<= pixel-width font-width)
	   (save-excursion
	     (goto-char start)
	     (insert ubb--space)))
	  ((< pixel-width (* 2 font-width))
	   (save-excursion
	     (goto-char start)
	     (insert ubb--thin-space)))
	  (t
	   ;; give up
	   ))))

(defface ubb-invisible
  '((t :inherit tooltip))
  "Face used for code-points that would otherwise be invisible/transparent."
  :group 'ubb)

(defun ubb--propertize (string codepoint)
  (let ((s (propertize string 'codepoint codepoint
		       'help-echo #'ubb--help-echo)))
    (cond ((memq (get-char-code-property codepoint 'general-category)
		 '(Cf Zs Zl Zp))
	   (propertize s 'face 'ubb-invisible))
	  (t s))))

(defun ubb--insert-codepoint (win codepoint font-width)
  (let* ((s (cl-case codepoint
	      (?\n "^J")
	      (t (string codepoint))))
	 (s (ubb--propertize s codepoint))
	 (start (point))
	 (_ (insert s))
	 (end (point)))
    (ubb--insert-space win start end font-width)))

;; FIXME: only for compatibility with Emacs 24
(defun ubb--default-font-width ()
  (cond ((fboundp 'default-font-width)
	 (default-font-width))
	(t
	 (frame-char-width))))

(defvar ubb--right-margin 10)
(defvar ubb--left-margin 1)

;; Insert a set of codepoints, trying to create lines of equal width.
;;
;; The window WIN is needed for pixel measuring functions.  Inserting
;; large sets can be slow, so this calls redisplay for every line to
;; give some visual feedback to the user.  Also, the progress is shown
;; in percent in the echo area.
(defun ubb--insert-set (win set)
  (let* ((font-width (ubb--default-font-width))
	 (right-limit (- (window-width win t)
			 (* ubb--right-margin font-width)))
	 (line-start (point))
	 (count (ubb--set-count set (lambda (c)
				      (not (ubb--hide-codepoint-p c)))))
	 (i 0))
    (insert-char ?\s ubb--left-margin)
    (ubb--set-foreach
     (lambda (codepoint)
       (unless (ubb--hide-codepoint-p codepoint)
	 (ubb--insert-codepoint win codepoint font-width)
	 (cl-incf i)
	 (let ((w (car (window-text-pixel-size win line-start (point)))))
	   (when (<= right-limit w)
	     (insert "\n")
	     (setq line-start (point))
	     (insert-char ?\s ubb--left-margin)
	     (message "%.f%%" (* 100.0 (/ (float i) count)))
	     (redisplay)))))
     set)
    (message nil)))

(defun ubb--clear-codepoint-info ()
  (message nil))

;; Return a short description for codepoint.  This basically the
;; Unicode name.
(defun ubb--short-description (codepoint)
  (let* ((name (get-char-code-property codepoint 'name))
	 (old (get-char-code-property codepoint 'old-name))
	 (cat (get-char-code-property codepoint 'general-category))
	 (catdesc (char-code-property-description 'general-category cat)))
    (format "\"%c\" %s (%s: %s)" codepoint (or name old "[no name]")
	     cat catdesc)))

(defun ubb--show-codepoint-info (codepoint)
  (message "%s" (ubb--short-description codepoint)))

(defun ubb--current-codepoint (&optional noerror)
  (let ((codepoint (get-text-property (point) 'codepoint)))
    (cond (codepoint)
	  (noerror nil)
	  (t (user-error "No code-point selected")))))

;; This is called from post-command-hook.
(defun ubb--codepoint-sensor ()
  (unless (current-message)
    (let ((codepoint (ubb--current-codepoint t)))
      (cond (codepoint (ubb--show-codepoint-info codepoint))
	    (t (ubb--clear-codepoint-info))))))

;; This called if the mouse pointer hovers around.
(defun ubb--help-echo (_ __ pos)
  (let ((codepoint (get-text-property pos 'codepoint)))
    (if codepoint (ubb--short-description codepoint))))

(defvar ubb--buffer-set)   ; The currently displayed ubb--set
(defvar ubb--buffer-group) ; The group to which ubb--buffer-set belongs

(define-derived-mode ubb-mode fundamental-mode "ubb"
  "Mode for viewing the characters in Unicode blocks and other charsets."
  (setq-local bidi-display-reordering nil)
  (setq-local truncate-lines t)
  (read-only-mode 1)
  (add-hook 'post-command-hook 'ubb--codepoint-sensor nil t))

(defun ubb--buffer-name () "*ubb*")

(defun ubb--get-buffer ()
  (or (get-buffer (ubb--buffer-name))
      (with-current-buffer (get-buffer-create (ubb--buffer-name))
	(ubb-mode)
	(current-buffer))))

(defun ubb--display-set (set)
  (let* ((inhibit-read-only t))
    (erase-buffer)
    (let* ((block-win (display-buffer (current-buffer)))
	   (_ (select-window block-win))
	   (pos (point)))
      (insert "\n")
      (ubb--insert-set block-win set)
      (set-window-point block-win pos)
      (current-buffer))))

;; This is the main entry point.
;;
;; Insert and display the ubb--set SET belonging to ubb--group GROUP.
;; Create a fresh buffer only if needed.  Display the buffer and
;; select its window.  Finally return the buffer.
(defun ubb--browse-set (group set)
  (with-current-buffer (ubb--get-buffer)
    (setq-local ubb--buffer-group group)
    (setq-local ubb--buffer-set set)
    (setq header-line-format (funcall (ubb--group-header group) set))
    (ubb--display-set set)))


;;; Commands

(defun ubb-describe-codepoint-briefly ()
  "Show name and category of the current code-point."
  (interactive)
  (ubb--show-codepoint-info (ubb--current-codepoint)))

(defun ubb-describe-codepoint ()
  "Describe the current code-point."
  (interactive)
  (when (ubb--current-codepoint)
    (describe-char (point))))

(defun ubb--next-set (next)
  (let* ((set ubb--buffer-set)
	 (group ubb--buffer-group)
	 (all (ubb--group-sets group))
	 (i (cl-position set all :test #'equal))
	 (j (funcall next i)))
    (cond ((and (<= 0 j) (< j (length all)))
	   (ubb--browse-set group (elt all j)))
	  (t
	   (user-error "No more sets (in group %S)"
		       (ubb--group-name group))))))

(defun ubb-next-set ()
  "Browse the next set of the group."
  (interactive)
  (ubb--next-set #'1+))

(defun ubb-prev-set ()
  "Browse the previous set of the group."
  (interactive)
  (ubb--next-set #'1-))

(defun ubb-select-set-by-name (name)
  "Select the set to browse by name."
  (interactive (list (ubb--read-set-name ubb--buffer-group "Set name: ")))
  (let ((set (or (ubb--find-set-by-name ubb--buffer-group name)
		 (user-error "No set with name: %S" name))))
    (ubb--browse-set ubb--buffer-group set)))

(defun ubb--search-property (prop &optional backward)
  "Search the next text range where PROP is non-nil.
Return the value of PROP.
If BACKWARD is non-nil, search backward."
  (let ((next (cond (backward #'previous-single-char-property-change)
		    (t #'next-single-char-property-change)))
        (start (point))
        (value nil))
    (while (progn
             (goto-char (funcall next (point) prop))
             (not (or (setq value (get-char-property (point) prop))
                      (eobp)
                      (bobp)))))
    (cond (value)
          (t (goto-char start) nil))))

(defun ubb-forward-codepoint ()
  "Move cursor to the next code-point."
  (interactive)
  (or (ubb--search-property 'codepoint nil)
      (user-error "No more codepoints")))

(defun ubb-backward-codepoint ()
  "Move cursor to the previous code-point."
  (interactive)
  (or (ubb--search-property 'codepoint t)
      (user-error "No more codepoints")))

(defun ubb-browse-block (block &optional codepoint)
  "Browse the Unicode block BLOCK.
Interactively without prefix arg, prompt for the block name.
With negative prefix arg, use the character at point to find
the corresponding block.
With positive positive arg, prompt for the name or number of the code-point
\(see `read-char-by-name')."
  (interactive
   (cond ((not current-prefix-arg)
	  (let ((name (ubb--read-set-name ubb--unicode-blocks-group
					  "Block name: ")))
	    (list (or (ubb--find-set-by-name ubb--unicode-blocks-group name)
		      (user-error "No block with named: %S" name)))))
	 (t
	  (let* ((codepoint
		  (cond ((< (prefix-numeric-value current-prefix-arg) 0)
			 (char-after))
			(t
			 (read-char-by-name
			  "Code-point (Unicode name or hex): "))))
		 (block (or (ubb--find-block-by-codepoint codepoint)
			    (user-error "No block for code-point: %X"
					codepoint))))
	    (list block codepoint)))))
  (with-current-buffer (ubb--browse-set ubb--unicode-blocks-group block)
    (when codepoint
      (search-forward (string codepoint))
      (backward-char))))

(defun ubb-browse-block-by-codepoint ()
  "Very similar to `ubb-browse-block'.
The only difference is that when invoked without prefix arg,
prompt for the codepoint instead for the Unicode block."
  (interactive)
  (let ((current-prefix-arg (or current-prefix-arg '(4))))
    (call-interactively #'ubb-browse-block)))

(defun ubb--browse-group (group)
  (ubb--browse-set group (elt (ubb--group-sets group) 0)))

(defun ubb-browse-group-by-name (name)
  "Prompt for a group name and display the first set in the group."
  (interactive (list (ubb--read-group-name "Group name: ")))
  (let ((group (or (ubb--find-group-by-name name)
		   (user-error "No group with name: %S" name))))
    (ubb--browse-group group)))

(defun ubb-reset-text-scale ()
  (interactive)
  (text-scale-set 0))

(defun ubb-redraw ()
  "Redraw the current set."
  (interactive)
  (ubb--display-set ubb--buffer-set))

(defun ubb-browse ()
  "Start the character browser."
  (interactive)
  (let ((buffer (get-buffer (ubb--buffer-name))))
    (cond (buffer (with-current-buffer buffer
		    (ubb--browse-set ubb--buffer-group ubb--buffer-set)))
	  (t (ubb--browse-group ubb--unicode-blocks-group)))))

(defun ubb-quit ()
  "Close the UBB window."
  (interactive)
  (quit-restore-window nil 'bury))


;;; Menu

;; Return an uninterned symbol with FUN set as it function.  This is a
;; trick to put closures into menus.
(defun ubb--fake-menu-symbol (fun)
  (let ((sym (make-symbol "fake-menu-filter-symbol")))
    (fset sym fun)
    sym))

(defun ubb--build-set-menu (group)
  (mapcar (lambda (set)
	    (vector (ubb--set-name set)
		    (ubb--fake-menu-symbol
		     (lambda ()
		       (interactive)
		       (ubb--browse-set group set)))))
	  (ubb--group-sets group)))

(defun ubb--set-menu-filter (_others)
  (ubb--build-set-menu ubb--buffer-group))

(defun ubb--build-group-menu ()
  (mapcar (lambda (group)
	    (list (ubb--group-name group)
		  :filter (ubb--fake-menu-symbol
			   (lambda (_) (ubb--build-set-menu group)))))
	  (ubb--all-groups)))

(easy-menu-define nil ubb-mode-map
  "Menu for UBB mode."
  `("Character-Browser"
    ,@(ubb--build-group-menu)
    "--"
    ["Select group by name" ubb-browse-group-by-name]
    ["Select set by name" ubb-select-set-by-name]
    ["Select Unicode block by code-point" ubb-browse-block-by-codepoint]
    ("Select set in current group"
     ("Sets in current group" :filter ubb--set-menu-filter)
     ["Next set in group" ubb-next-set :key-sequence ">"]
     ["Previous set in group" ubb-prev-set :key-sequence "<"])
    "--"
    ["Describe code-point briefly" ubb-describe-codepoint-briefly]
    ["Show code-point details" ubb-describe-codepoint]
    ("Movement"
     ["Move to next code-point" ubb-forward-codepoint]
     ["Move to previous code-point" ubb-backward-codepoint])
    ("Zoom"
     ["Increase scale factor" text-scale-increase]
     ["Decrease scale factor" text-scale-decrease]
     ["Reset scale factor" ubb-reset-text-scale])
    ("Options"
     ["Hide not assigned code-points" ubb-toggle-hide-not-assigned-codepoints
      :style toggle :selected (memq 'Cn ubb--categories-to-hide)]
     ["Hide surrogates" ubb-toggle-hide-surrogates
      :style toggle :selected (memq 'Cs ubb--categories-to-hide)]
     ["Hide code-points within private-use areas"
      ubb-toggle-hide-private-use-codepoints
      :style toggle :selected (memq 'Co ubb--categories-to-hide)])
    "--"
    ["Redraw" ubb-redraw]
    ["Quit" ubb-quit]))


;;; Key bindings

(define-key ubb-mode-map (kbd "<SPC>") #'ubb-describe-codepoint-briefly)
(define-key ubb-mode-map (kbd "D") #'ubb-describe-codepoint)
(define-key ubb-mode-map (kbd "f") #'ubb-forward-codepoint)
(define-key ubb-mode-map (kbd "b") #'ubb-backward-codepoint)
(define-key ubb-mode-map (kbd "n") #'ubb-next-set)
(define-key ubb-mode-map (kbd "p") #'ubb-prev-set)
(define-key ubb-mode-map (kbd ">") #'ubb-next-set)
(define-key ubb-mode-map (kbd "<") #'ubb-prev-set)
(define-key ubb-mode-map (kbd "N") #'ubb-select-set-by-name)
(define-key ubb-mode-map (kbd "G") #'ubb-browse-group-by-name)
(define-key ubb-mode-map (kbd "C") #'ubb-browse-block-by-codepoint)
(define-key ubb-mode-map (kbd "+") #'text-scale-increase)
(define-key ubb-mode-map (kbd "-") #'text-scale-decrease)
(define-key ubb-mode-map (kbd "*") #'ubb-reset-text-scale)
(define-key ubb-mode-map (kbd "g") #'ubb-redraw)
(define-key ubb-mode-map (kbd "q") #'ubb-quit)

(provide 'ubb)
