;;;; timecard.el --- keep track of time spent on tasks
;;;; Jim Blandy <jimb@red-bean.com> --- December 2002

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(require 'cl)

(defun timecard-mode ()
  "A major mode for keeping track of where the time goes.
\\<timecard-mode-map>
A timecard is an ordinary text buffer with time frobs in it.  It
might look like this:

  [2:25:05] public patch review
  [1:12:29] meetings
  [3:55:58] separate debug info patch

Each frob indicates the amount of time the user has spent on a given
task, in hours, minutes, and seconds.  A buffer may have one frob that
is `active', meaning that the user is currently doing that task.  The
currently active frob, if any, is shown in red.

The command '\\[timecard-activate]' makes the last frob at or before point in the current
buffer as active; if that frob is already active, then it deactivates it, and no
frob is active. Deactivating a frob updates its total time.

You can also include estimates, in the form '(HH:MM)' or '(:MM)'.
If you like, you can mark an estimate 'done', like this: '(03:00 done)'.

The command '\\[timecard-total-region]' displays totals for frobs and estimates
in the current region; the command '\\[timecard-total-page]' does the same for
the current page.  Estimates marked 'done' are totalled up
separately.  (More generally, estimates are grouped by whatever
appears in the parenthesis after the time.)

Here is the complete list of key bindings for this mode:
\\{timecard-mode-map}"
  (interactive)

  (kill-all-local-variables)
  (setq major-mode 'timecard-mode)
  (setq mode-name "Timecard")
  (use-local-map timecard-mode-map)

  ;; Mark up any active frobs in the file.
  (timecard-mark-up-active-frobs)

  (run-hooks 'timecard-mode-hooks))

(defvar timecard-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-c" 'timecard-activate)
    (define-key map "\C-c\C-i" 'timecard-insert-frob)
    (define-key map "\C-c\C-t" 'timecard-total-region)
    (define-key map "\C-c\C-p" 'timecard-total-page)
    (define-key map "\C-c\C-h" 'timecard-total-headline)
    (define-key map [M-tab] 'timecard-complete-task-name)
    map)
  "Keymap for timecard-code.")


;;; The timecard frob structure.  The real data is always stored in
;;; the buffer, not in some structure; these are just temporary things
;;; we pass around while working with a frob so we don't need to keep
;;; re-parsing it.
(defstruct timecard-frob

  ;; SECONDS is the number of seconds attributed to the frob, as a
  ;; floating-point number.
  seconds 

  ;; START is nil for an inactive frob; for the active frob, it is the
  ;; time at which the frob last became active, as a floating-point
  ;; number.
  start

  ;; FROB-REGION is a pair (START . END), giving the starting and ending
  ;; positions of the frob in the buffer (as integers, not markers ---
  ;; use this information quickly!).
  frob-region

  ;; START-REGION is a pair (START . END), giving the starting and
  ;; ending positions of the "start" portion of the frob (as integers,
  ;; not markers --- use this information quickly!), or nil if START
  ;; is nil.
  start-region)

(defun timecard-new-frob ()
  "Return a new, inactive frob, with no time accumulated, at point."
  (make-timecard-frob
   :seconds 0
   :start nil
   :frob-region (cons (point) (point))))

(defconst timecard-frob-regexp
  "\\[\\([0-9]+\\):\\([0-9]+\\):\\([0-9]+\\)\\( [0-9]+\\)?\\]"
  "A regexp matching a time frob.")

(defconst timecard-estimate-regexp
  "(\\([0-9]*\\):\\([0-9]+\\);? ?\\([^)]+\\)?)"
  "A regexp matching a parenthesized estimate.")

(defun timecard-parse-matched-frob ()
  "Parse the frob we just matched, and return the information it contains.
See `timecard-parse-frob' for details."
  (let ((hours (match-string 1))
        (minutes (match-string 2))
        (seconds (match-string 3))
        (start (match-string 4)))
    (make-timecard-frob 
     :seconds (+ (string-to-number seconds)
                 (* 60 (+ (string-to-number minutes)
                          (* 60 (string-to-number hours)))))
     :start (and start (string-to-number start))
     :frob-region (cons (match-beginning 0) (match-end 0))
     :start-region (and start (cons (match-beginning 4)
                                    (match-end 4))))))

(defun timecard-parse-frob ()
  "Parse the frob at point, and return the information it contains.
The return value is a 'timecard-frob' structure."
  (if (not (looking-at timecard-frob-regexp))
      (error "Misformed timecard frob."))
  (timecard-parse-matched-frob))

;;; We set the 'category' property on frobs' text to this
;;; symbol, so its properties determine how that text gets displayed.
(put 'timecard-frob-active 'face '(foreground-color . "red"))
(put 'timecard-frob-active 'rear-nonsticky '(category))

;;; We set the 'category' property on the active frob's start time
;;; text to this symbol, so its properties determine how that text
;;; gets displayed.
(put 'timecard-frob-start 'invisible t)
(put 'timecard-frob-start 'intangible t)
;(put 'timecard-frob-start 'face '(foreground-color . "blue"))
(put 'timecard-frob-start 'rear-nonsticky '(category))

(defun timecard-put-text-property (region prop value)
  "Set the text property PROP to VALUE on REGION in the current buffer.
REGION is a pair (START . END)."
  (put-text-property (car region) (cdr region) prop value))

(defun timecard-format-time (seconds)
  "Return a string representing the duration of SECONDS seconds, readably."
  (let* ((seconds (floor seconds))
         (seconds-only (mod seconds 60))
         (minutes (floor (/ seconds 60)))
         (minutes-only (mod minutes 60))
         (hours (floor (/ minutes 60))))
    (format "%d:%02d:%02d" hours minutes-only seconds-only)))

(defun timecard-rewrite-frob (frob)
  "Rewrite FROB as appropriate for its current SECONDS value."
  (save-excursion 
    (let ((seconds (timecard-frob-seconds frob))
          (start (timecard-frob-start frob))
          (frob-region (timecard-frob-frob-region frob))
          frob-start frob-end
          start-start start-end)        ;sorry
      (goto-char (car frob-region))
      (delete-region (point) (cdr frob-region))
      (setq frob-start (point))

      ;; We use insert-before-markers here so that the save-excursion
      ;; will restore point to sit after the frob we just processed.
      (insert-before-markers (format "[%s" (timecard-format-time seconds)))
      (setq start-start (point))
      (if start (insert-before-markers (format " %.0f" start)))
      (setq start-end (point))
      (insert-before-markers "]")
      (setq frob-end (point))
      (setf (timecard-frob-frob-region frob) (cons frob-start frob-end))
      (if start
          (progn
            (put-text-property frob-start frob-end
                               'category 'timecard-frob-active)
            (put-text-property start-start start-end 
                               'category 'timecard-frob-start)
            (setf (timecard-frob-start-region frob) 
                  (cons start-start start-end)))))))

(defun timecard-current-time ()
  "Return the current time, as a floating-point number."
  (let ((time (current-time)))
    (+ (* (float (elt time 0)) 65536) (elt time 1))))

(defun timecard-activate-frob (frob)
  "Make frob active, starting at the current time."
  (if (not (timecard-frob-start frob))
      (progn
        (setf (timecard-frob-start frob) (timecard-current-time))
        (timecard-rewrite-frob frob))))

(defun timecard-deactivate-frob (frob)
  "If FROB is active, add accumulated time into its total, and deactivate it."
  (if (timecard-frob-start frob)
      (progn
        (incf (timecard-frob-seconds frob)
              (- (timecard-current-time) (timecard-frob-start frob)))
        (setf (timecard-frob-start frob) nil)
        (timecard-rewrite-frob frob))))

(defmacro while: (var cond &rest body)
  "Like while, but bind VAR to the condition's value while evaluating BODY..."
  `(let (,var)
     (while (setq ,var ,cond)
       ,@body)))
(put 'while: 'lisp-indent-function 2)

(defun timecard-mark-up-active-frobs ()
  "Apply appropriate markup to any active frobs in the current buffer."
  (save-excursion
    (let ((modp (buffer-modified-p))
          (buffer-undo-list t))
      (unwind-protect
          (progn
            (goto-char (point-min))
            (while (re-search-forward timecard-frob-regexp nil t)
              (let ((frob (timecard-parse-matched-frob)))
                (if (timecard-frob-start frob)
                    (timecard-rewrite-frob frob)))))
        (set-buffer-modified-p modp)))))

(defun timecard-deactivate-all-frobs ()
  "Deactivate all frobs in the current buffer."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward timecard-frob-regexp nil t)
      (let ((frob (timecard-parse-matched-frob)))
        (if (timecard-frob-start frob)
            (timecard-deactivate-frob frob))))))

(defun timecard-active-frob ()
  "Return the first active frob in the buffer, or nil if there is none."
  (let ((pos (text-property-any (point-min) (point-max)
                                'category 'timecard-frob-active)))
    (and pos
         (save-excursion
           (goto-char pos)
           (timecard-parse-frob)))))
                   
(defun timecard-frob-at-point ()
  "Return the frob at or before point.
Raise an error if there is none."
  (save-excursion
    ;; This is not graceful, but the behavior of re-search-backward
    ;; means that we can't use it to search backward if point might
    ;; actually be *in* a frob --- a possibility we want to allow for.
    (let (frob)
      (while (not frob)
        (cond
         ((looking-at timecard-frob-regexp)
          (setq frob (timecard-parse-matched-frob)))
         ((not (search-backward "[" nil t))
          (error "There is no time frob at or before point."))))
      frob)))

(defun timecard-activate (arg)
  "Activate/deactivate the time frob at or before point.
Activation is normally exclusive: when you make a frob active, all
other frobs become inactive.  With a prefix, activation is not
exclusive: any other active frobs remain active."
  (interactive "*P")
  (let ((this-frob (timecard-frob-at-point)))
    (if (timecard-frob-start this-frob)

        ;; This frob is active, so de-activate it.
        (timecard-deactivate-frob this-frob)

      ;; We're activating this frob.
      ;; If there are any others active, deactivate them --- unless
      ;; the user told us to leave them alone.
      (or arg
          (progn
            (timecard-deactivate-all-frobs)
            ;; Since frobs use integers for their regions, we need to
            ;; re-parse it.
            (setq this-frob (timecard-frob-at-point))))
      (timecard-activate-frob this-frob))))

(defun timecard-insert-frob ()
  (interactive)
  (let ((new-frob (timecard-new-frob)))
    (timecard-rewrite-frob new-frob)
    (insert " ")))

(defun timecard-parse-matched-estimate ()
  "Parse the estimate just matched, and return its length and category.
The return value has the form (CATEGORY . LENGTH).
The category of an estimate is whatever appears in the parens after
the time.  If the estimate has no category, we return 'nil'."
  (let ((hours (match-string 1))
        (minutes (match-string 2))
        (category (match-string 3)))
    (cons (if (stringp category) (intern category) nil)
          (* 60
             (+ (string-to-number minutes)
                (* 60 (string-to-number hours)))))))

(defun timecard-alist-add (key inc alist)
  "Add INC to the number bound to KEY in ALIST, extending ALIST if needed.
Return the new alist."
  (let ((entry (assq key alist)))
    (if entry (progn
                (incf (cdr entry) inc)
                alist)
      (acons key inc alist))))

(defun timecard-compute-total-region (start end)
  "Return the total time for all frobs and estimates between START and END.
The return value is a list of the form (FROBS ESTIMATES), where
FROBS is the total of all frobs in seconds, and ESTIMATES is an
alist of estimate totals, whose keys are symbols."
  (let ((total 0)
        total-estimated)
    (save-excursion
      (goto-char start)
      (while (re-search-forward timecard-frob-regexp end t)
        (let* ((frob (timecard-parse-matched-frob))
               (seconds (timecard-frob-seconds frob))
               (start (timecard-frob-start frob))
               (region (timecard-frob-frob-region frob)))
          (incf total (timecard-frob-seconds frob))
          (if start
              (incf total (- (timecard-current-time) start)))
          (goto-char (cdr region))))
      
      ;; Total up times appearing in parens.  
      (goto-char start)
      (while (re-search-forward timecard-estimate-regexp end t)
        (let ((category-time (timecard-parse-matched-estimate)))
          (setq total-estimated
                (timecard-alist-add (car category-time) (cdr category-time)
                                    total-estimated)))))

    (list total total-estimated)))

(defun timecard-format-totals (totals)
  "Format a totals list nicely for display to the user.
TOTALS should have the form (FROBS ESTIMATES)."
  (let ((total-frobs (car totals))
        (total-estimated (cadr totals)))
    (format "%s%s"
            (timecard-format-time total-frobs)
            (if total-estimated
                (format " (%s)"
                        (mapconcat (lambda (entry)
                                     (let ((cat (car entry))
                                           (time (cdr entry)))
                                       (format "%s estimated%s"
                                               (timecard-format-time time)
                                               (if cat (format " %s" cat)
                                                 ""))))
                                   total-estimated
                                   "; "))
              ""))))

(defun timecard-total-region (start end)
  "Display the total time for all frobs and estimates between mark and point."
  (interactive "r")
  (message "Total time in region: %s"
           (timecard-format-totals
            (timecard-compute-total-region start end))))

(defun timecard-total-page ()
  "Display the total time for all frobs on the current page."
  (interactive)
  (save-excursion
    (forward-page)
    (let ((end (point)))
      (forward-page -1)
      (message "Total time in page: %s"
               (timecard-format-totals
                (timecard-compute-total-region (point) end))))))

(defun timecard-total-headline ()
  "Display the total time for all frobs under the current Org-style headline.
An Org-style headline is any line starting with one or more '*' characters."
  (interactive)
  (save-excursion
    (re-search-backward "^\\*+ .*$" nil 'move-to-limit)
    (let ((start (point))
          (headline (match-string-no-properties 0)))
      (forward-line 1)
      (re-search-forward "^\\*+ " nil 'move-to-limit)
      (message "Total time under '%s': %s"
               headline
               (timecard-format-totals
                (timecard-compute-total-region start (point)))))))

(defun timecard-complete-task-name ()
  "Complete the name of the task before point."
  (interactive)
  ...)

                 
(provide 'timecard)
