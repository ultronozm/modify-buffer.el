;;; modify-buffer.el --- Apply lossy diffs to buffers  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Paul D. Nelson

;; Author: Paul D. Nelson <ultrono@gmail.com>
;; Version: 0.1
;; Package-Requires: ((emacs "28.1"))
;; Keywords: tools, diff, llm
;; URL: https://github.com/username/modify-buffer
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This package provides functions to apply unified diff format changes
;; to Emacs buffers, designed especially for LLM tool integration.
;;
;; It offers several implementations:
;; - modify-buffer-apply-diff-basic: A simple implementation
;; - modify-buffer-apply-diff-patch: Uses the external 'patch' command
;; - modify-buffer-apply-diff-improved: A robust implementation with better
;;   whitespace and context handling
;;
;; See the README for integration with some llm packages.
;;
;;; Code:

;;; Customization and Variables

(require 'cl-lib)

(defcustom modify-buffer-debug-buffer "*modify-buffer-debug*"
  "Name of buffer for logging modify-buffer debug information."
  :type 'string
  :group 'modify-buffer)

(defcustom modify-buffer-debug-enabled t
  "Whether to log modify-buffer tool calls to the debug buffer."
  :type 'boolean
  :group 'modify-buffer)

(defcustom modify-buffer-default-implementation #'modify-buffer-apply-diff-improved
  "The default implementation for applying diffs to buffers."
  :type 'function
  :options '(modify-buffer-apply-diff-basic
             modify-buffer-apply-diff-patch
             modify-buffer-apply-diff-improved)
  :group 'modify-buffer)

;;; Logging

(defun modify-buffer-clear-debug-log ()
  "Clear the modify-buffer debug log buffer."
  (interactive)
  (when (get-buffer modify-buffer-debug-buffer)
    (with-current-buffer modify-buffer-debug-buffer
      (erase-buffer)
      (message "Cleared modify-buffer debug log")))
  (error "Buffer %s not found" buffer))

(defun modify-buffer-toggle-debug ()
  "Toggle modify-buffer debug logging."
  (interactive)
  (setq modify-buffer-debug-enabled (not modify-buffer-debug-enabled))
  (message "Modify-buffer debug logging %s"
           (if modify-buffer-debug-enabled "enabled" "disabled")))

(defun modify-buffer-log-call (buffer diff)
  "Log a modify-buffer tool call to the debug buffer.
BUFFER and DIFF are the arguments to be logged."
  (when modify-buffer-debug-enabled
    (with-current-buffer (get-buffer-create modify-buffer-debug-buffer)
      (goto-char (point-max))
      (unless (bolp) (insert "\n"))
      (insert "=== New modify-buffer call ===\n"
              (format-time-string "[%Y-%m-%d %H:%M:%S]\n"))
      (insert "Buffer: " buffer "\n"
              "Diff:\n" diff "\n"
              "=== End of call ===\n\n")
      (goto-char (point-max)))))

;;; Basic Implementation

(defun modify-buffer-apply-diff-basic (buffer diff)
  "Apply unified format DIFF to BUFFER."
  (message "Applying diff %s to buffer %s" diff buffer)
  (if-let ((buf (get-buffer buffer)))
      (with-current-buffer buf
        ;; Find the first @@ and ignore everything before it
        (if-let ((first-hunk-pos (string-match "^@@ .*@@\n" diff)))
            (dolist (hunk (split-string (substring diff first-hunk-pos) "^@@ .*@@\n" t))
              (let (before after)
                (dolist (line (split-string hunk "\n" t))
                  (cond
                   ((string-prefix-p " " line)
                    (push (substring line 1) before)
                    (push (substring line 1) after))
                   ((string-prefix-p "-" line)
                    (push (substring line 1) before))
                   ((string-prefix-p "+" line)
                    (push (substring line 1) after))))
                (setq before (string-join (nreverse before) "\n")
                      after (string-join (nreverse after) "\n"))
                (goto-char (point-min))
                (if (search-forward before nil t)
                    (replace-match after t t)
                  (message "Hunk not found in buffer %s" buffer))))
          (message "No hunks found in diff"))
        (format "Applied changes to buffer %s" buffer))
    (error "Buffer %s not found" buffer))
  "")

;;;###autoload
(defun modify-buffer-apply-diff (buffer diff)
  "Apply unified format DIFF to BUFFER using the default implementation.

BUFFER should be the name of an existing buffer.
DIFF should be a unified diff format string with @@ hunks and +/- lines."
  (funcall modify-buffer-default-implementation buffer diff))

;;; Patch-based Implementation

(defun modify-buffer-apply-diff-patch (buffer diff)
  "Apply unified format DIFF to BUFFER using external patch tool."
  (unless (string-match "\n\\'" diff)
    (setq diff (concat diff "\n")))
  ;; Fix malformed @@ headers by ensuring they start with @@<space>-
  (setq diff (replace-regexp-in-string
              "^@@ \\([0-9]\\)"
              "@@ -\\1"
              diff))
  (message "Applying diff %s to buffer %s" diff buffer)
  (if-let ((buf (get-buffer buffer)))
      (with-current-buffer buf
        (if (not (string-match "^@@ .*@@\n" diff))
            (progn
              (message "No hunks found in diff")
              (format "No changes applied to buffer %s" buffer))
          (let ((temp-file (make-temp-file "emacs-diff-"))
                (diff-file (make-temp-file "emacs-diff-"))
                (inhibit-message t))
            (unwind-protect
                (progn
                  (write-region (point-min) (point-max) temp-file nil 'quiet)
                  (with-temp-file diff-file
                    (insert diff))
                  (if (= 0 (call-process "patch" nil nil nil
                                         "--ignore-whitespace"
                                         "--forward"
                                         "--quiet"
                                         "--force"
                                         "--reject-file=-"
                                         temp-file
                                         diff-file))
                      (progn
                        (erase-buffer)
                        (insert-file-contents temp-file)
                        (format "Successfully applied changes to buffer %s" buffer))
                    (error "Failed to apply patch to buffer %s" buffer)))
              (when (file-exists-p temp-file)
                (delete-file temp-file))
              (when (file-exists-p diff-file)
                (delete-file diff-file))))))
    (error "Buffer %s not found" buffer))
  "")

;;; Attempt at an improved version, work in progress

(defun modify-buffer-apply-diff-improved (buffer diff)
  "Apply a unified format DIFF to BUFFER with robust, flexible matching.

BUFFER should be the name of an existing buffer.
DIFF should be a unified diff format string with @@ hunks and +/- lines.

Uses line-based, whitespace-flexible matching.  Ignores @@ line numbers.
Tolerates different counts of blank lines (including ^L) and matches
them flexibly (multiple blank lines in diff can match fewer blank lines
in buffer)."
  (unless (stringp buffer)
    (error "Argument BUFFER must be the name of an existing buffer (string)"))
  (unless (stringp diff)
    (error "Argument DIFF must be a string"))
  (let* ((buf (get-buffer buffer))
         (num-failed 0)
         (fail-messages nil)
         (num-applied 0))
    (unless buf
      (error "Buffer %s not found" buffer))
    (with-current-buffer buf
      (save-excursion
        (goto-char (point-min))
        (let ((hunks (modify-buffer--parse-diff-hunks diff)))
          (dolist (hunk hunks)
            (condition-case err
                (when (modify-buffer--apply-hunk hunk)
                  (cl-incf num-applied))
              (error
               (cl-incf num-failed)
               (push (format "%s" (error-message-string err)) fail-messages))))))
      (if (> num-failed 0)
          (error "Failed to apply %d hunks:\n%s"
                 num-failed
                 (mapconcat #'identity (nreverse fail-messages) "\n"))
        (format "Successfully applied %d hunks to buffer %s"
                num-applied buffer)))))


;;; 1) Parsing the raw DIFF into structured hunks

(defun modify-buffer--parse-diff-hunks (diff-text)
  "Parse DIFF-TEXT into a list of hunks.
Each hunk is a list of (TYPE . TEXT), where TYPE ∈ {context, remove, add}.

We ignore lines until we see `@@`.  Then each subsequent line is part of
the hunk until the next `@@` or end-of-file:

 - lines starting with '-' become (remove . rest)
 - lines starting with '+' become (add . rest)
 - lines starting with ' ' become (context . rest)
 - empty lines become (context . \"\").
 - all else is ignored (like '+++' or '---' file headers)."
  (let (hunks current-lines in-hunk)
    (with-temp-buffer
      (insert diff-text)
      (goto-char (point-min))

      (while (not (eobp))
        (let ((line (buffer-substring-no-properties
                     (line-beginning-position) (line-end-position))))
          (cond
           ((string-match "^@@" line)
            ;; Start of a new hunk
            (when current-lines
              (push (reverse current-lines) hunks))
            (setq current-lines nil
                  in-hunk t))

           (in-hunk
            ;; We're in a hunk; see if it's +/-/context or blank
            (cond
             ((string-prefix-p "-" line)
              (push (cons 'remove (substring line 1)) current-lines))
             ((string-prefix-p "+" line)
              (push (cons 'add (substring line 1)) current-lines))
             ((string-prefix-p " " line)
              (push (cons 'context (substring line 1)) current-lines))
             ((string-empty-p line)
              ;; truly empty => treat as context
              (push (cons 'context "") current-lines))
             ;; else not recognized => ignore
             )))

          (forward-line 1)))

      ;; End of file => push the last hunk if present
      (when current-lines
        (push (reverse current-lines) hunks)))

    (setq hunks (reverse hunks))
    ;; (message "Parsed hunks: %S" hunks)
    hunks))


;;; 2) Applying each hunk

(defun modify-buffer--apply-hunk (hunk)
  "Apply one parsed HUNK to the current buffer.
Returns t if successful, signals an error if not found."
  (let* ((buffer-lines (modify-buffer--current-buffer-lines))
         (match-pos (modify-buffer--find-hunk-in-buffer hunk buffer-lines)))
    (unless match-pos
      (error "Hunk not found in buffer (failed to match context/remove lines)"))
    ;; Rebuild the buffer content
    (let ((newlines (modify-buffer--apply-chunk-to-lines buffer-lines hunk match-pos)))
      (modify-buffer--replace-buffer-contents newlines)
      t)))

(defun modify-buffer--current-buffer-lines ()
  "Return current buffer text as a list of strings (without trailing newline)."
  (save-restriction
    (widen)
    (save-excursion
      (goto-char (point-min))
      (let (result)
        (while (not (eobp))
          (push (buffer-substring-no-properties
                 (line-beginning-position) (line-end-position))
                result)
          (forward-line 1))
        (nreverse result)))))

(defun modify-buffer--replace-buffer-contents (lines)
  "Replace entire current buffer with LINES (list of strings)."
  (save-excursion
    (goto-char (point-min))
    (delete-region (point-min) (point-max))
    (dolist (ln lines)
      (insert ln)
      (insert "\n"))))

(defun modify-buffer--apply-chunk-to-lines (orig-lines hunk match-pos)
  "Given ORIG-LINES and HUNK, apply the changes at MATCH-POS in ORIG-LINES.
Returns an updated list of lines.

For each entry in HUNK:
- If the entry is of type 'context, we assume the line in ORIG-LINES matches and simply increment our index.
- If the entry is of type 'remove, we remove the line at the current index (without advancing the index).
- If the entry is of type 'add, we insert the new line at the current index and then increment the index."
  (let ((result (cl-copy-list orig-lines))
        (i match-pos))
    (dolist (entry hunk)
      (pcase (car entry)
        ('context
         (cl-incf i))
        ('remove
         (setq result (modify-buffer--vec-remove result i)))
        ('add
         (setq result (modify-buffer--vec-insert result i (cdr entry)))
         (cl-incf i))))
    result))

(defun modify-buffer--vec-remove (vec idx)
  (append (cl-subseq vec 0 idx)
          (cl-subseq vec (1+ idx))))

(defun modify-buffer--vec-insert (vec idx item)
  (append (cl-subseq vec 0 idx)
          (list item)
          (cl-subseq vec idx)))


;;; 3) Finding the hunk in the buffer with flexible matching (blank lines, ^L, etc.)

(defun modify-buffer--find-hunk-in-buffer (hunk buffer-lines)
  "Return an index in BUFFER-LINES where HUNK can be matched (≥ 0.8 similarity).
HUNK is a list of (TYPE . TEXT), only 'context/'remove matter for searching.
We do:
 - ignoring repeated blank line mismatch (multiple blank lines in diff vs. buffer)
 - ignoring ^L
 - ignoring multiple consecutive spaces"
  (let ((pattern (modify-buffer--build-pattern hunk))
        (bcanon (modify-buffer--build-canonical buffer-lines)))
    ;; (message "Pattern: %S" pattern)
    ;; (message "Bcanon: %S" bcanon)
    (modify-buffer--best-match-position pattern bcanon)))

(defun modify-buffer--build-pattern (hunk)
  "Collect the 'context/'remove lines from HUNK into a canonical array of tokens.
We do NOT collapse consecutive blank lines here; we keep them as-is, but each line is normalized."
  (let (acc)
    (dolist (entry hunk)
      (pcase (car entry)
        ((or 'context 'remove)
         (push (modify-buffer--canonical-line (cdr entry)) acc))
        ('add nil)))  ; 'add lines do not matter for matching
    (nreverse acc)))

(defun modify-buffer--build-canonical (lines)
  "Convert LINES to a vector of strings or :BLANK, ignoring ^L and normalizing spaces.
No collapsing of consecutive blank lines here, either."
  (let (out)
    (cl-loop for i from 0 to (1- (length lines))
             for canon = (modify-buffer--canonical-line (nth i lines))
             do (push canon out))
    (nreverse out)))

(defun modify-buffer--canonical-line (line)
  "Return a canonical form of LINE:
 - remove all ^L
 - unify consecutive spaces/tabs to one space
 - trim
 - if empty => :BLANK, else => the normalized string"
  (let ((str (replace-regexp-in-string "\f+" "" line))) ; remove ^L
    (setq str (replace-regexp-in-string "[ \t]+" " " str)) ; unify spaces
    (setq str (string-trim str))
    (if (string-empty-p str)
        :BLANK
      str)))


;;; 4) The fuzzy matching that treats runs of :BLANK in pattern ↔ runs of :BLANK in buffer

(defun modify-buffer--best-match-position (pattern bcanon)
  "Slide PATTERN across BCANON. Return the best real index if score ≥ 0.8.
PATTERN, BCANON are lists of canonical tokens (strings or :BLANK).
We check each offset in BCANON, compute a score in [0..1], pick the best if ≥ 0.8."
  (let ((best-score 0.0)
        (best-idx nil)
        (N (length bcanon)))
    (cl-loop
     for i from 0 to N do
     (let ((score (modify-buffer--match-score pattern bcanon i)))
       (when (> score best-score)
         (setq best-score score best-idx i))))
    (if (and best-idx (>= best-score 0.8))
        ;; Convert that offset in BCANON to a real line index in the un-canonical buffer.
        ;; We do that by aligning the best-idx with the same offset, i.e. best-idx is
        ;; how many lines from the start of bcanon. So best-idx is a direct index into
        ;; buffer-lines as well, since we built bcanon in order.
        best-idx
      nil)))

(defun modify-buffer--match-score (pattern bcanon start)
  "Compute how well PATTERN matches BCANON from index START, returning a score between 0 and 1.
We allow multiple blank lines in one to match a single blank line in the other if the following token aligns.
Stop on mismatch or once pattern is exhausted, or if we run out of BCANON."
  (let ((pp 0)                        ; pointer into PATTERN
        (bp start)                    ; pointer into BCANON
        (plen (length pattern))
        (blen (length bcanon))
        (matches 0)
        (continue t))
    (while (and continue (< pp plen) (< bp blen))
      (let ((pt (nth pp pattern))
            (bt (nth bp bcanon)))
        ;; (message "Comparing pt=%S  bt=%S  matches=%d" pt bt matches)
        (cond
         ;; Both tokens are blank: collapse the run.
         ((and (eq pt :BLANK) (eq bt :BLANK))
          (setq matches (1+ matches))
          (setq pp (modify-buffer--skip-consecutive-blanks pattern pp))
          (setq bp (modify-buffer--skip-consecutive-blanks bcanon bp)))
         ;; Pattern expects a blank but BCANON does not.
         ((and (eq pt :BLANK) (not (eq bt :BLANK)))
          ;; If the next token in the pattern already equals the current BCANON token,
          ;; then skip over the extra blank in the pattern.
          (if (and (< (1+ pp) plen)
                   (stringp (nth (1+ pp) pattern))
                   (stringp bt)
                   (string= (nth (1+ pp) pattern) bt))
              (setq pp (1+ pp))
            (setq continue nil)))
         ;; BCANON has a blank while PATTERN does not.
         ((and (not (eq pt :BLANK)) (eq bt :BLANK))
          ;; If the next token in BCANON equals the current PATTERN token,
          ;; then skip the extra blank in BCANON.
          (if (and (< (1+ bp) blen)
                   (stringp pt)
                   (stringp (nth (1+ bp) bcanon))
                   (string= pt (nth (1+ bp) bcanon)))
              (setq bp (1+ bp))
            (setq continue nil)))
         ;; Both tokens are nonblank and must match exactly.
         ((and (stringp pt) (stringp bt) (string= pt bt))
          (setq matches (1+ matches))
          (cl-incf pp)
          (cl-incf bp))
         (t
          (setq continue nil)))))

    ;; (message "Final score: matches=%d  plen=%d  score=%f"
    ;;          matches plen (if (= plen 0) 1.0 (/ (float matches) plen)))
    (if (= plen 0)
        1.0
      (/ (float matches) plen))))


(defun modify-buffer--skip-consecutive-blanks (tokens start)
  "Given TOKENS, skip forward from index START while tokens are :BLANK.
Return the index after the last blank."
  (let ((i start)
        (n (length tokens)))
    (while (and (< i n)
                (eq (nth i tokens) :BLANK))
      (cl-incf i))
    i))

;;; NEW STUFF

(defun modify-buffer--find-best-match (pattern buffer-lines)
  "Find optimal (pos . blank-counts) for matching PATTERN in BUFFER-LINES.
PATTERN is a list of (TYPE . TEXT) pairs where TYPE is context/remove/add.
Returns (POS . BLANK-COUNTS) where:
- POS is the best matching position in BUFFER-LINES
- BLANK-COUNTS is a vector of numbers, where the nth number indicates how many
  blank lines to consume for the nth context blank sequence in the pattern.
Returns nil if no match found."
  (let* ((collapsed-pattern (modify-buffer--collapse-pattern pattern))
         (best-score 0)
         (best-result nil))
    ;; Try each possible starting position
    (cl-loop for pos from 0 to (length buffer-lines) do
             (let ((match-result
                    (modify-buffer--try-match-from collapsed-pattern buffer-lines pos)))
               (when match-result
                 (let ((score (car match-result))
                       (blank-counts (cdr match-result)))
                   (when (> score best-score)
                     (setq best-score score
                           best-result (cons pos blank-counts)))))))
    (when (>= best-score 0.8)          ; require at least 80% match
      best-result)))

(defun modify-buffer--collapse-pattern (pattern)
  "Collapse consecutive context blank lines in PATTERN into single entries.
Non-blank lines and removal/addition lines are unchanged."
  (let (result in-blank-run)
    (dolist (entry pattern)
      (let ((type (car entry))
            (text (cdr entry)))
        (cond
         ;; If we see a context blank...
         ((and (eq type 'context)
               (string-empty-p (string-trim text)))
          (unless in-blank-run
            (push (cons 'context-blank-seq 1) result)
            (setq in-blank-run t)))
         ;; If we see anything else...
         (t
          (push entry result)
          (setq in-blank-run nil)))))
    (nreverse result)))

(defun modify-buffer--try-match-from (pattern buffer-lines pos)
  "Try to match PATTERN against BUFFER-LINES starting at POS.
Returns (SCORE . BLANK-COUNTS) if successful, nil if not."
  (let ((blank-counts (make-vector (modify-buffer--count-blank-seqs pattern) 0))
        (blank-idx 0)
        (buf-idx pos)
        (pattern-items-matched 0)
        (total-pattern-items 0))
    (catch 'no-match
      (dolist (entry pattern)
        (cl-incf total-pattern-items)
        (when (>= buf-idx (length buffer-lines))
          (throw 'no-match nil))
        (pcase entry
          ;; Context blank sequence
          (`(context-blank-seq . ,_)
           (let ((num-blanks (modify-buffer--count-blanks-at buffer-lines buf-idx)))
             (when (= num-blanks 0)
               (throw 'no-match nil))
             (aset blank-counts blank-idx num-blanks)
             (cl-incf blank-idx)
             (cl-incf pattern-items-matched)
             (setq buf-idx (+ buf-idx num-blanks))))
          ;; Regular line (context or removal)
          (`(,type . ,text)
           (let ((buf-line (nth buf-idx buffer-lines)))
             (when (or (not buf-line)
                       (not (string= (modify-buffer--canonical-line text)
                                     (modify-buffer--canonical-line buf-line))))
               (throw 'no-match nil))
             (cl-incf pattern-items-matched)
             (cl-incf buf-idx)))))
      (cons (/ (float pattern-items-matched) total-pattern-items)
            blank-counts))))

(defun modify-buffer--count-blank-seqs (pattern)
  "Count number of context blank sequences in PATTERN."
  (cl-count-if (lambda (entry)
                 (eq (car entry) 'context-blank-seq))
               pattern))

(defun modify-buffer--count-blanks-at (lines pos)
  "Count consecutive blank lines in LINES starting at POS."
  (let ((count 0))
    (while (and (< pos (length lines))
                (string-empty-p (string-trim (nth pos lines))))
      (cl-incf count)
      (cl-incf pos))
    count))

(defun modify-buffer--match-rest (pattern buffer-lines pos blank-counts)
  "Try to match PATTERN against BUFFER-LINES starting at POS.
BLANK-COUNTS is a vector tracking how many blanks each context-blank-seq matched.
Returns (SCORE . UPDATED-BLANK-COUNTS) if successful, nil if no match possible."
  (cond
   ((null pattern)
    (cons 1.0 blank-counts))

   ((>= pos (length buffer-lines))
    nil)

   (t
    (let* ((pat-entry (car pattern))
           (pat-type (car pat-entry)))
      (pcase pat-type
        ('context-blank-seq
         (modify-buffer--try-blank-sequence pattern buffer-lines pos blank-counts))

        ((and 'remove (guard (string-empty-p (string-trim (cdr pat-entry)))))
         (if (string-empty-p (string-trim (nth pos buffer-lines)))
             (modify-buffer--match-rest (cdr pattern)
                                        buffer-lines
                                        (1+ pos)
                                        blank-counts)
           nil))

        (_
         (modify-buffer--try-regular-line pattern buffer-lines pos blank-counts)))))))

(defun modify-buffer--count-blanks-at (lines pos)
  "Count consecutive blank lines in LINES starting at POS."
  (let ((count 0))
    (while (and (< pos (length lines))
                (string-empty-p (string-trim (nth pos lines))))
      (cl-incf count)
      (cl-incf pos))
    count))

(defun modify-buffer--try-blank-sequence (pattern buffer-lines pos blank-counts)
  "Try to match a blank sequence at position POS.
Returns (score . new-blank-counts) for best match, or nil if no match possible."
  (let ((num-available (modify-buffer--count-consecutive-blanks buffer-lines pos))
        (best-score 0)
        (best-result nil))
    ;; (message "num-available: %d at pos %d" num-available pos)
    (when (> num-available 0)
      ;; Try each possible number of blanks
      (cl-loop for n from 1 to num-available do
               (let* ((new-counts (vconcat blank-counts (vector n)))
                      ;; (_ (message "Trying n=%d" n))
                      (rest-result (modify-buffer--match-rest-after-blank
                                    pattern buffer-lines (+ pos n) new-counts)))
                 (when rest-result
                   (let* ((rest-score (car rest-result))
                          ;; (_ (message "Got rest-score %f" rest-score))
                          (score (modify-buffer--score-blank-match n rest-score)))
                     ;; (message "Computed score %f" score)
                     (when (> score best-score)
                       (setq best-score score
                             best-result (cons score (cdr rest-result))))))))
      ;; (message "Final best-result: %S" best-result)
      best-result)))

(defun modify-buffer--try-regular-line (pattern buffer-lines pos blank-counts)
  "Try to match a regular (non-blank) line at position POS.
Returns (score . blank-counts) if successful, nil if not."
  (let* ((pat-entry (car pattern))
         (pat-text (cdr pat-entry))
         (buf-line (nth pos buffer-lines)))
    (when (and buf-line
               (equal (modify-buffer--canonical-line pat-text)
                      (modify-buffer--canonical-line buf-line)))
      (modify-buffer--match-rest (cdr pattern)
                                 buffer-lines
                                 (1+ pos)
                                 blank-counts))))

(defun modify-buffer--count-consecutive-blanks (lines pos)
  "Count number of consecutive blank lines in LINES starting at POS."
  (let ((count 0))
    (while (and (< pos (length lines))
                (string-empty-p (string-trim (nth pos lines))))
      (cl-incf count)
      (cl-incf pos))
    count))

(defun modify-buffer--score-blank-match (num-blanks rest-score)
  "Compute score for matching NUM-BLANKS lines with REST-SCORE.
Returns a score between 0 and 1, strongly preferring fewer blanks
when REST-SCORE is equal."
  (* rest-score (expt 0.5 (1- num-blanks))))

(defun modify-buffer--match-rest-after-blank (pattern buffer-lines pos blank-counts)
  "Simplified match-rest for testing try-blank-sequence.
Just matches the next pattern line, skipping leftover blanks in the buffer if needed."
  (let ((next-pat (cadr pattern)))
    (when next-pat
      ;; If the next pattern line is not blank, skip leftover blank lines in the buffer
      (let ((next-type (car next-pat))
            (next-text (cdr next-pat)))
        (when (and (eq next-type 'context)
                   (not (string-empty-p (string-trim next-text))))
          (while (and (< pos (length buffer-lines))
                      (string-empty-p (string-trim (nth pos buffer-lines)))))
          (cl-incf pos))))
    (when (< pos (length buffer-lines))
      (let ((buf-line (nth pos buffer-lines))
            (pat-text (cdr next-pat)))
        (if (equal (modify-buffer--canonical-line buf-line)
                   (modify-buffer--canonical-line pat-text))
            (cons 1.0 blank-counts)
          nil)))))

(provide 'modify-buffer)
;;; modify-buffer.el ends here
