;;; modify-buffer-test.el --- Test suite for buffer diff applications  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Paul Nelson

;; Author: Paul Nelson <ultrono@gmail.com>
;; Version: 0.1
;; Package-Requires: ((emacs "28.1"))
;; Keywords: testing, tools
;; URL: https://github.com/your-username/modify-buffer-test

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

;; This package provides a test suite for functions that apply diffs
;; to buffers.
;;
;; Usage:
;;
;; 1. Create your diff application function with this signature:
;;    (my-apply-diff buffer diff) where:
;;    - buffer is a buffer name (string)
;;    - diff is a unified diff format string
;;    - Returns a status message (string)
;;
;; 2. In your package's tests, use this test suite:
;;
;;    (require 'modify-buffer-test)
;;
;;    (ert-deftest test-my-package ()
;;      (modify-buffer-test-with-implementation #'my-apply-diff
;;        (modify-buffer-test-run-all)))
;;
;; The test suite covers:
;; - Basic modifications
;; - Multiple hunks
;; - Whitespace handling
;; - Error cases
;; - Overlapping changes
;; - Sequential modifications
;; - Line wrapping
;; - (...)

;;; Code:

(require 'ert)

(defgroup modify-buffer-test nil
  "Test suite for buffer diff applications."
  :group 'development
  :prefix "modify-buffer-test-")

(defvar modify-buffer-test-implementation nil
  "The current implementation being tested.")

(defmacro modify-buffer-test-with-implementation (implementation &rest body)
  "Run test BODY with IMPLEMENTATION as the current diff application function."
  (declare (indent 1))
  `(let ((modify-buffer-test-implementation ,implementation))
     ,@body))

(defun modify-buffer-test-run-all ()
  "Run all tests in the suite with the current implementation."
  (interactive)
  (if modify-buffer-test-implementation
      (ert "^modify-buffer-test-")
    (error "No implementation set; use modify-buffer-test-with-implementation")))

;;; Tests

(ert-deftest modify-buffer-test-basic ()
  "Test basic modification of a buffer."
  (skip-unless modify-buffer-test-implementation)
  (with-temp-buffer
    (insert "Hello\nWorld\n")
    (let ((buf-name (buffer-name)))
      (let ((diff "
@@ -1,2 +1,2 @@
 Hello
-World
+Universe
"))
        (should (equal (buffer-string) "Hello\nWorld\n"))  ; Pre-condition
        (funcall modify-buffer-test-implementation buf-name diff)
        (should (equal (buffer-string) "Hello\nUniverse\n"))))))

(ert-deftest modify-buffer-test-multiple-hunks ()
  "Test applying multiple hunks in a diff."
  (with-temp-buffer
    (insert "First line\nSecond line\nThird line\nFourth line\n")
    (let ((buf-name (buffer-name)))
      (let ((diff "
@@ -1,2 +1,2 @@
 First line
-Second line
+Modified second line
@@ -3,2 +3,2 @@
 Third line
-Fourth line
+Modified fourth line
"))
        (funcall modify-buffer-test-implementation buf-name diff)
        (should (equal (buffer-string)
                       "First line\nModified second line\nThird line\nModified fourth line\n"))))))

(ert-deftest modify-buffer-test-whitespace ()
  "Test handling of whitespace variations."
  (with-temp-buffer
    (insert "Line with    spaces\nAnother   line\n")
    (let ((buf-name (buffer-name)))
      (let ((diff "
@@ -1,2 +1,2 @@
 Line with    spaces
-Another   line
+Another modified   line
"))
        (funcall modify-buffer-test-implementation buf-name diff)
        (should (equal (buffer-string)
                       "Line with    spaces\nAnother modified   line\n"))))))

(ert-deftest modify-buffer-test-nonexistent ()
  "Test behavior with nonexistent buffer."
  (should-error
   (funcall modify-buffer-test-implementation "nonexistent-buffer" "some diff")
   :type 'error))

(ert-deftest modify-buffer-test-no-hunks ()
  "Test behavior when diff contains no hunks."
  (with-temp-buffer
    (insert "Some content\n")
    (let ((buf-name (buffer-name)))
      (let ((diff "Not a valid diff"))
        (funcall modify-buffer-test-implementation buf-name diff)
        (should (equal (buffer-string) "Some content\n"))))))

(ert-deftest modify-buffer-test-fuzzy-context ()
  "Test patch's ability to handle slightly mismatched context."
  (with-temp-buffer
    (insert "First line here\nSecond line here\nThird line here\nFourth line here\n")
    (let ((buf-name (buffer-name)))
      ;; Note: The diff's context has slightly different spacing
      (let ((diff "
@@ -1,4 +1,4 @@
 First  line here
-Second line here
+Second line MODIFIED
 Third line  here
 Fourth line here
"))
        (funcall modify-buffer-test-implementation buf-name diff)
        (should (equal (buffer-string)
                       "First line here\nSecond line MODIFIED\nThird line here\nFourth line here\n"))))))

(ert-deftest modify-buffer-test-distant-context ()
  "Test handling changes with minimal context but distant markers."
  (with-temp-buffer
    (insert "Header\n\n\n\nTarget line\n\n\n\nFooter\n")
    (let ((buf-name (buffer-name)))
      (let ((diff "
@@ -5 +5 @@
-Target line
+Modified target
"))
        (funcall modify-buffer-test-implementation buf-name diff)
        (should (equal (buffer-string)
                       "Header\n\n\n\nModified target\n\n\n\nFooter\n"))))))

(ert-deftest modify-buffer-test-overlapping-hunks ()
  "Test handling of hunks that overlap or are adjacent."
  (with-temp-buffer
    (insert "Alpha\nBeta\nGamma\nDelta\nEpsilon\n")
    (let ((buf-name (buffer-name)))
      (let ((diff "
@@ -1,3 +1,3 @@
 Alpha
 Beta
-Gamma
+Gamma Modified
@@ -3,3 +3,3 @@
 Gamma Modified
-Delta
+Delta Modified
 Epsilon
"))
        (funcall modify-buffer-test-implementation buf-name diff)
        (should (equal (buffer-string)
                       "Alpha\nBeta\nGamma Modified\nDelta Modified\nEpsilon\n"))))))

(ert-deftest modify-buffer-test-sequential-changes ()
  "Test handling of changes that must be applied in sequence."
  (with-temp-buffer
    (insert "Alpha\nBeta\nGamma\nDelta\nEpsilon\n")
    (let ((buf-name (buffer-name)))
      ;; First change
      (funcall modify-buffer-test-implementation buf-name "
@@ -1,3 +1,3 @@
 Alpha
-Beta
+Beta Modified
 Gamma
")
      ;; Second change
      (funcall modify-buffer-test-implementation buf-name "
@@ -2,2 +2,2 @@
-Gamma
+Gamma Modified
")
      (should (equal (buffer-string)
                     "Alpha\nBeta Modified\nGamma Modified\nDelta\nEpsilon\n")))))

(ert-deftest modify-buffer-test-partial-line ()
  "Test changes that modify only part of a line."
  (with-temp-buffer
    (insert "function example(x, y) {\n    return x + y;\n}\n")
    (let ((buf-name (buffer-name)))
      (let ((diff "
@@ -1,3 +1,3 @@
-function example(x, y) {
+function example(x, y, z) {
     return x + y;
 }
"))
        (funcall modify-buffer-test-implementation buf-name diff)
        (should (equal (buffer-string)
                       "function example(x, y, z) {\n    return x + y;\n}\n"))))))

(ert-deftest modify-buffer-test-indentation-changes ()
  "Test handling of indentation-only changes."
  (with-temp-buffer
    (insert "def example():\n    first_line\n    second_line\n")
    (let ((buf-name (buffer-name)))
      (let ((diff "
@@ -1,3 +1,3 @@
 def example():
-    first_line
-    second_line
+        first_line
+        second_line
"))
        (funcall modify-buffer-test-implementation buf-name diff)
        (should (equal (buffer-string)
                       "def example():\n        first_line\n        second_line\n"))))))

(ert-deftest modify-buffer-test-wrapped-lines ()
  "Test changes involving wrapped lines with different wrapping points."
  (with-temp-buffer
    (insert "This is a very long line that should be modified carefully and with attention to detail.\n")
    (let ((buf-name (buffer-name)))
      (let ((diff "
@@ -1 +1 @@
-This is a very long line that should be modified carefully and with attention to detail.
+This is a very long line that should be modified carefully and with attention to detail and some extra text added here.
"))
        (funcall modify-buffer-test-implementation buf-name diff)
        (should (equal (buffer-string)
                       "This is a very long line that should be modified carefully and with attention to detail and some extra text added here.\n"))))))

(ert-deftest modify-buffer-test-incorrect-line-numbers ()
  "Test handling of diffs with incorrect line numbers in @@ headers."
  (with-temp-buffer
    (insert "First line\nSecond line\n")
    (let ((buf-name (buffer-name)))
      ;; Note: The line numbers in the @@ header are intentionally wrong
      (let ((diff "
@@ -100,2 +100,2 @@
 First line
-Second line
+Modified second line
"))
        (funcall modify-buffer-test-implementation buf-name diff)
        (should (equal (buffer-string)
                       "First line\nModified second line\n"))))))

(ert-deftest modify-buffer-test-incorrect-line-numbers2 ()
  "Test handling of diffs with incorrect line numbers in @@ headers."
  (skip-unless modify-buffer-test-implementation)
  (with-temp-buffer
    (insert "def example():\n    return 42\n")
    (let ((buf-name (buffer-name)))
      ;; Use a much larger line number to ensure it's well beyond buffer size
      (let ((diff "
@@ -1162,2 +1162,3 @@
 def example():
-    return 42
+    # Added a comment
+    return 42"))
        (funcall modify-buffer-test-implementation buf-name diff)
        (should (equal (buffer-string)
                       "def example():\n    # Added a comment\n    return 42\n"))))))

(ert-deftest modify-buffer-test-comma-prefix ()
  "Test handling diff that removes a comma prefix."
  (skip-unless modify-buffer-test-implementation)
  (with-temp-buffer
    (insert "dfd\n,** Second heading\n")
    (let ((buf-name (buffer-name)))
      (let ((diff "
@@ -1,2 +1,2 @@
 dfd
-,** Second heading
+** Second heading
"))
        (funcall modify-buffer-test-implementation buf-name diff)
        (should (equal (buffer-string) "dfd\n** Second heading\n"))))))

(ert-deftest modify-buffer-test-comma-prefix2 ()
  "Test handling diff that removes a comma prefix."
  (skip-unless modify-buffer-test-implementation)
  (with-temp-buffer
    (let ((buf-name (buffer-name))
          (text "xy
")
          (diff "@@ -1,4 +1,4 @@
-xy
+y
")
          (after "y
"))
      (insert text)
      (funcall modify-buffer-test-implementation buf-name diff)
      (should (equal (buffer-string) after)))))

(ert-deftest modify-buffer-test-comma-prefix3 ()
  "Test handling diff that removes a comma prefix."
  (skip-unless modify-buffer-test-implementation)
  (with-temp-buffer
    (let ((buf-name (buffer-name))
          (text "First heading
:PROPERTIES:
:TOOLS: tool1
:END:

,xx Second heading
:PROPERTIES:
:TOOLS: tool2
:END:

,xxx Third heading
")
          ;; correct lines are -3,9 +3,9
          (diff "@@ 3,9 +3,9 @@\n :TOOLS: tool1\n :END:\n \n-,xx Second heading\n+xx Second heading\n :PROPERTIES:\n :TOOLS: tool2\n :END:\n \n-,xxx Third heading\n+xxx Third heading\n")
          (after "First heading
:PROPERTIES:
:TOOLS: tool1
:END:

xx Second heading
:PROPERTIES:
:TOOLS: tool2
:END:

xxx Third heading
"))
      (insert text)
      (funcall modify-buffer-test-implementation buf-name diff)
      (should (equal (buffer-string) after)))))

(ert-deftest modify-buffer-test-10 ()
  "Test handling diff that removes a comma prefix."
  (skip-unless modify-buffer-test-implementation)
  (with-temp-buffer
    (let ((buf-name (buffer-name))
          (text "          (cl-subseq vec idx)))

;;; Matching and Comparison Functions

;;; Matching the hunk in the buffer (ignoring line numbers)

(defun modify-buffer--find-hunk-in-buffer (hunk buffer-lines)
")
          (diff "@@ -376,8 +372,6 @@
           (cl-subseq vec idx)))

-;;; Matching and Comparison Functions
-
 ;;; Matching the hunk in the buffer (ignoring line numbers)

 (defun modify-buffer--find-hunk-in-buffer (hunk buffer-lines)")
          (after "          (cl-subseq vec idx)))

;;; Matching the hunk in the buffer (ignoring line numbers)

(defun modify-buffer--find-hunk-in-buffer (hunk buffer-lines)
"))
      (insert text)
      (funcall modify-buffer-test-implementation buf-name diff)
      (should (equal (buffer-string) after)))))

(ert-deftest modify-buffer-test-11 ()
  "Test handling diff that removes a comma prefix."
  (skip-unless modify-buffer-test-implementation)
  (with-temp-buffer
    (let ((buf-name (buffer-name))
          (text "          (cl-subseq vec idx)))

;;; Matching and Comparison Functions

;;; Matching the hunk in the buffer (ignoring line numbers)

(defun modify-buffer--find-hunk-in-buffer (hunk buffer-lines)
")
          (diff "@@ -376,8 +372,6 @@
           (cl-subseq vec idx)))


-;;; Matching and Comparison Functions
-
 ;;; Matching the hunk in the buffer (ignoring line numbers)

 (defun modify-buffer--find-hunk-in-buffer (hunk buffer-lines)")
          (after "          (cl-subseq vec idx)))

;;; Matching the hunk in the buffer (ignoring line numbers)

(defun modify-buffer--find-hunk-in-buffer (hunk buffer-lines)
"))
      (insert text)
      (funcall modify-buffer-test-implementation buf-name diff)
      (should (equal (buffer-string) after)))))

(provide 'modify-buffer-test)
;;; modify-buffer-test.el ends here
