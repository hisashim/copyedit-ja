;; -*- utf-8-unix -*-
;; Utility to help reviewing English to Japanese translation
;; Copyright (c) 2005-2011 Hisashi Morita
;; License: Public Domain
;; Usage:
;;   1) Load translation (Japanese) and its original edition (English).
;;   2) Display the buffers in two split windows.
;;   3) On translation buffer, call lookup-original-other-window
;;      e.g. M-x lookup-original-other-window
;;           M-: (lookup-original-other-window 2 'occur)
;;   4) You may be able to locate the counterpart in original.
;;      if you couldn't, try C-M-s (isearch-forward-regexp).

(eval-when-compile (require 'cl))

(defun lookup-original-other-window (&optional num &optional f)
  "Search buffer in other window using NUM latin words behind
current point in current buffer and using search function F

NUM is 3 by default.
F is re-search-forward by default.

You can search for next candidate using isearch-forward-regexp
 (C-M-s by default)."
  (interactive)
  (let* ((word-re "[^0-9A-Za-z]+?\\([0-9A-Za-z]+\\)")
         (ng-words-re '("Lisp" "[0-9]\{1\}" "begin" "end" "verbatim"
                        "verb" "emph" "quote" "dots" "chapter"
                        "section" "subsection" "wadash"))
         (pat (%make-pat-from-words-behind (if (null num) 3 num)
                                           word-re
                                           ng-words-re)))
    (progn
      (other-window 1)
      (beginning-of-buffer)
      (if (null f)
          (if (re-search-forward pat nil t) ; returns nil, not error
              (isearch-update-ring pat t)
            nil)
        (funcall f pat))
      (list 'Searched 'for pat 'Matched (match-string-no-properties 0))
      ; (other-window -1)
      )))

(defun %make-pat-from-words-behind (num word-re ng-words-re)
  "Make regexp searching NUM latin words using WORD-RE behind the
current point

ng-words-re is a list of words that occur in translation but not
in original thus may interfere searching."
  (save-excursion
    (mapconcat 'concat
               (mapcar 'regexp-quote
                       (%scan-uniq-words-backward word-re
                                                  ng-words-re
                                                  num))
               ".+?")))

(defun %scan-uniq-words-backward (re ng-words-re count)
  "Scan COUNT substrings that match RE and does not match
anything in NG-WORDS-RE"
  (if (zerop count)
      nil
    (progn
      (re-search-backward re)
      (assert (match-string-no-properties 1)
              t
              "use regexp with parens \\(\\) that match something")
      (if (not (%include ng-words-re
                         (match-string-no-properties 1)
                         'string-match))
          (reverse
           (cons (match-string-no-properties 1)
                 (reverse (%scan-uniq-words-backward re
                                                     ng-words-re
                                                     (- count 1)))))
        (%scan-uniq-words-backward re ng-words-re count)))))

(defun %member (elt lst &optional f)
  "Similar to member, but take arbitrary function for comparison"
  (delete nil (mapcar
               (if (null f)
                   '(lambda (e) (if (equal elt e) e nil))
                 '(lambda (e) (if (funcall (symbol-function f) elt e)
                                  e
                                nil)))
               lst)))

(defun %include (lst elt &optional f)
  "Check if LST contains ELT using F for comparison"
  (delete nil (mapcar
               (if (null f)
                   '(lambda (e) (if (equal e elt) e nil))
                 '(lambda (e) (if (funcall f e elt) e nil)))
               lst)))
