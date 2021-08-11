;; -*- encoding: utf-8 -*-
;; perform-replace-with-dict: dictionary-based perform-replace
;; Copyright (c) 2010-2011 Hisashi Morita
;; License: Public Domain
;;
;; Usage:
;;   (perform-replace-with-dict '((RE1 . SUBST1)
;;                                (RE2 . SUBST2) ...)
;;                              &optional START END)

(require 'cl-lib)

(defun %group-sequence (seq &optional &key key)
  "Group a sequence of elements to a sequence of groups of same
sucessive elements.
Example:
  '(a a a) => '((a a a))
  '(a a b) => '((a a) (b))
  '(a a b a a) => '((a a) (b) (a a))"
  (let* ((push
          ;; '(a b) c => '(a b c)
          (lambda (ls elem) (reverse (cons elem (reverse ls)))))
         (push-or-push-to-last
          ;; '(... (a a)) a => '(... (a a a))
          ;; '(... (a a)) b => '(... (a a) (b))
          (lambda (ls elem pred)
            (if (null ls)
                (funcall push ls (list elem))
              (let* ((last-group   (car (reverse ls)))
                     (last-elem    (car (reverse last-group)))
                     (without-last (reverse (cdr (reverse ls)))))
                (if (funcall pred last-elem elem)
                    (funcall push without-last (funcall push last-group elem))
                  (funcall push ls (list elem))))))))
    (cl-reduce (lambda (seed elem)
                 (if key ; somehow &key (name default) does not work
                     (funcall push-or-push-to-last seed elem key)
                   (funcall push-or-push-to-last seed elem #'equal)))
               seq
               :initial-value '())))

(defun %wrapup-group (seq)
  "Group together regular characters to a charclass, strings and
regexp special characters to an OR expression.
Example:
  '(\"a\" \"b\") => \"[ab]\"
  '(\"a\" \".\") => \"a\\|.\""
  (let ((regexp-charclassable
         (lambda (str) (and (stringp str)
                            (= 1 (length str))
                            (equal str (regexp-quote str)))))
        (regexp-charclass
         ;; '("a" "b") => "[ab]"
         (lambda (strs) (apply #'concat `("[" ,@strs "]"))))
        (regexp-or
         ;; '("a" "b") => "a\\|b"
         (lambda (strs) (mapconcat #'identity strs "\\|"))))
    (if (cl-every regexp-charclassable seq)
        (funcall regexp-charclass seq)
      (funcall regexp-or seq))))

(defun %regexp-opt-re (ls)
  "Return a (naively) optimized regexp. Unlike standard `regexp-opt',
this function accepts regexps as its input.
Example:
  '(\"a\" \"b\" \".\" \"cd+\") => \"[ab]\\|.\\|cd+\""
  (let* ((regexp-charclassable
         (lambda (str) (and (stringp str)
                            (= 1 (length str))
                            (equal str (regexp-quote str)))))
         (regexp-or
          ;; '("a" "b") => "a\\|b"
          (lambda (strs) (mapconcat #'identity strs "\\|")))
         (grouped (%group-sequence ls
                                   :key (lambda (a b)
                                          (cl-every regexp-charclassable
                                                    (list a b)))))
         (wrapped-up (mapcar #'%wrapup-group
                             grouped))
         (or-ed (funcall regexp-or wrapped-up)))
    or-ed))

(defun %assoc-exact-match (str dict)
  "Return an element from DICT which exactly matches STR.
DICT must consist of regexp and replacement string pairs (re . repstr)."
  (cl-assoc str dict
            :test (lambda (s r)
                    (let ((external-md (match-data)))
                      (unwind-protect
                          (if (string-match r s) (equal (match-string 0 s) s) nil)
                        (set-match-data external-md))))))

(defun %sort-dict (dict)
  "Sort DICT by key type (regular strings to regexps) and by key
length (long to short)."
  (let ((strings-to-regexps-and-long-to-short
         (lambda (pair-a pair-b)
           (let* ((key-a (car pair-a))
                  (key-b (car pair-b))
                  (src-a (regexp-quote key-a))
                  (src-b (regexp-quote key-b))
                  (a-is-regexp (not (equal src-a key-a)))
                  (b-is-regexp (not (equal src-b key-b))))
             (cond ((and (not a-is-regexp) b-is-regexp) t)
                   ((and a-is-regexp (not b-is-regexp)) nil)
                   (t (>= (length src-a) (length src-b))))))))
    (sort dict strings-to-regexps-and-long-to-short)))

(defun %find-replacement (str dict)
  "Return DICT value whose key exactly matches STR. If none found,
return STR.

DICT keys are regexps (including plain strings), while values are
strings or functions.  DICT must be sorted by key type (plain
strings to regexps) and by key length (long to short).

Example:
  '((\"foobar\" . \"Foobar\")
    (\"bar\"    . \"Bar\")
    (\"[a-z]+\" . (lambda (s) (upcase s)))
    ...)"
  (let* ((case-fold-search nil)
         (matching-entry (%assoc-exact-match str dict)))
    (if matching-entry
        (let* ((key (car matching-entry))
               (val (cdr matching-entry))
               (dummy (cond ((stringp val) val)
                            ((functionp val) (funcall val str))
                            (t val))) ; FIXME: This exp should be unnecessary
               (replacement (save-match-data
                              (string-match key str)
                              (replace-match
                               (cond ((stringp val) val)
                                     ((functionp val) (funcall val str))
                                     (t "Error: Invalid dictionary entry")) ; NEWTEXT
                               nil ; FIXEDCASE
                               nil ; LITERAL
                               str ; STRING
                               nil ; SUBEXP
                               ))))
          replacement)
      str)))

(defun perform-replace-with-dict (dict &optional start end)
  "Invoke perform-replace using DICT."
  (let* ((dict-sorted (%sort-dict dict))
         (re (%regexp-opt-re (mapcar #'car dict-sorted))))
    (save-mark-and-excursion
      (perform-replace re    ; FROM-STRING
                       `((lambda (data _count)
                           (funcall #'%find-replacement (match-string 0) data))
                         . ,dict-sorted) ; REPLACEMENTS
                       t     ; QUERY-FLAG
                       t     ; REGEXP-FLAG
                       nil   ; DELIMITED-FLAG
                       nil   ; &optional REPEAT-COUNT
                       nil   ; MAP
                       start ; START
                       end   ; END
                       ))))

(provide 'perform-replace-with-dict)
