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

;; dict: '(("foo"          . "bar")
;;         ("\\([A-Z]+\\)" . "_\\1_")
;;         ("[a-z]"        . (lambda (s) (upcase s)))
;;         ...)
(defun %lookup-dict (dict count)
  "Dictionary lookup handler for perform-replace"
  (let* ((substr   (match-string 0))
         (dic      dict)
         (the-pair (%assoc-exact-match substr dic))
         (key      (car the-pair))
         (val      (cdr the-pair))
         (replacement (cond ((stringp val) val)
                            ((functionp val) (funcall val substr))
                            (t val)))
         (replaced (save-match-data
                     (string-match key substr)
                     (replace-match (cond ((stringp val) val)
                                          ((functionp val) (funcall val substr))
                                          (t "bummers!"))    ; NEWTEXT
                                    nil    ; FIXEDCASE
                                    nil    ; LITERAL
                                    substr ; STRING
                                    nil    ; SUBEXP
                                    ))))
    replaced))

(defun perform-replace-with-dict (dict &optional start end)
  "Invoke perform-replace using dict"
  (let* ((re (%regexp-opt-re (mapcar #'car dict))))
    (perform-replace re    ; FROM-STRING
                     `((lambda (data count)
                         (funcall #'%lookup-dict data count))
                       . ,dict) ; REPLACEMENTS
                     t     ; QUERY-FLAG
                     t     ; REGEXP-FLAG
                     nil   ; DELIMITED-FLAG
                     nil   ; &optional REPEAT-COUNT
                     nil   ; MAP
                     start ; START
                     end   ; END
                     )))

(provide 'perform-replace-with-dict)
