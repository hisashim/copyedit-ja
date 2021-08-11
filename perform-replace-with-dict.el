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

;; '(a b) c => '(a b c)
(defun %push (ls elem)
  (reverse (cons elem (reverse ls))))

;; '(... (a a)) a => '(... (a a a))
;; '(... (a a)) b => '(... (a a) (b))
(defun %push-or-push-to-last (ls elem pred)
  (if (null ls)
      (%push ls (list elem))
      (let* ((last-group   (car (reverse ls)))
             (last-elem    (car (reverse last-group)))
             (without-last (reverse (cdr (reverse ls)))))
        (if (funcall pred last-elem elem)
            (%push without-last (%push last-group elem))
            (%push ls (list elem))))))

(defun %group-sequence (seq &optional &key key)
  "group-sequence similar to Gauche's"
  (cl-reduce (lambda (seed elem)
               (if key ; somehow &key (name default) does not work
                   (%push-or-push-to-last seed elem key)
                   (%push-or-push-to-last seed elem #'equal)))
             seq
             :initial-value '()
             ))

(defun %regexp-special (str)
  (not (equal str (regexp-quote str))))

(defun %charclassable (str)
  (and (stringp str)
       (= 1 (length str))
       (not (%regexp-special str))))

;; "a" => "\\(a\\)"
(defun %rx-group (str)
  (concat "\\(" str "\\)"))

;; '("a" "b") => "[ab]"
(defun %rx-charclass (strings)
  (apply #'concat `("[" ,@strings "]")))

;; '("a" "b") => "a\\|b"
(defun %rx-or (strings)
  (mapconcat #'identity strings "\\|"))

;; '("a" "b") => "[ab]", '("a" ".") => "a\\|."
(defun %wrapup-group (seq)
  (if (cl-every #'%charclassable seq)
      (%rx-charclass seq)
      (%rx-or seq)))

;; '("a" "b" "." "cd+") => "[ab]\\|.\\|cd+"
(defun %regexp-opt-re (ls)
  (let* ((grouped (%group-sequence ls
                                   :key (lambda (a b)
                                          (cl-every #'%charclassable
                                                    (list a b)))))
         (wrapped-up (mapcar #'%wrapup-group
                             grouped))
         (or-ed (%rx-or wrapped-up)))
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
