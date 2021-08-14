;; -*- encoding: utf-8 -*-
;; Japanese copyediting commands for Emacs
;; Copyright (c) 2008-2019 Hisashi Morita
;; License: Public Domain
;;
;; Usage example:
;;   1. check
;;     M-x copyedit-ja-check-typo-particle
;;   2. edit
;;     M-x copyedit-ja-normalize-typo-particle
;;
;; Requirements:
;;   * MeCab: Yet another Japanese morphological analyzer
;;     - https://github.com/taku910/mecab
;;   * color-moccur.el
;;     - v2.73: https://web.archive.org/web/20180527232131/www.bookshelf.jp/elc/color-moccur.el
;;     - v2.71: https://www.emacswiki.org/emacs/color-moccur.el
;;     - fork from v2.71: https://github.com/myuhe/color-moccur.el
;;
;; Bibliography:
;;   * Kurata Masanori, Suganuma Akira, Ushijima Kazuo,
;;     "Development of a System of Writing Tools for Japanese Documents on
;;      a Personal Computer."
;;     https://ci.nii.ac.jp/naid/110003743558

(require 'cl-lib)

;; ------------------------------------------------------------------------
;; utility: perform-replace-with-dict

(defun copyedit-ja--group-sequence (seq &optional &key key)
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

(defun copyedit-ja--wrapup-group (seq)
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

(defun copyedit-ja--regexp-opt-re (ls)
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
         (grouped (copyedit-ja--group-sequence ls
                                   :key (lambda (a b)
                                          (cl-every regexp-charclassable
                                                    (list a b)))))
         (wrapped-up (mapcar #'copyedit-ja--wrapup-group
                             grouped))
         (or-ed (funcall regexp-or wrapped-up)))
    or-ed))

(defun copyedit-ja--assoc-exact-match (str dict)
  "Return an element from DICT which exactly matches STR.
DICT must consist of regexp and replacement string pairs (re . repstr)."
  (cl-assoc str dict
            :test (lambda (s r)
                    (let ((external-md (match-data)))
                      (unwind-protect
                          (if (string-match r s) (equal (match-string 0 s) s) nil)
                        (set-match-data external-md))))))

(defun copyedit-ja--sort-dict (dict)
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

(defun copyedit-ja--find-replacement (str dict)
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
         (matching-entry (copyedit-ja--assoc-exact-match str dict)))
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

(defun copyedit-ja--perform-replace-with-dict (dict &optional start end)
  "Invoke perform-replace using DICT.
Usage:
  (copyedit-ja--perform-replace-with-dict '((REGEXP1 . REPLACEMENT1)
                                            (REGEXP2 . REPLACEMENT2) ...)
                                          &optional START END)"
  (let* ((dict-sorted (copyedit-ja--sort-dict dict))
         (re (copyedit-ja--regexp-opt-re (mapcar #'car dict-sorted))))
    (save-mark-and-excursion
      (perform-replace re    ; FROM-STRING
                       `((lambda (data _count)
                           (funcall #'copyedit-ja--find-replacement (match-string 0) data))
                         . ,dict-sorted) ; REPLACEMENTS
                       t     ; QUERY-FLAG
                       t     ; REGEXP-FLAG
                       nil   ; DELIMITED-FLAG
                       nil   ; &optional REPEAT-COUNT
                       nil   ; MAP
                       start ; START
                       end   ; END
                       ))))

;; ------------------------------------------------------------------------
;; utility: shell-command-string

(defun copyedit-ja--shell-command-string (cmd input &rest opts)
  "Run CMD as sub-process with INPUT for stdin. Return stdout as string.

Example:
  (copyedit-ja--shell-command-string \"grep\" STRING \"-in\")

Note:
  If something gets wrong when dealing with non-ASCII strings,
  try setting character encoding explicitly, e.g.
  (progn (prefer-coding-system 'utf-8-unix) ..."
  (save-window-excursion
    (with-temp-buffer
      (if input (insert input) nil)
      (apply 'call-process-region
             (point-min)
             (point-max)
             cmd
             t   ; delete: replace current buffer
             t   ; destination
             nil ; display: update display
             opts)
      (replace-regexp-in-string "\\(\r\n\\|\r\\|\n\\)\\'"
                                ""
                                (buffer-string)))))

;; ------------------------------------------------------------------------
;; misc utilities

(defun copyedit-ja--grep-buffers-with-dict (dict)
  "Search REGEXP in buffers with DICT."
  (interactive)
  (let ((regexp-src (copyedit-ja--regexp-opt-re (mapcar 'car dict))))
     (moccur regexp-src t)))

(defun copyedit-ja--zip-naive (&rest seq)
  "Zip lists, e.g. '(1 2 3) '(4 5) => '((1 4) (2 5) (3 nil))"
  (if (cl-every #'null (mapcar 'car seq))
      '()
    (cons (mapcar 'car seq) (apply #'copyedit-ja--zip-naive (mapcar 'cdr seq)))))

(defun copyedit-ja--translate (s dict)
  (let* ((counterpart (cdr (assoc s dict))))
    (if counterpart
        counterpart
        s)))

(defun copyedit-ja--filter-region (f begin end)
  (let* ((src (buffer-substring begin end))
         (replacement (funcall f src)))
    (progn
      (delete-region begin end)
      (insert replacement))))

;; ------------------------------------------------------------------------
;; applications
;; inspired by Suikou

(defconst copyedit-ja--dict-paren-width
  '(("\\(\\cj\\) ?(\\([^()]*?\\)) ?\\(\\cj\\)" . "\\1（\\2）\\3")
    ("\\(\\cj\\) ?(" . "\\1（")
    (") ?\\(\\cj\\)" . "）\\1")))

(defconst copyedit-ja--dict-paren-width-paranoid
  '(("\\(^\\|\\cj\\) ?(\\([^()]*?\\)) ?\\(\\cj\\|$\\)" . "\\1（\\2）\\3")
    ("\\(^\\|\\cj\\) ?(" . "\\1（")
    (") ?\\(\\cj\\|$\\)" . "）\\1")))

(defun copyedit-ja-check-paren-width (&optional paranoid)
  "Check the widths of parenthesis characters considering contexts."
  (interactive "P")
  (if paranoid
      (copyedit-ja--grep-buffers-with-dict copyedit-ja--dict-paren-width-paranoid)
      (copyedit-ja--grep-buffers-with-dict copyedit-ja--dict-paren-width)))

(defun copyedit-ja-normalize-paren-width (&optional paranoid)
  "Replace half-width parens with full-width ones."
  (interactive "P")
  (if paranoid
      (copyedit-ja--perform-replace-with-dict copyedit-ja--dict-paren-width-paranoid)
      (copyedit-ja--perform-replace-with-dict copyedit-ja--dict-paren-width)))

(defun copyedit-ja-normalize-paren-width-region (start end &optional paranoid)
  "Replace half-width parens in region with full-width ones."
  (interactive "Pr")
  (save-excursion
    (save-restriction
      (if paranoid
          (copyedit-ja--perform-replace-with-dict copyedit-ja--dict-paren-width-paranoid
                                                  start end)
          (copyedit-ja--perform-replace-with-dict copyedit-ja--dict-paren-width
                                                  start end)))))

(defconst copyedit-ja--dict-paren-matching
  '(("\\(([^)]*?）\\)" . "\\1")
    ("\\(（[^）]*?)\\)" . "\\1")))

(defun copyedit-ja-check-paren-matching ()
  "Check if opening and closing parentheses match."
  (interactive)
  (copyedit-ja--grep-buffers-with-dict copyedit-ja--dict-paren-matching))

(defconst copyedit-ja--dict-punctuation-tenmaru
  '(("\\(\\cj\\),"   . "\\1、")
    ("\\(\\cj\\)\\." . "\\1。")
    ("，" . "、")
    ("．" . "。")))

(defconst copyedit-ja--dict-punctuation-kanpiri
  '(("\\(\\cj\\),"   . "\\1，")
    ("\\(\\cj\\)\\." . "\\1．")
    ("、" . "，")
    ("。" . "．")))

(defun copyedit-ja-check-punctuation-tenmaru ()
  "Check irregular punctuation, preferring ten-maru."
  (interactive)
  (copyedit-ja--grep-buffers-with-dict copyedit-ja--dict-punctuation-tenmaru))

(defun copyedit-ja-check-punctuation-kanpiri ()
  "Check irregular punctuation preferring kan-piri."
  (interactive)
  (copyedit-ja--grep-buffers-with-dict copyedit-ja--dict-punctuation-kanpiri))

(defun copyedit-ja-normalize-punctuation-tenmaru ()
  "Normalize irregular punctuation, preferring ten and maru."
  (interactive)
  (copyedit-ja--perform-replace-with-dict copyedit-ja--dict-punctuation-tenmaru))

(defun copyedit-ja-normalize-punctuation-kanpiri ()
  "Normalize irregular punctuation, preferring commas and dots."
  (interactive)
  (copyedit-ja--perform-replace-with-dict copyedit-ja--dict-punctuation-kanpiri))

(defconst copyedit-ja--dict-macron
  '(("\\([^ァ-ンヴヵヶ]ー\\)" . "\\1")))

(defun copyedit-ja-check-macron ()
  "Check suspicious macron (onbiki)."
  (interactive)
  (copyedit-ja--grep-buffers-with-dict copyedit-ja--dict-macron))

(defun copyedit-ja-normalize-macron ()
  "Normalize suspicious macron (onbiki)."
  (interactive)
  (copyedit-ja--perform-replace-with-dict copyedit-ja--dict-macron))

(defconst copyedit-ja--dict-typo-particle
  '(("\\([てにをはのが]\\)\\1" . "\\1")))

(defun copyedit-ja-check-typo-particle ()
  "Check possible typos in particles."
  (interactive)
  (copyedit-ja--grep-buffers-with-dict copyedit-ja--dict-typo-particle))

(defun copyedit-ja-normalize-typo-particle ()
  "Normalize possible typos in particles."
  (interactive)
  (copyedit-ja--perform-replace-with-dict copyedit-ja--dict-typo-particle))

(defconst copyedit-ja--dict-passive-voice
  '(("\\([あかさたなはまやらわ]ら?れ\\)" . "\\1")))

(defun copyedit-ja-check-passive-voice ()
  "Find passive voice, which are sometimes misused."
  (interactive)
  (copyedit-ja--grep-buffers-with-dict copyedit-ja--dict-passive-voice))

(defconst copyedit-ja--dict-demonstrative
  '(("\\([こそあど][れの]\\)" . "\\1")))

(defun copyedit-ja-check-demonstrative ()
  "Find demonstratives (kore, soer, are, dore, etc.)."
  (interactive)
  (copyedit-ja--grep-buffers-with-dict copyedit-ja--dict-demonstrative))

(defconst copyedit-ja--dict-toritate-focalizer
  '(("\\(こそ\\)" . "\\1")
    ("\\(すら\\)" . "\\1")
    ("\\(さえ\\)" . "\\1")
    ("\\(だけ\\)" . "\\1")
    ("\\(でも\\)" . "\\1")
    ("\\(まで\\)" . "\\1")
    ("\\(など\\)" . "\\1")
    ("\\([くぐ]らい\\)" . "\\1")
    ("\\(ばかり\\)" . "\\1")
    ("\\(だって\\)" . "\\1")))

(defun copyedit-ja-check-toritate-focalizer ()
  "Find toritate focalizers."
  (interactive)
  (copyedit-ja--grep-buffers-with-dict copyedit-ja--dict-toritate-focalizer))

(defconst copyedit-ja--dict-conjunctive-particle-ga
  '(("\\([ぁ-ん]が[、，]\\)" . "\\1")))

(defun copyedit-ja-check-conjunctive-particle-ga ()
  "Find conjunctive particle \"ga\", which means either \"and\" and \"but\"."
  (interactive)
  (copyedit-ja--grep-buffers-with-dict copyedit-ja--dict-conjunctive-particle-ga))

(defconst copyedit-ja--dict-user-specified-keyword
  '(("\\(すなわち)" . "\\1")
    ("\\(ゆえに\\)" . "\\1")
    ("\\(したがって\\)" . "\\1")
    ("\\(非常に\\)" . "\\1")
    ("\\(著しく\\)" . "\\1")))

(defun copyedit-ja-check-user-specified-keyword ()
  "Find user specified keywords."
  (interactive)
  (copyedit-ja--grep-buffers-with-dict copyedit-ja--dict-user-specified-keyword))

(defconst copyedit-ja--dict-long-hiragana-sequence
  '(("\\([ぁ-ん]\\{10,\\}\\)" . "\\1")))

(defun copyedit-ja-check-long-hiragana-sequence ()
  "Find long sequence of hiragana, which can lower readability."
  (interactive)
  (copyedit-ja--grep-buffers-with-dict copyedit-ja--dict-long-hiragana-sequence))

(defconst copyedit-ja--dict-monotonous-sentence
  '(()))

(defun copyedit-ja-check-monotonous-sentence ()
  "Find monotonous, possibly dull sentence.
Not implememted yet."
  (interactive)
  (copyedit-ja--grep-buffers-with-dict copyedit-ja--dict-monotonous-sentence))

(defconst copyedit-ja--dict-beginning-of-sentence
  '(("\\(^\\cj\\|[。．]\\cj\\)" . "\\1")))

(defun copyedit-ja-enumerate-beginning-of-sentence ()
  "Enumerate beginning of sentences.
Not implemented yet."
  (interactive)
  (copyedit-ja--grep-buffers-with-dict copyedit-ja--dict-beginning-of-sentence))

(defconst copyedit-ja--dict-writing-style
  '(("\\(\\cj\\{1,5\\}[。．]\\)" . "\\1")))

(defun copyedit-ja-check-writing-style ()
  "Check writing style, by sorting sentences by the end of them.
Not implemented yet."
  (interactive)
  (copyedit-ja--grep-buffers-with-dict copyedit-ja--dict-writing-style))

(defconst copyedit-ja--dict-long-sentence
  '(("\\([。．^]\\cj\\{100,\\}[。．]\\)" . "\\1")))

(defun copyedit-ja-enumerate-long-sentence ()
  "Check sentences that are too long.
Not implemented yet."
  (interactive)
  (copyedit-ja--grep-buffers-with-dict copyedit-ja--dict-long-sentence))

(defconst copyedit-ja--dict-heading
  '(("\\(^[■□▲△▼▽●○].*?$\\)" . "\\1")))

(defun copyedit-ja-enumerate-heading ()
  "Enumerate headings.
Not implemented yet."
  (interactive)
  (copyedit-ja--grep-buffers-with-dict copyedit-ja--dict-heading))

(defconst copyedit-ja--dict-youni-negative
  '(("\\(ように[^。．]+?な[かいくけ]\\)" . "\\1")))

(defun copyedit-ja-check-youni-negative ()
  "Check \"youni...nai\"."
  (interactive)
  (copyedit-ja--grep-buffers-with-dict copyedit-ja--dict-youni-negative))

(defconst copyedit-ja--dict-multiple-negative
  '(("\\(な[かいくけ][^。．]+な[かいくけ]\\)" . "\\1")))

(defun copyedit-ja-check-multiple-negative ()
  "Check for multiple negative expressions in a sentence."
  (interactive)
  (copyedit-ja--grep-buffers-with-dict copyedit-ja--dict-multiple-negative))

(defconst copyedit-ja--dict-word-by-character-type
  '(("\\(\\)" . "\\1")))

(defun copyedit-ja-enumerate-word-by-character-type ()
  "Enumerate words by character type.
Not implemented yet."
  (interactive)
  (copyedit-ja--grep-buffers-with-dict copyedit-ja--dict-word-by-character-type))

(defun copyedit-ja-show-text-stat ()
  "Show text statistics.
Not implemented yet."
  (interactive)
  "Not implemented yet.")

;; ----------------------------------------------------------------
;; more applications

(defconst copyedit-ja--dict-unwanted-space
  `(("\\([0-9A-Za-z]\\) \\(\\cC\\|\\cK\\|\\cH\\)" . "\\1\\2")
    ("\\(\\cC\\|\\cK\\|\\cH\\) \\([0-9A-Za-z]\\)" . "\\1\\2")))

(defun copyedit-ja-check-unwanted-space ()
  "Find unwanted space between Japanese character and latin character."
  (interactive)
  (copyedit-ja--grep-buffers-with-dict copyedit-ja--dict-unwanted-space))

(defun copyedit-ja-remove-unwanted-space ()
  "Remove unwanted space in current buffer."
  (interactive)
  (copyedit-ja--perform-replace-with-dict copyedit-ja--dict-unwanted-space))

(defun copyedit-ja-remove-unwanted-space-region (start end)
  "Remove unwanted space in region."
  (interactive "r")
  (save-excursion
    (save-restriction
      (copyedit-ja--perform-replace-with-dict copyedit-ja--dict-unwanted-space
                                              start end))))

(defconst copyedit-ja--dict-alnum-fullwidth-halfwidth
  '(("０" . "0")  ("１" . "1")  ("２" . "2")  ("３" . "3")  ("４" . "4")
    ("５" . "5")  ("６" . "6")  ("７" . "7")  ("８" . "8")  ("９" . "9")
    ("Ａ" . "A")  ("Ｂ" . "B")  ("Ｃ" . "C")  ("Ｄ" . "D")  ("Ｅ" . "E")
    ("Ｆ" . "F")  ("Ｇ" . "G")  ("Ｈ" . "H")  ("Ｉ" . "I")  ("Ｊ" . "J")
    ("Ｋ" . "K")  ("Ｌ" . "L")  ("Ｍ" . "M")  ("Ｎ" . "N")  ("Ｏ" . "O")
    ("Ｐ" . "P")  ("Ｑ" . "Q")  ("Ｒ" . "R")  ("Ｓ" . "S")  ("Ｔ" . "T")
    ("Ｕ" . "U")  ("Ｖ" . "V")  ("Ｗ" . "W")  ("Ｘ" . "X")  ("Ｙ" . "Y")
    ("Ｚ" . "Z")
    ("ａ" . "a")  ("ｂ" . "b")  ("ｃ" . "c")  ("ｄ" . "d")  ("ｅ" . "e")
    ("ｆ" . "f")  ("ｇ" . "g")  ("ｈ" . "h")  ("ｉ" . "i")  ("ｊ" . "j")
    ("ｋ" . "k")  ("ｌ" . "l")  ("ｍ" . "m")  ("ｎ" . "n")  ("ｏ" . "o")
    ("ｐ" . "p")  ("ｑ" . "q")  ("ｒ" . "r")  ("ｓ" . "s")  ("ｔ" . "t")
    ("ｕ" . "u")  ("ｖ" . "v")  ("ｗ" . "w")  ("ｘ" . "x")  ("ｙ" . "y")
    ("ｚ" . "z")
    ("，" . ", ") ("．" . ". ") ("：" . ": ") ("；" . "; ")
    ("？" . "?")  ("！" . "!")
    ("゛" . nil)  ("゜" . nil)  ("´" . nil)  ("¨" . nil)
    ("＾" . "^")  ("￣" . nil)
    ("　" . "  ")))

(defun copyedit-ja-check-charwidth ()
  "Find fullwidth alnum characters."
  (interactive)
  (copyedit-ja--grep-buffers-with-dict copyedit-ja--dict-alnum-fullwidth-halfwidth))

(defun copyedit-ja-normalize-charwidth ()
  "Normalize fullwidth alnum characters to halfwidth ones."
  (interactive)
  (copyedit-ja--perform-replace-with-dict copyedit-ja--dict-alnum-fullwidth-halfwidth))

(defun copyedit-ja-normalize-charwidth-region (start end)
  "Normalize fullwidth alnum characters in region to halfwidth ones."
  (interactive "r")
  (save-excursion
    (save-restriction
      (copyedit-ja--perform-replace-with-dict copyedit-ja--dict-alnum-fullwidth-halfwidth
                                              start end))))

;; ----------------------------------------------------------------
;; Japanese style converter
;; dictionary initially based on Takeshi Urayama "e-editing using sed"

(defconst copyedit-ja--dict-distal-direct
  '(("でしょう"      . "だろう")
    ("でした"        . "だった")
    ("りました"      . "った")
    ("\\(\\cC\\)きました"  . "\\1いた")
    ; ("\\(\\cH\\)きました" . "\\1いた")
    ("\\(\\cC\\)いました" . "\\1った")
    ("ました"        . "た")
    ("\\(\\cC\\)みましょう" . "\\1もう")
    ("ましょう"      . "よう")
    ("しまいます"    . "しまう")
    ("しまいました"  . "しまった")
    ("\\(\\cC\\)います" . "\\1う")
    ("\\(\\cH\\)います" . "\\1いる")
    ("してきます"    . "してくる")
    ("してきました"  . "してきた")
    ("てきます"      . "てくる")
    ("できます"      . "できる")
    ("いきます"      . "いく")
    ("きます"        . "くる")
    ("\\(\\cC\\)はします"   . "\\1はする")
    ("\\(\\cC\\)はしません" . "\\1はしない")
    ("\\(\\cC\\cC\\)します" . "\\1する")
    ("\\(\\cC\\)します"     . "\\1す")
    ("します"        . "する")
    ("ちます"        . "つ")
    ("にます"        . "ぬ")
    ("びます"        . "ぶ")
    ("びました"      . "んだ") ; (現在+過去)*(肯定+否定)*(活用形)*(活用形の種類) ; 順序に注意
    ("てみます"      . "てみる")
    ("みます"        . "む")
    ("ります"        . "る")
    ("えます"        . "える")
    ("けます"        . "ける")
    ("せます"        . "せる")
    ("てます"        . "てる")
    ("ねます"        . "ねる")
    ("べます"        . "べる")
    ("めます"        . "める")
    ("れます"        . "れる")
    ("しまいません"  . "しまわない")
    ("てきませんでしたか" . "てこなかっただろうか")
    ("てきませんでした" . "てこなかった")
    ("てきません"    . "てこない")
    ("できません"    . "できない")
    ("\\(\\cC\\)いませんでした" . "\\1わなかった")
    ("いませんでした". "いなかった")
    ("いません"      . "いない")
    ("\\(\\cH\\)ませんでした" . "\\1なかった")
    ("きません"      . "かない")
    ("ぎませんでした" . "ぎなかった")
    ("りませんでした" . "らなかった")
    ("ぎません"      . "ぎない")
    ("ちません"      . "たない")
    ("にません"      . "なない")
    ("びません"      . "ばない")
    ("みません"      . "まない")
    ("\\(\\cC\\cC\\)しましょう"     . "\\1しよう")
    ("\\(\\cC\\cC\\)します"         . "\\1する")
    ("\\(\\cC\\cC\\)しました"       . "\\1した")
    ("\\(\\cC\\cC\\)しませんでした" . "\\1しなかった")
    ("\\(\\cC\\cC\\)しません"       . "\\1しない")
    ("できましょう"  . "できよう")
    ("できます"      . "できる")
    ("できました"    . "できた")
    ("できませんでした" . "できなかった")
    ("できません"    . "できない")
    ("ありませんでした" . "なかった")
    ("ありません"    . "ない")
    ("りませんでした" . "らなかった")
    ("りません"      . "らない")
    ("えません"      . "えない")
    ("けません"      . "けない")
    ("せません"      . "せない")
    ("てません"      . "てない")
    ("ねません"      . "ねない")
    ("べません"      . "べない")
    ("めません"      . "めない")
    ("れません"      . "れない")
    ("\\(\\cC\\)いません"            . "\\1わない")
    ("\\(\\cH\\)いませんでした"      . "\\1いなかった")
    ("\\(\\cH\\)いません"            . "\\1いない")
    ; ("しません" . "さない") ; 生かしません=>生かさない 消費はしません=>消費はしない ; FIXME
    ("\\(\\cC\\)しません"            . "\\1さない")
    ("\\(\\cC\\cC\\)しません"        . "\\1しない")
    ("\\(\\cH\\)しません"            . "\\1しない")
    ("ください"      . "ほしい")
    ("んですか"      . "のか")
    ("どうですか"    . "どうだい")
    ("ですから"      . "だから")
    ("\\(\\cC\\)ですか" . "\\1かい")
    ("たいですか"    . "たいか")
    ("ですか"        . "だい")
    ("です"          . "だ")
    ("ますます"      . "ますます")
    ("\\([来]\\)ます" . "\\1る")
    ("\\(\\cC\\)ます" . "\\1ませる")
    ("\\(\\cC\\)ません" . "\\1ない")
    ("ます"          . "る")
    ("ません"        . "しない")
    ))

(defun copyedit-ja-check-desumasu-to-dadearu ()
  "Check style of Japanese text if distal (desu/masu)."
  (interactive)
  (copyedit-ja--grep-buffers-with-dict copyedit-ja--dict-distal-direct))

(defun copyedit-ja-convert-desumasu-to-dadearu ()
  "Change style of Japanese text from distal (desu/masu) to direct (da/dearu)."
  (interactive)
  (copyedit-ja--perform-replace-with-dict copyedit-ja--dict-distal-direct))

(defun copyedit-ja-convert-desumasu-to-dadearu-region (start end)
  (interactive "r")
  (save-excursion
    (save-restriction
      (copyedit-ja--perform-replace-with-dict copyedit-ja--dict-distal-direct
                                              start end))))

(defconst copyedit-ja--dict-direct-distal
  (let ((rx-grp "\\(\\\\(.*\\\\)\\)\\(.*\\)")
        (rx-ref "\\(\\\\[0-9]\\)\\(.*\\)"))
    (mapcar (lambda (pair)
              (let* ((key (car pair))
                     (val (cdr pair))
                     (kparts (if (string-match rx-grp key)
                                 (cons (match-string 1 key) (match-string 2 key))
                               (cons "" key)))
                     (vparts (if (string-match rx-ref val)
                                 (cons (match-string 1 val) (match-string 2 val))
                               (cons "" val)))
                     (src (concat (car kparts) (cdr vparts)))
                     (tgt (concat (car vparts) (cdr kparts))))
                (cons src tgt)))
            copyedit-ja--dict-distal-direct)))

(defun copyedit-ja-check-dadearu-to-desumasu ()
  "Check style of Japanese text if direct (da/dearu)."
  (interactive)
  (copyedit-ja--grep-buffers-with-dict copyedit-ja--dict-direct-distal))

(defun copyedit-ja-convert-dadearu-desumasu ()
  "Change style of Japanese text from direct (da/dearu) to distal (desu/masu)."
  (interactive)
  (copyedit-ja--perform-replace-with-dict copyedit-ja--dict-direct-distal))

(defun copyedit-ja-convert-dadearu-to-desumasu-region (start end)
  (interactive "r")
  (save-excursion
    (save-restriction
      (copyedit-ja--perform-replace-with-dict copyedit-ja--dict-direct-distal
                                              start end))))

;; ----------------------------------------------------------------
;; katakana and hiragana conversion

(defconst copyedit-ja--katakana
  (let ((str (concat "ァ ア ィ イ ゥ ウ ェ エ ォ オ "
                     "カ ガ キ ギ ク グ ケ ゲ コ ゴ "
                     "サ ザ シ ジ ス ズ セ ゼ ソ ゾ "
                     "タ ダ チ ヂ ッ ツ ヅ テ デ ト ド "
                     "ナ ニ ヌ ネ ノ "
                     "ハ バ パ ヒ ビ ピ フ ブ プ ヘ ベ ペ ホ ボ ポ "
                     "マ ミ ム メ モ "
                     "ャ ヤ ュ ユ ョ ヨ "
                     "ラ リ ル レ ロ "
                     "ヮ ワ ヰ ヱ ヲ ン "
                     "ヴ ヵ ヶ ヷ ヸ ヹ ヺ ヽ ヾ")))
    (split-string str " ")))

(defconst copyedit-ja--hiragana
  (let ((str (concat "ぁ あ ぃ い ぅ う ぇ え ぉ お "
                     "か が き ぎ く ぐ け げ こ ご "
                     "さ ざ し じ す ず せ ぜ そ ぞ "
                     "た だ ち ぢ っ つ づ て で と ど "
                     "な に ぬ ね の "
                     "は ば ぱ ひ び ぴ ふ ぶ ぷ へ べ ぺ ほ ぼ ぽ "
                     "ま み む め も "
                     "ゃ や ゅ ゆ ょ よ "
                     "ら り る れ ろ "
                     "ゎ わ ゐ ゑ を ん "
                     "ゔ ゕ ゖ わ゙ ゐ゙ ゑ゙ を゙ ゝ ゞ")))
    (split-string str " ")))

(defconst copyedit-ja--dict-katakana-hiragana
  (mapcar (lambda (ls) (cons (car ls) (cadr ls)))
          (copyedit-ja--zip-naive copyedit-ja--katakana copyedit-ja--hiragana)))

(defconst copyedit-ja--dict-hiragana-katakana
  (mapcar (lambda (ls) (cons (car ls) (cadr ls)))
          (copyedit-ja--zip-naive copyedit-ja--hiragana copyedit-ja--katakana)))

(defun copyedit-ja--katakana-to-hiragana (str)
  (mapconcat (lambda (s)
               (copyedit-ja--translate s copyedit-ja--dict-katakana-hiragana))
             (split-string str "" t)
             ""))
(defun copyedit-ja--hiragana-to-katakana (str)
  (mapconcat (lambda (s)
               (copyedit-ja--translate s copyedit-ja--dict-hiragana-katakana))
             (split-string str "" t)
             ""))

(defun copyedit-ja-katakana-to-hiragana-region (begin end)
  "Convert katakana in region to hiragana."
  (interactive "r")
  (copyedit-ja--filter-region #'copyedit-ja--katakana-to-hiragana begin end))

(defun copyedit-ja-hiragana-to-katakana-region (begin end)
  "Convert hiragana in region to katakana."
  (interactive "r")
  (copyedit-ja--filter-region #'copyedit-ja--hiragana-to-katakana begin end))

;; ----------------------------------------------------------------
;; Aquire reading of Japanese text

(defun copyedit-ja--get-reading-katakana (str)
  "Acquire reading of Japanese text in katakana using MeCab."
  (copyedit-ja--shell-command-string "mecab" str "--output-format-type=yomi"))

(defun copyedit-ja--get-reading-hiragana (str)
  "Acquire reading of Japanese text in hiragana using MeCab."
  (copyedit-ja--katakana-to-hiragana (copyedit-ja--get-reading-katakana str)))

;; ----------------------------------------------------------------
;; hiragana, katakna, and kanji to katakana/hiragana conversion

(defun copyedit-ja-kanakanji-to-katakana-region (begin end)
  "Convert hiragana, katakna, and kanji in region to katakana."
  (interactive "r")
  (copyedit-ja--filter-region #'copyedit-ja--get-reading-katakana begin end))

(defun copyedit-ja-kanakanji-to-hiragana-region (begin end)
  "Convert hiragana, katakna, and kanji in region to hiragana."
  (interactive "r")
  (copyedit-ja--filter-region #'copyedit-ja--get-reading-hiragana begin end))

;; ----------------------------------------------------------------
;; miscellaneous applications

(defconst copyedit-ja--dict-notation-hou
  '(("\\([^一両双前後左右上下]\\|^\\)\\(方\\)\\([^法式向面]\\|$\\)" .
     "\\1ほう\\3")))

(defun copyedit-ja-check-notation-hou ()
  "Check notation (hou)."
  (interactive)
  (copyedit-ja--grep-buffers-with-dict copyedit-ja--dict-notation-hou))

(defconst copyedit-ja--dict-numeric
  '(("\\([0-9一二三四五六七八九十][つ番]\\)[め]" . "\\1目")))

(defun copyedit-ja-check-numeric()
  "Check Japanese numeric"
  (interactive)
  (copyedit-ja--grep-buffers-with-dict copyedit-ja--dict-numeric))

(defun copyedit-ja-normalize-numeric()
  "Normalize Japanese numeric"
  (interactive)
  (copyedit-ja--perform-replace-with-dict copyedit-ja--dict-numeric))

;; ----------------------------------------------------------------

(provide 'copyedit-ja)
