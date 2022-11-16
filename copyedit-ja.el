;;; copyedit-ja.el --- Copyediting tool for Japanese text
;; -*- encoding: utf-8; lexical-binding: t -*-
;;
;; Copyright (c) 2008-2021 Hisashi Morita
;; License: Public Domain
;;
;;; Commentary:
;;
;; Synopsis:
;;   1. Check possible typos.
;;      M-x copyedit-ja-check-typo-particles
;;   2. Edit them.
;;      M-x copyedit-ja-normalize-typo-particles
;;   (See color-moccur for detail, since copyedit-ja is basically just a
;;   wrapper around color-moccur with preset regexp patterns.)
;;
;; Features:
;;   Copyedit-ja helps checking and resolving issues with:
;;     * Possible typos (successive ga, no, ni, wo, etc.)
;;     * Character widths (numerals, parentheses, etc.)
;;     * Matching of opening and closing parentheses
;;     * Style (direct style (da-dearu) and distal style (desu-masu))
;;     * Commas and periods (ten-maru and kan-piri)
;;     * Uses of prolonged sound marks (onbiki)
;;     * Space characters between latin text and Japanese text,
;;   etc.
;;   Try M-x copyedit-ja- TAB to list available commands.
;;
;; Requirements:
;;   * color-moccur: multi-buffer occur (grep) mode
;;     - v2.71 (MELPA):
;;       https://melpa.org/#/color-moccur
;;       (source: https://github.com/myuhe/color-moccur.el)
;;     - v2.73 from the original author:
;;       https://web.archive.org/web/20180527232131/www.bookshelf.jp/elc/color-moccur.el
;;   * MeCab: Yet another Japanese morphological analyzer (optional)
;;     - https://github.com/taku910/mecab
;;
;; Installation:
;;   1. Install requirements.
;;      - Install color-moccur using Emacs package manager.
;;        (See https://melpa.org for instructions.)
;;      - Install MeCab and its UTF-8 dictionary, e.g.
;;        $ sudo apt install mecab mecab-ipadic-utf8
;;   2. Install copyedit-ja.
;;      - Put copyedit-ja.el somewhere in your load-path, e.g.
;;        $ cp copyedit-ja.el ~/.emacs.d/lisp/
;;      - If you prefer, load copyedit-ja on startup, e.g.
;;        $ echo "(require 'copyedit-ja)" >> ~/emacs.d/init.el
;;   3. Configure copyedit-ja (optional).
;;      - Tweak dictionaries to fit your needs in your init file, e.g.
;;        (setq copyedit-ja--dict-user-specified-keywords
;;              (append copyedit-ja--dict-user-specified-keywords
;;                      '(("TCP-IP". "TCP/IP"))))
;;
;; References:
;;   Copyedit-ja is inspired and heavily influenced by respectable forerunners'
;;   works.
;;   * [KSU1989]
;;     Masanori Kurata, Akira Suganuma, Kazuo Ushijima,
;;     "Development of a System of Writing Tools for Japanese Documents on
;;     a Personal Computer.",
;;     Computer Software, Vol. 6, No. 4 (1989), pp. 55-67.
;;       - https://ci.nii.ac.jp/naid/110003743558
;;       - https://www.jstage.jst.go.jp/article/jssst/6/4/6_4_373/_article/-char/ja
;;   * [Urayama1998]
;;     Takeshi Urayama, Denshi Henshu no Susume: sed no Katsuyou
;;     (E-editing using Sed), 1998, Doseisha.
;;       - https://ci.nii.ac.jp/ncid/BA38403160

(require 'cl-lib)
(require 'color-moccur)

;; ------------------------------------------------------------------------
;; Utilities: perform-replace-using-dict

(defun copyedit-ja--group-sequence (seq &optional &key key)
  "Group SEQ elements to lists of same sucessive elements.
&KEY is used for comparing elements if given.

Example:
  (copyedit-ja--group-sequence '(a a a))    ;=> '((a a a))
  (copyedit-ja--group-sequence '(a a b))    ;=> '((a a) (b))
  (copyedit-ja--group-sequence '(a a b a a) ;=> '((a a) (b) (a a))"
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

(defun copyedit-ja--regexp-opt-re-or (seq)
  "Group together string sequence SEQ.
Regular characters are grouped to a character class, while regexp special
characters and longer strings are grouped to an \"or\" expression.

Example:
  (copyedit-ja--regexp-opt-re-or '(\"a\" \"b\")) ;=> \"[ab]\"
  (copyedit-ja--regexp-opt-re-or '(\"a\" \".\")) ;=> \"a\\|.\""
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
  "Generate a (naively) optimized regexp from string/regexp list LS.
Unlike standard `regexp-opt', this function accepts regexps as its input.

Example:
  (copyedit-ja--regexp-opt-re '(\"a\" \"b\" \".\" \"cd+\")) ;=> \"[ab]\\|.\\|cd+\""
  (let* ((regexp-charclassable
          (lambda (str) (and (stringp str)
                             (= 1 (length str))
                             (equal str (regexp-quote str)))))
         (regexp-or
          ;; '("a" "b") => "a\\|b"
          (lambda (strs) (mapconcat #'identity strs "\\|")))
         (grouped
          (copyedit-ja--group-sequence ls
                                       :key (lambda (a b)
                                              (cl-every regexp-charclassable
                                                        (list a b)))))
         (wrapped-up
          (mapcar #'copyedit-ja--regexp-opt-re-or grouped))
         (or-ed (funcall regexp-or wrapped-up)))
    or-ed))

(defun copyedit-ja--assoc-re-exact (str dict)
  "Return the first entry of DICT whose key exactly matches STR.
DICT keys are regexps, while values are strings or functions.

Example:
  (copyedit-ja--assoc-re-exact
    \"foo\"
    '((\"foobar\" . \"Foobar\")
      (\"bar\"    . \"Bar\")
      (\"[a-z]+\" . (lambda (s) (upcase s)))))
  ;=> (\"[a-z]+\" . (lambda (s) (upcase s)))"
  (cl-assoc str dict
            :test (lambda (s r)
                    (let ((external-md (match-data)))
                      (unwind-protect
                          (if (string-match r s) (equal (match-string 0 s) s) nil)
                        (set-match-data external-md))))))

(defun copyedit-ja--sort-dict (dict)
  "Sort DICT by key type (regular strings first, regexps last)
and by key length (longest first and shortest last).

Example:
  (copyedit-ja--sort-dict
    '((\"bar\"    . \"Bar\")
      (\"[a-z]+\" . (lambda (s) (upcase s)))
      (\"foobar\" . \"Foobar\")))
  ;=>
    '((\"foobar\" . \"Foobar\")
      (\"bar\"    . \"Bar\")
      (\"[a-z]+\" . (lambda (s) (upcase s))))"
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
    (sort (copy-alist dict) strings-to-regexps-and-long-to-short)))

(defun copyedit-ja--find-replacement (str dict)
  "Return DICT value whose key exactly matches STR.
If none found, return STR.

DICT keys are regexps (including plain strings), while values are
strings or functions.  DICT must be sorted by key type (plain
strings to regexps) and by key length (long to short).

Example:
  (copyedit-ja--find-replacement
    \"foo\"
    '((\"foobar\" . \"Foobar\")
      (\"bar\"    . \"Bar\")
      (\"[a-z]+\" . (lambda (s) (upcase s)))))
  ;=> \"FOO\""
  (let* ((case-fold-search nil)
         (matching-entry (copyedit-ja--assoc-re-exact str dict)))
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

(defun copyedit-ja--perform-replace-using-dict (dict &optional start end)
  "Invoke `perform-replace' using DICT on region between START and END.

Example:
  (copyedit-ja--perform-replace-using-dict
    '((REGEXP1 . REPLACEMENT1)
      (REGEXP2 . REPLACEMENT2)
      ...)
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
;; Utilities: shell-command-string

(defun copyedit-ja--shell-command-string (cmd input &rest opts)
  "Run CMD (with OPTS) as sub-process with INPUT for stdin.
Return stdout as string.

Example:
  (copyedit-ja--shell-command-string \"grep\" STRING \"-in\")

Note:
  If something gets wrong when dealing with non-ASCII strings,
  try setting character encoding explicitly, e.g.
  (progn (prefer-coding-system 'utf-8-unix) ...)"
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
;; Miscllaneous utilities

(defun copyedit-ja--grep-buffers-using-dict (dict)
  "Search buffers for DICT keys.

Example:
  (copyedit-ja--grep-buffers-using-dict
    '((\"foobar\" . \"Foobar\")
      (\"bar\"    . \"Bar\")
      (\"[a-z]+\" . (lambda (s) (upcase s)))))"
  (let ((regexp-src (copyedit-ja--regexp-opt-re (mapcar 'car dict))))
    (moccur regexp-src t)))

(defun copyedit-ja--zip-lists (&rest seq)
  "Zip SEQ of lists and return a list.

Example:
  (copyedit-ja--zip-lists '(1 2 3) '(4 5)) ;=> '((1 4) (2 5) (3 nil))"
  (if (cl-every #'null (mapcar 'car seq))
      '()
    (cons (mapcar 'car seq) (apply #'copyedit-ja--zip-lists (mapcar 'cdr seq)))))

(defun copyedit-ja--translate (str dict)
  "Return a DICT value corresponding to STR.
If no matching key is found, return the original STR.

Example:
  (copyedit-ja--translate
   \"foo\"
   '((\"foo\" . \"Foo\")
     (\"bar\" . \"Bar\")))
  ;=> \"Foo\"

See `copyedit-ja--find-replacement' and `copyedit-ja--sort-dict' for detail."
  (copyedit-ja--find-replacement str (copyedit-ja--sort-dict dict)))

(defun copyedit-ja--filter-region (f start end)
  "Destructively apply function F to the region between START and END."
  (let* ((src (buffer-substring start end))
         (replacement (funcall f src)))
    (progn
      (delete-region start end)
      (insert replacement))))

;; ------------------------------------------------------------------------
;; Checking parentheses (inspired by [KSU1989])

(defconst copyedit-ja--dict-paren-width
  '(("\\(\\cj\\) ?(\\([^()]*?\\)) ?\\(\\cj\\)" . "\\1（\\2）\\3")
    ("\\(\\cj\\) ?(" . "\\1（")
    (") ?\\(\\cj\\)" . "）\\1"))
  "Dictionary of half-width parentheses in Japanese text to their full-width
counterparts.")

(defconst copyedit-ja--dict-paren-width-paranoid
  '(("\\(^\\|\\cj\\) ?(\\([^()]*?\\)) ?\\(\\cj\\|$\\)" . "\\1（\\2）\\3")
    ("\\(^\\|\\cj\\) ?(" . "\\1（")
    (") ?\\(\\cj\\|$\\)" . "）\\1"))
  "Dictionary of half-width parentheses in Japanese text to their full-width
counterparts.

(Unlike `copyedit-ja--dict-paren-width', this dictionary matches parens
at start-of-line and/or end-of-line as well, that is normally excessive.)")

(defun copyedit-ja-check-paren-width (&optional paranoid)
  "Check the widths of parenthesis characters considering contexts.

If PARANOID is non-nil, paranoiac dictionary
`copyedit-ja--dict-paren-width-paranoid' is used."
  (interactive "P")
  (let ((dict (if paranoid
                  copyedit-ja--dict-paren-width-paranoid
                copyedit-ja--dict-paren-width)))
    (copyedit-ja--grep-buffers-using-dict dict)))

(defun copyedit-ja-normalize-paren-width (&optional paranoid)
  "Replace half-width parentheses with their full-width counterparts.

If region is active, application is restricted to the region.

If PARANOID is non-nil, paranoiac dictionary
`copyedit-ja--dict-paren-width-paranoid' is used."
  (interactive "P")
  (let ((start (if (use-region-p) (region-beginning) nil))
        (end (if (use-region-p) (region-end) nil))
        (f #'copyedit-ja--perform-replace-using-dict)
        (dict (if paranoid
                  copyedit-ja--dict-paren-width-paranoid
                copyedit-ja--dict-paren-width)))
    (if (use-region-p)
        (save-mark-and-excursion
          (save-restriction
            (narrow-to-region start end)
            (goto-char (point-min))
            (funcall f dict)))
      (funcall f dict))))

(defconst copyedit-ja--dict-paren-matching
  '(("\\(([^)]*?）\\)" . "\\1")
    ("\\(（[^）]*?)\\)" . "\\1"))
  "Dictionary of unmatched parentheses (opening and closing parens are of
different width).")

(defun copyedit-ja-check-paren-matching ()
  "Check if opening and closing parentheses match."
  (interactive)
  (copyedit-ja--grep-buffers-using-dict copyedit-ja--dict-paren-matching))

;; ------------------------------------------------------------------------
;; Checking punctuation symbols (inspired by [KSU1989])

(defconst copyedit-ja--dict-punctuation-tenmaru
  '(("\\(\\cj\\),"   . "\\1、")
    ("\\(\\cj\\)\\." . "\\1。")
    ("，" . "、")
    ("．" . "。"))
  "Dictionary to normalize punctuation characters: commas/periods to ten/maru.")

(defconst copyedit-ja--dict-punctuation-kanpiri
  '(("\\(\\cj\\),"   . "\\1，")
    ("\\(\\cj\\)\\." . "\\1．")
    ("、" . "，")
    ("。" . "．"))
  "Dictionary to normalize punctuation characters: commas/periods and ten/maru
to full-width commas/periods.")

(defun copyedit-ja-check-punctuation-tenmaru ()
  "Check irregular punctuations, preferring ten-maru."
  (interactive)
  (copyedit-ja--grep-buffers-using-dict copyedit-ja--dict-punctuation-tenmaru))

(defun copyedit-ja-check-punctuation-kanpiri ()
  "Check irregular punctuations preferring kan-piri."
  (interactive)
  (copyedit-ja--grep-buffers-using-dict copyedit-ja--dict-punctuation-kanpiri))

(defun copyedit-ja-normalize-punctuation-tenmaru ()
  "Normalize irregular punctuations, preferring ten and maru."
  (interactive)
  (copyedit-ja--perform-replace-using-dict copyedit-ja--dict-punctuation-tenmaru))

(defun copyedit-ja-normalize-punctuation-kanpiri ()
  "Normalize irregular punctuations, preferring commas and dots."
  (interactive)
  (copyedit-ja--perform-replace-using-dict copyedit-ja--dict-punctuation-kanpiri))

;; ------------------------------------------------------------------------
;; Checking hiragana-katakana prolonged sound marks (inspired by [KSU1989])

(defconst copyedit-ja--dict-suspicious-prolonged-sound-marks
  '(("\\([^ァ-ンヴヵヶ]ー\\)" . "\\1"))
  "Dictionary to find irregular usage of prolonged sound marks (onbiki).
(They typically appear after katakana characters in technical documents,
although it is not the case with literature, etc.)")

(defun copyedit-ja-check-suspicious-prolonged-sound-marks ()
  "Check suspicious uses of katakana-hiragana prolonged sound marks (onbiki)."
  (interactive)
  (copyedit-ja--grep-buffers-using-dict copyedit-ja--dict-suspicious-prolonged-sound-marks))

(defun copyedit-ja-normalize-suspicious-prolonged-sound-marks ()
  "Normalize suspicious uses of katakana-hiragana prolonged sound marks (onbiki)."
  (interactive)
  (copyedit-ja--perform-replace-using-dict copyedit-ja--dict-suspicious-prolonged-sound-marks))

;; ------------------------------------------------------------------------
;; Checking typos (inspired by [KSU1989])

(defconst copyedit-ja--dict-typo-particles
  '(("\\([てにをはのが]\\)\\1" . "\\1"))
  "Dictionary to find possible typos, namely successive particles (joshi).")

(defun copyedit-ja-check-typo-particles ()
  "Check possible typos in particles."
  (interactive)
  (copyedit-ja--grep-buffers-using-dict copyedit-ja--dict-typo-particles))

(defun copyedit-ja-normalize-typo-particles ()
  "Normalize possible typos in particles."
  (interactive)
  (copyedit-ja--perform-replace-using-dict copyedit-ja--dict-typo-particles))

(defconst copyedit-ja--dict-passive-voices
  '(("\\([あかさたなはまやらわ]ら?れ\\)" . "\\1"))
  "Dictionary to find passive voices (judoutai).")

;; ------------------------------------------------------------------------
;; Checking writing style (inspired by [KSU1989])

(defun copyedit-ja-check-passive-voices ()
  "Find passive voice, which are sometimes misused."
  (interactive)
  (copyedit-ja--grep-buffers-using-dict copyedit-ja--dict-passive-voices))

(defconst copyedit-ja--dict-demonstratives
  '(("\\([こそあど][れの]\\)" . "\\1"))
  "Dictionary to find demonstratives (shijishi).")

(defun copyedit-ja-check-demonstratives ()
  "Find demonstratives (kore, soer, are, dore, etc.)."
  (interactive)
  (copyedit-ja--grep-buffers-using-dict copyedit-ja--dict-demonstratives))

(defconst copyedit-ja--dict-toritate-focalizers
  '(("\\(こそ\\)" . "\\1")
    ("\\(すら\\)" . "\\1")
    ("\\(さえ\\)" . "\\1")
    ("\\(だけ\\)" . "\\1")
    ("\\(でも\\)" . "\\1")
    ("\\(まで\\)" . "\\1")
    ("\\(など\\)" . "\\1")
    ("\\([くぐ]らい\\)" . "\\1")
    ("\\(ばかり\\)" . "\\1")
    ("\\(だって\\)" . "\\1"))
  "Dictionary to find modifier particles (toritateshi).")

(defun copyedit-ja-check-toritate-focalizers ()
  "Find toritate focalizers."
  (interactive)
  (copyedit-ja--grep-buffers-using-dict copyedit-ja--dict-toritate-focalizers))

(defconst copyedit-ja--dict-conjunctive-particles-ga
  '(("\\([ぁ-ん]が[、，]\\)" . "\\1"))
  "Dictionary to find conjunctive particles (setsuzoku joshi) \"ga\",
which may introduce ambiguity.")

(defun copyedit-ja-check-conjunctive-particles-ga ()
  "Find conjunctive particle \"ga\", which means either \"and\" and \"but\"."
  (interactive)
  (copyedit-ja--grep-buffers-using-dict copyedit-ja--dict-conjunctive-particles-ga))

(defvar copyedit-ja--dict-user-specified-keywords
  '(("\\(すなわち\\)" . "\\1")
    ("\\(ゆえに\\)" . "\\1")
    ("\\(したがって\\)" . "\\1")
    ("\\(非常に\\)" . "\\1")
    ("\\(著しく\\)" . "\\1"))
  "Dictionary to find user-specified keywords. Preset values include several
adjectives which are sometimes overused.")

(defun copyedit-ja-check-user-specified-keywords ()
  "Find user specified keywords."
  (interactive)
  (copyedit-ja--grep-buffers-using-dict copyedit-ja--dict-user-specified-keywords))

(defconst copyedit-ja--dict-long-hiragana-sequences
  '(("\\([ぁ-ん]\\{10,\\}\\)" . "\\1"))
  "Dictionary to find long hiragana sequences.")

(defun copyedit-ja-check-long-hiragana-sequences ()
  "Find long sequences of hiragana, which can lower readability."
  (interactive)
  (copyedit-ja--grep-buffers-using-dict copyedit-ja--dict-long-hiragana-sequences))

(defun copyedit-ja-check-monotonous-sentences ()
  "Find monotonous, possibly dull sentences.
Not implememted yet."
  (interactive)
  (message "Not implemented yet."))

(defun copyedit-ja-enumerate-beginnings-of-sentences ()
  "Enumerate beginnings of sentences.
Not implemented yet."
  (interactive)
  (message "Not implemented yet."))

(defun copyedit-ja-check-writing-style ()
  "Check writing style, by sorting sentences by the end of them.
Not implemented yet."
  (interactive)
  (message "Not implemented yet."))

(defun copyedit-ja-enumerate-long-sentences ()
  "Check sentences that are too long.
Not implemented yet."
  (interactive)
  (message "Not implemented yet."))

;; ------------------------------------------------------------------------
;; Document analysis (inspired by [KSU1989])

(defconst copyedit-ja--dict-headings
  '(("\\(^#\\{1,6\\} .+$\\)" . "\\1"))
  "Dictionary to find headings.
(Markdown ATX headings are supported for now.)")

(defun copyedit-ja-enumerate-headings ()
  "Enumerate headings."
  (interactive)
  (copyedit-ja--grep-buffers-using-dict copyedit-ja--dict-headings))

(defconst copyedit-ja--dict-youni-negatives
  '(("\\(ように[^。．]+?な[かいくけ]\\)" . "\\1"))
  "Dictionary to find \"youni\" followed by negative form.")

(defun copyedit-ja-check-youni-negatives ()
  "Check \"youni...nai\"."
  (interactive)
  (copyedit-ja--grep-buffers-using-dict copyedit-ja--dict-youni-negatives))

(defconst copyedit-ja--dict-multiple-negatives
  '(("\\(な[かいくけ][^。．]+な[かいくけ]\\)" . "\\1"))
  "Dictionary to find double negative expressions (nijuu hitei).")

(defun copyedit-ja-check-multiple-negatives ()
  "Check for multiple negative expressions in a sentence."
  (interactive)
  (copyedit-ja--grep-buffers-using-dict copyedit-ja--dict-multiple-negatives))

(defconst copyedit-ja--dict-word-by-character-type-kanji
  (let ((pat
         (concat
          "["
          "\U00004e00-\U00009ffc" ; CJK Unified Ideographs
          "\U00003400-\U00004dbf" ; CJK Unified Ideographs Extension A
          "\U00020000-\U0002a6dd" ; CJK Unified Ideographs Extension B
          "\U0002a700-\U0002b734" ; CJK Unified Ideographs Extension C
          "\U0002b740-\U0002b81d" ; CJK Unified Ideographs Extension D
          "\U0002b820-\U0002cea1" ; CJK Unified Ideographs Extension E
          "\U0002ceb0-\U0002ebe0" ; CJK Unified Ideographs Extension F
          "\U00030000-\U0003134a" ; CJK Unified Ideographs Extension G
          "\U0000f900-\U0000faff" ; CJK Compatibility Ideographs
          "\U0002f800-\U0002fa1f" ; CJK Compatibility Ideographs Supplement
          "\U00002e80-\U00002ef3" ; CJK Radicals Supplement
          "\U00002f00-\U00002fd5" ; Kangxi Radicals
          "\u3005" ; Ideographic Iteration Mark (Odoriji) (々)
          "\u3006" ; Ideographic Closing Mark (Shime) (〆)
          "\u3007" ; Ideographic Number Zero (Kanji Zero) (〇)
          "\u303b" ; Vertical Ideographic Iteration Mark (Odoriji) (〻)
          "]+")))
    `((,pat . "\\1")))
  "Dictionary to find kanji.")

(defun copyedit-ja-enumerate-words-by-character-type-kanji ()
  "Enumerate words by character type (kanji)."
  (interactive)
  (copyedit-ja--grep-buffers-using-dict copyedit-ja--dict-word-by-character-type-kanji))

(defconst copyedit-ja-dict--word-by-character-type-hiragana
  (let ((pat
         (concat
          "["
          "\u3041-\u3096\u3099-\u309f" ; Hiragana Letters (Hiragana)
          "\u30a0" ; Katakana-Hiragana Double Hyphen (Katakana)
          "\u30fc" ; Katakana-Hiragana Prolonged Soundmark
          "\U0001b001" ; Hiragana Letter Archaic Ye (Kana Supplement)
          "\U0001b002-\U0001b0ff" ; Hentaigana (Kana Supplement)
          "\U0001b100-\U0001b11e" ; Hentaigana (Kana Extended A)
          "\U0001b150-\U0001b152" ; Historic small hiragana letters (Small Kana Extension)
          "]+")))
    `((,pat . "\\1")))
  "Dictionary to find hiragana.")

(defun copyedit-ja-enumerate-word-by-character-type-hiragana ()
  "Enumerate words by character type (hiragana)."
  (interactive)
  (copyedit-ja--grep-buffers-using-dict copyedit-ja-dict--word-by-character-type-hiragana))

(defconst copyedit-ja-dict--word-by-character-type-katakana
  (let ((pat
         (concat
          "["
          "\u30a1-\u30ff" ; Katakana Letters (Katakana)
          "\u30a0" ; Katakana-Hiragana Double Hyphen (Katakana)
          "\u31f0-\u31ff" ; Phonetic extensions for Ainu (Katakana Phonetic Extensions)
          "\U0001b000" ; Katakana Letter Archaic E (Kana Supplement)
          "\U0001b164-\U0001b167" ; Historic small katakana letters (Small Kana Extension)
          "\uff65-\uff9f" ; Halfwidth Katakana variants (Halfwidth and Fullwidth Forms)
          "]+")))
    `((,pat . "\\1")))
  "Dictionary to find katakana.")

(defun copyedit-ja-enumerate-word-by-character-type-katakana ()
  "Enumerate words by character type (katakana)."
  (interactive)
  (copyedit-ja--grep-buffers-using-dict copyedit-ja-dict--word-by-character-type-katakana))

(defun copyedit-ja-show-text-stats ()
  "Show text statistics.
Not implemented yet."
  (interactive)
  (message "Not implemented yet."))

;; ----------------------------------------------------------------
;; Removing extraneous spaces

(defconst copyedit-ja--dict-unwanted-spaces
  `(("\\([0-9A-Za-z]\\) \\(\\cC\\|\\cK\\|\\cH\\)" . "\\1\\2")
    ("\\(\\cC\\|\\cK\\|\\cH\\) \\([0-9A-Za-z]\\)" . "\\1\\2"))
  "Dictionary to normalize unwanted space characters in Japanese text.")

(defun copyedit-ja-check-unwanted-spaces ()
  "Find unwanted spaces between Japanese character and latin character."
  (interactive)
  (copyedit-ja--grep-buffers-using-dict copyedit-ja--dict-unwanted-spaces))

(defun copyedit-ja-remove-unwanted-spaces ()
  "Remove unwanted spaces in current buffer.

If region is active, application is restricted to the region."
  (interactive)
  (let ((start (if (use-region-p) (region-beginning) nil))
        (end (if (use-region-p) (region-end) nil))
        (f #'copyedit-ja--perform-replace-using-dict)
        (dict copyedit-ja--dict-unwanted-spaces))
    (if (use-region-p)
        (save-mark-and-excursion
          (save-restriction
            (narrow-to-region start end)
            (goto-char (point-min))
            (funcall f dict)))
      (funcall f dict))))

;; ----------------------------------------------------------------
;; Normalizing character widths

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
    ("　" . "  "))
  "Dictionary to translate full-width numbers, alphabets, punctuations and
symbols to their half-width counterparts.")

(defun copyedit-ja-check-charwidth ()
  "Find fullwidth alphabets and numbers."
  (interactive)
  (copyedit-ja--grep-buffers-using-dict copyedit-ja--dict-alnum-fullwidth-halfwidth))

(defun copyedit-ja-normalize-charwidth ()
  "Normalize fullwidth alphabets and numbers to their halfwidth counterparts.

If region is active, application is restricted to the region."
  (interactive)
  (let ((start (if (use-region-p) (region-beginning) nil))
        (end (if (use-region-p) (region-end) nil))
        (f #'copyedit-ja--perform-replace-using-dict)
        (dict copyedit-ja--dict-alnum-fullwidth-halfwidth))
    (if (use-region-p)
        (save-mark-and-excursion
          (save-restriction
            (narrow-to-region start end)
            (goto-char (point-min))
            (funcall f dict)))
      (funcall f dict))))

;; ----------------------------------------------------------------
;; Converting style between distal and direct
;; (Dictionary initially based on [Urayama1998].)

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
    )
    "Dictionary to translate distal style (keitai, desu/masu) to direct style
(jotai, da/dearu).")

(defun copyedit-ja-check-desumasu ()
  "Check style of Japanese text if distal (keitai, desu/masu)."
  (interactive)
  (copyedit-ja--grep-buffers-using-dict copyedit-ja--dict-distal-direct))

(defun copyedit-ja-convert-desumasu-to-dadearu ()
  "Change style of Japanese text from distal (keitai, desu/masu) to direct
(jotai, da/dearu).

If region is active, application is restricted to the region."
  (interactive)
  (let ((start (if (use-region-p) (region-beginning) nil))
        (end (if (use-region-p) (region-end) nil))
        (f #'copyedit-ja--perform-replace-using-dict)
        (dict copyedit-ja--dict-distal-direct))
    (if (use-region-p)
        (save-mark-and-excursion
          (save-restriction
            (narrow-to-region start end)
            (goto-char (point-min))
            (funcall f dict)))
      (funcall f dict))))

(defconst copyedit-ja--dict-direct-distal
  (let* ((re-grp "\\(\\\\(.*\\\\)\\)\\(.*\\)")
         (re-ref "\\(\\\\[0-9]\\)\\(.*\\)")
         (swap-pair-considering-regexp
          (lambda (pair)
            (let* ((key (car pair))
                   (val (cdr pair))
                   (kparts (if (string-match re-grp key)
                               (cons (match-string 1 key) (match-string 2 key))
                             (cons "" key)))
                   (vparts (if (string-match re-ref val)
                               (cons (match-string 1 val) (match-string 2 val))
                             (cons "" val)))
                   (src (concat (car kparts) (cdr vparts)))
                   (tgt (concat (car vparts) (cdr kparts))))
              (cons src tgt)))))
    (mapcar swap-pair-considering-regexp
            copyedit-ja--dict-distal-direct))
  "Dictionary to translate direct style (jotai, da/dearu) to
distal style (keitai, desu/masu).")

(defun copyedit-ja-check-dadearu ()
  "Check style of Japanese text if direct (jotai, da/dearu)."
  (interactive)
  (copyedit-ja--grep-buffers-using-dict copyedit-ja--dict-direct-distal))

(defun copyedit-ja-convert-dadearu-to-desumasu ()
  "Change style of Japanese text from direct (jotai, da/dearu) to distal
(keitai, desu/masu).

If region is active, application is restricted to the region."
  (interactive)
  (let ((start (if (use-region-p) (region-beginning) nil))
        (end (if (use-region-p) (region-end) nil))
        (f #'copyedit-ja--perform-replace-using-dict)
        (dict copyedit-ja--dict-direct-distal))
    (if (use-region-p)
        (save-mark-and-excursion
          (save-restriction
            (narrow-to-region start end)
            (goto-char (point-min))
            (funcall f dict)))
      (funcall f dict))))

;; ----------------------------------------------------------------
;; Converting katakana and hiragana

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
    (split-string str " "))
  "List of katakana characters.
FIXME: it should cover more characters.
See `copyedit-ja--dict-word-by-character-type-katakana'.")

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
    (split-string str " "))
  "List of Hiragana characters.
FIXME: it should cover more characters.
See `copyedit-ja--dict-word-by-character-type-hiragana'.")

(defconst copyedit-ja--dict-katakana-hiragana
  (mapcar (lambda (ls) (cons (car ls) (cadr ls)))
          (copyedit-ja--zip-lists copyedit-ja--katakana copyedit-ja--hiragana))
  "Dictionary to translate katakana to hiragana.")

(defconst copyedit-ja--dict-hiragana-katakana
  (mapcar (lambda (ls) (cons (car ls) (cadr ls)))
          (copyedit-ja--zip-lists copyedit-ja--hiragana copyedit-ja--katakana))
  "Dictionary to translate hiragana to katakana.")

(defun copyedit-ja--katakana-to-hiragana (str)
  "Convert katakana in STR to hiragana."
  (mapconcat (lambda (s)
               (copyedit-ja--translate s copyedit-ja--dict-katakana-hiragana))
             (split-string str "" t)
             ""))
(defun copyedit-ja--hiragana-to-katakana (str)
  "Convert hiragana in STR to katakana."
  (mapconcat (lambda (s)
               (copyedit-ja--translate s copyedit-ja--dict-hiragana-katakana))
             (split-string str "" t)
             ""))

(defun copyedit-ja-convert-katakana-to-hiragana-region (start end)
  "Convert katakana in region between START and END to hiragana."
  (interactive "r")
  (copyedit-ja--filter-region #'copyedit-ja--katakana-to-hiragana start end))

(defun copyedit-ja-convert-hiragana-to-katakana-region (start end)
  "Convert hiragana in region between START and END to katakana."
  (interactive "r")
  (copyedit-ja--filter-region #'copyedit-ja--hiragana-to-katakana start end))

;; ----------------------------------------------------------------
;; Acquiring reading of Japanese text

(defun copyedit-ja--get-reading-katakana (str)
  "Acquire reading of Japanese text in STR in katakana using MeCab.

(Skip ASCII parts of STR to prevent MeCab with NAIST-JDIC from converting
ASCII characters to their pronunciations which is unwanted for our purpose.)

TODO: Skip non-ASCII-yet-non-Japanese characters from being converted,
e.g. latin letters with diacritical marks, as in \"Bézier\"."
  (let ((regexp "[[:nonascii:]]+")
        (rep (lambda (s)
               (copyedit-ja--shell-command-string "mecab" s "--output-format-type=yomi")))
        (string str))
    (replace-regexp-in-string regexp rep string)))

(defun copyedit-ja--get-reading-hiragana (str)
  "Acquire reading of Japanese text in STR in hiragana using MeCab.

See `copyedit-ja--get-reading-katakana' for detail."
  (copyedit-ja--katakana-to-hiragana (copyedit-ja--get-reading-katakana str)))

;; ----------------------------------------------------------------
;; Converting hiragana, katakna, and kanji to katakana/hiragana

(defun copyedit-ja-convert-kanakanji-to-katakana-region (start end)
  "Convert hiragana, katakna, and kanji in region between START and END
to katakana."
  (interactive "r")
  (copyedit-ja--filter-region #'copyedit-ja--get-reading-katakana start end))

(defun copyedit-ja-convert-kanakanji-to-hiragana-region (start end)
  "Convert hiragana, katakna, and kanji in region between START and END
to hiragana."
  (interactive "r")
  (copyedit-ja--filter-region #'copyedit-ja--get-reading-hiragana start end))

;; ----------------------------------------------------------------
;; Miscellaneous applications

(defconst copyedit-ja--dict-notation-hou
  '(("\\([^一両双前後左右上下]\\|^\\)\\(方\\)\\([^法式向面]\\|$\\)" .
     "\\1ほう\\3"))
  "Dictionary to find \"hou\" being written not in hiragana (U+307B U+3046)
but in kanji (U+65B9).

(This is just a personal preference. You may prefer otherwise.)")

(defun copyedit-ja-check-notation-hou ()
  "Check notation (hou)."
  (interactive)
  (copyedit-ja--grep-buffers-using-dict copyedit-ja--dict-notation-hou))

(defconst copyedit-ja--dict-ordinal-numerals
  '(("\\([0-9一二三四五六七八九十][つ番]\\)[め]" . "\\1目"))
  "Dictionary to find \"-me\" after ordinal number being written in hiragana.

(This is just a personal preference. You may prefer otherwise.)")

(defun copyedit-ja-check-ordinal-numerals()
  "Check Japanese ordinal numerals."
  (interactive)
  (copyedit-ja--grep-buffers-using-dict copyedit-ja--dict-ordinal-numerals))

(defun copyedit-ja-normalize-ordinal-numerals()
  "Normalize Japanese ordinal numerals.

If region is active, application is restricted to the region."
  (interactive)
  (let ((start (if (use-region-p) (region-beginning) nil))
        (end (if (use-region-p) (region-end) nil))
        (f #'copyedit-ja--perform-replace-using-dict)
        (dict copyedit-ja--dict-ordinal-numerals))
    (if (use-region-p)
        (save-mark-and-excursion
          (save-restriction
            (narrow-to-region start end)
            (goto-char (point-min))
            (funcall f dict)))
      (funcall f dict))))

(defconst copyedit-ja--dict-suspicious-hiragana-katakana-ri
  (let ((hira (apply #'concat copyedit-ja--hiragana))
        (kata (apply #'concat (append copyedit-ja--katakana '("ー")))))
    `((,(concat "\\(^\\|[^" hira "]\\)り\\($\\|[^" hira "]\\)") . "\\1リ\\2")
      (,(concat "\\(^\\|[^" kata "]\\)リ\\($\\|[^" kata "]\\)") . "\\1り\\2")))
  "Dictionary to find hiragana and katakana ri's that do not have preceding
or succeeding character of the same type, which are possibly typos that are
easy for human eyes to overlook, e.g. \"ツりー\" or \"つまリ\".")

(defun copyedit-ja-check-suspicious-hiragana-katakana-ri()
  "Check suspicisous uses of hiragana or katakana ri's."
  (interactive)
  (copyedit-ja--grep-buffers-using-dict copyedit-ja--dict-suspicious-hiragana-katakana-ri))

(defun copyedit-ja-normalize-suspicious-hiragana-katakana-ri()
  "Normalize suspicisous uses of hiragana or katakana ri's.

If region is active, application is restricted to the region."
  (interactive)
  (let ((start (if (use-region-p) (region-beginning) nil))
        (end (if (use-region-p) (region-end) nil))
        (f #'copyedit-ja--perform-replace-using-dict)
        (dict copyedit-ja--dict-suspicious-hiragana-katakana-ri))
    (if (use-region-p)
        (save-mark-and-excursion
          (save-restriction
            (narrow-to-region start end)
            (goto-char (point-min))
            (funcall f dict)))
      (funcall f dict))))

;; ----------------------------------------------------------------

(provide 'copyedit-ja)

;;; copyedit-ja.el ends here
