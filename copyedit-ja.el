;; -*- utf-8-unix -*-
;; Japanese copyediting commands for Emacs and xyzzy
;; Copyright (c) 2008-2011 Hisashi Morita
;; License: Public Domain
;;
;; Usage example:
;;   1. check
;;     M-x copyedit-ja-check-typo-particle
;;   2. edit
;;     M-x copyedit-ja-normalize-typo-particle
;;
;; Requirements:
;;   * perform-replace-with-dict.el
;;   * shell-command-string.el
;;   * color-moccur.el
;;     http://www.bookshelf.jp/elc/color-moccur.el
;;
;; Bibliography:
;;   * Kurata Masanori, Suganuma Akira, Ushijima Kazuo,
;;     "Development of a System of Writing Tools for Japanese Documents on
;;      a Personal Computer."
;;     http://ci.nii.ac.jp/naid/110003743558

(require 'perform-replace-with-dict)

;; ------------------------------------------------------------------------
;; utilities

;; compatibility for xyzzy
(if (and (not (boundp 'emacs-version))
         (boundp 'software-type)
         (equal software-type "xyzzy"))
    (defun emacs-version ()
      (concat (software-type) " " (software-version)))
    (defun xyzzyfy-regexp-src (emacs-regexp-src)
      "Translate Emacs style regexp source to xyzzy style.
(e.g. '\\sj' to '\\cj', '\\Sj' to '\\Sj'.)"
      (let* ((regexp-src (substitute-string emacs-regexp-src "\\\\cj" "\\sj"))
             (regexp-src (substitute-string regexp-src       "\\\\Cj" "\\Sj")))
        regexp-src)))

(defun my-grep-buffers (regexp-src)
  "Search REGEXP in buffers."
  (interactive)
  (cond ((string-match "Emacs" (emacs-version))
         (moccur regexp-src t))
        ((string-match "xyzzy" (emacs-version))
         (grep (xyzzyfy-regexp-src regexp-src)))))

(defun grep-buffers-with-dict (dict)
  "Search REGEXP in buffers with DICT."
  (interactive)
  (let ((regexp-src (%regexp-opt-re (%keys dict))))
    (cond ((string-match "Emacs" (emacs-version))
           (moccur regexp-src t))
          ((string-match "xyzzy" (emacs-version))
           (grep (xyzzyfy-regexp-src regexp-src))))))

;; ------------------------------------------------------------------------
;; applications
;; inspired by Suikou

(defconst copyedit-ja-dict-paren-width
  '(("\\(\\cj\\) ?(\\([^()]*?\\)) ?\\(\\cj\\)" . "\\1（\\2）\\3")
    ("\\(\\cj\\) ?(" . "\\1（")
    (") ?\\(\\cj\\)" . "）\\1")))

(defconst copyedit-ja-dict-paren-width-paranoid
  '(("\\(^\\|\\cj\\) ?(\\([^()]*?\\)) ?\\(\\cj\\|$\\)" . "\\1（\\2）\\3")
    ("\\(^\\|\\cj\\) ?(" . "\\1（")
    (") ?\\(\\cj\\|$\\)" . "）\\1")))

(defun copyedit-ja-check-paren-width (&optional paranoid)
  "Check the widths of parenthesis characters considering contexts."
  (interactive "P")
  (if paranoid
      (grep-buffers-with-dict copyedit-ja-dict-paren-width-paranoid)
      (grep-buffers-with-dict copyedit-ja-dict-paren-width)))

(defun copyedit-ja-normalize-paren-width (&optional paranoid)
  "Replace half-width parens with full-width ones."
  (interactive "P")
  (if paranoid
      (perform-replace-with-dict copyedit-ja-dict-paren-width-paranoid)
      (perform-replace-with-dict copyedit-ja-dict-paren-width)))

(defun copyedit-ja-normalize-paren-width-region (start end &optional paranoid)
  "Replace half-width parens in region with full-width ones."
  (interactive "Pr")
  (save-excursion
    (save-restriction
      (if paranoid
          (perform-replace-with-dict copyedit-ja-dict-paren-width-paranoid
                                     start end)
          (perform-replace-with-dict copyedit-ja-dict-paren-width
                                     start end)))))

(defconst copyedit-ja-dict-paren-matching
  '(("\\(([^)]*?）\\)" . "\\1")
    ("\\(（[^）]*?)\\)" . "\\1")))

(defun copyedit-ja-check-paren-matching ()
  "Check if opening and closing parentheses match."
  (interactive)
  (grep-buffers-with-dict copyedit-ja-dict-paren-matching))

(defconst copyedit-ja-dict-punctuation-tenmaru
  '(("\\(\\cj\\),"   . "\\1、")
    ("\\(\\cj\\)\\." . "\\1。")
    ("，" . "、")
    ("．" . "。")))

(defconst copyedit-ja-dict-punctuation-kanpiri
  '(("\\(\\cj\\),"   . "\\1，")
    ("\\(\\cj\\)\\." . "\\1．")
    ("、" . "，")
    ("。" . "．")))

(defun copyedit-ja-check-punctuation-tenmaru ()
  "Check irregular punctuation, preferring ten-maru."
  (interactive)
  (grep-buffers-with-dict copyedit-ja-dict-punctuation-tenmaru))

(defun copyedit-ja-check-punctuation-kanpiri ()
  "Check irregular punctuation preferring kan-piri."
  (interactive)
  (grep-buffers-with-dict copyedit-ja-dict-punctuation-kanpiri))

(defun copyedit-ja-normalize-punctuation-tenmaru ()
  "Normalize irregular punctuation, preferring ten and maru."
  (interactive)
  (perform-replace-with-dict copyedit-ja-dict-punctuation-tenmaru))

(defun copyedit-ja-normalize-punctuation-kanpiri ()
  "Normalize irregular punctuation, preferring commas and dots."
  (interactive)
  (perform-replace-with-dict copyedit-ja-dict-punctuation-kanpiri))

(defconst copyedit-ja-dict-macron
  '(("\\([^ァ-ンヴヵヶ]ー\\)" . "\\1")))

(defun copyedit-ja-check-macron ()
  "Check suspicious macron (onbiki)."
  (interactive)
  (grep-buffers-with-dict copyedit-ja-dict-macron))

(defun copyedit-ja-normalize-macron ()
  "Normalize suspicious macron (onbiki)."
  (interactive)
  (perform-replace-with-dict copyedit-ja-dict-macron))

(defconst copyedit-ja-dict-typo-particle
  '(("\\([てにをはのが]\\)\\1" . "\\1")))

(defun copyedit-ja-check-typo-particle ()
  "Check possible typos in particles."
  (interactive)
  (grep-buffers-with-dict copyedit-ja-dict-typo-particle))

(defun copyedit-ja-normalize-typo-particle ()
  "Normalize possible typos in particles."
  (interactive)
  (perform-replace-with-dict copyedit-ja-dict-typo-particle))

(defconst copyedit-ja-dict-passive-voice
  '(("\\([あかさたなはまやらわ]ら?れ\\)" . "\\1")))

(defun copyedit-ja-check-passive-voice ()
  "Find passive voice, which are sometimes misused."
  (interactive)
  (grep-buffers-with-dict copyedit-ja-dict-passive-voice))

(defconst copyedit-ja-dict-demonstrative
  '(("\\([こそあど][れの]\\)" . "\\1")))

(defun copyedit-ja-check-demonstrative ()
  "Find demonstratives (kore, soer, are, dore, etc.)."
  (interactive)
  (grep-buffers-with-dict copyedit-ja-dict-demonstrative))

(defconst copyedit-ja-dict-toritate-focalizer
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
  (grep-buffers-with-dict copyedit-ja-dict-toritate-focalizer))

(defconst copyedit-ja-dict-conjunctive-particle-ga
  '(("\\([ぁ-ん]が[、，]\\)" . "\\1")))

(defun copyedit-ja-check-conjunctive-particle-ga ()
  "Find conjunctive particle \"ga\", which means either \"and\" and \"but\"."
  (interactive)
  (grep-buffers-with-dict copyedit-ja-dict-conjunctive-particle-ga))

(defconst copyedit-ja-dict-user-specified-keyword
  '(("\\(すなわち)" . "\\1")
    ("\\(ゆえに\\)" . "\\1")
    ("\\(したがって\\)" . "\\1")
    ("\\(非常に\\)" . "\\1")
    ("\\(著しく\\)" . "\\1")))

(defun copyedit-ja-check-user-specified-keyword ()
  "Find user specified keywords."
  (interactive)
  (grep-buffers-with-dict copyedit-ja-dict-user-specified-keyword))

(defconst copyedit-ja-dict-long-hiragana-sequence
  '(("\\([ぁ-ん]\\{10,\\}\\)" . "\\1")))

(defun copyedit-ja-check-long-hiragana-sequence ()
  "Find long sequence of hiragana, which can lower readability."
  (interactive)
  (grep-buffers-with-dict copyedit-ja-dict-long-hiragana-sequence))

(defconst copyedit-ja-dict-monotonous-sentence
  '(()))

(defun copyedit-ja-check-monotonous-sentence ()
  "Find monotonous, possibly dull sentence.
Not implememted yet."
  (interactive)
  (grep-buffers-with-dict copyedit-ja-dict-monotonous-sentence))

(defconst copyedit-ja-dict-beginning-of-sentence
  '(("\\(^\\cj\\|[。．]\\cj\\)" . "\\1")))

(defun copyedit-ja-enumerate-beginning-of-sentence ()
  "Enumerate beginning of sentences.
Not implemented yet."
  (interactive)
  (grep-buffers-with-dict copyedit-ja-dict-beginning-of-sentence))

(defconst copyedit-ja-dict-writing-style
  '(("\\(\\cj\\{1,5\\}[。．]\\)" . "\\1")))

(defun copyedit-ja-check-writing-style ()
  "Check writing style, by sorting sentences by the end of them.
Not implemented yet."
  (interactive)
  (grep-buffers-with-dict copyedit-ja-dict-writing-style))

(defconst copyedit-ja-dict-long-sentence
  '(("\\([。．^]\\cj\\{100,\\}[。．]\\)" . "\\1")))

(defun copyedit-ja-enumerate-long-sentence ()
  "Check sentences that are too long.
Not implemented yet."
  (interactive)
  (grep-buffers-with-dict copyedit-ja-dict-long-sentence))

(defconst copyedit-ja-dict-heading
  '(("\\(^[■□▲△▼▽●○].*?$\\)" . "\\1")))

(defun copyedit-ja-enumerate-heading ()
  "Enumerate headings.
Not implemented yet."
  (interactive)
  (grep-buffers-with-dict copyedit-ja-dict-heading))

(defconst copyedit-ja-dict-youni-negative
  '(("\\(ように[^。．]+?な[かいくけ]\\)" . "\\1")))

(defun copyedit-ja-check-youni-negative ()
  "Check \"youni...nai\"."
  (interactive)
  (grep-buffers-with-dict copyedit-ja-dict-youni-negative))

(defconst copyedit-ja-dict-multiple-negative
  '(("\\(な[かいくけ][^。．]+な[かいくけ]\\)" . "\\1")))

(defun copyedit-ja-check-multiple-negative ()
  "Check for multiple negative expressions in a sentence."
  (interactive)
  (grep-buffers-with-dict copyedit-ja-dict-multiple-negative))

(defconst copyedit-ja-dict-word-by-character-type
  '(("\\(\\)" . "\\1")))

(defun copyedit-ja-enumerate-word-by-character-type ()
  "Enumerate words by character type.
Not implemented yet."
  (interactive)
  (grep-buffers-with-dict copyedit-ja-dict-word-by-character-type))

(defun copyedit-ja-show-text-stat ()
  "Show text statistics.
Not implemented yet."
  (interactive)
  "Not implemented yet.")

;; ----------------------------------------------------------------
;; more applications

(defconst copyedit-ja-dict-unwanted-space
  `(("\\([0-9A-Za-z]\\) \\(\\cC\\|\\cK\\|\\cH\\)" . "\\1\\2")
    ("\\(\\cC\\|\\cK\\|\\cH\\) \\([0-9A-Za-z]\\)" . "\\1\\2")))

(defun copyedit-ja-check-unwanted-space ()
  "Find unwanted space between Japanese character and latin character."
  (interactive)
  (grep-buffers-with-dict copyedit-ja-dict-unwanted-space))

(defun copyedit-ja-remove-unwanted-space ()
  "Remove unwanted space in current buffer."
  (interactive)
  (perform-replace-with-dict copyedit-ja-dict-unwanted-space))

(defun copyedit-ja-remove-unwanted-space-region (start end)
  "Remove unwanted space in region."
  (interactive "r")
  (save-excursion
    (save-restriction
      (perform-replace-with-dict copyedit-ja-dict-unwanted-space
                                 start end))))

(defconst copyedit-ja-dict-alnum-fullwidth-halfwidth
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
  (grep-buffers-with-dict copyedit-ja-dict-alnum-fullwidth-halfwidth))

(defun copyedit-ja-normalize-charwidth ()
  "Normalize fullwidth alnum characters to halfwidth ones."
  (interactive)
  (perform-replace-with-dict copyedit-ja-dict-alnum-fullwidth-halfwidth))

(defun copyedit-ja-normalize-charwidth-region (start end)
  "Normalize fullwidth alnum characters in region to halfwidth ones."
  (interactive "r")
  (save-excursion
    (save-restriction
      (perform-replace-with-dict copyedit-ja-dict-alnum-fullwidth-halfwidth
                                 start end))))

;; ----------------------------------------------------------------
;; Japanese style converter
;; dictionary initially based on Takeshi Urayama "e-editing using sed"

(defconst copyedit-ja-dict-distal-direct
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
  (grep-buffers-with-dict copyedit-ja-dict-distal-direct))

(defun copyedit-ja-convert-desumasu-to-dadearu ()
  "Change style of Japanese text from distal (desu/masu) to direct (da/dearu)."
  (interactive)
  (perform-replace-with-dict copyedit-ja-dict-distal-direct))

(defun copyedit-ja-convert-desumasu-to-dadearu-region (start end)
  (interactive "r")
  (save-excursion
    (save-restriction
      (perform-replace-with-dict copyedit-ja-dict-distal-direct start end))))

(defconst copyedit-ja-dict-direct-distal
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
            copyedit-ja-dict-distal-direct)))

(defun copyedit-ja-check-dadearu-to-desumasu ()
  "Check style of Japanese text if direct (da/dearu)."
  (interactive)
  (grep-buffers-with-dict copyedit-ja-dict-direct-distal))

(defun copyedit-ja-convert-dadearu-desumasu ()
  "Change style of Japanese text from direct (da/dearu) to distal (desu/masu)."
  (interactive)
  (perform-replace-with-dict copyedit-ja-dict-direct-distal))

(defun copyedit-ja-convert-dadearu-to-desumasu-region (start end)
  (interactive "r")
  (save-excursion
    (save-restriction
      (perform-replace-with-dict copyedit-ja-dict-direct-distal start end))))

;; ----------------------------------------------------------------
;; katakana and hiragana conversion

(defconst copyedit-ja-katakana
  (let ((str (concat "ァ ア ィ イ ゥ ウ ェ エ ォ オ "
                     "カ ガ キ ギ ク グ ケ ゲ コ ゴ "
                     "サ ザ シ ジ ス ズ セ ゼ ソ ゾ "
                     "タ ダ チ ヂ ッ ツ ヅ テ デ ト ド "
                     "ナ ニ ヌ ネ ノ "
                     "ハ バ パ ヒ ビ ピ フ ブ プ ヘ ベ ペ ホ ボ ポ "
                     "マ ミ ム メ モ "
                     "ャ ヤ ュ ユ ョ ヨ "
                     "ラ リ ル レ ロ ゎ ワ ゐ ゑ ヲ ン")))
    (split-string str " ")))

(defconst copyedit-ja-hiragana
  (let ((str (concat "ぁ あ ぃ い ぅ う ぇ え ぉ お "
                     "か が き ぎ く ぐ け げ こ ご "
                     "さ ざ し じ す ず せ ぜ そ ぞ "
                     "た だ ち ぢ っ つ づ て で と ど "
                     "な に ぬ ね の "
                     "は ば ぱ ひ び ぴ ふ ぶ ぷ へ べ ぺ ほ ぼ ぽ "
                     "ま み む め も "
                     "ゃ や ゅ ゆ ょ よ "
                     "ら り る れ ろ ゎ わ ゐ ゑ を ん")))
    (split-string str " ")))

(defconst copyedit-ja-dict-katakana-hiragana
  (mapcar (lambda (ls) (cons (car ls) (cadr ls)))
          (%zip-naive copyedit-ja-katakana copyedit-ja-hiragana)))

(defconst copyedit-ja-dict-hiragana-katakana
  (mapcar (lambda (ls) (cons (car ls) (cadr ls)))
          (%zip-naive copyedit-ja-hiragana copyedit-ja-katakana)))

(defun %katakana-to-hiragana (str)
  (mapconcat (lambda (s)
               (%translate s copyedit-ja-dict-katakana-hiragana))
             (split-string str "" t)
             ""))
(defun %hiragana-to-katakana (str)
  (mapconcat (lambda (s)
               (%translate s copyedit-ja-dict-hiragana-katakana))
             (split-string str "" t)
             ""))

(defun %filter-region (f begin end)
  (let* ((src (buffer-substring begin end))
         (replacement (funcall f src)))
    (progn
      (delete-region begin end)
      (insert replacement))))

(defun copyedit-ja-katakana-to-hiragana-region (begin end)
  "Convert katakana in region to hiragana."
  (interactive "r")
  (%filter-region #'%katakana-to-hiragana begin end))

(defun copyedit-ja-hiragana-to-katakana-region (begin end)
  "Convert hiragana in region to katakana."
  (interactive "r")
  (%filter-region #'%hiragana-to-katakana begin end))

;; ----------------------------------------------------------------
;; Aquire reading of Japanese text
;; requirement:
;;   * MeCab: Yet Another Part-of-Speech and Morphological Analyzer
;;     http://mecab.sourceforge.net/

(require 'shell-command-string)

(defun copyedit-ja-get-reading-katakana (str)
  "Acquire reading of Japanese text in katakana using MeCab."
  (shell-command-string "mecab" str "--output-format-type=yomi"))

(defun copyedit-ja-get-reading-hiragana (str)
  "Acquire reading of Japanese text in hiragana using MeCab."
  (%katakana-to-hiragana (copyedit-ja-get-reading-katakana str)))

(defun copyedit-ja-katakana-region (begin end)
  "Convert region to katakana."
  (interactive "r")
  (%filter-region #'copyedit-ja-get-reading-katakana begin end))

(defun copyedit-ja-hiragana-region (begin end)
  "Convert region to hiragana."
  (interactive "r")
  (%filter-region #'copyedit-ja-get-reading-hiragana begin end))

;; ----------------------------------------------------------------
;; miscellaneous applications

(defconst copyedit-ja-dict-notation-hou
  '(("\\([^一両双前後左右上下]\\|^\\)\\(方\\)\\([^法式向面]\\|$\\)" .
     "\\1ほう\\3")))

(defun copyedit-ja-check-notation-hou ()
  "Check notation (hou)."
  (interactive)
  (grep-buffers-with-dict copyedit-ja-dict-notation-hou))

(defconst copyedit-ja-dict-numeric
  '(("\\([0-9一二三四五六七八九十][つ番]\\)[め]" . "\\1目")))

(defun copyedit-ja-check-numeric()
  "Check Japanese numeric"
  (interactive)
  (grep-buffers-with-dict copyedit-ja-dict-numeric))

(defun copyedit-ja-normalize-numeric()
  "Normalize Japanese numeric"
  (interactive)
  (perform-replace-with-dict copyedit-ja-dict-numeric))

;; ----------------------------------------------------------------

(provide 'copyedit-ja)
