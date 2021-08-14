;; -*- encoding: utf-8 -*-
;; tests for copyedit-ja
;; Copyright (c) 2010-2011 Hisashi Morita
;; License: Public Domain

(require 'ert)
(require 'copyedit-ja)

(ert-deftest test-copyedit-ja--group-sequence ()
             (should (equal '((1 1 1) (2) (3) (4 4) (2 2) (3) (1 1) (3))
                            (copyedit-ja--group-sequence '(1 1 1 2 3 4 4 2 2 3 1 1 3)))))

(ert-deftest test-copyedit-ja--wrapup-group-charclass ()
             (should (equal "[ab]"
                            (copyedit-ja--wrapup-group '("a" "b")))))

(ert-deftest test-copyedit-ja--wrapup-group-or ()
             (should (equal "a\\|bc"
                            (copyedit-ja--wrapup-group '("a" "bc")))))

(ert-deftest test-copyedit-ja--wrapup-group-or-dot ()
             (should (equal "a\\|."
                            (copyedit-ja--wrapup-group '("a" ".")))))

(ert-deftest test-copyedit-ja--regexp-opt-re ()
             (should (equal "[ab]\\|.\\|cd+"
                            (copyedit-ja--regexp-opt-re '("a" "b" "." "cd+")))))

(ert-deftest test-copyedit-ja--assoc-exact-match ()
  (let ((dict '(("foobar" . "Foobar")
                ("foo" . "Foo")
                ("bar" . "Bar")
                ("[a-z]+" . "OTHER"))))
    (should (equal '("foobar" . "Foobar") (copyedit-ja--assoc-exact-match "foobar" dict)))
    (should (equal '("foo" . "Foo") (copyedit-ja--assoc-exact-match "foo" dict)))
    (should (equal '("bar" . "Bar") (copyedit-ja--assoc-exact-match "bar" dict)))
    (should (equal '("[a-z]+" . "OTHER") (copyedit-ja--assoc-exact-match "baz" dict)))
    (should (equal nil (copyedit-ja--assoc-exact-match "123" dict)))))

(ert-deftest test-copyedit-ja--sort-dict ()
  (let ((dict-unsorted '(("bar" . "Bar")
                         ("[a-z]+" . "OTHER")
                         ("foobar" . "Foobar")))
        (dict-sorted '(("foobar" . "Foobar")
                       ("bar" . "Bar")
                       ("[a-z]+" . "OTHER"))))
    (should (equal dict-sorted (copyedit-ja--sort-dict dict-unsorted)))))

(ert-deftest test-copyedit-ja--find-replacement ()
  (let ((dict '(("foobar" . "Foobar")
                ("bar" . "Bar")
                ("[a-z]+" . (lambda (s) (upcase s))))))
    (should (equal "Foobar" (copyedit-ja--find-replacement "foobar" dict)))
    (should (equal "Bar" (copyedit-ja--find-replacement "bar" dict)))
    (should (equal "BAZ" (copyedit-ja--find-replacement "baz" dict)))
    (should (equal "123" (copyedit-ja--find-replacement "123" dict)))))

(ert-deftest test-copyedit-ja--shell-command-string-ascii ()
             (should (equal "bar"
                            (copyedit-ja--shell-command-string "grep"
                                                               "foo\nbar\n"
                                                               "--color" "b"))))

(ert-deftest test-copyedit-ja--shell-command-string-unicode ()
             (should (equal "/dev/stdin: UTF-8 Unicode text"
                            (copyedit-ja--shell-command-string "file"
                                                               "ひらがな\n"
                                                               "-"))))

(ert-deftest test-copyedit-ja--zip-naive ()
  (should (equal '((1 4) (2 5) (3 nil))
                 (copyedit-ja--zip-naive '(1 2 3) '(4 5)))))

(ert-deftest test-copyedit-ja--translate ()
  (should (equal "A"
                 (copyedit-ja--translate "a" '(("a" . "A"))))))

(ert-deftest test-copyedit-ja--katakana-to-hiragana ()
             (should (equal "a1ひらかた漢字"
                            (copyedit-ja--katakana-to-hiragana "a1ひらカタ漢字"))))

(ert-deftest test-copyedit-ja--katakana-to-hiragana-thorough ()
  (let ((hira (concat "ぁあぃいぅうぇえぉお"
                      "かがきぎくぐけげこご"
                      "さざしじすずせぜそぞ"
                      "ただちぢっつづてでとど"
                      "なにぬねの"
                      "はばぱひびぴふぶぷへべぺほぼぽ"
                      "まみむめも"
                      "ゃやゅゆょよ"
                      "らりるれろ"
                      "ゎわゐゑをん"
                      "ゔゕゖわ゙ゐ゙ゑ゙を゙ゝゞ"))
        (kata (concat "ァアィイゥウェエォオ"
                      "カガキギクグケゲコゴ"
                      "サザシジスズセゼソゾ"
                      "タダチヂッツヅテデトド"
                      "ナニヌネノ"
                      "ハバパヒビピフブプヘベペホボポ"
                      "マミムメモ"
                      "ャヤュユョヨ"
                      "ラリルレロ"
                      "ヮワヰヱヲン"
                      "ヴヵヶヷヸヹヺヽヾ")))
    (should (equal hira (copyedit-ja--katakana-to-hiragana kata)))))

(ert-deftest test-copyedit-ja--hiragana-to-katakana ()
             (should (equal "a1ヒラカタ漢字"
                            (copyedit-ja--hiragana-to-katakana "a1ひらカタ漢字"))))

(ert-deftest test-copyedit-ja--get-reading-katakana ()
             (should (equal "a1ヒラカタカンジ"
                            (copyedit-ja--get-reading-katakana "a1ひらカタ漢字"))))

(ert-deftest test-copyedit-ja--get-reading-hiragana ()
             (should (equal "a1ひらかたかんじ"
                            (copyedit-ja--get-reading-hiragana "a1ひらカタ漢字"))))

(ert-run-tests-batch-and-exit)
