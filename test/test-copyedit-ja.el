;; -*- encoding: utf-8 -*-
;; tests for copyedit-ja
;; Copyright (c) 2010-2011 Hisashi Morita
;; License: Public Domain

(require 'ert)
(require 'copyedit-ja)

(ert-deftest test-%zip-naive ()
  (should (equal '((1 4) (2 5) (3 nil))
                 (%zip-naive '(1 2 3) '(4 5)))))

(ert-deftest test-%translate ()
  (should (equal "A"
                 (%translate "a" '(("a" . "A"))))))

(ert-deftest test-%katakana-to-hiragana ()
             (should (equal "a1ひらかた漢字"
                            (%katakana-to-hiragana "a1ひらカタ漢字"))))

(ert-deftest test-%katakana-to-hiragana-thorough ()
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
    (should (equal hira (%katakana-to-hiragana kata)))))

(ert-deftest test-%hiragana-to-katakana ()
             (should (equal "a1ヒラカタ漢字"
                            (%hiragana-to-katakana "a1ひらカタ漢字"))))

(ert-deftest test-copyedit-ja-get-reading-katakana ()
             (should (equal "a1ヒラカタカンジ"
                            (copyedit-ja-get-reading-katakana "a1ひらカタ漢字"))))

(ert-deftest test-copyedit-ja-get-reading-hiragana ()
             (should (equal "a1ひらかたかんじ"
                            (copyedit-ja-get-reading-hiragana "a1ひらカタ漢字"))))

(ert-run-tests-batch-and-exit)
