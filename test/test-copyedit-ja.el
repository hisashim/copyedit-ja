;; -*- encoding: utf-8 -*-
;; tests for copyedit-ja
;; Copyright (c) 2010-2011 Hisashi Morita
;; License: Public Domain

(require 'ert)
(require 'copyedit-ja)

(ert-deftest test-%katakana-to-hiragana ()
             (should (equal "a1ひらかた漢字"
                            (%katakana-to-hiragana "a1ひらカタ漢字"))))

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
