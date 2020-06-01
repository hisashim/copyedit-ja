;; -*- encoding: utf-8 -*-
;; tests for copyedit-ja
;; Copyright (c) 2010-2011 Hisashi Morita
;; License: Public Domain

(require 'ert)
(require 'copyedit-ja)

(ert-deftest test-%katakana-to-hiragana ()
             (should (equal "い"
                            (%katakana-to-hiragana "イ"))))

(ert-deftest test-%hiragana-to-katakana ()
             (should (equal "イ"
                            (%hiragana-to-katakana "い"))))

(ert-deftest test-copyedit-ja-get-reading-katakana ()
             (should (equal "カンジ"
                            (copyedit-ja-get-reading-katakana "漢字"))))

(ert-deftest test-copyedit-ja-get-reading-hiragana ()
             (should (equal "かんじ"
                            (copyedit-ja-get-reading-hiragana "漢字"))))

(ert-run-tests-batch-and-exit)
