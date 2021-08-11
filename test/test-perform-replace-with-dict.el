;; -*- encoding: utf-8 -*-
;; tests for perform-replace-with-dict
;; Copyright (c) 2010-2011 Hisashi Morita
;; License: Public Domain

(require 'ert)
(require 'perform-replace-with-dict)

(ert-deftest test-%assoc-exact-match ()
             (should (equal '("aa" . "A2")
                            (%assoc-exact-match "aa" '(("a"  . "A")
                                                       ("aa" . "A2")
                                                       ("b"  . "B"))))))

(ert-deftest test-%group-sequence ()
             (should (equal '((1 1 1) (2) (3) (4 4) (2 2) (3) (1 1) (3))
                            (%group-sequence '(1 1 1 2 3 4 4 2 2 3 1 1 3)))))

(ert-deftest test-%wrapup-group-charclass ()
             (should (equal "[ab]"
                            (%wrapup-group '("a" "b")))))

(ert-deftest test-%wrapup-group-or ()
             (should (equal "a\\|bc"
                            (%wrapup-group '("a" "bc")))))

(ert-deftest test-%wrapup-group-or-dot ()
             (should (equal "a\\|."
                            (%wrapup-group '("a" ".")))))

(ert-deftest test-%regexp-opt-re ()
             (should (equal "[ab]\\|.\\|cd+"
                            (%regexp-opt-re '("a" "b" "." "cd+")))))

(ert-run-tests-batch-and-exit)
