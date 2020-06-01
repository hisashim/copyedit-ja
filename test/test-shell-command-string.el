;; -*- encoding: utf-8 -*-
;; tests for shell-command-string
;; Copyright (c) 2010-2011 Hisashi Morita
;; License: Public Domain
;; Requirements: grep, file

(require 'ert)
(require 'shell-command-string)

(ert-deftest test-shell-command-string-ascii ()
             (should (equal "bar"
                            (shell-command-string "grep"
                                                  "foo\nbar\n"
                                                  "--color" "b"))))

(ert-deftest test-shell-command-string-unicode ()
             (should (equal "/dev/stdin: UTF-8 Unicode text"
                            (shell-command-string "file"
                                                  "ひらがな\n"
                                                  "-"))))

(ert-run-tests-batch-and-exit)
