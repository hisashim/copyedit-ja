;; -*- encoding: utf-8 -*-
;; tests for shell-command-string
;; Copyright (c) 2010-2011 Hisashi Morita
;; License: Public Domain

(require 'ert)
(require 'shell-command-string)

(ert-deftest test-shell-command-string ()
             (should (equal "bar"
                            (shell-command-string "grep"
                                                  "foo\nbar\n"
                                                  "--color" "b"))))

(ert-run-tests-batch-and-exit)
