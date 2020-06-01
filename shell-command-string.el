;; -*- encoding: utf-8 -*-
;; shell-command-string: command invocation
;; Copyright (c) 2010-2011 Hisashi Morita
;; License: Public Domain
;;
;; Usage:
;;   (shell-command-string "grep" STRING "-in")
;;
;; Note:
;;   If something gets wrong when dealing with non-ASCII strings,
;;   try setting character encoding explicitly, e.g.
;;   (progn (prefer-coding-system 'utf-8-unix) ...

(defun shell-command-string (cmd input &rest opts)
  "Run CMD as sub-process with INPUT for stdin. Return stdout as string."
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

(provide 'shell-command-string)
