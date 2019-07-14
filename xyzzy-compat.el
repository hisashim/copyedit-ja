;; -*- encoding: utf-8 -*-
;; Utility to write compatible code for Emacs and xyzzy
;;   * GNU Emacs: https://www.gnu.org/software/emacs/
;;   * xyzzy: https://github.com/xyzzy-022/xyzzy
;; Copyright (c) 2008-2019 Hisashi Morita
;; License: Public Domain
;;
;; Usage example:
;;   (defun get-minibuffer-history ()
;;     (if-emacs-else-xyzzy
;;       minibuffer-history           ; emacs
;;       *minibuffer-default-history* ; xyzzy equivalent of minibuffer-history
;;       ))

(if (boundp 'emacs-version)
    nil
  (progn
    (defun emacs-version ()
      (concat (software-type) " " (software-version)))
    (defun read-from-minibuffer (prompt &optional opts)
      (read-string prompt :default opts))))

(defmacro if-emacs-else-xyzzy (emacs-form xyzzy-form)
  "Evaluate a form for current platform.

Usage example:
  (defun get-minibuffer-history ()
    (if-emacs-else-xyzzy
      minibuffer-history           ; emacs
      *minibuffer-default-history* ; xyzzy equivalent of minibuffer-history
      ))"
  `(cond ((string-match "Emacs" (emacs-version)) ,emacs-form)
         ((string-match "xyzzy" (emacs-version)) ,xyzzy-form)
         (else (error (format "Unknown platform (emacs-version: %S)"
			      (emacs-version))))))

(provide 'xyzzy-compat)
