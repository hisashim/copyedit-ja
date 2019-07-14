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

;; define emacs-version for non-Emacs
(if (not (boundp 'emacs-version))
    (let ((type (if (fboundp 'lisp-implementation-type)
                    (lisp-implementation-type)
                  "unknown-lisp-implementation-type"))
          (version (if (fboundp 'lisp-implementation-version)
                       (lisp-implementation-version))
                   "unknown-lisp-implementation-version"))
      (defun emacs-version () (concat type " " version))))

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

;; define or adjust some of Emacs standard stuff for xyzzy
(if-emacs-else-xyzzy
 (emacs-version) ; Emacs
 (progn ; xyzzy
   (defmacro defconst (name val) `(defconstant ,name ,val))
   (defun mapconcat (f seq sep)
     (reduce (lambda (acc e) (concat acc sep (apply f (list e)))) seq))
   ;; backup match-string as match-string-orig
   (setf (symbol-function 'match-string-orig) #'match-string)
   ;; let match-string ignore extra args
   (defun match-string (nth &rest args) (match-string-orig nth))
   (defun read-from-minibuffer (prompt &optional opts)
     (read-string prompt :default opts))
   (emacs-version)))

(provide 'xyzzy-compat)
