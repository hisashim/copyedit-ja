;; -*- utf-8-unix -*-
;; LaTeX copyediting helper
;; Copyright (c) 2008-2011 Hisashi Morita
;; License: Public Domain

(require xyzzy-compat)
(require 'copyedit-ja)

(defun latex-env-around-region (begin end pre &optional env-name)
  "Enclose region with named LaTeX environment"
  (interactive
      (list (region-beginning)
            (region-end)
            (if-emacs-else-xyzzy current-prefix-arg
				 (if *prefix-args* *prefix-value* nil))
            (read-from-minibuffer "Env name: "
                                  (car (if-emacs-else-xyzzy
					minibuffer-history
					*minibuffer-default-history*)))))
  (let* ((env-begin (concat "\\begin{" (or env-name "") "}\n"))
         (env-end (concat "\\end{" (or env-name "") "}\n")))
    (save-excursion
      (save-restriction
        (narrow-to-region begin end)
        (goto-char (point-min))
        (insert env-begin)
        (goto-char (point-max))
        (insert env-end)))))

(defun latex-insert-index-ja (begin end pre &optional key)
  "Insert \\index{key@term} using region text (or last killed text if
   with prefix arg) as index term. If the term is Japanese and the key is
   ommited, term's reading is copied as key."
  (interactive "r\nP\nsKey:")
  (let ((term (if pre
                  (caar *kill-ring*)
                (buffer-substring begin end))))
    (insert (concat
             "\\index{"
             (if (or (equal key nil) (equal key ""))
                 (if (string-match "\\cj" term)
                     (concat (copyedit-ja-get-reading-katakana term) "@")
                   "")
               (concat key "@"))
             term
             "}" )
            )))
