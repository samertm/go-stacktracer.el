;;; go-stacktracer.el --- parse Go stack traces

;; Copyright (C) 2014 Samer Masterson

;; Author: Samer T. Masterson <samer@samertm.com>
;; Keywords: Go
;; URL: https://github.com/samertm/go-stacktracer

;;; Commentary:

;;; Code:

(defun go-stacktracer--get-buffer ()
  "Get an empty buffer for src's output."
  (let* ((buffer-name "*go-stacktracer*")
         (buffer (get-buffer buffer-name)))
    ;; Kill the existing buffer if it already exists.
    (when buffer (kill-buffer buffer))
    (get-buffer-create buffer-name)))

(defconst go-stacktracer-re "^\\s-*\\([^ ]*\\.go\\):\\([[:digit:]]+\\)")

;;;###autoload
(defun go-stacktracer-parse-region (start end)
  "Parses a go stacktrace in a region"
  (interactive "r")
  (let ((trace (split-string (buffer-substring start end) "\n" t))
        (buf (go-stacktracer--get-buffer)))
    (with-current-buffer buf
      (insert "go-stacktracer results:\n\n"))
    (while trace
      (let ((line (car trace)))
        (if (not (eq (string-match go-stacktracer-re line) nil))
            (let ((file-path (substring line (match-beginning 1) (match-end 1)))
                  (line-num  (substring line (match-beginning 2) (match-end 2))))
              (with-current-buffer buf
                (insert "    " file-path " on line " line-num "\n")))))
      (setq trace (cdr trace)))
    (display-buffer buf)))


