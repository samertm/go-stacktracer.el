;;; go-stacktracer.el --- parse Go stack traces

;; Copyright (C) 2014 Samer Masterson

;; Author: Samer T. Masterson <samer@samertm.com>
;; Keywords: Go
;; URL: https://github.com/samertm/go-stacktracer

;;; Commentary:

;; Jump through Go stacktraces easily.

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
(defun go-stacktracer-region (start end)
  "Parse a Go stacktrace from START to END."
  (interactive "r")
  (let ((trace (split-string (buffer-substring start end) "\n" t))
        (buf (go-stacktracer--get-buffer))
        (last-line ""))
    (with-current-buffer buf
      (insert "go-stacktracer results:\n\n"))
    (while trace
      (let ((line (car trace)))
        (if (not (eq (string-match go-stacktracer-re line) nil))
            (let ((file-path (substring line (match-beginning 1) (match-end 1)))
                  (line-num  (substring line (match-beginning 2) (match-end 2))))
              ;; strip default-directory from file-path.
              (if (not (eq (string-match (concat "^" default-directory) file-path) nil))
                  (setq file-path (substring file-path (match-end 0) (length file-path))))
              (with-current-buffer buf
                (insert file-path ":" line-num ": " last-line "\n"))))
        (setq last-line line))
      (setq trace (cdr trace)))
    (with-current-buffer buf
      (grep-mode))
    (display-buffer buf)))

(provide 'go-stacktracer)
;;; go-stacktracer.el ends here


