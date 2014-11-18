;;; go-stacktracer.el --- parse Go stack traces

;; Copyright (C) 2014 Samer Masterson

;; Author: Samer T. Masterson <samer@samertm.com>
;; Keywords: tools
;; URL: https://github.com/samertm/go-stacktracer

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

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


