;; -*- lexical-binding: t -*-

(require 'org-clock-csv)

;;; Helper Code:

(defun org-clock-csv-should-match (input output)
  "Test that clock entries in INPUT match the .csv OUTPUT file."
  (let* ((entries (with-temp-buffer
                    (insert-file-contents input)
                    (org-mode)
                    (org-element-map (org-element-parse-buffer) 'clock
                      #'org-clock-csv--parse-element nil nil)))
         (in (with-temp-buffer
               (insert org-clock-csv-header "\n")
               (mapc (lambda (entry)
                       (insert (concat (funcall org-clock-csv-row-fmt entry) "\n")))
                     entries)
               (buffer-string)))
         (out (with-temp-buffer
                (insert-file-contents output)
                (buffer-string))))
    (should (equal in out))))

;;; Tests:

(ert-deftest test-sample ()
  "Docs."
  (org-clock-csv-should-match "tests/sample.org" "tests/sample.csv"))

;; Local Variables:
;; coding: utf-8
;; End:

;;; org-clock-csv-tests.el ends here
