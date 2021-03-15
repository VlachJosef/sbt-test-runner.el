;; sbt-test-munit.el --- Utility for running scala test suites in sbt -*- lexical-binding: t; -*-

(require 'sbt-test-runner)

(defun sbt-test-munit-matches-in-buffer (regexp)
  "Return a list of matches of REGEXP in the current buffer."
  (let ((matches))
    (save-match-data
      (save-excursion
        (with-current-buffer (current-buffer)
          (save-restriction
            (goto-char 1)
            (while (search-forward-regexp regexp nil t 1)
              (push (cons (number-to-string (line-number-at-pos)) (substring-no-properties (match-string 1))) matches)))))
      (reverse matches))))

(defun sbt-test-munit-propertize-test (ele)
  (pcase ele
    (`(,line-number . ,test-name) (let ((trimmed-test-name (string-trim test-name)))
                                    (propertize (format "%s: %s" line-number trimmed-test-name)
                                               :line-number line-number
                                               :test-name trimmed-test-name)))))

(defun sbt-test-munit-find-test-name ()
  (save-excursion
    (search-backward "test(" nil t)
    (search-forward-regexp "test(\\(.*\\(?:\n.*\\)?\\)) {" nil t)
    (string-trim (substring-no-properties (match-string 1)))))

(defun sbt-test-munit-select-test ()
  (interactive)
  (if (derived-mode-p 'scala-mode)
      (when-let ((buffer (current-buffer))
                 (current-scala-file buffer-file-name))
        (sbt-test-gather-file-info
         current-scala-file
         (lambda (sbt-test-file-data)
           (with-current-buffer buffer
             (when (string= "munit" (sbt-test-file-data->suite sbt-test-file-data))
               (let* ((raw-collection (sbt-test-munit-matches-in-buffer "test(\\(.*\\(?:\n.*\\)?\\)) {"))
                      (collection (seq-map #'sbt-test-munit-propertize-test raw-collection)))
                 (if (null collection)
                     (message "No test found! %s" raw-collection)
                   (let* ((selection (ivy-read "Run test: " collection
                                               :caller 'sbt-test-read
                                               :action (lambda (x)
                                                         (goto-char (point-min))
                                                         (forward-line (1- (string-to-number (get-text-property 0 :line-number x)))))))
                          (test-name (get-text-property 0 :test-name selection)))
                     (message "selection %s" selection)))))))))
    (user-error "No Scala file.")))


(defun sbt-test-munit-prefer-current ()
  (let* ((test-name (sbt-test-munit-find-test-name))
         (orig (sbt-test-munit-matches-in-buffer "test(\\(.*\\(?:\n.*\\)?\\)) {"))
         (collection (seq-map #'sbt-test-munit-propertize-test orig)))
    (if (null collection)
        (message "No test found!")
      (let*
          ((selection (ivy-read "Run test: " collection
                               :initial-input test-name
                               :caller 'sbt-test-read
                               :action (lambda (x)
                                         (goto-char (point-min))
                                         (forward-line (1- (string-to-number (get-text-property 0 :line-number x)))))))
           (test-name (get-text-property 0 :test-name selection)))
        (with-current-buffer (current-buffer)
          (let ((line-number (string-to-number (get-text-property 0 :line-number selection))))
            (goto-char (point-min))
            (forward-line (1- line-number))))
        (format " -- \"--tests=%s" (substring test-name 1))))))


(provide 'sbt-test-munit)
;;; sbt-test-munit.el ends here
