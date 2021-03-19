;; sbt-test-munit.el --- Utility for running scala test suites in sbt -*- lexical-binding: t; -*-

(require 'sbt-test-runner)

(defconst sbt-test-munit-regex "test(\\(.*\\(?:\n.*\\)?\\)) {")

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
  (when (derived-mode-p 'scala-mode)
    (save-excursion
      (search-backward "test(" nil t)
      (search-forward-regexp sbt-test-munit-regex nil t)
      (string-trim (substring-no-properties (match-string 1))))))

(defun sbt-test-absolute-path (base-directory test-data)
  (let ((file-source (substring (plist-get test-data :source) 7)))  ;; Drop ${BASE}
    (concat base-directory file-source)))

(defun sbt-test-munit--find-defined-test (base-directory project-data)
  (seq-find (lambda (test-data)
              (string= buffer-file-name (sbt-test-absolute-path base-directory test-data)))
            (plist-get project-data :definedTests)))

(defmacro sbt-test-with-munit-test (&rest body)
  (interactive)
  `(sbt-test-with-project-data project-data
      (if (derived-mode-p 'scala-mode)
          (let ((test-data (sbt-test-munit--find-defined-test base-directory project-data)))
            (if (string= "munit" (plist-get test-data :suite))
                ,@body
              (message "Not munit suite %s" buffer-file-name)))
        (message "Not a Scala file."))))

(defun sbt-test-munit-jump-to-test-name (test-name current-test-name)
  (let ((selected-test-name (get-text-property 0 :test-name current-test-name))
        (line-number (string-to-number (get-text-property 0 :line-number current-test-name))))
    (unless (and (eq this-command 'ivy-done)
                 (string= test-name selected-test-name))
      (goto-char (point-min))
      (forward-line (1- line-number)))))

(defun sbt-test-munit-select-test ()
  (interactive)
  (sbt-test-munit-select-and-run nil
                                 (lambda (x)
                                   (goto-char (point-min))
                                   (forward-line (1- (string-to-number (get-text-property 0 :line-number x)))))))

(defun sbt-test-munit-prefer-current ()
  (interactive)
  (let ((test-name (sbt-test-munit-find-test-name)))
    (sbt-test-munit-select-and-run test-name (lambda (x) (sbt-test-munit-jump-to-test-name test-name x)))))

(defun sbt-test-munit-select-and-run (initial-input action)
  (sbt-test-with-munit-test
   (let* ((raw-collection (sbt-test-munit-matches-in-buffer sbt-test-munit-regex))
          (collection (seq-map #'sbt-test-munit-propertize-test raw-collection)))
     (if (null collection)
         (message "No test found!")
       (let* ((selection (ivy-read "Run test: " collection
                                   :initial-input initial-input
                                   :caller 'sbt-test-read
                                   :action action))
              (selected-test-name (get-text-property 0 :test-name selection))
              (defined-test (sbt-test--defined-test project-data))
              (project (plist-get project-data :project))
              (test-class (plist-get defined-test :test))
              (command (format "testOnly %s -- \"--tests=%s" test-class (substring selected-test-name 1)) ))
         (sbt-hydra:run-run-project-command command project)
         (message (format "Running: %s/%s" project command)))))))

(provide 'sbt-test-munit)
;;; sbt-test-munit.el ends here
