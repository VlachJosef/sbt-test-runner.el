;; sbt-test-runner.el --- Utility for running scala test suites in sbt -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'jsonrpc)
(require 'eieio)
(require 'sbt-mode)
(require 'ivy)

(eval-when-compile (require 'subr-x))

(defvar-local sbt-test-sbt-jsonrpc-endpoint nil)
(defvar-local sbt-test-sbt-data nil)

(defconst sbt-test-sbt-port-file "project/target/active.json"
  "https://www.scala-sbt.org/1.x/docs/sbt-server.html#Server+discovery+and+authentication")

(defmacro sbt-test-with-sbt-buffer (&rest body)
  `(let ((sbt-root (sbt:find-root)))
     (unless sbt-root
       (user-error "You're not in an sbt project."))

     (unless (comint-check-proc (sbt:buffer-name))
       (user-error "No sbt running for current project."))

     (with-current-buffer (sbt:buffer-name)
       ,@body)))

(defmacro sbt-test-with-socket (uri &rest body)
  (declare (indent 1) (debug t))
  `(sbt-test-with-sbt-buffer
    (unless (file-readable-p sbt-test-sbt-port-file)
      (user-error "No %s port file detected in %s." sbt-test-sbt-port-file (sbt:find-root)))
    (let* ((json (json-read-file sbt-test-sbt-port-file))
           (uri (alist-get 'uri json)))
      (if uri
          (let ((,uri (substring uri 8))) ;; Drop local:// prefix
            ,@body)
        (user-error "No uri found.")))))

(defmacro sbt-test-with-initialized-socket (&rest body)
  (declare (indent 1) (debug t))
  `(sbt-test-with-socket uri
                      (unless sbt-test-sbt-jsonrpc-endpoint
                        (setq sbt-test-sbt-jsonrpc-endpoint (sbt-test-sbt-jsonrpc-connection uri))
                        (jsonrpc-request sbt-test-sbt-jsonrpc-endpoint 'initialize '(initializationOptions #s(hash-table))))
                      ,@body))

(defun sbt-test-run-pipa (callback)
  (sbt-test-with-initialized-socket
   (condition-case err
       (jsonrpc-async-request sbt-test-sbt-jsonrpc-endpoint 'sbt/exec '(commandLine "pipa")
                              :success-fn (jsonrpc-lambda (&key status exitCode &allow-other-keys)
                                            (message "Server replied back with %s and %s!" status exitCode)
                                            (funcall callback))
                              :error-fn (lambda (response)
                                          (error "Sadly, server reports %s" response)))
     ('jsonrpc-error (sbt-test-sbt-jsonrp-handle-error err)))))

(defun sbt-test-fetch-test-runner-data (callback)
  (sbt-test-with-initialized-socket
   (condition-case err
       (jsonrpc-async-request sbt-test-sbt-jsonrpc-endpoint 'sbt/setting '(setting "*/testRunnerData")
                              :success-fn (jsonrpc-lambda (&key value contentType)
                                            (funcall callback value))
                              :error-fn (lambda (response)
                                          (error "Sadly, server reports %s" response)))
     ('jsonrpc-error (sbt-test-sbt-jsonrp-handle-error err)))))

(defun sbt-test-refresh-data (callback)
  (sbt-test-run-pipa
   (lambda ()
     (sbt-test-fetch-test-runner-data
      (lambda (data)
        (with-current-buffer (sbt:buffer-name)
          (setq sbt-test-sbt-data data)
          (funcall callback data)))))))

(defun sbt-test-test-data-to-string (project test-data)
  (propertize (concat project "/" (plist-get test-data :test))
              :project project
              :test (plist-get test-data :test)
              :source (plist-get test-data :source)))

(defun sbt-test-process (acc el)
  (let ((project (plist-get el :project))
        (definedTests (plist-get el :definedTests)))
    (append acc (seq-map (lambda (test-data) (sbt-test-test-data-to-string project test-data)) definedTests))))

(defun sbt-test-read-data (base-directory projects &optional initial-input)
  (let ((test-collection (seq-reduce #'sbt-test-process projects nil)))
    (when-let ((test-to-run (ivy-read "Run test: " test-collection
                                      :initial-input initial-input
                                      :action (lambda (x)
                                                (let* ((file-source (substring (get-text-property 0 :source x) 7))  ;; Drop ${BASE}
                                                       (file-path (concat base-directory file-source)))
                                                  (if (file-readable-p file-path)
                                                      (find-file file-path)
                                                    (error "Problem open file: %s" file-path))))))
               (project (get-text-property 0 :project test-to-run))
               (test (get-text-property 0 :test test-to-run)))
      (sbt:command (format "%s/testOnly %s" project test)))))

(defun sbt-test-refresh ()
  (interactive)
  (sbt-test-with-sbt-buffer
   (setq sbt-test-sbt-data nil)
   (sbt-test-refresh-data #'ignore)))

(defun sbt-test-detect-scala-class ()
  (save-excursion
    (goto-char (point-min))
    (when (search-forward-regexp "class \\([[:word:]]+\\)\\( \\|\n\\)*extends" nil t)
      (match-string-no-properties 1))))

(defun sbt-test-identify-file ()
  (interactive)
  (if (derived-mode-p 'scala-mode)
      (let ((current-scala-file buffer-file-name)
            (scala-class (sbt-test-detect-scala-class)))
        (sbt-test-with-sbt-buffer
         (if (null sbt-test-sbt-data)
             (user-error "No sbt data available.")
           (let* ((bases (seq-map (lambda (record)
                                    (plist-get record :base))
                                  sbt-test-sbt-data))
                  (sorted-bases (seq-sort-by #'length #'> bases))
                  (project-base (seq-find (lambda (base)
                                            (string-prefix-p base current-scala-file))
                                          sorted-bases))
                  (file-project-data (seq-find (lambda (record)
                                                 (eq project-base (plist-get record :base)))
                                               sbt-test-sbt-data))
                  (project (plist-get file-project-data :project))
                  (is-test-file (seq-some (lambda (source-directory)
                                            (string-prefix-p source-directory current-scala-file))
                                          (plist-get file-project-data :sourceDirectories))))
             (if is-test-file
                 (sbt-test-read-data sbt-test-sbt-data (format "%s %s" project scala-class))
               (user-error "No Scala test file: %s" current-scala-file))))))
    (user-error "No Scala file.")))

(defun sbt-test-run-tests ()
  (interactive)
  (sbt-test-with-sbt-buffer
   (if (null sbt-test-sbt-data)
       (sbt-test-refresh-data #'sbt-test-read-data)
     (sbt-test-read-data (plist-get sbt-test-sbt-data :baseDirectory) (plist-get sbt-test-sbt-data :projects)))))

(defun sbt-test-sbt-jsonrp-handle-error (jsonrpc-error)
  (cl-destructuring-bind ((_ . code) (_ . message) (_ . data)) (cdr (cdr jsonrpc-error))
    ;; See https://github.com/sbt/sbt/blob/develop/protocol/src/main/scala/sbt/internal/langserver/ErrorCodes.scala
    (pcase code
      (-33000 nil) ;; Returned when compilation fails
      (_ (error "Unexpected error %s %s %s" code message data)))))

(defun sbt-test-test-runner-data (data)
  (with-current-buffer (sbt:buffer-name)
    (setq sbt-test-sbt-data data)))

(defun sbt-test-sbt-jsonrpc-new-connection (socket)
  (make-network-process
   :name "sbt-test-bsp-connection"
   :service socket
   ;;:host "local"
   :family 'local))

(defun sbt-test-sbt-jsonrpc-connection (socket)
  (make-instance
   'jsonrpc-process-connection
   :name "Sbt-Test rpc client"
   :process (sbt-test-sbt-jsonrpc-new-connection socket)
   :on-shutdown
   (lambda (conn)
     (message "Shutdown conn: %s" (jsonrpc-name conn)))))

(provide 'sbt-test-runner)
;;; sbt-test-runner.el ends here
