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

(defmacro sbt-test-in-sbt-buffer (&rest body)
  `(let ((sbt-root (sbt:find-root)))
     (unless sbt-root
       (user-error "You're not in an sbt project."))

     (unless (comint-check-proc (sbt:buffer-name))
       (user-error "No sbt running for current project %s." (sbt:buffer-name)))

     (with-current-buffer (sbt:buffer-name)
       ,@body)))

(defmacro sbt-test-with-socket (uri &rest body)
  (declare (indent 1) (debug t))
  `(sbt-test-in-sbt-buffer
    (unless (file-readable-p sbt-test-sbt-port-file)
      (user-error "No %s port file detected in %s." sbt-test-sbt-port-file (sbt:find-root)))
    (let* ((json (json-read-file sbt-test-sbt-port-file))
           (uri (alist-get 'uri json)))
      (if uri
          (let ((,uri (substring uri 8))) ;; Drop local:// prefix
            ,@body)
        (user-error "No uri found.")))))

(defmacro sbt-test-with-initialized-socket (&rest body)
  (declare (indent 0) (debug t))
  `(sbt-test-with-socket uri
     (unless sbt-test-sbt-jsonrpc-endpoint
       (setq sbt-test-sbt-jsonrpc-endpoint (sbt-test-sbt-jsonrpc-connection uri))
       (jsonrpc-request sbt-test-sbt-jsonrpc-endpoint 'initialize '(initializationOptions #s(hash-table))))
     ,@body))

(defmacro sbt-test-with-test-data (&rest body)
  (declare (indent 0) (debug t))
  `(sbt-test-in-sbt-buffer
    (if (null sbt-test-sbt-data)
        (sbt-test-refresh-data
         (lambda () ,@body))
      ,@body)))

(defun sbt-test-refresh-metadata (cb)
  (sbt-test-with-initialized-socket
    (let ((buffer (current-buffer)))
      (message "Refreshing tests metadata... This may take a while.")
      (condition-case err
          (jsonrpc-async-request sbt-test-sbt-jsonrpc-endpoint 'sbt/exec '(commandLine "testsMetadataRefresh")
                                 :success-fn (jsonrpc-lambda (&key status exitCode &allow-other-keys)
                                               (with-current-buffer buffer
                                                 (funcall cb)))
                                 :error-fn (lambda (response)
                                             (error "Sadly, server reports %s" response))
                                 :timeout-fn (lambda ()
                                               (error "Sadly, request for 'testsMetadataRefresh' timeout"))
                                 :timeout 120)
        ('jsonrpc-error (sbt-test-sbt-jsonrp-handle-error err))))))

(defun sbt-test-fetch-test-runner-data (cb)
  (sbt-test-with-initialized-socket
    (let ((buffer (current-buffer)))
      (condition-case err
          (jsonrpc-async-request sbt-test-sbt-jsonrpc-endpoint 'sbt/setting '(setting "*/testsMetadata")
                                 :success-fn (jsonrpc-lambda (&key value contentType)
                                               (with-current-buffer buffer
                                                 (with-local-quit
                                                   (setq sbt-test-sbt-data value)
                                                   (funcall cb))))
                                 :error-fn (lambda (response)
                                             (error "Sadly, server reports %s" response))
                                 :timeout-fn (lambda ()
                                               (error "Sadly, request for '*/testsMetadata' timeout")))
        ('jsonrpc-error (sbt-test-sbt-jsonrp-handle-error err))))))

(defun sbt-test-refresh-data (cb)
  (sbt-test-refresh-metadata
   (lambda ()
     (sbt-test-fetch-test-runner-data
      (lambda ()
        (funcall cb))))))

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
                                      :caller 'sbt-test-read
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
  (sbt-test-in-sbt-buffer
   (setq sbt-test-sbt-data nil)
   (sbt-test-refresh-data #'ignore)))

(defun sbt-test-detect-scala-class ()
  (save-excursion
    (goto-char (point-min))
    (when (search-forward-regexp "class \\([[:word:]]+\\)\\( \\|\n\\)*extends" nil t)
      (match-string-no-properties 1))))

(defun sbt-test-absolute-path(base-directory test-data)
  (let ((file-source (substring (plist-get test-data :source) 7)))  ;; Drop ${BASE}
    (concat base-directory file-source)))

(defun sbt-test-gather-file-info (scala-file cb)
  (sbt-test-with-test-data
    (let* ((base-directory (plist-get sbt-test-sbt-data :baseDirectory))
           (projects (plist-get sbt-test-sbt-data :projects))
           (bases (seq-map (lambda (record)
                             (plist-get record :base))
                           projects))
           (sorted-bases (seq-sort-by #'length #'> bases))
           (project-base (seq-find (lambda (base)
                                     (string-prefix-p base scala-file))
                                   sorted-bases))
           (file-project-data (seq-find (lambda (record)
                                          (eq project-base (plist-get record :base)))
                                        projects))
           (project (plist-get file-project-data :project))
           (test-data (seq-find (lambda (test-data)
                                  (string= scala-file (sbt-test-absolute-path base-directory test-data)))
                                (plist-get file-project-data :definedTests))))
      (if test-data
          (funcall cb (sbt-test-file-data-create
                       :absolute-path scala-file
                       :project project
                       :suite (plist-get test-data :suite)
                       :test (plist-get test-data :test)))
        (message "No Scala test file: %s" scala-file)))))

(defmacro sbt-test-with-project-data (file-project &rest body)
  (declare (indent 2) (debug t))
  `(let ((cur-buf (current-buffer)))
     (sbt-test-with-test-data
       (let* ((base-directory (plist-get sbt-test-sbt-data :baseDirectory))
              (projects (plist-get sbt-test-sbt-data :projects))
              (bases (seq-map (lambda (record)
                                (plist-get record :base))
                              projects))
              (sorted-bases (seq-sort-by #'length #'> bases))
              (project-base (seq-find (lambda (base)
                                        (string-prefix-p base (buffer-file-name cur-buf)))
                                      sorted-bases))
              (,file-project (seq-find (lambda (record)
                                         (eq project-base (plist-get record :base)))
                                       projects)))
         (with-current-buffer cur-buf
           ,@body)))))

(cl-defstruct (sbt-test-file-data (:constructor sbt-test-file-data-create)
                                  (:copier nil)
                                  (:conc-name sbt-test-file-data->))
  absolute-path
  project
  suite
  test)

(defun sbt-test-identify-file ()
  (interactive)
  (if (derived-mode-p 'scala-mode)
      (let ((current-scala-file buffer-file-name)
            (scala-class (sbt-test-detect-scala-class)))
        (sbt-test-in-sbt-buffer
         (if (null sbt-test-sbt-data)
             (user-error "No sbt data available.")
           (let* ((base-directory (plist-get sbt-test-sbt-data :baseDirectory))
                  (projects (plist-get sbt-test-sbt-data :projects))
                  (bases (seq-map (lambda (record)
                                    (plist-get record :base))
                                  projects))
                  (sorted-bases (seq-sort-by #'length #'> bases))
                  (project-base (seq-find (lambda (base)
                                            (string-prefix-p base current-scala-file))
                                          sorted-bases))
                  (file-project-data (seq-find (lambda (record)
                                                 (eq project-base (plist-get record :base)))
                                               projects))
                  (project (plist-get file-project-data :project))
                  (is-test-file (seq-some (lambda (source-directory)
                                            (string-prefix-p source-directory current-scala-file))
                                          (plist-get file-project-data :sourceDirectories))))
             (if is-test-file
                 (sbt-test-read-data base-directory projects (format "%s %s" project scala-class))
               (user-error "No Scala test file: %s" current-scala-file))))))
    (user-error "No Scala file.")))

(defun sbt-test-select-all-tests ()
  (interactive)
  (sbt-test-with-test-data
    (sbt-test-read-data (plist-get sbt-test-sbt-data :baseDirectory) (plist-get sbt-test-sbt-data :projects))))

(defun sbt-test--drop-base (source)
  "SOURCE start with ${BASE} prefix. We need to drop it."
  (substring source 7))

(defun sbt-test-run-current ()
  "Run current test file."
  (interactive)
  (sbt-test-with-project-data project-data
      (if (derived-mode-p 'scala-mode)
          (let* ((defined-test (seq-find (lambda (defined-test)
                                           (string-suffix-p (sbt-test--drop-base (plist-get defined-test :source)) buffer-file-name))
                                         (plist-get project-data :definedTests)))
                 (project (plist-get project-data :project))
                 (test-class (plist-get defined-test :test)))
            (message "%s/testOnly %s" project test-class))
        (message "No Scala file."))))

(defun sbt-test-select-current-project ()
  (interactive)
  (sbt-test-with-test-data
    (sbt-test-read-data (plist-get sbt-test-sbt-data :baseDirectory) (plist-get sbt-test-sbt-data :projects))))

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
   :name (format "sbt-test-connection-process %s" (sbt:find-root))
   :service socket
   ;;:host "local"
   :family 'local))

(defun sbt-test-sbt-jsonrpc-connection (socket)
  (make-instance
   'jsonrpc-process-connection
   :name "Sbt-Test rpc client"
   :process (sbt-test-sbt-jsonrpc-new-connection socket)
   :request-dispatcher #'sbt-test-request-dispatcher
   :notification-dispatcher #'sbt-test-notification-dispatcher
   :events-buffer-scrollback-size 0
   :on-shutdown
   (lambda (conn)
     (message "Shutdown conn: %s" (jsonrpc-name conn)))))

(defun sbt-test-request-dispatcher (conn method params)
  ;; (message "[sbt-test-request-dispatcher] method: %s" method)
  ;; (message "[sbt-test-request-dispatcher] params: %s" params)
  )


(defun sbt-test-notification-dispatcher (conn method params)
  ;; (message "[sbt-test-notification-dispatcher] method: %s" method)
  ;; (message "[sbt-test-notification-dispatcher] params: %s" params)
  )

(provide 'sbt-test-runner)
;;; sbt-test-runner.el ends here
