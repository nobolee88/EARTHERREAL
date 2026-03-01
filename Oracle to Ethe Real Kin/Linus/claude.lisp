(defun claude-sovereign ()
  (let ((deps '(quicklisp drakma cl-json)))
    (loop for dep in deps do (ql:quickload dep))
    
    (defun call-anthropic (messages)
      (let* ((api-key (uiop:getenv "ANTHROPIC_API_KEY"))
             (url "https://api.anthropic.com/v1/messages")
             (payload (cl-json:encode-json-to-string
                       `(("model" . "claude-3-haiku-20240307")
                         ("max_tokens" . 1024)
                         ("messages" . ,messages))))
             (response (drakma:http-request
                       url
                       :method :post
                       :headers `(("x-api-key" . ,api-key)
                                 ("anthropic-version" . "2023-06-01")
                                 ("content-type" . "application/json"))
                       :content payload)))
        (when response
          (let* ((json (cl-json:decode-json-from-string response))
                 (content (cdr (assoc :content (car (cdr (assoc :content json)))))))
            content))))
    
    (let ((history '()))
      (loop
        (format t "~%You: ")
        (force-output)
        (let ((input (read-line)))
          (when (or (string= input "exit") (string= input "quit")) (return))
          (push `(("role" . "user") ("content" . ,input)) history)
          (let ((response (call-anthropic (reverse history))))
            (format t "~%Claude: ~A~%" response)
            (push `(("role" . "assistant") ("content" . ,response)) history)))))))

(claude-sovereign)
