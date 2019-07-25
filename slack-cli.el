(defun slack-cli-send ()
  "Sends slack message"
  (interactive)
  (let ((channel (completing-read "Select channel" slack-cli-channels))
	(message (read-string "Your message: "))
	)
    (start-process-shell-command "slack-cli" "*slack-cli*" (concat "slack-cli -d " channel " \"" message "\""))))

(defun slack-cli-retrieve ()
  "Retrieve slack messages"
  (interactive)
  (let* ((channel (completing-read "Select channel" slack-cli-channels))
	 (num (read-string "Num to retrieve (default: 10): " nil nil "10"))
	 (buffer-name (concat "*slack-cli:" channel "*"))
	 (retrieved (shell-command-to-string (concat "slack-cli -s " channel " -l " num))))
    (switch-to-buffer buffer-name)
    (insert retrieved)))

(defun slack-cli-listen ()
  "Listen continuously for slack messages"
  (interactive)
  (let* ((channel (completing-read "Select channel" slack-cli-channels))
	 (buffer-name (concat "*slack-cli:" channel "*")))
    (start-process (concat "slack-cli:" channel) buffer-name "slack-cli" "-s" "bot-test-channel")
    (set-process-filter (get-buffer-process buffer-name) 'slack-cli-process-output)
    (switch-to-buffer buffer-name)))

(defun slack-cli-process-output(proc string)
  "Custom process filter to handle specific charaters."
  (goto-char (process-mark proc))
  (insert (ansi-color-apply string))
  (set-marker (process-mark proc) (point)))
