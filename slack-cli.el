(define-derived-mode slack-cli-mode special-mode "slack-cli-mode")

(defun slack-cli-mode--setup-keys ()
  "Initial config for setting of keys."
  (local-set-key (kbd "r") 'slack-cli-reply-on-buffer))

(add-hook 'slack-cli-mode-hook 'slack-cli-mode--setup-keys)

(defun slack-cli--refresh-buffer ()
  "Refreshes the channel buffer with the latest details"
  (let (( inhibit-read-only 1))
    (erase-buffer)
    (slack-cli-mode)
    (slack-cli-retrieve channel "10")))

(defun slack-cli-send (&optional _channel)
  "Sends slack message"
  (interactive)
  (let* ((channel
	  (if _channel
	      _channel
	    (completing-read "Select channel" slack-cli-channels)))
	 (message (read-string (concat "Send message to " channel ": ")))
	 (buffer-name (concat "*slack-cli:" channel "*")))
    (switch-to-buffer buffer-name)
    (when (shell-command-to-string (concat "slack-cli -d " channel " \"" message "\""))
      (slack-cli--refresh-buffer))))

(defun slack-cli-retrieve (&optional _channel _retrieve-count)
  "Retrieve slack messages"
  (interactive)
  (let* ((channel (if _channel
		      _channel
		    (completing-read "Select channel" slack-cli-channels)))
	 (num (if _retrieve-count
		  _retrieve-count
		(read-string "Num to retrieve (default: 10): " nil nil "10")))
	 (buffer-name (concat "*slack-cli:" channel "*"))
	 (retrieved (shell-command-to-string (concat "slack-cli -s " channel " -l " num))))
    (switch-to-buffer buffer-name)
    (let ((inhibit-read-only 1))
      (insert retrieved))))

(defun slack-cli-listen ()
  "Listen continuously for slack messages"
  (interactive)
  (let* ((channel (completing-read "Select channel" slack-cli-channels))
	 (buffer-name (concat "*slack-cli:" channel "*")))
    (start-process (concat "slack-cli:" channel) nil "slack-cli" "-s" channel)
    (set-process-filter (get-buffer-process buffer-name) 'slack-cli-process-output)
    (switch-to-buffer buffer-name)
    (slack-cli--refresh-buffer)))

(defun slack-cli-process-output(proc string)
  "Custom process filter to handle specific charaters."
  (let ((inhibit-read-only 1))
    (goto-char (process-mark proc))
    (insert (ansi-color-apply string))
    (set-marker (process-mark proc) (point))))

(defun slack-cli-reply-on-buffer()
  "Replies to the slack buffer."
  (interactive)
  (let ((channel (car (last (split-string (replace-regexp-in-string "\*" "" (buffer-name)) ":")))))
    (slack-cli-send channel)))

