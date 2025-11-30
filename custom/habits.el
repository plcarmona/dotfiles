(defun add-habit-entry-consult ()
	"Prompt for habit action and value, then add entry to habits.csv."
	(interactive)
	(let* ((habits '("wheels" "sleep_time" "plancha" "ultima_comida"))
		   (action (completing-read "Select action: " habits))
		   (value (read-number "Enter value: "))
		   (timestamp (format-time-string "%s"))
		   (entry (format "%s,%s,%d\n" timestamp action value)))
	  (with-temp-buffer
		(insert-file-contents "~/notes/data/habits.csv")
		(goto-char (point-max))
		(insert entry)
		(write-region (point-min) (point-max) "~/notes/data/habits.csv"))
	  (message "Added entry: %s" entry)))
