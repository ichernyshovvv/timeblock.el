;; -*- lexical-binding: t; -*-

(require 'org) ;; for `org-read-date'

(let* ((date (parse-time-string "2024-11-08 00:00"))
       (entries
        (list (list
               (cons 'start (parse-time-string "2024-11-08 10:00"))
               (cons 'end (parse-time-string "2024-11-08 11:00"))
               (cons 'title "Block 1"))
              (list
               (cons 'start (parse-time-string "2024-11-08 09:30"))
               (cons 'end (parse-time-string "2024-11-08 11:00"))
               (cons 'title "Block 2"))
              (list
               (cons 'start (parse-time-string "2024-11-08 12:00"))
               (cons 'end (parse-time-string "2024-11-08 17:00"))
               (cons 'title "Block 4"))
              (list
               (cons 'start (parse-time-string "2024-11-08"))
               (cons 'end nil)
               (cons 'title "All-day Block 3")))))
  (timeblock-insert-column
   entries date 200 350
   :show-date t :show-all-day-entries t
   :scope '(6 . 24)
   :keymap (let ((map timeblock-column-map))
             (keymap-set map "e" #'timeblock-reschedule)
             (keymap-set map "<drag-mouse-1>" #'timeblock-drag-n-drop)
             map)))
 
(defun timeblock-reschedule ()
  (interactive)
  (when-let* ((svg (get-text-property (point) 'dom))
              (entries (dom-attr svg 'entries))
              (date (encode-time (dom-attr svg 'date)))
              (block-id (dom-attr (timeblock-get-selected svg) 'id))
              (entry (nth (string-to-number block-id) entries)))
    (setf (alist-get 'start entry)
          (decode-time (org-read-date t t nil "Start: " date)))
    (setf (alist-get 'end entry)
          (decode-time (org-read-date t t nil "End: " date)))
    (timeblock-redisplay-column)))

(defun timeblock-drag-n-drop (event)
  "Draw a line from the start of EVENT to its end."
  (interactive "e")
  (when-let* ((start (posn-object-x-y (event-start event)))
              (end (posn-object-x-y (event-end event)))
              (svg (get-text-property (point) 'dom))
              (entries (dom-attr svg 'entries))
              (block-id
               (dom-attr
                (timeblock-block-at-position svg (car start) (cdr start)) 'id))
              (entry (nth (string-to-number block-id) entries))
              (hour (timeblock-hour-at-position svg (cdr end))))
    (let* ((start-ts (alist-get 'start entry))
           (end-ts (alist-get 'end entry))
           (duration (and end-ts (timeblock-time-diff end-ts start-ts)))
           (new-start-ts (timeblock-time-apply start-ts :hour hour :minute 0))
           (new-end-ts (and duration
                            (timeblock-time-inc 'minute duration new-start-ts))))
      (setf (alist-get 'start entry) new-start-ts)
      (setf (alist-get 'end entry) new-end-ts)
      (timeblock-redisplay-column))))

(cl-defun timeblock-time-apply (time &key second minute hour day month year)
  "Return new timestamp based on TIME with new slot values from keys."
  (declare (indent 1))
  ;; This code is borrowed from `ts-apply' function which is part of ts.el
  ;; project written by Adam Porter
  (let ((time (copy-sequence time)))
    (and second (setf (dt-second time) second))
    (and minute (setf (dt-minute time) minute))
    (and hour (setf (dt-hour time) hour))
    (and day (setf (dt-day time) day))
    (and month (setf (dt-month time) month))
    (and year (setf (dt-year time) year))
    time))

(defun timeblock-time-diff (a b)
  "Return difference between times A and B in minutes."
  (when-let* ((a (encode-time a))
              (b (encode-time b)))
    (/ (time-convert (time-subtract a b) 'integer) 60)))
