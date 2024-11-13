;;; timeblock.el --- Interactive SVG calendar -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Ilya Chernyshov

;; Author: Ilya Chernyshov <ichernyshovvv@gmail.com>
;; Version: 0.1-pre
;; Package-Requires: ((emacs "28.1") (svg "1.1"))
;; Keywords: calendar, timeblocking, agenda
;; URL: https://github.com/ichernyshovvv/timeblock

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; timeblock.el is a package that allows you to visually
;; understand your day schedule, quickly reschedule your tasks and set
;; TODO statuses

;; API

;; timeblock-make-column

;;; Code:

;;;; Requirements

(require 'svg)
(require 'seq)
(require 'map)
(require 'cl-lib)
(require 'timeblock-faces)

;;;; Custom Variables

(defgroup timeblock nil
  "Customization for `timeblock'."
  :link '(url-link "https://github.com/ichernyshovvv/timeblock")
  :group 'applications)

(defcustom tb-scale-options t
  "Options that are used to decide which part of visual schedule must be hidden."
  :type '(choice
          (const :tag "Hide hours in the past (if there are no timeblocks)." t)
          (const :tag "Do not hide anything.  All 24 hours will be displayed." nil)
          (const :tag "Hide all free hours before the first timeblock." hide-all)
          (cons :tag "Display specified range of hours [earliest; latest)."
                (integer :tag "Min Hour")
                (integer :tag "Max Hour"))))

(defcustom tb-current-time-indicator t
  "Whether to show current time indicator in the `tb-list' buffer."
  :type 'boolean)

;;;; Variables

(defvar tb-colors nil)

;;;; Keymaps

(defvar-keymap tb-column-map
  "<mouse-1>" #'tb-select-with-cursor
  "<drag-mouse-1>" #'tb-handle-drag
  "<remap> <right-char>" #'tb-right
  "<remap> <left-char>" #'tb-left
  "<remap> <next-line>" #'tb-down
  "<remap> <previous-line>" #'tb-up
  "g" #'tb-update-column
  "m" #'tb-mark
  "%" #'tb-mark-by-regexp
  "u" #'tb-unmark
  "U" #'tb-unmark-all)

;;;; Functions

(defsubst tb-format-time (format-string time)
  "Use FORMAT-STRING to format the time value TIME."
  (let ((time (copy-sequence time)))
    (unless (dt-second time) (setf (dt-second time) 0))
    (unless (dt-minute time) (setf (dt-minute time) 0))
    (unless (dt-hour time) (setf (dt-hour time) 0))
    (format-time-string format-string (encode-time time))))

(defun tb-get-dates (from to)
  "Return a list of decoded-time dates between FROM and TO."
  (let (dates)
    (while (and (push from dates)
                (setq from (tb-time-inc 'day 1 from))
                (tb-date<= from to)))
    (nreverse dates)))

(defun tb-get-selected (svg)
  "Return an id of the entry of selected timeblock.
id is constructed via `tb-construct-id'"
  (car (dom-search svg (lambda (node) (dom-attr node 'select)))))

(defmacro tb-on (accessor op lhs rhs)
  "Run OP on ACCESSOR's return values from LHS and RHS."
  `(,op (,accessor ,lhs) (,accessor ,rhs)))

(defun tb-date= (a b)
  "Return non-nil if dates of A and B time values are equal."
  (cond
   ((not (or a b)))
   ((and a b)
    (and (tb-on dt-year  = a b)
         (tb-on dt-month = a b)
         (tb-on dt-day   = a b)))))

(defun tb-date< (a b)
  "Return non-nil if A's date is less than B's date."
  (cond
   ;; nil is less than non-nil
   ((not b) nil)
   ((not a) t)
   (t (or (tb-on dt-year < a b)
          (and (tb-on dt-year = a b)
               (or (tb-on dt-month < a b)
                   (and (tb-on dt-month = a b)
                        (tb-on dt-day < a b))))))))

(defun tb-time-diff (a b)
  "Return difference between times A and B in minutes."
  (when-let* ((a (encode-time a))
              (b (encode-time b)))
    (/ (time-convert (time-subtract a b) 'integer) 60)))

(defun tb-decoded< (a b)
  "Return non-nil if A is earlier then B."
  (cond
   ((not b) nil)
   ((not a) t)
   (t (time-less-p
       (encode-time a)
       (encode-time b)))))

(defun tb-decoded= (a b)
  "Return non-nil if A is earlier then B."
  (cond
   ;; nil is less than non-nil
   ((not b) nil)
   ((not a) t)
   (t (time-equal-p (encode-time a) (encode-time b)))))

(defun tb-time< (a b)
  "Return non-nil if A's time is earlier then B's time.
Compare only hours and minutes."
  (cond
   ;; nil is less than non-nil
   ((not b) nil)
   ((not a) t)
   (t (or (tb-on dt-hour < a b)
          (and (tb-on dt-hour = a b)
               (tb-on dt-minute < a b))))))

(defun tb-date<= (a b)
  "Return non-nil if A's date is <= B's date."
  (cond
   ;; nil is less than non-nil
   ((not b) nil)
   ((not a) t)
   (t
    (or (tb-on dt-year < a b)
        (and
         (tb-on dt-year = a b)
         (or (tb-on dt-month < a b)
             (and (tb-on dt-month = a b)
                  (tb-on dt-day <= a b))))))))

(defsubst tb-get-saved-random-face (title)
  "Get saved random color face for TITLE.
If not found, generate it with `tb--random-color',
save it and return."
  (or (alist-get title tb-colors nil nil #'equal)
      (setf (alist-get title tb-colors nil nil #'equal)
            (tb--random-color))))

(defun tb-intersect-p (block-1 block-2)
  "Return t, if two blocks intersect each other. Otherwise, return nil."
  (let ((y1 (alist-get 'y block-1)) (y2 (alist-get 'y block-2)))
    (or (< y2 y1 (+ (alist-get 'block-height block-2) y2))
        (< y1 y2 (+ (alist-get 'block-height block-1) y1))
        (= y2 y1))))

(defun tb--random-color ()
  "Return random timeblock color face."
  (seq-random-elt '(tb-red tb-green tb-yellow tb-blue tb-magenta tb-cyan)))

(defun tb-add-blocks (svg entries)
  (let* ((width (dom-attr svg 'width))
         (left-padding (dom-attr svg 'left-padding))
         (date (dom-attr svg 'date))
         (block-max-width (- width left-padding)))
    (cl-loop
     for entry in entries for ind from 0
     if (tb-notime-p date entry) do
     (svg-rectangle svg 0 (alist-get 'y entry)
                    width (default-font-height)
                    :stroke "#cdcdcd" :stroke-width 1 :opacity "0.7"
                    :fill (face-attribute
                           (tb-get-saved-random-face
                            (alist-get 'title entry))
                           :background nil 'default)
                    :id (number-to-string ind))
     (svg-text svg (alist-get 'title entry)
               :x 0 :y (+ (alist-get 'y entry) (aref (font-info (face-font 'default)) 2))
               :font-size (aref (font-info (face-font 'default)) 2)
               :fill (face-attribute 'default :foreground))
     else do
     (when-let* ((length
                  (1+ (length
                       (seq-uniq
                        (mapcar
                         ;; get columns for those entries
                         (lambda (x) (alist-get 'column x))
                         ;; find those with which current entry is in intersection
                         (seq-filter (lambda (x) (tb-intersect-p entry x))
                                     (seq-remove-at-position entries ind)))
                        #'eq))))
                 (y (alist-get 'y entry))
                 (block-height (alist-get 'block-height entry))
                 ((> (+ y block-height) 0))
                 (x (+ 1 left-padding (* (alist-get 'column entry)
                                         (/ block-max-width length))))
                 (block-width (1- (/ block-max-width length)))
                 (title (concat (alist-get 'title entry)
                                (alist-get 'n-day-indicator entry)))
                 ;; Splitting the title of an entry
                 (heading-list
                  (if (> (* (length title) (default-font-width)) block-width)
                      (seq-take
                       (seq-partition title (/ block-width (default-font-width)))
                       (max 1 (/ block-height (default-font-height))))
                    `(,title))))
       (let ((time-string (alist-get 'time-string entry)))
         (when (< (/ block-width (default-font-width)) (length time-string))
           (setq time-string nil))
         (when-let* ((time-string)
                     ((< (- block-height
                            (* (length heading-list) (default-font-height)))
                         (- (default-font-height) 6)))
                     (diff (- (+ (length (car (last heading-list)))
                                 (length time-string))
                              (/ block-width (default-font-width))))
                     ((> diff 0)))
           (cl-callf
               (lambda (x)
                 (if (> (- (length x) diff) 10)
                     (substring x 0 (- diff))
                   (setq time-string nil)
                   x))
               (car (last heading-list))))
         ;; Appending generated rectangle for current entry
         (svg-rectangle
          svg x y block-width block-height
          :stroke "#cdcdcd"
          :stroke-width 1
          :opacity "0.7"
          :title (alist-get 'title entry)
          :fill
          (face-attribute
           (tb-get-saved-random-face title) :background nil 'default)
          :id (number-to-string ind))
         ;; Setting the title of current entry
         (let ((y (- y 5)))
           (dolist (heading-part heading-list)
             (svg-text svg heading-part
                       :x x :y (cl-incf y (default-font-height))
                       :fill (face-attribute
                              (tb-get-saved-random-face title)
                              :foreground nil 'default)
                       :font-size (aref (font-info (face-font 'default)) 2))))
         (when time-string
           (svg-text svg time-string
                     :x (- (+ x block-width)
                           (* (length time-string) (default-font-width)))
                     :y (- (+ y block-height) 2)
                     :fill (face-attribute 'tb-hours-line :background nil t)
                     :font-size (aref (font-info (face-font 'default)) 2))))))))

(defun tb-add-hour-lines! (svg)
  (map-let (min-hour max-hour left-padding width scale y-start)
      (dom-attributes svg)
    (let ((lines-iter (if (> min-hour 0) (1- min-hour) 0)) y)
      (while (< (cl-incf lines-iter) max-hour)
        (setq y (+ y-start (round (* scale (- lines-iter min-hour) 60))))
        (svg-line svg left-padding y width y
                  :stroke-dasharray "4"
                  :stroke (face-attribute 'tb-hours-line :background nil t)
                  :hour lines-iter)
        (svg-text svg (format "%d" lines-iter)
                  :y (+ y 5) :x 0
                  :fill (face-attribute 'default :foreground))))))

(defun tb-place-algorithm (_svg entries)
  (let (placed)
    (dolist (entry entries (nreverse placed))
      (setf (alist-get 'column entry)
            (catch 'found-column
              (let ((k 0))
                (while t
                  (catch 'next-column
                    (dolist (el (seq-filter
                                 (lambda (x) (eq (alist-get 'column x) k))
                                 placed))
                      (and (tb-intersect-p entry el)
                           (cl-incf k)
                           (throw 'next-column t)))
                    (throw 'found-column k))))))
      (push entry placed))))

(defun tb-add-date-header (svg date)
  (let ((width (dom-attr svg 'width)))
    (svg-rectangle svg 0 0 width (default-font-height)
                   :stroke "#cdcdcd" :stroke-width 1 :opacity "0.7"
                   :fill (face-attribute 'region :background))
    (svg-text svg (format-time-string "%Y-%m-%d %a" (encode-time date))
              :x 0 :y (aref (font-info (face-font 'default)) 2)
              :fill (face-attribute 'default :foreground)
              :font-size (aref (font-info (face-font 'default)) 2))))

(defun tb-add-display-data (svg entries)
  (map-let (min-hour max-hour date left-padding height scale y-start
                     all-day-section-y-start display-time)
      (dom-attributes svg)
    (let ((all-day-y all-day-section-y-start))
      (cl-loop
       for entry in entries
       collect
       (let* ((start (alist-get 'start entry))
              (title (alist-get 'title entry))
              (end (alist-get 'end entry))
              (start-date-earlier-p (tb-date< start date))
              (end-date-later-p (tb-date< date end))
              (all-day-p (tb-notime-p date entry)))
         (nconc
          (if all-day-p
              (let ((y all-day-y))
                (cl-incf all-day-y (default-font-height))
                (list
                 (cons 'block-height (default-font-height))
                 (cons 'y y)))
            (list
             (cons 'time-string
                   (and display-time
                        (not (or end-date-later-p start-date-earlier-p))
                        (concat
                         (tb-format-time " %H:%M" start)
                         (and end (tb-format-time "-%H:%M" end)))))
             (cons 'block-height
                   (1- (if (and start end)
                           (max
                            (default-font-height)
                            (round
                             (* (tb-time-diff
                                 (if (or end-date-later-p
                                         (tb-decoded<
                                          (tb-time-apply date
                                            :hour (1- max-hour)
                                            :minute 59 :second 0)
                                          (tb-time-apply date
                                            :hour (dt-hour end)
                                            :minute (dt-minute end))))
                                     (tb-time-apply date
                                       :hour (1- max-hour)
                                       :minute 59 :second 0)
                                   end)
                                 (if (or start-date-earlier-p
                                         (tb-decoded<
                                          (tb-time-apply date
                                            :hour (dt-hour start)
                                            :minute (dt-minute start))
                                          (tb-time-apply date
                                            :hour min-hour :minute 0
                                            :second 0)))
                                     (tb-time-apply date
                                       :hour min-hour :minute 0 :second 0)
                                   start))
                                scale)))
                         (default-font-height))))
             (cons 'y
                   (if-let* ((value (+ (round (* (if (or start-date-earlier-p
                                                         (tb-decoded<
                                                          (tb-time-apply date
                                                            :hour (dt-hour start)
                                                            :minute (dt-minute start))
                                                          (tb-time-apply date
                                                            :hour min-hour :minute 0
                                                            :second 0)))
                                                     0
                                                   (- (+ (* 60 (dt-hour start))
                                                         (dt-minute start))
                                                      (* min-hour 60)))
                                                 scale))
                                       1 y-start))
                             ((< (- height value) (default-font-height))))
                       (- height (default-font-height))
                     value))
             (cons 'n-day-indicator
                   (cond
                    ((and end-date-later-p start-date-earlier-p) "↕️")
                    (end-date-later-p "⬇️")
                    (start-date-earlier-p "⬆️")))))
          entry))))))

(defun tb-add-current-time-line! (svg)
  (when tb-current-time-indicator
    (map-let (scale min-hour date width y-start) (dom-attributes svg)
      (when-let* ((now (decode-time))
                  ((tb-date= date now))
                  (y (+ (* scale (- (+ (* (dt-hour now) 60) (dt-minute now))
                                    (* min-hour 60)))
                        y-start)))
        (svg-line svg 0 y width y
                  :stroke (face-attribute
                           'tb-current-time-indicator
                           :background nil t))))))

(defun tb-notime-p (date entry)
  (let ((start (alist-get 'start entry))
        (end (alist-get 'end entry)))
    (and (not (or (dt-hour start) (dt-minute start) (dt-second start)
                  (dt-hour end) (dt-minute end) (dt-second end)))
         (or (tb-date= start date)
             (when-let* ((end (alist-get 'end entry)))
               (and (tb-date< start date)
                    (tb-date<= date end)))))))

(cl-defun tb-make-column
    (entries date width height
             &optional min-hour max-hour show-date show-all-day-entries
             display-time)
  "Make timeblock column."
  (let* ((max-hour (or max-hour
                       (if (consp tb-scale-options)
                           (min (1+ (cdr tb-scale-options)) 24)
                         24)))
         (min-hour
          (or min-hour
              (pcase tb-scale-options
                ((pred consp) (car tb-scale-options))
                (`nil 0)
                (_ (cl-loop for x in
                            (cons
                             (unless (eq tb-scale-options 'hide-all)
                               (dt-hour (decode-time)))
                             (mapcar (lambda (entry)
                                       (let ((start (plist-get entry :start)))
                                         (if (tb-date< start date)
                                             0 (dt-hour start))))
                                     entries))
                            when x minimize x)))))
         (date-header-height (if show-date (default-font-height) 0))
         (allday-entries-count
          (when show-all-day-entries
            (seq-count (apply-partially #'tb-notime-p date) entries)))
         (all-day-entries-header-height
          (* allday-entries-count (default-font-height)))
         (y-start (+ date-header-height all-day-entries-header-height
                     (/ (aref (font-info (face-font 'default)) 2) 2)))
         (svg (svg-create width height
                          :max-hour max-hour :min-hour min-hour
                          :scale (/ (- height y-start)
                                    (* (- max-hour min-hour) 60.0))
                          :left-padding (* 2 (default-font-width))
                          :y-start y-start :date date :show-date show-date
                          :show-all-day-entries show-all-day-entries
                          :all-day-section-y-start date-header-height
                          :display-time display-time))
         (entries-filtered (tb-filter-for-scope svg entries)))
    (and show-date (tb-add-date-header svg date))
    (tb-add-hour-lines! svg)
    (tb-add-current-time-line! svg)
    (seq-reduce
     (lambda (entries func) (funcall func svg entries))
     '( tb-add-display-data tb-place-algorithm tb-add-blocks)
     entries-filtered)
    (dom-set-attribute svg 'entries (copy-tree entries-filtered))
    svg))

(defun tb-redisplay-column ()
  (when-let* ((svg (get-text-property (point) 'dom)))
    (let ((keymap (get-text-property (point) 'keymap))
          (entries-function (get-text-property (point) 'entries-function)))
      (map-let ( width height min-hour max-hour display-time
                 date show-date show-all-day-entries entries)
          (dom-attributes svg)
        (set-marker (dom-attr svg :image) nil)
        (delete-char 1)
        (tb-insert-column entries date width height
                          :show-all-day-entries show-all-day-entries
                          :min-hour min-hour :max-hour max-hour
                          :keymap keymap :entries-function entries-function
                          :show-date show-date :display-time display-time)
        (backward-char 1)))))

(defun tb-update-column ()
  (interactive)
  (when-let* ((svg (get-text-property (point) 'dom))
              (entries-function (get-text-property (point) 'entries-function)))
    (let ((keymap (get-text-property (point) 'keymap)))
      (map-let ( width height min-hour max-hour display-time
                 date show-date show-all-day-entries entries)
          (dom-attributes svg)
        (set-marker (dom-attr svg :image) nil)
        (let ((entries (funcall entries-function)))
          (delete-char 1)
          (tb-insert-column
           entries date width height
           :keymap keymap :min-hour min-hour :max-hour max-hour
           :show-all-day-entries show-all-day-entries
           :show-date show-date :entries-function entries-function
           :display-time display-time)
          (backward-char 1))))))

(cl-defun tb-insert-column
    (entries date width height &key min-hour max-hour
             keymap show-date show-all-day-entries entries-function
             display-time)
  "Insert timeblock column into the current buffer."
  (let ((svg (tb-make-column entries date width height
                             min-hour max-hour show-date
                             show-all-day-entries display-time)))
    (svg-insert-image svg)
    (add-text-properties (1- (point)) (point)
                         (list 'keymap keymap 'dom svg
                               'entries-function entries-function))))

(defun tb-filter-for-scope (svg entries)
  "entry format: "
  (map-let (date min-hour max-hour) (dom-attributes svg)
    (seq-filter
     (lambda (x)
       (let ((start (alist-get 'start x)) (end (alist-get 'end x))
             (title (alist-get 'title x)))
         (and
          (or (tb-date= start date)
              (when-let* ((end (alist-get 'end x)))
                (and (tb-date< start date)
                     (tb-date<= date end))))
          (or (and (not (dt-hour end)) (not (dt-hour start)))
              (<= min-hour (dt-hour start) max-hour)
              (and (dt-hour end)
                   (or (<= (dt-hour start) min-hour (dt-hour end))
                       (<= min-hour (dt-hour end) max-hour)))))))
     entries)))

;;;; Planning commands

(defun tb-time-inc (slot value time)
  "Return a new time object based on TIME with its SLOT incremented by VALUE.

SLOT should be specified as a plain symbol, not a keyword."
  (let ((time (copy-sequence time)))
    (dt-add time (make-decoded-time (intern (format ":%s" slot)) value))))

(cl-defun tb-time-apply (time &key second minute hour day month year)
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

;;;; Navigation commands

(defun tb-handle-drag (event)
  "Draw a line from the start of EVENT to its end."
  (interactive "e")
  (when-let* ((start (posn-object-x-y (event-start event)))
              (end (posn-object-x-y (event-end event)))
              (svg (get-text-property (point) 'dom)))
    (message "HOUR: %s" (tb-hour-under-cursor svg nil (cdr end)))))

(defun tb-block-under-cursor (svg cursor-x cursor-y)
  (car (dom-search
        svg
        (lambda (node)
          (let ((x (dom-attr node 'x))
                (y (dom-attr node 'y)))
            (and (eq (dom-tag node) 'rect)
                 (> cursor-x x)
                 (<= cursor-x (+ x (dom-attr node 'width)))
                 (<= cursor-y (+ y (dom-attr node 'height)))
                 (> cursor-y y)))))))

(defun tb-hour-under-cursor (svg _cursor-x cursor-y)
  (dom-attr
   (car (dom-search
         svg
         (lambda (node)
           (let ((y (dom-attr node 'y1))
                 (hour (dom-attr node 'hour)))
             (and y hour
                  (eq (dom-tag node) 'line)
                  (> cursor-y y)
                  (< (- cursor-y y)
                     (round (* (dom-attr svg 'scale) 60))))))))
   'hour))

(defun tb-select-with-cursor (event)
  "Select the block under current position of the cursor."
  (interactive "e")
  (when-let* ((svg (get-text-property (point) 'dom))
              (posn (posn-object-x-y (event-start event))))
    (tb-unselect svg)
    (when-let* ((node (tb-block-under-cursor svg (car posn) (cdr posn))))
      (unless (dom-attr node 'mark)
        (dom-set-attribute node 'orig-fill (dom-attr node 'fill)))
      (dom-set-attribute node 'fill (face-attribute 'tb-select :background))
      (dom-set-attribute node 'select t))
    (svg-possibly-update-image svg)))

(defun tb-unselect (svg)
  "Unselect selected block.
Return the numerical order of the unselected block on success.
Otherwise, return nil."
  (when-let* ((node (car (dom-search
                          svg (lambda (node) (dom-attr node 'select))))))
    (dom-set-attribute
     node 'fill (if (dom-attr node 'mark)
                    (face-attribute 'tb-mark :background)
                  (or (dom-attr node 'orig-fill) "#ffffff")))
    (dom-remove-attribute node 'select)
    (cons (dom-attr node 'x) (dom-attr node 'y))))

(defun tb-mark ()
  "Mark selected block."
  (interactive)
  (when-let* ((svg (get-text-property (point) 'dom))
              (block (tb-get-selected svg))
              ((not (dom-attr block 'mark))))
    (dom-set-attribute block 'fill (face-attribute 'tb-mark :background))
    (dom-set-attribute block 'mark t))
  (tb-down))

(defun tb-mark-by-regexp (regexp)
  "Mark blocks by REGEXP."
  (interactive "sMark entries matching regexp: ")
  (when-let* ((svg (get-text-property (point) 'dom)))
    (dolist (block (dom-search
                    svg (lambda (x)
                          (when (dom-attr x 'title)
                            (string-match-p regexp
                                            (dom-attr x 'title))))))
      (dom-set-attribute block 'orig-fill (dom-attr block 'fill))
      (dom-set-attribute block 'fill (face-attribute 'tb-mark :background))
      (dom-set-attribute block 'mark t))
    (svg-possibly-update-image svg)))

(defun tb-unmark ()
  "Unmark selected block."
  (interactive)
  (when-let* ((svg (get-text-property (point) 'dom))
              (node (tb-get-selected svg))
              ((dom-attr node 'mark)))
    (dom-remove-attribute node 'mark)
    (dom-set-attribute node 'fill (dom-attr node 'orig-fill))
    (tb-down)))

(defun tb-unmark-all ()
  "Unmark all marked blocks."
  (interactive)
  (when-let* ((svg (get-text-property (point) 'dom)))
    (dolist (node (dom-search svg (lambda (node) (dom-attr node 'mark))))
      (dom-remove-attribute node 'mark)
      (unless (dom-attr node 'select)
        (dom-set-attribute node 'fill (dom-attr node 'orig-fill))))
    (svg-possibly-update-image svg)))

(defun tb--lower-block (svg y)
  (car
   (seq-sort
    (lambda (x y) (< (dom-attr x 'y) (dom-attr y 'y)))
    (dom-search svg
                (lambda (node)
                  (and
                   (dom-attr node 'id)
                   (dom-attr node 'y)
                   (> (dom-attr node 'y) y)))))))

(defun tb--right-block (svg pos)
  (car
   (seq-sort
    (lambda (x y) (< (dom-attr x 'x) (dom-attr y 'x)))
    (dom-search svg
                (lambda (node)
                  (and
                   (dom-attr node 'id)
                   (= (dom-attr node 'y) (cdr pos))
                   (> (dom-attr node 'x) (car pos))))))))

(defun tb--left-block (svg pos)
  (car
   (seq-sort
    (lambda (x y) (> (dom-attr x 'x) (dom-attr y 'x)))
    (dom-search svg
                (lambda (node)
                  (and
                   (dom-attr node 'id)
                   (= (dom-attr node 'y) (cdr pos))
                   (< (dom-attr node 'x) (car pos))))))))

(defun tb--upper-block (svg y)
  (car
   (seq-sort
    (lambda (x y) (> (dom-attr x 'y) (dom-attr y 'y)))
    (dom-search svg
                (lambda (node)
                  (and
                   (dom-attr node 'id)
                   (dom-attr node 'y)
                   (< (dom-attr node 'y) y)))))))

(defun tb-down ()
  "Select the next timeblock in *timeblock* buffer."
  (interactive)
  (when-let* ((svg (get-text-property (point) 'dom))
              (unsel-y (or (cdr (tb-unselect svg)) (dom-attr svg 'height)))
              (node (or (tb--lower-block svg unsel-y)
                        (tb--lower-block svg 0)))
              (inhibit-read-only t))
    (unless (dom-attr node 'mark)
      (dom-set-attribute node 'orig-fill (dom-attr node 'fill)))
    (dom-set-attribute node 'fill (face-attribute 'tb-select :background))
    (dom-set-attribute node 'select t)
    (svg-possibly-update-image svg)))

(defun tb-right ()
  "Select the previous timeblock in *timeblock* buffer.
Return t on success, otherwise - nil."
  (interactive)
  (when-let* ((svg (get-text-property (point) 'dom))
              (selected-node (tb-get-selected svg))
              (sel-pos (cons (dom-attr selected-node 'x)
                             (dom-attr selected-node 'y)))
              (node (tb--right-block svg sel-pos))
              (inhibit-read-only t))
    (tb-unselect svg)
    (unless (dom-attr node 'mark)
      (dom-set-attribute node 'orig-fill (dom-attr node 'fill)))
    (dom-set-attribute node 'fill (face-attribute 'tb-select :background))
    (dom-set-attribute node 'select t)
    (svg-possibly-update-image svg)))

(defun tb-left ()
  "Select the previous timeblock in *timeblock* buffer.
Return t on success, otherwise - nil."
  (interactive)
  (when-let* ((svg (get-text-property (point) 'dom))
              (selected-node (tb-get-selected svg))
              (sel-pos (cons (dom-attr selected-node 'x)
                             (dom-attr selected-node 'y)))
              (node (tb--left-block svg sel-pos))
              (inhibit-read-only t))
    (tb-unselect svg)
    (unless (dom-attr node 'mark)
      (dom-set-attribute node 'orig-fill (dom-attr node 'fill)))
    (dom-set-attribute node 'fill (face-attribute 'tb-select :background))
    (dom-set-attribute node 'select t)
    (svg-possibly-update-image svg)))

(defun tb-up ()
  "Select the previous timeblock in *timeblock* buffer.
Return t on success, otherwise - nil."
  (interactive)
  (when-let* ((svg (get-text-property (point) 'dom))
              (unsel-y (or (cdr (tb-unselect svg)) 0))
              (node (or (tb--upper-block svg unsel-y)
                        (tb--upper-block
                         svg (dom-attr svg 'height))))
              (inhibit-read-only t))
    (unless (dom-attr node 'mark)
      (dom-set-attribute node 'orig-fill (dom-attr node 'fill)))
    (dom-set-attribute node 'fill (face-attribute 'tb-select :background))
    (dom-set-attribute node 'select t)
    (svg-possibly-update-image svg)))

(defun tb-insert-view (entries start-date end-date width height
                               &optional min-hour max-hour show-date
                               show-all-day-entries keymap display-time
                               entries-function)
  (let ((dates (tb-get-dates start-date end-date)))
    (dolist (date dates)
      (tb-insert-column entries date (/ width (length dates)) height
                        :show-all-day-entries show-all-day-entries
                        :min-hour min-hour :max-hour max-hour
                        :keymap keymap :show-date show-date
                        :display-time display-time
                        :entries-function entries-function)
      (insert " "))))

;;;; Footer

(provide 'timeblock)

;; Local Variables:
;;   read-symbol-shorthands: (("tb-" . "timeblock-") ("dt-" . "decoded-time-"))
;;   outline-regexp: "\\(;\\{3,\\} \\)"
;; End:

;;; timeblock.el ends here
