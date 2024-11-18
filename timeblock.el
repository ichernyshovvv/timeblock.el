;;; timeblock.el --- library for visualizing time data via interactive blocks -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Ilya Chernyshov

;; Author: Ilya Chernyshov <ichernyshovvv@gmail.com>
;; Version: 0.1-pre
;; Package-Requires: ((emacs "29.1"))
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
(require 'time-date)
(require 'timeblock-faces)

;;;; Variables

(defvar tb-colors nil)

;;;; Keymaps

(defvar-keymap tb-column-map
  "<mouse-1>" #'tb-select-with-cursor
  "<remap> <imenu>" #'tb-jump
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

(cl-defun tb-make-column
    (entries date width height
             &key scope show-date show-all-day-entries show-time
             show-current-time face)
  "Make a timeblocks column and return as SVG image.

ENTRIES is a list of alists with the following required keys:
  \\='start
  \\='end

  \\='title is used as a name for a block

DATE is a decoded-time value for the column.

WIDTH, HEIGHT define the size of the column in pixels.

SCOPE defines the hour scope for the column.  Available values:
  t: Hide hours in the past (if there are no timeblocks).
  nil: Do not hide anything.  All 24 hours will be displayed.
  \\='hide-all: Hide all free hours before the first timeblock.
  (MIN-HOUR . MAX-HOUR): display data from MIN-HOUR to MAX-HOUR.

SHOW-DATE: if non-nil, show date on the top of the column

SHOW-ALL-DAY-ENTRIES: if non-nil, show all-day entries on the top of the
column

SHOW-TIME: if non-nil, display time in HH:MM-[HH:MM] format on each block

SHOW-CURRENT-TIME: if non-nil, show current time line.

FACE is a face to use for the text inside the blocks.  If not specified,
the default face is used."
  (let* ((face (or face 'default))
         (font-height (window-font-height nil face))
         (max-hour (if (consp scope)
                       (min (1+ (cdr scope)) 24)
                     24))
         (min-hour
          (pcase scope
            ((pred consp) (car scope))
            (`nil 0)
            (_ (cl-loop for x in
                        (cons
                         (unless (eq scope 'hide-all) (dt-hour (decode-time)))
                         (mapcar (lambda (entry)
                                   (let ((start (alist-get 'start entry)))
                                     (if (tb-date< start date)
                                         0 (dt-hour start))))
                                 entries))
                        when x minimize x
                        finally return (or x 0)))))
         (date-header-height (if show-date font-height 0))
         (allday-entries-count
          (if show-all-day-entries
              (seq-count (apply-partially #'tb-notime-p date) entries)
            0))
         (all-day-entries-header-height (* allday-entries-count font-height))
         (y-start (+ date-header-height all-day-entries-header-height
                     (/ (aref (font-info (face-font face)) 2) 2)))
         (svg (svg-create width height
                          :max-hour max-hour :min-hour min-hour :scope scope
                          :face face :show-time show-time
                          :scale (/ (- height y-start)
                                    (* (- max-hour min-hour) 60.0))
                          :left-padding (* 2 (window-font-width nil face))
                          :y-start y-start :date date :show-date show-date
                          :show-all-day-entries show-all-day-entries
                          :all-day-section-y-start date-header-height))
         (entries-filtered (tb-filter svg entries)))
    (dom-set-attribute svg 'entries (copy-tree entries-filtered))
    (and show-date (tb-add-date-header! svg date))
    (and show-current-time (tb-add-current-time-line! svg))
    (tb-add-hour-lines! svg)
    (tb-add-display-data! svg entries-filtered)
    (tb-place-horizontally! svg entries-filtered)
    (tb-add-blocks! svg entries-filtered)
    svg))

(cl-defun tb-insert-column
    (entries date width height &key scope
             keymap show-date show-all-day-entries entries-function
             show-time show-current-time face)
  "Insert a timeblocks column into the current buffer.

ENTRIES is a list of alists with the following required keys:
  \\='start
  \\='end

  \\='title is used as a name for a block

DATE is a decoded-time value for the column.

WIDTH, HEIGHT define the size of the column in pixels.

SCOPE defines the hour scope for the column.  Available values:
  t: Hide hours in the past (if there are no timeblocks).
  nil: Do not hide anything.  All 24 hours will be displayed.
  \\='hide-all: Hide all free hours before the first timeblock.
  (MIN-HOUR . MAX-HOUR): display data from MIN-HOUR to MAX-HOUR.

SHOW-DATE: if non-nil, show date on the top of the column

SHOW-ALL-DAY-ENTRIES: if non-nil, show all-day entries on the top of the column

SHOW-TIME: if non-nil, display time in HH:MM-[HH:MM] format on each block

SHOW-CURRENT-TIME: if non-nil, show current time line.

FACE is a face to use for the text inside the blocks.  If not specified,
the default face is used.

ENTRIES-FUNCTION: function to call to get the entries.
KEYMAP is a keymap to use when the point is on the column."
  (let ((svg (tb-make-column entries date width height
                             :scope scope :face face
                             :show-date show-date :show-time show-time
                             :show-all-day-entries show-all-day-entries
                             :show-current-time show-current-time)))
    (svg-insert-image svg)
    (add-text-properties (1- (point)) (point)
                         (list 'keymap keymap 'dom svg
                               'entries-function entries-function))))

(defsubst tb-format-time (format-string time)
  "Use FORMAT-STRING to format the time value TIME."
  (let ((time (copy-sequence time)))
    (unless (dt-second time) (setf (dt-second time) 0))
    (unless (dt-minute time) (setf (dt-minute time) 0))
    (unless (dt-hour time) (setf (dt-hour time) 0))
    (format-time-string format-string (encode-time time))))

(defun tb-get-selected (svg)
  "Get the node of the selected timeblock in SVG."
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

(defsubst tb-random-face (title)
  "Get saved random face for TITLE.
If not found, generate, save in `tb-colors' and return it."
  (or (alist-get title tb-colors nil nil #'equal)
      (setf (alist-get title tb-colors nil nil #'equal)
            (seq-random-elt
             '(tb-red tb-green tb-yellow tb-blue tb-magenta tb-cyan)))))

(defun tb-intersect-p (block-1 block-2)
  "Return t, if BLOCK-1 and BLOCK-2 intersect each other.
Otherwise, return nil."
  (let ((y1 (alist-get 'y block-1)) (y2 (alist-get 'y block-2)))
    (or (< y2 y1 (+ (alist-get 'block-height block-2) y2))
        (< y1 y2 (+ (alist-get 'block-height block-1) y1))
        (= y2 y1))))

(defun tb-add-blocks! (svg entries)
  "Make blocks from ENTRIES and insert into SVG."
  (let* ((width (dom-attr svg 'width))
         (left-padding (dom-attr svg 'left-padding))
         (date (dom-attr svg 'date))
         (face (dom-attr svg 'face))
         (font-width (window-font-width nil face))
         (font-height (window-font-height nil face))
         (font-size (aref (font-info (face-font face)) 2))
         (block-max-width (- width left-padding)))
    (cl-loop
     for entry in entries for ind from 0
     if (tb-notime-p date entry) do
     (svg-rectangle svg 0 (alist-get 'y entry)
                    width font-height
                    :stroke "#cdcdcd" :stroke-width 1
                    :fill (face-background (tb-random-face
                                            (alist-get 'title entry)))
                    :id (number-to-string ind))
     (svg-text svg (alist-get 'title entry)
               :x 0 :y (+ (alist-get 'y entry) font-size)
               :font-size font-size
               :fill (face-foreground 'default))
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
                  (if (> (* (length title) font-width) block-width)
                      (take (max 1 (/ block-height font-height))
                            (seq-partition title (/ block-width font-width)))
                    `(,title))))
       (let ((time-string (alist-get 'time-string entry)))
         (when (< (/ block-width font-width) (length time-string))
           (setq time-string nil))
         (when-let* ((time-string)
                     ((< (- block-height
                            (* (length heading-list) font-height))
                         (- font-height 6)))
                     (diff (- (+ (length (car (last heading-list)))
                                 (length time-string))
                              (/ block-width font-width)))
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
          :stroke "#cdcdcd" :stroke-width 1
          :title (alist-get 'title entry)
          :fill (face-background (tb-random-face title))
          :id (number-to-string ind))
         ;; Setting the title of current entry
         (cl-loop for heading-part in heading-list
                  for title-y from (+ y font-size) by font-size do
                  (svg-text svg heading-part
                            :x x :y title-y
                            :fill (face-foreground (tb-random-face title))
                            :font-size font-size))
         (when time-string
           (svg-text svg time-string
                     :x (- (+ x block-width)
                           (* (length time-string) font-width))
                     :y (- (+ y block-height) 2)
                     :fill (face-background 'tb-hours-line)
                     :font-size font-size)))))))

(defun tb-add-hour-lines! (svg)
  "Add hour lines into SVG."
  (map-let (min-hour max-hour left-padding width scale y-start face)
      (dom-attributes svg)
    (let ((lines-iter (max (1- min-hour) 0))
          (font-size (aref (font-info (face-font face)) 2))
          y)
      (while (< (cl-incf lines-iter) max-hour)
        (setq y (+ y-start (round (* scale (- lines-iter min-hour) 60))))
        (svg-line svg left-padding y width y
                  :stroke-dasharray "4" :hour lines-iter
                  :stroke (face-background 'tb-hours-line))
        (svg-text svg (format "%d" lines-iter)
                  :y (+ y 5) :x 0 :font-size font-size
                  :fill (face-foreground 'default))))))

(defun tb-place-horizontally! (_svg entries)
  "Place ENTRIES horizontally to avoid intersections."
  (let (placed)
    (dolist (entry entries)
      (nconc
       entry
       (list
        (cons 'column
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
                      (throw 'found-column k))))))))
      (push entry placed))))

(defun tb-add-date-header! (svg date)
  "Add DATE date header into SVG.
DATE is a decoded-time value."
  (let* ((width (dom-attr svg 'width))
         (face (dom-attr svg 'face))
         (font-size (aref (font-info (face-font face)) 2)))
    (svg-rectangle svg 0 0 width (window-font-height nil face)
                   :stroke "#cdcdcd" :stroke-width 1
                   :fill (face-background 'region))
    (svg-text svg (format-time-string "%Y-%m-%d %a" (encode-time date))
              :x 0 :y font-size :font-size font-size
              :fill (face-foreground 'default))))

(defun tb-add-display-data! (svg entries)
  "Add display data to ENTRIES for SVG.
This includes the following keys:
\\='time-string, \\='block-height, \\='n-day-indicator, \\='y"
  (map-let ( min-hour max-hour date height scale y-start
             all-day-section-y-start show-time face)
      (dom-attributes svg)
    (let ((all-day-y all-day-section-y-start)
          (font-height (window-font-height nil face)))
      (dolist (entry entries)
        (let* ((start (alist-get 'start entry))
               (end (alist-get 'end entry))
               (start-date-earlier-p (tb-date< start date))
               (end-date-later-p (tb-date< date end))
               (all-day-p (tb-notime-p date entry)))
          (nconc
           entry
           (if all-day-p
               (let ((y all-day-y))
                 (cl-incf all-day-y font-height)
                 (list (cons 'block-height font-height)
                       (cons 'y y)))
             (list
              (cons 'time-string
                    (and show-time
                         (not (or end-date-later-p start-date-earlier-p))
                         (concat
                          (tb-format-time " %H:%M" start)
                          (and end (tb-format-time "-%H:%M" end)))))
              (cons 'block-height
                    (1- (if (and start end)
                            (max
                             font-height
                             (let ((y1
                                    (if (or start-date-earlier-p
                                            (< (dt-hour start) min-hour))
                                        y-start
                                      (+ (round (* scale
                                                   (- (+ (* 60 (dt-hour start))
                                                         (dt-minute start))
                                                      (* min-hour 60))))
                                         y-start)))
                                   (y2
                                    (if (or end-date-later-p
                                            (< max-hour (dt-hour end)))
                                        height
                                      (+ (round (* scale
                                                   (- (+ (* 60 (dt-hour end))
                                                         (dt-minute end))
                                                      (* min-hour 60))))
                                         y-start))))
                               (- y2 y1)))
                          font-height)))
              (cons 'y
                    (let ((value (+ (round
                                     (* (if (or start-date-earlier-p
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
                                    1 y-start)))
                      (if (< (- height value) font-height)
                          (- height font-height)
                        value)))
              (cons 'n-day-indicator
                    (cond
                     ((and end-date-later-p start-date-earlier-p) "↕️")
                     (end-date-later-p "⬇️")
                     (start-date-earlier-p "⬆️")))))))))))

(defun tb-add-current-time-line! (svg)
  "Add current time line to SVG."
  (map-let (scale min-hour date width y-start) (dom-attributes svg)
    (when-let* ((now (decode-time))
                ((tb-date= date now))
                (y (+ (* scale (- (+ (* (dt-hour now) 60) (dt-minute now))
                                  (* min-hour 60)))
                      y-start)))
      (svg-line svg 0 y width y
                :stroke (face-background 'tb-current-time-indicator)))))

(defun tb-notime-p (date entry)
  "Return non-nil if ENTRY has no time"
  (let ((start (alist-get 'start entry))
        (end (alist-get 'end entry)))
    (and (not (or (dt-hour start) (dt-minute start) (dt-second start)
                  (dt-hour end) (dt-minute end) (dt-second end)))
         (or (tb-date= start date)
             (when-let* ((end (alist-get 'end entry)))
               (and (tb-date< start date)
                    (tb-date<= date end)))))))

(defun tb-redisplay-column ()
  "Redisplay the column at point."
  (when-let* ((svg (get-text-property (point) 'dom)))
    (let ((keymap (get-text-property (point) 'keymap))
          (entries-function (get-text-property (point) 'entries-function)))
      (map-let ( width height scope show-time face show-current-time
                 date show-date show-all-day-entries entries)
          (dom-attributes svg)
        (set-marker (dom-attr svg :image) nil)
        (delete-char 1)
        (tb-insert-column entries date width height
                          :show-all-day-entries show-all-day-entries
                          :show-time show-time :keymap keymap
                          :entries-function entries-function :scope scope
                          :show-current-time show-current-time
                          :show-date show-date :face face)
        (backward-char 1)))))

(defun tb-update-column ()
  "Update the column at point."
  (interactive)
  (when-let* ((svg (get-text-property (point) 'dom))
              (entries-function (get-text-property (point) 'entries-function)))
    (let ((keymap (get-text-property (point) 'keymap)))
      (map-let ( width height min-hour max-hour show-time
                 date show-date show-all-day-entries entries)
          (dom-attributes svg)
        (set-marker (dom-attr svg :image) nil)
        (let ((entries (funcall entries-function)))
          (delete-char 1)
          (tb-insert-column
           entries date width height
           :keymap keymap :scope scope
           :show-all-day-entries show-all-day-entries
           :show-date show-date :entries-function entries-function
           :show-time show-time)
          (backward-char 1))))))

(defun tb-filter (svg entries)
  "Filter ENTRIES before inserting to SVG."
  (map-let (date min-hour max-hour show-all-day-entries) (dom-attributes svg)
    (seq-filter
     (lambda (x)
       (let ((start (alist-get 'start x)) (end (alist-get 'end x)))
         (and
          (or show-all-day-entries (dt-hour start))
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

(defun tb-jump (id)
  "Jump to a block with specified ID.
If called interactively, the block is chosen via `completing-read'."
  (interactive (list (get-text-property
                      0 'id
                      (completing-read
                       "Block: "
                       (when-let* ((svg (get-text-property (point) 'dom))
                                   (entries (dom-attr svg 'entries)))
                         (cl-loop
                          for e in entries for inx from 0 collect
                          (propertize (alist-get 'title e) 'id inx)))))))
  (when-let* ((svg (get-text-property (point) 'dom))
              (node (dom-by-id svg (number-to-string id))))
    (tb-unselect svg)
    (unless (dom-attr node 'mark)
      (dom-set-attribute node 'orig-fill (dom-attr node 'fill)))
    (dom-set-attribute node 'fill (face-background 'tb-select))
    (dom-set-attribute node 'select t)
    (svg-possibly-update-image svg)))

(defun tb-block-at-position (svg x y)
  "Get the node of the block at position (X . Y) in SVG."
  (car (dom-search
        svg
        (lambda (node)
          (let ((node-x (dom-attr node 'x))
                (node-y (dom-attr node 'y)))
            (and (eq (dom-tag node) 'rect)
                 (> x node-x)
                 (<= x (+ node-x (dom-attr node 'width)))
                 (<= y (+ node-y (dom-attr node 'height)))
                 (> y node-y)))))))

(defun tb-select-with-cursor (event)
  "Select the block under current position of the cursor.
EVENT is a mouse event."
  (interactive "e")
  (when-let* ((svg (get-text-property (point) 'dom))
              (posn (posn-object-x-y (event-start event))))
    (tb-unselect svg)
    (when-let* ((node (tb-block-at-position svg (car posn) (cdr posn))))
      (unless (dom-attr node 'mark)
        (dom-set-attribute node 'orig-fill (dom-attr node 'fill)))
      (dom-set-attribute node 'fill (face-background 'tb-select))
      (dom-set-attribute node 'select t))
    (svg-possibly-update-image svg)))

(defun tb-unselect (svg)
  "Unselect the selected block in SVG.
Return the position in (X . Y) format of the unselected block on success.
Otherwise, return nil."
  (when-let* ((node (car (dom-search
                          svg (lambda (node) (dom-attr node 'select)))))
              (inhibit-read-only t))
    (dom-set-attribute
     node 'fill (if (dom-attr node 'mark)
                    (face-background 'tb-mark)
                  (dom-attr node 'orig-fill)))
    (dom-remove-attribute node 'select)
    (cons (dom-attr node 'x) (dom-attr node 'y))))

(defun tb-mark ()
  "Mark selected block."
  (interactive)
  (when-let* ((svg (get-text-property (point) 'dom))
              (block (tb-get-selected svg))
              ((not (dom-attr block 'mark))))
    (dom-set-attribute block 'fill (face-background 'tb-mark))
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
      (dom-set-attribute block 'fill (face-background 'tb-mark))
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

(defun tb-down ()
  "Select the lower block.
If not found, select the first block from the top."
  (interactive)
  (when-let* ((svg (get-text-property (point) 'dom))
              (unsel-y (or (cdr (tb-unselect svg)) (dom-attr svg 'height)))
              (node (or (tb--lower-block svg unsel-y)
                        (tb--lower-block svg 0))))
    (tb--select svg node)))

(defun tb-right ()
  "Select the block on the right.  If not found, return nil."
  (interactive)
  (when-let* ((svg (get-text-property (point) 'dom))
              (selected-node (tb-get-selected svg))
              (sel-pos (cons (dom-attr selected-node 'x)
                             (dom-attr selected-node 'y)))
              (node (tb--right-block svg sel-pos)))
    (tb-unselect svg)
    (tb--select svg node)
    t))

(defun tb-left ()
  "Select the block on the left.  If not found, return nil."
  (interactive)
  (when-let* ((svg (get-text-property (point) 'dom))
              (selected-node (tb-get-selected svg))
              (sel-pos (cons (dom-attr selected-node 'x)
                             (dom-attr selected-node 'y)))
              (node (tb--left-block svg sel-pos)))
    (tb-unselect svg)
    (tb--select svg node)
    t))

(defun tb-hour-at-position (svg cursor-y)
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

(defun tb-up ()
  "Select the lower block.
If not found, select the first block from the bottom."
  (interactive)
  (when-let* ((svg (get-text-property (point) 'dom))
              (unsel-y (or (cdr (tb-unselect svg)) 0))
              (node (or (tb--upper-block svg unsel-y)
                        (tb--upper-block
                         svg (dom-attr svg 'height)))))
    (tb--select svg node)))

(defun tb--select (svg node)
  "Select NODE in SVG."
  (let ((inhibit-read-only t))
    (unless (dom-attr node 'mark)
      (dom-set-attribute node 'orig-fill (dom-attr node 'fill)))
    (dom-set-attribute node 'fill (face-background 'tb-select))
    (dom-set-attribute node 'select t)
    (svg-possibly-update-image svg)
    (message "%s" (alist-get 'title
                             (nth (string-to-number (dom-attr node 'id))
                                  (dom-attr svg 'entries))))))

;;;; Footer

(provide 'timeblock)

;; Local Variables:
;;   read-symbol-shorthands: (("tb-" . "timeblock-") ("dt-" . "decoded-time-"))
;;   outline-regexp: "\\(;\\{3,\\} \\)"
;; End:

;;; timeblock.el ends here
