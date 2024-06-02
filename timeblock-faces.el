;; -*- lexical-binding: t; -*-

;; The colors are borrowed from pulsar.el project written by Protesilaos Stavrou
(defface timeblock-red
  '((default :extend t)
    (((class color) (min-colors 88) (background light))
     :background "#ffcccc" :foreground "#77002a")
    (((class color) (min-colors 88) (background dark))
     :background "#77002a" :foreground "#ffcccc")
    (t :inverse-video t))
  "Red face."
  :group 'timeblock)

(defface timeblock-green
  '((default :extend t)
    (((class color) (min-colors 88) (background light))
     :background "#aceaac" :foreground "#00422a")
    (((class color) (min-colors 88) (background dark))
     :background "#00422a" :foreground "#aceaac")
    (t :inverse-video t))
  "Green face."
  :group 'timeblock)

(defface timeblock-yellow
  '((default :extend t)
    (((class color) (min-colors 88) (background light))
     :background "#fff29a" :foreground "#693200")
    (((class color) (min-colors 88) (background dark))
     :background "#693200" :foreground "#fff29a")
    (t :inverse-video t))
  "Yellow face."
  :group 'timeblock)

(defface timeblock-blue
  '((default :extend t)
    (((class color) (min-colors 88) (background light))
     :background "#8fcfff" :foreground "#242679")
    (((class color) (min-colors 88) (background dark))
     :background "#242679" :foreground "#8fcfff")
    (t :inverse-video t))
  "Blue face."
  :group 'timeblock)

(defface timeblock-magenta
  '((default :extend t)
    (((class color) (min-colors 88) (background light))
     :background "#ffccff" :foreground "#71206a")
    (((class color) (min-colors 88) (background dark))
     :background "#71206a" :foreground "#ffccff")
    (t :inverse-video t))
  "Magenta face."
  :group 'timeblock)

(defface timeblock-cyan
  '((default :extend t)
    (((class color) (min-colors 88) (background light))
     :background "#8eecf4" :foreground "#004065")
    (((class color) (min-colors 88) (background dark))
     :background "#004065" :foreground "#8eecf4")
    (t :inverse-video t))
  "Cyan face."
  :group 'timeblock)

(defface timeblock-list-header '((t (:inherit org-agenda-structure)))
  "Face used in timeblock-list for dates."
  :group 'timeblock)

(defface timeblock-select
  '((default :extend t)
    (((class color)
      (min-colors 88)
      (background light))
     :background "#ffe0aa")
    (((class color) (min-colors 88) (background dark))
     :background "#3f1651")
    (t :inverse-video t))
  "Face used for selected blocks."
  :group 'timeblock)

(defface timeblock-mark
  '((default :extend t)
    (((class color)
      (min-colors 88)
      (background light))
     :background "#7b435c")
    (((class color) (min-colors 88) (background dark))
     :background "#7b435c")
    (t :inverse-video t))
  "Face used for marked blocks."
  :group 'timeblock)

(defface timeblock-hours-line
  '((default :extend t)
    (((class color)
      (min-colors 88)
      (background light))
     :background "#7b435c")
    (((class color) (min-colors 88) (background dark))
     :background "#cdcdcd")
    (t :inverse-video t))
  "Face used for hour lines."
  :group 'timeblock)

(defface timeblock-current-time-indicator
  '((default :extend t)
    (((class color)
      (min-colors 88)
      (background light))
     :background "red")
    (((class color) (min-colors 88) (background dark))
     :background "red")
    (t :inverse-video t))
  "Color face used for current time indicator."
  :group 'timeblock)

;;;; Custom Variables

(defgroup timeblock nil
  "Customization for `timeblock'."
  :group 'org
  :link '(url-link "https://github.com/ichernyshovvv/timeblock"))

(provide 'timeblock-faces)
