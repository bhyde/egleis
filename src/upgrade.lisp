;; -*- mode: lisp; syntax: common-lisp; -*-

(in-package "EGLEIS")

(defvar *max-upgrade-version*)

(setf *max-upgrade-version* 1)

(defvar *current-upgrade-version* 0)

(when (< *current-upgrade-version* *max-upgrade-version*)
  (when (< *current-upgrade-version* 1)
     (setf hunchentoot:*session-max-time* 144000))

  (setf *current-upgrade-version* *max-upgrade-version*))
