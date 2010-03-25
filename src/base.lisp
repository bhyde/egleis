;; -*- mode: lisp; syntax: common-lisp; -*-

(in-package "EGLEIS")

;;;;

(defun pathname-of-system (system-name)
  (slot-value (asdf:find-system system-name)
              'asdf::relative-pathname))

;;;; Memoization

(defvar *memos* (make-hash-table :test #'equal))

;; (clrhash *memos*)

(defmacro memoize (form)
  `(let* ((key (list ',(car form) ,@(rest form)))
          (value? (gethash key *memos* '#1=unknown)))
     (if (eq '#1# value?)
         (setf (gethash key *memos*) (apply (car key) (rest key)))
         value?)))

(defmacro rememoize (form)
  `(let ((key (list ',(car form) ,@(rest form))))
     (setf (gethash key *memos*) (apply (car key) (rest key)))))

(defun intern-user-name-or-id-string (user-name-or-id-string)
  (if (every #'digit-char-p user-name-or-id-string)
      (parse-integer user-name-or-id-string)
      user-name-or-id-string))

(defun get-user-given-id-from-cgi (user-name-or-id-string)
  (first
   (etsy:get-user-details
    (if (digit-char-p (char user-name-or-id-string 0))
        (parse-integer user-name-or-id-string)
        user-name-or-id-string)
    :detail-level :high)))

(defun etsy-epoch->text (value)
  (setf value (round value))
  (incf value 2209006800) ;; convert to universal time
  (multiple-value-bind (second minute hour date month year)
      (decode-universal-time value)
    (format nil "~D/~D/~D ~D:~D:~D" month date year hour minute second)))

(defun etsy-price->text (amount units)
  (cond
    ((string= units "USD")
     (format nil "$~,2F" amount))
    (t
     (format nil "~S ~A" amount units))))

(defun etsy-markup-to-html (text)
  (cl-ppcre:regex-replace-all #\return text "<br/>"))

(defmacro defhandler (denotation args &body body)
  (cond
    ((symbolp denotation)
     `(defhandler-1 (,denotation) ,args ,@body))
    (t
     `(defhandler-1 ,denotation ,args ,@body))))

(defmacro defhandler-1 ((name &key (uri
                                    (format nil "~(~{/~A~}~)" (cl-ppcre:split "-" (symbol-name name)))))
                        args &body body)
  `(hunchentoot:define-easy-handler (,name :uri ,uri) ,args ,@body))


;;;; Color Handling

;;; See http://www.cliki.net/simple-rgb

(deftype rgb ()
  '(vector (unsigned-byte 8) 3))

(defun rgb (r g b)
  (make-array '(3) :element-type '(unsigned-byte 8)
                   :initial-contents (list r g b)))

(defun hsv (h s v)
  ;; The :ELEMENT-TYPE option in MAKE-ARRAY will only raise a condition on
  ;; type oddities if you try to insert a float that wants too much space
  ;; (CLHS 15.1.2.1).
  (unless (and (typep h '(float 0.0e0 1.0e0))
               (typep s '(float 0.0e0 1.0e0))
               (typep v '(float 0.0e0 1.0e0)))               
    (error (make-condition 'hsv-type-error :bugged (vector h s v))))
  (make-array '(3) :element-type '(float 0.0e0 1.0e0)
                   :initial-contents (list h s v)))

(defun rgb->hsv (a)
  (declare (type rgb a))
  (let* ((r (/ (aref a 0) 255.0))
         (g (/ (aref a 1) 255.0))
         (b (/ (aref a 2) 255.0))
         (max (max r g b))
         (min (min r g b))
         (v max))
    (if (= max min)
        (hsv 0.0 0.0 v)
        (let ((s (/ (- max min) max))
              (h 0))
          (cond ((= r max)
                 (setf h (- (/ (- max b) (- max min))
                            (/ (- max g) (- max min)))))
                ((= g max)
                 (setf h (+ 2.0 (- (/ (- max r) (- max min))
                                   (/ (- max b) (- max min))))))
                (t (setf h (+ 4.0 (- (/ (- max g) (- max min))
                                     (/ (- max r) (- max min)))))))
          (setf h (mod (/ h 6.0) 1))
          (hsv h s v)))))

(defun hsv->etsy (hsv)
  (let ((h (aref hsv 0))
        (s (aref hsv 1))
        (v (aref hsv 2)))
    (make-array '(3) :element-type 'fixnum
                :initial-contents (list (round (* 360 h)) (round (* 100 s)) (round (* 100 v))))))

(defvar *color-dictionary* (make-hash-table :test #'equal))

(defmacro color-info (name)
  `(gethash ,name *color-dictionary*))

(defun color-name (color-info) (first color-info))
(defun color-hsv (color-info) (second color-info))
(defun color-rgb (color-info) (third color-info))

(defun build-color-dictionary ()
  (flet ((load-rgb-file (name)
         (with-open-file (s (merge-pathnames name (egleis::pathname-of-system "egleis")))
           (loop for line = (read-line s nil nil)
                 while line
                 unless (eq #\! (char line 0))
                   do (destructuring-bind (r g b name)
                          (cl-ppcre:split "[ 	]+" 
                                          (cl-ppcre:regex-replace "^ +" line "")
                                          :limit 4)
                        (setf 
                         name (string-downcase name)
                         r (parse-integer r)
                         g (parse-integer g)
                         b (parse-integer b))
                        (let* ((rgb (rgb r g b))
                               (hsv (rgb->hsv rgb)))
                          (setf (color-info name) (list name hsv rgb))))))))
    (load-rgb-file "src/rgb1.txt")
    (load-rgb-file "src/rgb2.txt")))

(defun find-color-info (text)
  (or (color-info text)
      (loop for n being each hash-key of *color-dictionary* 
            when
            (search text n) 
            return (color-info n))))

;;;; Date Handling

(defun month-name-short (n)
  (svref #("" "Jan" "Feb" "Mar" "Apr"
           "May" "Jun" "Jul" "Aug"
           "Sep" "Oct" "Nov" "Dec") n))

;;;; List Handling

(defvar *english-list* "~#[ none~; ~a~; ~a and ~a~:;~@{~#[~; and~] ~a~^,~}~]")

(defun list->english-text (list &optional (item->text #'identity))
  (format nil "~?" *english-list* (mapcar item->text list)))

(defun tag-list->english-text (list &optional (item->text #'identity))
  (format nil "~?" *english-list* 
          (loop for i in list
                collect (substitute #\space #\_  (funcall item->text i)))))

;;;; Map Handling

(defvar *yahoo-app-id* nil)

(defvar *map-url-cache* (make-hash-table :test #'equal))
(defvar *map-url-cache-counter* 0)

(defun drain-map-url-cache () ;; this shouldn't be so severe
  (clrhash *map-url-cache*)
  (setf *map-url-cache-counter* 0))

(defun get-map-url (&key latitude longitude (width 25) (height 25) (radius 700))
  (let* ((key (list latitude longitude width height radius))
         (cache-entry (gethash key *map-url-cache*)))
    (unless cache-entry
      (when (zerop (mod (incf *map-url-cache-counter*) 1000))
        (drain-map-url-cache))
      (let ((body (flet ((f (x) (format nil "~s" x)))
                    (drakma:http-request 
                     "http://local.yahooapis.com/MapsService/V1/mapImage?"
                     :method :get
                     :parameters `(("appid" . ,*yahoo-app-id*)
                                   ("latitude" . ,(f latitude))
                                   ("longitude" . ,(f longitude))
                                   ("image_width" . ,(f width))
                                   ("image_height" . ,(f height))
                                   ("radius" . ,(f radius)))))))
        (setf cache-entry 
              (cons (incf *map-url-cache-counter*)
                    (multiple-value-bind (a b)
                        (cl-ppcre:scan-to-strings (cl-ppcre:create-scanner ">(http:.*)</result>" :single-line-mode t :case-insensitive-mode t) body)
                        (declare (ignore a))
                      (svref b 0))))
        (setf (gethash key *map-url-cache*) cache-entry)))
    (cdr cache-entry)))
