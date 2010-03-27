;; -*- mode: lisp; syntax: common-lisp; -*-

(in-package "EGLEIS")

(setf hunchentoot:*message-log-pathname* "/tmp/hmsg.log")
(setf hunchentoot:*access-log-pathname* "/tmp/hacc.log")
(setf hunchentoot:*log-lisp-errors-p* t)
(setf hunchentoot:*log-lisp-warnings-p* t)
(setf hunchentoot:*show-lisp-errors-p* t)
(defvar *utf-8* (flex:make-external-format :utf-8 :eol-style :lf))
(defvar *server* nil)

(defun start-my-server (&key (port 4000) etsy-api-key yahoo-api-id)
  (setf hunchentoot:*session-max-time* 144000)
  (hunchentoot:reset-session-secret)
  (setf *server* (make-instance 'hunchentoot:acceptor :port port))
  (setf etsy:*api-key* etsy-api-key)
  (setf *yahoo-app-id* yahoo-api-id)
  (build-color-dictionary)
  (hunchentoot:start *server*))

#+nil
(hunchentoot:stop *server*)

(defvar *the-html-stream*)

(defmacro with-html-out (() &body body)
  `(with-html-output (*the-html-stream*)
     ,@body))

(defvar *last-page*) 

(defgeneric emit-html-for-slot-view (a b c d e))
(defgeneric emit-html-for-slot (a b c))

(defmacro with-html-page ((title) &body content)
  `(flet ((emit-title (stream)
            (with-html-output (stream) 
              ,title
              nil))
          (emit-body (stream)
            (with-html-output (stream)
              ,@content
              nil)))
     (emit-html-page-1 #'emit-title #'emit-body)))

(defun fill-out-sesison-if-necessary ()
  (unless (hunchentoot:session-value 'page-count)
    (setf (hunchentoot:session-value 'page-count) 0)))



(defun emit-html-page-1 (title-lambda content-lambda)
  (hunchentoot:start-session)
  (fill-out-sesison-if-necessary)
  (incf (hunchentoot:session-value 'page-count))
  (setf (hunchentoot:content-type*) "text/html; charset=utf-8"
        (hunchentoot:reply-external-format*) *utf-8*)
  (setf *last-page*
        (with-html-output-to-string (*the-html-stream* nil :prologue t)
          (:html
           (:head (:title "Etsy Inspector:" (funcall title-lambda *the-html-stream*))
                  (emit-style))
           (:body
            (:span 
             :class "header"
             (:a :href "/" (:img :width 20 :height 20 :style "padding-right: 3px" :src "/static/egleis-icon.png"))
             (:a :href "/" "Etsy Inspector"))
            (:span :class "content" 
                   (funcall content-lambda *the-html-stream*))
            (:span 
             :class "footer"
             (:span 
              :class "left" 
              (:a :href (about-link) "About?")
              "  "
              (emit-timer-sparkline etsy::*etsy-api-request-timer*))
             (:span 
              :class "right"
              "The term '" (:a :href "http://www.etsy.com/" "Etsy")
              "' is a trademark of " (:a :href "http://www.etsy.com/about.php" "Etsy, Inc.")
              "  This application uses the " (:a :href "http://developer.etsy.com/" "Etsy API")
              " but is not endorsed or certified by Etsy, Inc.")))))))
     


(defmacro with-field-html ((label) &body who-body)
  `(with-html-out ()
     (:span :class "field"
            (:span :class "field-name" (fmt "~:(~A~)" (symbol-name ,label)))
            (:span :class "field-value" ,@who-body))))


(defun slot-value-or-default (object slot-name &optional (default :unbound))
  (if (slot-boundp object slot-name) 
      (slot-value object slot-name) 
      default))

(defmacro get-viewer-of-slot (slot-description view-name)
  `(getf (slot-value ,slot-description 'etsy:plist) ,view-name))

(defmethod emit-html-for-slot ((object t) (slot-description t) (view-name symbol))
  (with-slots (name) slot-description
    (let ((viewer-info
           (or
            (get-viewer-of-slot slot-description view-name)
            '(default))))
      (emit-html-for-slot-view object slot-description view-name (car viewer-info) (rest viewer-info)))))

(defhandler inspect-user0 ((user-name-or-id :real-name "id"))
  (let ((user (first 
               (etsy:get-user-details 
                (intern-user-name-or-id-string user-name-or-id)
                :detail-level :high))))
    (with-html-page ((fmt "User - ~:(~A~)" (slot-value user 'etsy:user-name)))
      (emit-view-of-object 'inspect user))))

(defun inspect-user0-link (&key user-name-or-id)
  (format nil "/inspect/user0?id=~A" user-name-or-id))

(defmacro tooltip ((text) &body body)
  `(with-html-out ()
     (:div :class ".tooltip" :title ,text ,@body)))

(defun emit-etsy-markup (text)
  (with-html-out ()
    (:span :class "etsy-markup"
           (str (etsy-markup-to-html text)))))

(defun inspect-user-link (&key user-name-or-id)  
  (format nil "/inspect/user?id=~A" user-name-or-id))

(defhandler inspect-user ((user-name-or-id :real-name "id"))
  (let ((object (first 
                 (etsy:get-user-details 
                  (intern-user-name-or-id-string user-name-or-id)
                  :detail-level :high))))
    (with-slots (etsy:user-name etsy:user-id etsy:is-seller
                                etsy:join-epoch etsy:image-url-75x75
                                etsy:transaction-buy-count etsy:transaction-sold-count
                                etsy:city etsy:lat etsy:lon
                                etsy:birth-day etsy:birth-month
                                etsy:feedback-count etsy:feedback-percent-positive
                                etsy:materials etsy:status
                                etsy:referred-User-Count etsy:bio) object
      (with-html-page ((fmt "User - ~:(~A~)" etsy:user-name))
        (:span 
         :class "user-card"
         (:span :class "user-summary"
                (let ((touch-up-image etsy:image-url-75x75))
                  (when (string= "images/grey.gif" touch-up-image)
                    (setf touch-up-image "http://www.etsy.com/images/grey.gif"))
                  (htm
                   (:img :style "float:left" :width "75" :height "75" :src touch-up-image)))
                (when (slot-boundp object 'etsy:status) (htm (fmt "Status: ~S" etsy:status)))
                (:b (str etsy:user-name) )
                (fmt " (~D) member since ~A" etsy:user-id (etsy-epoch->text etsy:join-epoch))
                :br
                (fmt "bought: ~D, sold: ~D" etsy:transaction-buy-count etsy:transaction-sold-count)
                (cond
                  (etsy:is-seller
                   (htm " " (:a :href (inspect-shop-link :user-name-or-id etsy:user-name) "Shop")))
                  (t
                   (htm " no shop")))
                :br
                (cond
                  ((equal "0" etsy:feedback-count) (fmt "No feedback"))
                  (t
                   (fmt "~A% positive of ~A feedback~:P"
                        (or etsy:feedback-percent-positive
                            0)
                        (if (stringp etsy:feedback-count)
                            (parse-integer etsy:feedback-count)
                            etsy:feedback-count))))
                (fmt " and has refered ~r other~p." 
                     etsy:referred-User-Count
                     etsy:referred-User-Count)
                :br
                (cond
                  ((or etsy:city etsy:lat etsy:lon)
                   (htm  (:img :src (get-map-url :latitude etsy:lat :longitude etsy:lon :width 35 :height 35)
                               :width 35 :height 35
                               :alt (format nil "lat: ~D long: ~D" etsy:lat etsy:lon)
                               :title (format nil "~A: lat: ~D long: ~D" etsy:city etsy:lat etsy:lon)
                               :class ".tooltip")))
                  (t
                   (htm "Unknown location.")))
                (cond
                  ((or (null etsy:birth-month)
                       (string= "" etsy:birth-month))
                   (htm (str " Birthday: unknown")))
                  (etsy:birth-month
                   (htm (fmt " Birthday: ~A ~A"
                             (month-name-short (parse-integer etsy:birth-month))
                             etsy:birth-day))))
                (when etsy:materials
                   (htm :br "Materials: " 
                        (str (list->english-text etsy:materials)))))
         (:span :class "etsy-markup"
                (tooltip ("bio")
                  (str (etsy-markup-to-html etsy:bio)))))))))


(defhandler inspect-shop0 ((user-name-or-id :real-name "id"))
  (let ((shop (first 
               (etsy:get-shop-details 
                (intern-user-name-or-id-string user-name-or-id)
                :detail-level :high))))
    (with-html-page ((fmt "shop - ~:(~A~)" (slot-value shop 'etsy:user-name)))
      (emit-view-of-object 'inspect shop))))

(defun inspect-shop-link (&key user-name-or-id)  
  (format nil "/inspect/shop?id=~A" user-name-or-id))

(defhandler inspect-shop ((user-name-or-id :real-name "id"))
  (let ((shop (first 
               (etsy:get-shop-details 
                (intern-user-name-or-id-string user-name-or-id)
                :detail-level :high))))
    (with-slots (etsy:banner-image-url etsy:last-updated-epoch etsy:creation-epoch
                                       etsy:listing-count etsy:shop-name etsy:title
                                       etsy:sale-message etsy:announcement etsy:is-vacation
                                       etsy:vacation-message etsy:currency-code etsy:policy-welcome
                                       etsy:policy-payment etsy:policy-shipping etsy:policy-refunds
                                       etsy:policy-additional etsy:sections etsy:transaction-sold-count
                                       etsy:user-id etsy:url etsy:user-id) shop
      (macrolet ((f (slot)
                   `(tooltip (,(format nil "~:(~A~)" slot))
                      (emit-etsy-markup ,slot))))
        (with-html-page ((fmt "shop - ~:(~A~)" (slot-value shop 'etsy:user-name)))
          (:span 
           :class "shop-card"
           (:span :class "header"
                  (:span :class "title" (str etsy:title))
                  (:img :src etsy:Banner-Image-Url))
           (:span :class "body"
                  (:span :class "metadata"
                         (f etsy:announcement)
                         (f etsy:sale-message)
                         (when etsy:is-vacation
                           (f etsy:vacation-message))
                         (f etsy:policy-welcome)
                         (f etsy:policy-payment)
                         (f etsy:policy-shipping)
                         (f etsy:policy-refunds)
                         (f etsy:policy-additional)
                         (fmt "Store created ~A and last updated ~A."
                              (etsy-epoch->text etsy:creation-epoch)
                              (etsy-epoch->text etsy:last-updated-epoch)))
                  (:span :class "sidebar"
                         "This " (:a :href etsy:url "etsy shop") " has "
                         (fmt "~r listing~:p in ~r section~:p:" 
                              etsy:listing-count
                              (length etsy:sections))
                         (:span
                          :class "shop-sections"
                          (:span :class "shop-section" 
                                 (:a :href (inspect-shop-listings-link
                                            :id etsy:user-id :offset 0 :limit 10)
                                     "All")
                                 (fmt " (~D)" etsy:listing-count))
                          (loop 
                             for shop-section in etsy:sections
                             do 
                             (with-slots 
                                   (etsy:section-id etsy:title etsy:listing-count) shop-section
                               (htm (:span :class "shop-section" :style "display: block"
                                           (:a :href (inspect-shop-section-link
                                                      :user-name-or-id etsy:user-id
                                                      :section-id etsy:section-id)
                                               (str etsy:title))
                                           (fmt " (~A)" etsy:listing-count))))))
                         "The owner "
                         (:a :href (inspect-user-link :user-name-or-id etsy:shop-name)
                             (str etsy:shop-name))
                         (fmt " has sold ~D items." etsy:transaction-sold-count)
                         "  Currency " (str etsy:currency-code)
                         (:span
                          :class "item-grid"
                          (loop for listing in (etsy:get-shop-listings etsy:user-id :limit 50)
                             do
                             (with-slots (etsy:image-url-25x25 etsy:listing-id) listing
                               (htm 
                                (:a :href (format nil "/inspect/listing?id=~D" etsy:listing-id)
                                    (:img :width 25 :height 25 :src etsy:image-url-25x25))))))
                         (multiple-value-call #'emit-tag-cloud
                           (collect-shop-tag-cloud etsy:user-id))
                         ))))))))

(defhandler (inspect-random-shop :uri "/inspect/random-shop") ()
  (inspect-shop :user-name-or-id (random-shop)))

(defhandler (inspect-gift-guides :uri "/inspect/gift-guides") ()
  (let ((gift-guides (memoize (etsy:get-gift-guides))))
    (with-html-page ("Gift Guides")
      (flet ((do-section (section-title)
               (htm
                 (:li (str section-title))
                 (:ul
                  (loop for gift-guide in gift-guides
                        do 
                     (with-slots (etsy:title etsy:guide-id etsy:guide-section-title
                                             etsy:description) gift-guide
                       (when (string= section-title etsy:guide-section-title)
                         (htm
                           (:li
                            (:a :href (inspect-gift-guide-link :guide-id etsy:guide-id)
                                (:span :class "tooltip"
                                       :title (escape-string etsy:description)
                                       (str etsy:title))))))))))))
        (htm
          (:span :class "gift-guides-toc"
                 (:ul
                  (loop 
                    with section = "unknown"
                    for gift-guide in gift-guides
                    do 
                 (with-slots (etsy:guide-section-title) gift-guide
                   (unless (string= section etsy:guide-section-title)
                     (setf section etsy:guide-section-title)
                     (do-section section))))))
          (:span :class "gift-guides"
                 (loop for gift-guide in gift-guides
                       do 
                    (with-slots (etsy:guide-id) gift-guide
                      (htm
                        (:span 
                         :id (format nil "g~D" etsy:guide-id)
                         (emit-view-of-object 'inspect gift-guide)))))))))))

(defun inspect-gift-guide-link (&key guide-id)
  (format nil "/inspect/gift-guide?id=~A" guide-id))

(defhandler (inspect-gift-guide :uri "/inspect/gift-guide") ((guide-id :parameter-type 'integer
                                                                       :real-name "id"))
  (let* ((guides (etsy:get-gift-guides))
         (guide (find guide-id guides :key #'(lambda (x) (slot-value x 'etsy:guide-id))))
         (listings (etsy:get-gift-guide-listings guide-id :detail-level :high)))
    (with-slots (etsy:guide-section-title etsy:title etsy:description) guide
      (with-html-page ((fmt "Gift Guide - ~A - ~A" etsy:guide-section-title etsy:title))
        (:span :class "header"
               (str etsy:guide-section-title)
               ": "(str etsy:title)
               :br
               (str etsy:description))
        (:span :class "body"
              (:span :class "listings"
                     (loop for listing in listings
                           do (emit-small-listing-card listing))))))))
             

;        (view-listings-1 listings 0 10 "hi" 
;                         (format nil "/inspect/gift-guide?id=~d&offset=~~d&limit=~~d"
;                                 guide-id))))

  

(defhandler inspect-methods ()
  (let ((methods (etsy:get-method-table)))
    (with-html-page ("Methods")
      (:span :class "methods-toc"
             (loop for method in methods
                   do (with-slots (name) method
                        (htm (:span :class "method-toc"
                                    (:a :href (format nil "#~A" name) (str name)))))))
      (:span :class "methods"
             (loop for method in methods
                   do (with-slots (name) method
                        (htm (:span :class "method" :id name
                                    (emit-view-of-object 'inspect method)))))))))

(defhandler (inspect-index :uri "/") ()
  (with-html-page ("Home")
    (flet ((listing-by-form (label fn &rest examples)
             (htm
              (:p
               (:form :method :get :action (funcall fn :for-form t) :class "inline"
                      (:table
                       (:tr
                        (:td (:input :type "text" :width "3" :name "q" ))
                        (:td :style "width:60pt" (str label))
                        (:td "Examples: "
                             (loop 
                                for x in examples
                                as first = t then nil
                                do
                                (htm
                                 (unless first (htm ", "))
                                 (:a :href (funcall fn :q x) (str x))))))))))))
      (htm
        (:p"The API can fetch info about users, or shops.")
        (:p (:form :method :get :action "/inspect/user"
               (:input :type "text" :width "10em" :name "id" )" Etsy Member"

               ", " (:a :href (inspect-user-link :user-name-or-id (+ 11000 (random 10000))) "Random")))
        (:p (:form  :method :get :action "/inspect/shop"
                (:input :type "text" :width "10em" :name "id" )
                " Etsy shop by owner"
                ", " (:a :href "/inspect/random-shop" "Random")
                #+nil ", " #+nil (:a :href (inspect-shop-link :user-name-or-id "mck254") "Mimi")))
        (:p "The API can also search for listings in various ways (i.e. keyword, tags, materials, and categories)...")
        (listing-by-form "Keyword"
                          'inspect-listings-by-keyword-link
                          "tattoo" "love" "piercing" "cotton" "smoke")
        (listing-by-form "Tags"
                          'inspect-listings-by-tags-link
                          "hot" "cheap" "cool" "brother")        
        (listing-by-form "Materials"
                          'inspect-listings-by-materials-link
                          "steel" "sugar" "love" "smoke")
        (listing-by-form "Category"
                          'inspect-listings-by-category-link
                          "art" "art:drawing" "candles" "supplies")
        (listing-by-form "Color"
                          'inspect-listings-by-color-link
                          "red" "smoke" "purple" "silver")
        (:p "The API can fetch the gift-guides, the hierarhcy of categories, and a bit of information about it's own methods")
        (:ul
         (:li (:a :href "/inspect/gift-guides" "Gift Guides"))
         (:li (:a :href "/inspect/categories" "Categories"))
         (:li (:a :href "/inspect/methods" "Methods")))))))

(defhandler site-maintenance ()
  (with-html-page ("Site Maintenance")
    (:span :class "maintenance"
           (:ul
            (:li (:a :href "/site/session" "Show my session"))
            (:li (:a :href "/site/reload" "Load Changes"))
            (:li (:a :href "/site/clear-timers" "Clear timers"))))))

(defhandler site-session ()
  (with-html-page ("Site Maintenance - Session")
    (:span :class "maintenance"
           (loop for i in '(page-count)
                 do (htm (:p (str i) " " (str (hunchentoot:session-value i)))))
           (loop for i in '(hunchentoot::session-id
                            hunchentoot::session-string
                            hunchentoot::user-agent
                            hunchentoot::remote-addr
                            hunchentoot::session-start
                            hunchentoot::last-click)
                 do (htm (:p (str i) " " (str (slot-value hunchentoot:*session* i))))))))

(defhandler site-reload ()
  (with-html-page ("Site Restart")
    (:span 
     :class "maintenance"
     (:p (:b "perform load-op on the system"))
     (:pre :style "margin-left: 20px; font-size: 70%"
      (str
       (with-output-to-string (*standard-output*)
         (asdf:oos 'asdf:load-op "market")))))))

(defhandler (site-clear-timers :uri "/site/clear-timers") ()
  (etsy::clear-timer etsy::*etsy-api-request-timer*)
  (with-html-page ("Site Restart")
    (:span  :class "maintenance" "done")))

(defhandler selected-shops ()
  (with-html-page ("Selected Shops")
      (loop for shop-name in *selected-shops*
            do (htm (:a :href (inspect-shop-link :user-name-or-id shop-name) (str shop-name)) ", "))))

(defvar *z*) 

(defun view-listings (kind api-func query-text query offset limit)
  (multiple-value-bind (listings max)
      (funcall api-func query :detail-level :high :offset offset :limit limit)
    (setf *z* listings)
    (view-listings-1 listings offset limit max
                     (format nil "~A via ~(~A~)" query-text kind)
                     (format nil "/inspect/listings/by/~(~a~)?q=~a&offset=~~d&limit=~~d" kind query-text))))

(defun view-listings-1 (listings offset limit max header url-template)
  (with-html-page ((str header))
    (:span :class "header" header)
    (emit-pager url-template offset limit max)
    (:span
     :class "listings-widget"
     (cond
       (listings
        (htm
          (emit-tiles-for-listings listings t)
          (emit-listings-via-small-listing-card listings)))
       (t
        (htm "None Found"))))))


(defun emit-pager (url-template offset limit max)
  (let ((this-page (floor offset limit))
        (page-count (round max limit)))
    (with-html-out ()
      (flet ((f (page)
               (let ((offset (* limit page)))
                 (htm
                   " "
                   (:span 
                    :class (if (eq page this-page)
                               "current-page"
                               "page")
                    (:a :href (format nil url-template offset limit) (str page)))
                   " "))))
        (htm
          (if (< 0 offset)
              (htm
                (:span :class "pager" (:a :href 
                                          (format nil url-template (- offset limit) limit)
                                          "Previous"))
                " ")
              (htm (:span :class "pager-disabled" "Previous ")))
          
          (cond
            ((< max limit)
             (htm "All showing"))
            ;;  prev 1 2 3 next
            ((< (round max limit) 11)
             ; (htm "a")
             (loop for i from 0 to page-count do (f i)))
            ;; prev 1 2 3 ... 100 next
            ((< this-page 5)
             ; (htm "b")
             (loop for i from 0 to (1+ this-page) do (f i))
             (htm "...")
             (loop for i from (- page-count 2) to page-count do (f i)))
            ((< (- page-count 4) this-page)
             ; (htm "c")
             (f 0)
             (f 1)
             (htm "...")
             (loop for i from (- this-page 1) to page-count do (f i)))
            (t
             ; (htm "d")
             (f 0)
             (f 1)
             (htm "...")
             (f (1- this-page))
             (f this-page)
             (f (1+ this-page))
             (htm "...")
             (f page-count)
             (f (1- page-count))))
          " "
          (if (< (+ offset limit) max)
              (htm
                (:span :class "pager" 
                       " " (:a :href 
                               (format nil url-template (+ offset limit) limit)
                               "next")))
              (htm
                (:span :class "pager-disabled" " Next")))
          #+nil
          (fmt "page: ~D offset ~D, limit ~D, max ~D, pages: ~D" this-page offset limit max page-count)

          )))))

(defun emit-tiles-for-listings (listings &optional linked?)
  (with-html-out ()
    (:span
     :class "listing-tiles"
     (loop for listing in listings
           when (string= "active" (slot-value listing 'state)) ;; TBD
             do 
          (with-slots (etsy:image-url-50x50 etsy:listing-id) listing
            (htm 
              (:span :class "listing-tile"
                     (flet ((f ()
                              (htm (:img :src (slot-value listing 'image-url-50x50)
                                         :width 50 :height 50))))
                       (if linked?
                           (htm (:a :href (format nil "#L~D" etsy:listing-id) (f)))
                           (f))))))))))

(defun emit-listings-via-small-listing-card (listings)
  (with-html-out ()
    (:span :class "listings"
          (loop for listing in listings
                when (string= "active" (slot-value listing 'state)) ;; TBD
                  do (emit-small-listing-card listing)))))

(defun emit-small-listing-card (listing)
  (with-slots (etsy:section-id etsy:section-title etsy:hsv-color
                               etsy:rgb-color etsy:title etsy:url
                               etsy:description etsy:price etsy:quantity
                               etsy:currency-code etsy:image-url-200x200 etsy:ending-epoch
                               etsy:user-id etsy:user-name etsy:tags
                               etsy:materials etsy:city etsy:state
                               etsy:lat etsy:lon etsy:creation-epoch
                               etsy:listing-id etsy:views etsy:user-image-id
                               etsy:image-url-25x25 etsy:image-url-50x50 etsy:image-url-75x75
                               etsy:image-url-155x125 etsy:image-url-430xN etsy:favorite-creation-epoch
                               ) listing
    (with-html-out ()
      (:span :class "small-listing-card" :id (format nil "L~D" etsy:listing-id)
             (:span :class "header"
                    (:a :href (inspect-listing-link :id etsy:listing-id)
                        (str etsy:title)))
             (:span :class "body"
                    (:span :class "left-side"
                           (:img :width 200 :height 200 :src etsy:image-url-200x200))
                    (:span :class "right-side"
                           (str (etsy-price->text etsy:price etsy:currency-code))
                           (when (< 1 etsy:quantity)
                             (htm " (" (str etsy:quantity) " available)"))
                           " at the "(:a :href (inspect-shop-link :user-name-or-id etsy:user-id)
                                         "shop")
                           " of "
                           (:a :href (inspect-user-link :user-name-or-id etsy:user-id) 
                               (str etsy:user-name))
                           
                                        ; (str etsy:ending-epoch)
                
                           "  Tags: " (str 
                                       (tag-list->english-text etsy:tags)) "."
                           "  Materials: " (str (tag-list->english-text etsy:materials)) "."
                                                 
                           (fmt "In ~A, ~A at ~D,~D" etsy:city etsy:state
                                etsy:lat
                                etsy:lon)
                           (fmt " listed: ~A" (etsy-epoch->text etsy:creation-epoch))
                                        ;(str etsy:views)
                           (fmt " viewed ~r time~:p" etsy:views)
                           (fmt "In \"~A\" (~D)" etsy:section-title
                                etsy:section-id)
                           (:span :class "color-swash"
                                  :style (format nil "background: X~6,'0X" etsy:hsv-color))
                           (:span :class "description"
                                  (str (etsy-markup-to-html etsy:description)))
                           
                           ))
             ;; etsy:user-image-id
             ;; etsy:image-url-25x25
             ;; etsy:image-url-50x50
             ;; etsy:image-url-75x75
             ;; etsy:image-url-155x125
             ;; etsy:image-url-430xN
             ;; etsy:favorite-creation-epoch
             ))))

(defun inspect-shop-listings-link (&key id (offset 0) (limit 10) for-form)
  (if for-form
      "/inspect/shop-listings"
      (format nil "/inspect/shop-listings?id=~A&offset=~D&limit=~D" id offset limit)))

(defhandler (inspect-shop-listings :uri "/inspect/shop-listings") (id (offset
                                                                       :parameter-type 'integer
                                                                       :init-form 0)
                                                                      (limit
                                                                       :parameter-type 'integer
                                                                       :init-form 10))
  (multiple-value-bind (listings max) 
      (etsy:get-shop-listings id :offset offset :limit limit :detail-level :high)
    (view-listings-1  listings offset limit max "Header All"
                     (format nil "/inspect/shop-listings?id=~A&offset=~~d&limit=~~d" id))))


(defun inspect-listings-by-keyword-link (&key q (offset 0) (limit 10) for-form)
  (if for-form
      "/inspect/listings/by/keyword"
      (format nil "/inspect/listings/by/keyword?q=~A&offset=~D&limit=~D" q offset limit)))

(defhandler inspect-listings-by-keyword (q (offset
                                            :parameter-type 'integer
                                            :init-form 0)
                                           (limit
                                            :parameter-type 'integer
                                            :init-form 10))
  (view-listings 'keyword 'etsy:get-listings-by-keyword q (list q) offset limit))


(defun inspect-listings-by-tags-link (&key q (offset 0) (limit 10) for-form)
  (if for-form
      "/inspect/listings/by/tags"
      (format nil "/inspect/listings/by/tags?q=~A&offset=~d&limit=~d" q offset limit)))

(defhandler inspect-listings-by-tags (q
                                      (offset
                                       :parameter-type 'integer
                                       :init-form 0)
                                      (limit
                                       :parameter-type 'integer
                                       :init-form 10))
  (let ((list-of-tags (cl-ppcre:split "[ 	]+" q)))
    (view-listings 'tags #'etsy:get-listings-by-tags q list-of-tags offset limit)))

(defun inspect-listings-by-materials-link (&key q (offset 0) (limit 10) for-form)
  (if for-form
      "/inspect/listings/by/materials"
      (format nil "/inspect/listings/by/materials?q=~A&offset=~D&limit=~D" q offset limit)))

(defhandler inspect-listings-by-materials (q
                                           (offset
                                            :parameter-type 'integer
                                            :init-form 0)
                                           (limit
                                            :parameter-type 'integer
                                            :init-form 10)) 
  (view-listings 'materials #'etsy:get-listings-by-materials q (list q) offset limit))

(defun inspect-listings-by-category-link (&key q (offset 0) (limit 10) for-form)
  (if for-form
      "/inspect/listings/by/category"
      (format nil "/inspect/listings/by/category?q=~A&offset=~D&limit=~D" q offset limit)))

(defhandler inspect-listings-by-category (q
                                          (offset
                                           :parameter-type 'integer
                                           :init-form 0)
                                          (limit
                                           :parameter-type 'integer
                                           :init-form 10)) 
  (view-listings 'category #'etsy:get-listings-by-category q q offset limit))

(defun inspect-listings-by-color-link (&key q (offset 0) (limit 10) for-form)
  (if for-form
      "/inspect/listings/by/color"
      (format nil "/inspect/listings/by/color?q=~A&offset=~D&limit=~D" q offset limit)))

(defhandler inspect-listings-by-color (q
                                       (offset
                                        :parameter-type 'integer
                                        :init-form 0)
                                       (limit
                                        :parameter-type 'integer
                                        :init-form 10)) 
  (view-listings 'color #'etsy:get-listings-by-color q (hsv->etsy (color-hsv (find-color-info q))) offset limit))

(defun inspect-listings-by-category-color-and-keywords (&key q (offset 0) (limit 10))
  (format nil "/inspect/listings/by/color/and/keywords?q=~A&offset=~D&limit=~D" q offset limit))

(defhandler inspect-listings-by-color-and-keywords (q
                                                    (offset
                                                     :parameter-type 'integer
                                                     :init-form 0)
                                                    (limit
                                                     :parameter-type 'integer
                                                     :init-form 10)) 
  (view-listings 'color-and-keywords #'etsy:get-listings-by-color-and-keywords q (list q) offset limit))

(defun prime-categories ()
  (labels ((f (c)
             (print c)
             (map nil #'f (memoize (etsy:get-child-categories c)))))
    (map nil #'f (etsy:get-top-categories))))

(defhandler inspect-categories ()
  (with-html-page ("Categories")
    (flet ((f (id n)
             (htm
               (:a :href (inspect-category-link :q id) (str (subseq id n))))))
      (htm
        (:span :class "categories"
               (loop for category in (memoize (etsy:get-top-categories))
                     as len = (1+ (length category))
                     do 
                  (htm 
                    (:span :class "category" (f category 0))
                    (:span :class "subcategories"
                           (loop for subcategory in (memoize (etsy:get-child-categories category))
                                 as len2 = (1+ (length subcategory))
                                 do 
                              (htm (:span :class "subcategory" (f subcategory len))
                                   (:span :class "subsubcategories"
                                          (unless (find #\3 subcategory)
                                            (loop for subsubcategory in (memoize (etsy:get-child-categories subcategory))
                                                  do
                                               (htm
                                                 (:span :class "subsubcategory" 
                                                        (f subsubcategory len2))
                                                 ))))))))))))))

(defhandler inspect-listing0 ((id :parameter-type 'integer))
  (with-html-page ("Listing")
    (emit-view-of-object
     'inspect
     (first (etsy:get-listing-details id :detail-level :high)))))

(defun inspect-listing-link (&key id)
  (format nil "/inspect/listing?id=~D" id))


(defhandler inspect-listing ((id :parameter-type 'integer))
  (with-slots (etsy:image-url-430xN
               etsy:title
               etsy:description
               etsy:listing-id
               etsy:views etsy:tags etsy:materials
               etsy:price etsy:currency-code

               etsy:Creation-Epoch
               etsy:Ending-Epoch
               etsy:User-Name
               etsy:Quantity
               etsy:Rgb-Color
               etsy:Hsv-Color
               etsy:lat etsy:lon
               etsy:city
               etsy:section-id
               etsy:section-title) (first (etsy:get-listing-details id :detail-level :high))
    (with-html-page ("Listing")
      (:span
       :class "listing-card"
       (:span :class "header"
              (:span :class "title" (str title)))
       (:span :class "body"
              (:span :class "left-side"
                     (:img :width 430 :src etsy:image-url-430xN))
              (:span :class "right-side"
                     (:p (str (etsy-price->text etsy:price etsy:currency-code)))
                     (emit-etsy-markup etsy:description)
                     (:p (fmt "Quantity: ~R.  " etsy:Quantity))
                     (:p (fmt "Viewed ~R times.  " etsy:views))
                     (htm
                       (:p "Tagged: " (str (tag-list->english-text etsy:tags)))
                       (:p "Materials: " (str (tag-list->english-text etsy:materials))))
                     (:p (fmt "This listing was created on ~A in the \"" (etsy-epoch->text etsy:Creation-Epoch))
                         (:a :href (format nil "http:/inspect/shop-section?id=~D&shop=~a"
                                           etsy:section-id
                                           etsy:user-name)
                             (str etsy:section-title))
                         "\" section of "
                         (:a :href (inspect-user-link :user-name-or-id etsy:user-name) (str etsy:user-name))
                         "'s "
                         (:a :href (inspect-shop-link :user-name-or-id etsy:user-name)"shop")
                         ".  ")
                              ;; etsy:section-id
                         (fmt "Listing paid up until ~A.  " 
                              (etsy-epoch->text etsy:Ending-Epoch))
                     (:p
                      (cond
                        ((or etsy:city etsy:lat etsy:lon)
                         (htm  (str etsy:city) " - " (str etsy:lat) ", " (str etsy:lon)))
                        (t
                         (htm ", unknown location"))))
              ;; etsy:Rgb-Color
               ;; etsy:Hsv-Color
                     ))))))

(defun inspect-category-link (&key q (offset 0) (limit 10) for-form)
  (if for-form
      "/inspect/category"
      (format nil "/inspect/category?q=~A&offset=~d&limit=~d" q offset limit)))

(defhandler inspect-category (q (offset
                                  :parameter-type 'integer
                                  :init-form 0)
                                 (limit
                                  :parameter-type 'integer
                                  :init-form 10))
  (view-listings 'category #'etsy:get-listings-by-category q q offset limit))

(defun inspect-shop-section-link (&key user-name-or-id section-id (offset 0) (limit 10))
  (format nil "/inspect/shop-section?shop=~d&id=~d&offset=~d&limit=~d"
          user-name-or-id section-id offset limit))

(defhandler (inspect-shop-section :uri "/inspect/shop-section") ((section-id :real-name "id"
                                                                             :parameter-type 'integer)
                                                                 (user-name-or-id :real-name "shop")
                                                                 (offset
                                                                  :parameter-type 'integer
                                                                  :init-form 0)
                                                                 (limit
                                                                  :parameter-type 'integer
                                                                  :init-form 10))
  (setf user-name-or-id (if (every #'digit-char-p user-name-or-id)
                            (parse-integer user-name-or-id)
                            user-name-or-id))
  (multiple-value-bind (listings max) (etsy:get-shop-listings user-name-or-id 
                                                              :detail-level :high
                                                              :section-id section-id
                                                              :offset offset
                                                              :limit limit)
    (view-listings-1 listings offset limit max 
                     "Section"
                     (format nil "/inspect/shop-section?id=~A&shop=~A&offset=~~d&limit=~~d"
                             section-id user-name-or-id))))

(defun about-link () "/about")

(defhandler about ()
  (with-html-page ("About")
    (:span 
     :class "image-card"
          (:span :class "left"
                (:img :src "/static/egleis.png" :width 406 :height 274)
                :br
                (:span :class "caption"
                      "Speyeria egleis ("
                      (:a :href "http://www.flickr.com/photos/pommie_zaius/206255220/"
                          "thanks")
                      ")"))
          (:span :class "right" :style "padding-left: 420px"
                (:P
                "Welcome.  " 
                (:a :href "http://www.google.com/search?q=+site%3Aenthusiasm.cozy.org+butterfly" "Egleis")
                " is some fairly random peices based on the Etsy API.  As I am just fooling around I have no idea where this is going. ")
                (:P
                 "This software is based upon:"
                 (:ul
                  (:li (:a :href "http://www.apple.com/macosx/"
                           "OS X")
                       (:li (:a :href "http://sbcl.sourceforge.net/" "SBCL")
                            " Steelbank Ccommon Lisp.")
                       (:li (:a :href "http://developer.etsy.com/" "Etsy")
                            " Their API.")
                       (:li (:a :href "http://github.com/bhyde/cl-etsy/tree/master" "CL-Esty")
                            " Common Lisp support for the Etsy API.")
                       (:li (:a :href "http://www.weitz.de/hunchentoot/" "Hunchentoot")
                            " Web Server, in Common Lisp.")
                       (:li (:a :href "http://www.weitz.de/drakma/" "Drakma")
                            " Web Client, in Common Lisp.")
                       (:li (:a :href "http://www.weitz.de/cl-ppcre/" "CL-PPCRE")
                            " A Regex library, in Common Lisp.")
                       (:li (:a :href "http://common-lisp.net/project/cl-json/" "CL-JSON")
                            " A JSON library, in Common Lisp.")
                       (:li (:a :href "http://jquery.com/" "JQuery")
                            " A web brower side javascript library.")
                       (:li (:a :href "http://omnipotent.net/jquery.sparkline/"
                                "JQuery Sparklines")))))
                (:p
                 "Changes:"
                 (:ul
                  (:li "3/28/09: Testing framework for cl-etsy.")
                  (:li "4/1/09: A simple and incomplete browser of data revealed by the Etsy API")
                  (:li "4/5/9: Public")
                  (:li "4/5/9: Tag clouds on shops")))
                (:p
                 "The tiny chart in the footer shows the time taken by recent calls to Etsy via the API.  "
                 (multiple-value-bind (count min max avg)
                     (etsy::timer-stats etsy::*etsy-api-request-timer*)
                   (htm (fmt "There are currently ~D samples ranging from ~,4F to ~,4F seconds with an average of ~F seconds" count min max avg))))))))


(defun emit-view-of-object (view-name object)
  (with-html-out ()
    (:span 
     :class (format nil "object ~(~a~)" (type-of object))
     (:span :class "header" (str (type-of object)))
     (:span 
      :class "object-fields"
      (loop
       with api-class-description = (etsy:api-class-info (type-of object))
       for slot-description in (slot-value api-class-description 'etsy:slot-descriptions)
       do (emit-html-for-slot object slot-description view-name))))))

;;;; Define our view
                   
(defun define-view (view-name outline)
  (loop 
    for (class . slots) in outline
    as api-class-description = (etsy:api-class-info class)
    do
 (print api-class-description)
 (loop for (slot-name . how-to-view) in slots
       as slot-description = (etsy:slot-description api-class-description slot-name)
       do (setf (get-viewer-of-slot slot-description view-name) how-to-view))))

(define-view 'inspect '((etsy:user
                         (etsy:image-url-25x25 inline-image)
                         (etsy:image-url-30x30 inline-image)
                         (etsy:image-url-50x50 inline-image)
                         (etsy:image-url-75x75 inline-image)
                         (etsy:bio etsy-markup)
                         (etsy:join-epoch epoch)
                         (etsy:last-login-epoch epoch)
                         (etsy:favorite-creation-epoch epoch)
                         (etsy:url url))
                        (etsy:shop
                         (etsy:banner-image-url inline-image)
                         (etsy:last-updated-epoch epoch)
                         (etsy:creation-epoch epoch)
                         (etsy:sale-message etsy-markup)
                         (etsy:announcement etsy-markup)
                         (etsy:vacation-message etsy-markup)
                         (etsy:policy-welcome etsy-markup)
                         (etsy:policy-payment etsy-markup)
                         (etsy:policy-shipping etsy-markup)
                         (etsy:policy-refunds etsy-markup)
                         (etsy:policy-additional etsy-markup)
                         (etsy:sections sections))
                        (etsy:listing
                         (etsy:url url)
                         (etsy:rgb-color rgb-color)
                         (etsy:creation-epoch epoch)
                         (etsy:image-url-25x25 inline-image)
                         (etsy:image-url-50x50 inline-image)
                         (etsy:image-url-75x75 inline-image)
                         (etsy:image-url-155x125 inline-image)
                         (etsy:image-url-200x200 inline-image)
                         (etsy:image-url-430xn inline-image)
                         (etsy:ending-epoch epoch)
                         (etsy:user-name link-to shop)
                         (etsy:description etsy-markup)
                         (etsy:favorite-creation-epoch epoch))
                        (etsy:gift-guide
                         (etsy:Creation-Tsz-Epoch epoch)
                         (etsy:description etsy-markup))))

(defmethod emit-html-for-slot-view (object slot-description view-name (slot-view-name (eql 'inline-image))  slot-view-particulars)
  (with-slots (name) slot-description
    (let ((value (slot-value object name)))
      (when (string= "images/grey.gif" value)
        (setf value "http://www.etsy.com/images/grey.gif"))
      (with-field-html (name)
        (:a :href value :style "float: right"
            (:img :src value :alt (str name)))))))

(defmethod emit-html-for-slot-view (object slot-description view-name (slot-view-name (eql 'etsy-markup))  slot-view-particulars)
  (with-slots (name) slot-description
    (let ((value (slot-value object name)))
      (with-field-html (name)
        (:div :class "etsy-markup" (str (etsy-markup-to-html value)))))))

(defmethod emit-html-for-slot-view  (object slot-description view-name (slot-view-name (eql 'epoch))  slot-view-particulars)
  (with-slots (name) slot-description
    (let ((value (slot-value-or-default object name nil)))
      (with-field-html (name)
        (cond
          (value
           (htm (str (etsy-epoch->text value))))
          (t (htm "unknown")))))))

(defmethod emit-html-for-slot-view  (object slot-description view-name (slot-view-name (eql 'url))  slot-view-particulars)
  (with-slots (name) slot-description
    (let ((value (slot-value-or-default object name "unknown")))
    (with-field-html (name)
      (:a :href value (str value))))))

(defmethod emit-html-for-slot-view  (object slot-description view-name (slot-view-name (eql 'default))  slot-view-particulars)
  (with-slots (name) slot-description
    (with-field-html (name)
      (str (slot-value-or-default object name "none")))))

(defmethod emit-html-for-slot-view  (object slot-description view-name (slot-view-name (eql 'sections))  slot-view-particulars)
  (with-slots (name) slot-description
    (let ((sections (slot-value object name)))
      (with-field-html (name)
        (cond
          (sections 
           (htm (:span 
                 :class "sections"
                 (loop for section in sections
                       do
                    (with-slots (etsy:user-id) object
                      (with-slots (etsy:section-id etsy:title etsy:listing-count) section
                        (htm (:span 
                              :class "section"
                              (str etsy:listing-count)
                              " in \"" (:a :href
                                           (inspect-shop-section-link
                                            :user-name-or-id etsy:user-id
                                            :section-id etsy:section-id)
                                           (str etsy:title))
                              "\""))))))))
          (t
           (htm (:span :class "field-value" (str "none")))))))))

(defmethod emit-html-for-slot-view  (object slot-description view-name (slot-view-name (eql 'link-to))  slot-view-particulars)
  (with-slots (name) slot-description
    (let* ((type (car slot-view-particulars))
           (value (slot-value object name))
           (url (format nil "/inspect/~(~A~)?id=~A" type value)))
      (with-field-html (name)
        (:a :href (str url) (str value))))))

(defmethod emit-html-for-slot-view  (object slot-description view-name (slot-view-name (eql 'rgb-color))  slot-view-particulars)
  (with-slots (name) slot-description
    (let* ((value (slot-value object name)))
      (with-field-html (name)
        (fmt "~6,'0x" value)
        (str "&nbsp;&nbsp;&nbsp;")
        (:span 
         :class "swatch" 
         :style (format nil "background: #~6,'0x;" value)
         (str "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;"))))))

(defun emit-style ()
  (with-html-out ()
    (:script :type "text/javascript" :src "http://www.google.com/jsapi")
    (:script :type "text/javascript" "google.load(\"jquery\", \"1.3.2\")")
    (:script :type "text/javascript" :src "/js/jquery.sparkline.js")
    (:script :type "text/javascript" :src "/js/tooltip.js")
    #+nil (:script :type "text/javascript" :src "/js/jquery.qtip-1.0.0-rc3.min.js")
    (:script :type "text/javascript" "$(function() { $('.inlinesparkline').sparkline(); });")
    (:style :type "text/css"
 
            "body { width: 820px; } "
            "A { color: inherit; } "
            "A IMG { border:none; } "
            ".left-side { display:block; } "
            ".right-side { display:block; } "
            ".header { display:block; font-family: helvetica; }"
            ".caption { display:block; font-family: helvetica; text-align: center; font-size: 80%; } "

            "span .maintenance { display:block; background-color: gold; } "

            "body > .header { margin-bottom: 5px; border-bottom: thin solid black;} "
            "body > .header { font-size: 110%; padding-bottom: .3em } "
            ".content { display:block; } "


            ".footer { display:block; clear: both; margin-top: 5px; border-top: thin solid black; } "
            ".footer { font-family: helvetica; font-size: 8pt; padding-top: .3em; } "
            ".footer .left { float: left; display: block; text-align: left; } "
            ".footer .right { display: block;  text-align: right; } "

            ".object { display:block;  margin-top: 10px; border-top: thin dotted; }"
            ".object .header { font-size: 120%; } "
            ".object-fields { display:block; margin-left: 5px; border-left: thin dotted; padding-left: 2px;; }"
            ".field-name { font: helvetica; font-size: 80%; padding-top: 2px; border-top: 1; } "
            ".field-name::after { content: \": \"} "
            ".field { display: block;} "
            ".sections { display:block; margin-left: 10px;  } "
            ".swatch { border: thin dotted ; } "
            ".section { display:block; } "

            ".pager {}"
            ".pager-disabled {color: grey; } "
            ".current-page { font-weight: bold; font-weight: bolder; } "

            ".user-card {display:block; margin:2px; padding: 2px; border: thin dotted; width:505px} "
            ".user-summary img { padding-right: 5px} "
            ".user-card .etsy-markup { padding-top: 1 px; border: none ; border-top: thin dotted; margin-left: 2px; margin-right: 2px; } "

            ".shop-card { display:block; width: 800px; margin: 2px; border: thin dotted; padding: 2px;  } "
            ".shop-card .title { display:block; font-family:helvetica; font-size:150%; text-align:center; } "

            ".etsy-markup { display: block; margin-left: 5px; width: 500px; border-right: thin dotted; padding-right: 2px;  } "


            ".section .object .header { display: none; } "
            ".section .object .object-fields { border: thin dotted; margin-top: 3px; } "
            

            ".shop-card .etsy-markup { border-top: thin dotted; margin-top: 2px; text-size: 80% } "
            ".shop-card .body .sidebar { display: block; float: right; width: 280px; top-margin: 10px; } "
            ".shop-card .body .metadata { display: block; float: left; width: 280px} "
            ".shop-sections { display: block; margin: 4px; margin-left: 6px }"

            ".item-grid { display: block; width: 250px; padding-top: 5px; line-height: 12px; } " ;; 12 px?

            ".image-card { display: block; } "
            ".image-card .left { display: block; float: left } "
            ".image-card .right { display: block; } "


            ".listing-card { display: both; width: 900px; } "
            ".listing-card .header { display: block; text-align: center; font-size: 140%; } "
            ".listing-card .body { display:block; margin-top: 5px; } "
            ".listing-card .body .left-side {  float: left; width: 440px;} "
            ".listing-card .body .right-side { display: block;  padding-left: 450px; } "
            ".listing-card .body .right-side .etsy-markup { width: 420px; border-right: none }"

            ".listing-tiles {display: block; float: right; width: 30px; line-height:0} "
            ".listing-tiles IMG {border: none; } "

            ".listings {display:block; overflow-y: scroll; overflow-x:hidden; height: 500px; }"

            ".listings-widget { display: block; border-top: thin solid; border-bottom: thin solid; } "
            ".listings .small-listing-card { margin-top: 4px; margin-bottom: 4px; } "
            ".listings .small-listing-card:first { border-top:none; } "

            ".small-listing-card { display:block; width: 750px; height: 225px; } "
            ".small-listing-card { border-top: thin dotted; padding-top: 4px; } "
            ".small-listing-card .header { height: 25px; overflow: hidden; font-weight: bold; font-size: 90%; } "
            ".small-listing-card .body { display:block; height: 200px; margin-left: 10px; overflow: hidden; } "
            ".small-listing-card .body .left-side { float: left; } "
            ".small-listing-card .body .right-side { padding-left: 230px; } "


            ".gift-guide-toc { display:block; } "
            ".subcategory::after { content: \": \" } "
            ".subcategories { display:block; margin-left: 12px; border-left: thin solid; border-bottom: thin solid; padding-left: 6px; } "
            ".subsubcategories { font-size: 80%; }"
            ".subsubcategory::after { content: \", \"; } "
            ".subsubcategories:last-child:after {content \"!\";} "
            "form.inline {display: inline; }"
            ".outline-with-inline-forms {line-height: 170%; } "

            ".category { display:block; } "

            )))

;;;;;

(defmacro define-file-handler (name (&key url content-type) path)
  `(progn
     (let ((handler (hunchentoot:create-static-file-dispatcher-and-handler
                     ,url 
                     ,(merge-pathnames path (pathname-of-system "egleis"))
                     ,@(when content-type `(,content-type)))))
       (defun ,name (request)
         (funcall handler request)))
     (pushnew ',name hunchentoot:*dispatch-table*)))

(define-file-handler favicon  (:url "/favicon.ico" :content-type "image/x-icon")
  "static/egleis.ico")

(define-file-handler jquery-sparklines (:url "/js/jquery.sparkline.js" :content-type "application/x-javascript")
  "jquery/jquery.sparkline.min.js")

(define-file-handler jquery-tooltip (:url "/js/tooltip.js" :content-type "application/x-javascript")
  "jquery/tooltip.js")

(define-file-handler jquery-tooltip (:url "/js/jquery.qtip-1.0.0-rc3.min.js" :content-type "application/x-javascript")
  "jquery/jquery.qtip-1.0.0-rc3.min.js")

(define-file-handler egleis-image (:url "/static/egleis.png" :content-type "image/png")
  "static/egleis.png")

(define-file-handler egleis-icon-image (:url "/static/egleis-icon.png" :content-type "image/png")
  "static/egleis-icon.png")


(hunchentoot:define-easy-handler (test12 :uri "/test12") ()
  (setf (hunchentoot:reply-external-format*) :utf-8)
  (setf *last-page*
        (with-html-output-to-string (*the-html-stream* nil :prologue t :indent t)
          (:html
           (:head 
            (:title "TEST12")
            (emit-style))
           (:body
            (:span :class "header" (:a :href "/" "Etsy Inspector"))
            (:span :class "content"
                   "Inline Sparkline: " 
                   (emit-timer-sparkline etsy::*etsy-api-request-timer*))
            (:span :class "footer"
                   "The term 'Etsy' is a trademark of Etsy, Inc.  This application uses the Etsy API but is not endorsed or certified by Etsy, Inc."))))))

(defun emit-timer-sparkline (timer)
  (with-html-out ()
    (:span :class "inlinesparkline" 
           (let ((samples ()))
             (etsy::do-timer-samples (x timer)
               (push x samples))
             (fmt "~{~D~^,~}" (nreverse samples))))))

(defun hash-table->alist (hash-table)
  (loop
    for key being each hash-key of hash-table using (hash-value value)
    collect (cons key value)))

(defun collect-shop-tag-cloud (user-name-or-id)
  (loop 
    with tag-cloud = (make-hash-table :test #'equal)
    with max = 1000
    with most = 0
    with listings
    finally (return (values tag-cloud most))
    for offset from 0 by 50
    until (<= max offset)
    do (multiple-value-setq (listings max) 
         (etsy:get-shop-listings
          user-name-or-id :offset offset :limit 50 :detail-level :medium))
       (loop
         for listing in listings
         do (with-slots (etsy:tags) listing
              (loop for tag in etsy:tags
                    as x = (incf (gethash tag tag-cloud 0))
                    do
                 (setf most (max most x)))))))

(defun emit-tag-cloud (cloud most)
  (cond
    ((zerop most)
     (with-html-out () 
       (:span :class "tag-cloud" "No Tags")))
    (t
     (let* ((min 50)
            (max 170)
            (range (- max min))
            (step (round (/ range most))))
       (decf min step)
       (flet ((f (x) (+ min (* x step))))
         (with-html-out ()
           (:span :class "tag-cloud"
                  (loop 
                    with a = (hash-table->alist cloud)
                    with b = (sort a #'string< :key #'car)
                    with c = (sort b #'> :key #'cdr)
                    for ((tag . count) . remainder) on c by #'cdr
                    ; for last-font-style = nil then font-style
                    for font-style = (format nil "font-size: ~d%" (f count))
                    do (htm
                         (:span :style font-style 
                                (str (substitute #\space #\_ tag))
                                (when remainder
                                  (htm ", "))))))))))))

(defun emit-narcl-for-shop (user-name-or-id)
  (with-open-file (s "/tmp/narcl.txt" :direction :output :if-exists :rename-and-delete)
    (loop 
      with max = 1000
      with listings
      for offset from 0 by 50
      until (<= max offset)
      do (multiple-value-setq (listings max) 
           (etsy:get-shop-listings
            user-name-or-id :offset offset :limit 50 :detail-level :medium))
         (loop
           for listing in listings
           do (with-slots (etsy:tags) listing
                (when etsy:tags
                  (format s "~&~{~A~^,~}" etsy:tags)))))))
        
