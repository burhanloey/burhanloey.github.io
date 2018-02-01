;;
;; HTML generator for burhanloey.com homepage.
;;
;; Add more project in *projects*, and then run:
;;
;;   sbcl --load generator.lisp
;;

(ql:quickload :spinneret)

(in-package #:spinneret)

(defparameter *projects* '((:title "spacebar-workout"
                            :image "images/spacebar.png"
                            :description "Assistive website for my workout routine."
                            :source "https://github.com/burhanloey/spacebar-workout"
                            :cta "Live"
                            :cta-url "http://www.burhanloey.com/spacebar-workout")
                           (:title "rumahsewa141"
                            :image "images/rumahsewa.png"
                            :description "Rent and bills management website."
                            :source "https://github.com/burhanloey/rumahsewa141"
                            :cta "Demo"
                            :cta-url "http://rumahsewa141.burhanloey.com")
                           (:title "MyTimeTable"
                            :image "images/timetable.png"
                            :description "Simplified version of my undergraduate timetable."
                            :source "https://github.com/burhanloey/MyTimetable"
                            :cta "Demo"
                            :cta-url "http://my-timetable-demo.appspot.com")
                           (:title "cljs-simple-cache-buster"
                            :image "images/cache_buster.png"
                            :description "Automatically bust cache on compilation."
                            :source "https://github.com/burhanloey/cljs-simple-cache-buster"
                            :cta "Get it"
                            :cta-url "https://clojars.org/cljs-simple-cache-buster")))

(defmacro with-page (&body body)
  `(with-html-string
     (:doctype)
     (:html
      (:head
       (:meta :name "description" :content "burhanloey - JVM enthusiast, functional programming fanboy, and computer vision hobbyist")
       (:meta :name "author" :content "burhanloey")
       (:meta :name "keywords" :content "burhanloey")
       (:link :rel "stylesheet" :href "https://cdnjs.cloudflare.com/ajax/libs/twitter-bootstrap/3.3.7/css/bootstrap.min.css")
       (:link :rel "stylesheet" :href "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/4.7.0/css/font-awesome.min.css")
       (:link :rel "stylesheet" :href "css/style.css")
       (:link :rel "shortcut icon" :href "favicon.ico")
       (:title "burhanloey"))
      (:body.container
       (:div :class "row content"
             (:div :class "col-md-8 col-md-offset-2"
                   ,@body))
       (:footer.footer)))))

(defun social-media ()
  (with-html
    (:p (:img :src "images/face.png" :alt "portrait photo of me" :class "img-circle img-responsive center-block" :height 200 :width 200))
    (:h2 :class "lead text-center" "Burhanuddin bin Baharuddin")
    (:p :class "text-center"
        (:span :class "glyphicon glyphicon-envelope" :aria-hidden t)
        " burhanclj@gmail.com")
    (:p.text-center
     (:a :class "btn" :href "https://github.com/burhanloey"  (:i :class "fa fa-github" :aria-hidden t)  " GitHub") " "
     (:a :class "btn" :href "https://twitter.com/burhanloey" (:i :class "fa fa-twitter" :aria-hidden t) " Twitter") " "
     (:a :class "btn" :href "https://www.linkedin.com/in/burhanuddin-baharuddin-ba892314a/" (:i :class "fa fa-linkedin" :aria-hidden t) " LinkedIn") ""
     (:a :class "btn" :href "blog"                          (:i :class "fa fa-rss" :aria-hidden t)     " Blog"))))

(defun languages-of-choice ()
  (with-html
    (:p.text-center
     "Languages of choice"
     (:br)
     (:span.lead (:strong "Clojure, Common Lisp, Java 8")))))

(defun project-card (data)
  (destructuring-bind (&key title image description source cta cta-url) data
    (with-html
      (:div.col-md-4
       (:div.thumbnail
        (:img :src image :alt (concatenate 'string title " image"))
        (:div.caption
         (:h3 title)
         (:p description)
         (:p
          (:a :href source :class "btn btn-primary" :role "button" "View code") " "
          (:a :href cta-url :class "btn btn-default" :role "button" cta))))))))

(defun projects-gallery ()
  (loop for (x y z) on *projects* by #'cdddr
        do (with-html
             (:div.row
              (dolist (data (remove nil (list x y z)))
                (project-card data))))))

(defun generate ()
  (with-page
    (:br)
    (social-media)
    (:hr)
    (languages-of-choice)
    (:hr)
    (projects-gallery)))

(defun output-to-file ()
  (with-open-file (output-file "index.html"
                               :direction :output
                               :if-exists :supersede)
    (format output-file (generate))))

(output-to-file)

(format t "Done generating index.html.~%")

(sb-ext:quit)
