(ql:quickload :spinneret)

(load "generator.lisp")

(in-package :cl-user)

(defun watch ()
  (loop
    (let ((generator-mtime (file-write-date "generator.lisp"))
          (index-mtime     (file-write-date "index.html")))
      (when (> generator-mtime index-mtime)
        (format t "Generating index.html~%")
        (load "generator.lisp")
        (generator:output-to-file)))
    (sleep 1)))
