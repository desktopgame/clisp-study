(ql:quickload :cl-fad)

(defparameter *ql-local-projects* "/Users/koya/.roswell/lisp/quicklisp/local-projects/")
(defparameter *target* nil)
(block top
   (let* ((name (read)))
      (unless (stringp name)
         (return-from top (princ "input value must be string."))
      )
      (setf *target* (concatenate 'string *ql-local-projects* name))
      (unless (cl-fad:directory-exists-p *target*)
         (return-from top (princ "already deleted.")))
       (cl-fad:delete-directory-and-files (pathname *target*)))
    )