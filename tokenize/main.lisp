(load "io.lisp")
(load "ctype.lisp")
(load "lex.lisp")

(time
   (with-open-file (file "sample_lex.text" :direction :output
                                           :if-exists :overwrite
                                           :if-does-not-exist :create)
   (let ((li (read-lex-all (coerce (read-all-text "sample.text") 'list) *readers*)))
      (dolist (e (remove-if (lambda (e) (segment-undefined e)) li))
         (format file "~A~%" (concatenate 'string (segment-chars e)))))))
