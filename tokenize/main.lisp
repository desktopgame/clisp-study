(load "io.lisp")
(load "ctype.lisp")
(load "lex.lisp")
(ql:quickload :cl-fad)

(defparameter *oridinal_readers* (define-readers
   #'read-identifier :ident
   #'read-digit :digit
))

(defun lex-one(input-file output-file)
   "input-file を解析して output-file へ出力します。"
   (time
      (with-open-file (file output-file :direction :output
                                       :if-exists :overwrite
                                       :if-does-not-exist :create)
      (let ((li (read-lex-all (coerce (read-all-text input-file) 'list) *oridinal_readers*)))
         (dolist (e li)
            (format file "~A ~A~%" (concatenate 'string (segment-chars e)) (symbol-name (segment-id e))))))))

(defun lex-test(input-dir output-dir)
   "input-dir を解析して output-dir へ出力します。"
   (mapcar (lambda (path)
              (let ((input-file path) (output-file  (concatenate 'string output-dir (subseq (namestring path) (length input-dir)))))
                 (lex-one input-file output-file))
              )
              (cl-fad:list-directory (pathname input-dir))))

;https://stackoverflow.com/questions/44682199/common-lisp-relative-path-to-absolute
(defun absolute-path(path-string)
   (uiop:unix-namestring
    (uiop:merge-pathnames*
     (uiop:parse-unix-namestring path-string))))

(defparameter *input-dir* (absolute-path "input"))
(defparameter *output-dir* (absolute-path "output"))
(lex-test *input-dir* *output-dir*)
