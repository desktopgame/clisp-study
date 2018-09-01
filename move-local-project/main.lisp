;new-local-projectで作成したプロジェクトを quicklisk/local-project へ移動します。
(ql:quickload :cl-fad)

(defparameter *ql-local-projects* "/Users/koya/.roswell/lisp/quicklisp/local-projects")
(defparameter *temp-projects* "/Users/koya/Work/VSCode/clisp-work/new-local-project")
(defparameter *src-project* nil)

(defun add-path(base str)
   (concatenate 'string (concatenate 'string base "/") str))

(defun list-directory-entry-list(src)
   (unless (stringp src)
      (return-from list-directory-string-list (princ "src is not string")))
   (mapcar (lambda (path)
              (subseq (namestring path) (length src)))
           (cl-fad:list-directory (pathname src))))

(defun list-directory-path-list(src entry-list)
   (mapcar (lambda (entry)
              (concatenate 'string src entry))
           entry-list))

(defun copy-dir(src dst)
   (let* ((src-entry-list (list-directory-entry-list src))
          (src-path-list (list-directory-path-list src src-entry-list)))
      (mapcar (lambda (e)
                 (let ((to (concatenate 'string dst (subseq e (length src))) ))
                    (if (cl-fad:directory-pathname-p (pathname e))
                       (progn
                          (format t "copy directory... ~A~%" to)
                          (ensure-directories-exist to)
                          (copy-dir e to))
                       (progn
                          (cl-fad:copy-file e to)
                          (format t "copy file~%    ~A~%    ~A~%;~%" e to))
                    )))
              src-path-list)))

(block top
   (let ((name (read)))
      (unless (stringp name)
         (return-from top (princ "input value must be string.")))
      (format t "src: ~A~%" (setf *src-project* (add-path *temp-projects* name)))
      (format t "dst: ~A~%" (setf *dst-project* (add-path *ql-local-projects* name)))
      (if (cl-fad:directory-exists-p *dst-project*)
         (return-from top (princ "already move."))
         (format t "make directory:~A~%" (ensure-directories-exist (concatenate 'string *dst-project* "/"))))
      (copy-dir *src-project* *dst-project*)
      (cl-fad:delete-directory-and-files (pathname *src-project*))
      ))
