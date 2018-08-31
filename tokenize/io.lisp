(defun each-lines-from-file (filename body)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          while line
          collect (funcall body line))))

(defun read-all-text(filename)
   (with-output-to-string (*standard-output*)
      (each-lines-from-file filename (lambda (line)
         (format t "~A~%" line)))))

(defun read-all-lines(filename)
   (let ((buf (list)))
      (each-lines-from-file filename (lambda (line)
         (nconc buf line)))))