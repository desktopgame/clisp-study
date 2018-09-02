
(defun read-word(source word)
   (if word
       (if (char= (car source) (car word))
           (read-word (cdr source) (cdr word))
           source)
        source))

(princ (read-word (coerce "publicdomain" 'list) (coerce "public" 'list)))
(format t "~%")
(princ (read-word (coerce "publicdomain" 'list) (coerce "private" 'list)))