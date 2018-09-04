
(defun scan-word(source word)
   (if word
       (if (char= (car source) (car word))
           (scan-word (cdr source) (cdr word))
           source)
        source))

(princ (scan-word (coerce "publicdomain" 'list) (coerce "public" 'list)))
(format t "~%")
(princ (scan-word (coerce "publicdomain" 'list) (coerce "private" 'list)))