(load "io.lisp")
(load "ctype.lisp")

(defun not-eq(a b)
   (not (eq a b)))

(defstruct segment (chars (list)) (next nil) (undefined nil))

(defun debug-print(a)
   (princ ": ")
   (princ a)
   (format t "~%"))

(defun read-while(source cond)
   (let ((c (car source)) (tail (cdr source)) (buf '()) (len 0))
      (loop while (and (not-eq c nil) (funcall cond c)) do
         (setf e c)
         (setf c (car tail))
         (setf tail (cdr tail))
         (incf len)
         (push e buf))  (make-segment :chars (nreverse buf)
                                   :next (make-segment :chars (push c tail))) ))

(defun read-identifier(source)
   (read-while source #'isident))

(defun read-digits(source)
   (read-while source #'isdigit))

(defmacro defun-read-once(name a)
    `(defun ,name(source)
        (if (char= ,a (car source))
            (make-segment :chars (list (car source))
                       :next (make-segment :chars (cdr source)))
            (make-segment :chars nil
                       :next (make-segment :chars source)))))

(defmacro defun-read-fusion(name a b)
   `(defun ,name(source)
       (let* ((al (funcall ,a source)) (ala (segment-chars al)) (ald (segment-next al)))
          (if ala
             (let* ((bl (funcall ,b  (segment-chars ald))) (bla (segment-chars bl)) (bld (segment-next bl)))
                (make-segment :chars (append ala bla)
                           :next (make-segment :chars bld))
             )
             (make-segment :chars nil
                        :next (make-segment :chars ald)) ))))

(defun test-lex(source body)
   (let ((li (funcall body (coerce source 'list)) ))
      (princ "! ")
      (princ li)
      (format t "~%")))

;readerの一覧を定義する
(setf *readers* (list))
(defun reader-register(reader)
   (push reader *readers*))

;デフォルトの readerを定義する
(defun-read-once read-left-paren *left-paren*)
(defun-read-once read-right-paren *right-paren*)
(defun-read-once read-left-brace *left-brace*)
(defun-read-once read-right-brace *right-brace*)
(defun-read-once read-plus *plus*)
(defun-read-once read-minus *minus*)
(defun-read-once read-multiply *multiply*)
(defun-read-once read-divide *divide*)
(defun-read-once read-mod *mod*)
(defun-read-once read-bit-or *bit-or*)
(defun-read-once read-bit-and *bit-and*)
(defun-read-once read-colon *colon*)
(defun-read-once read-semi-colon *semi-colon*)
(defun-read-once read-dot *dot*)
(defun-read-fusion read-ident+digit #'read-identifier #'read-digits)

(reader-register #'read-identifier)
(reader-register #'read-digits)
(reader-register #'read-left-paren)
(reader-register #'read-right-paren)
(reader-register #'read-left-brace)
(reader-register #'read-right-brace)

(reader-register #'read-plus)
(reader-register #'read-minus)
(reader-register #'read-multiply)
(reader-register #'read-divide)
(reader-register #'read-mod)
(reader-register #'read-bit-or)
(reader-register #'read-bit-and)
(reader-register #'read-colon)
(reader-register #'read-semi-colon)
(reader-register #'read-dot)

;
;
(defun read-lex-one(source readers)
   (if readers
      (let ((ca (funcall (car readers) source)))
         (if (segment-chars ca)
             ca
             (read-lex-one source (cdr readers))))
      (make-segment)))

(defun read-lex-all(source readers)
   (let ((buf (list)))
      (loop while source do
         (let ((ca (read-lex-one source readers)))
            (if (not-eq (segment-chars ca) nil)
                (progn
                   (push ca buf)
                   (setf source (segment-chars (segment-next ca))))
                (progn
                   (let ((segment (make-segment)))
                      (setf (segment-undefined segment) t)
                      (push (car source) (segment-chars segment))
                      (push segment buf)
                      (setf source (cdr source))
                      ))
            )))
    (nreverse buf)))

(time
   (with-open-file (file "sample_lex.text" :direction :output
                                           :if-exists :overwrite
                                           :if-does-not-exist :create)
   (let ((li (read-lex-all (coerce (read-all-text "sample.text") 'list) *readers*)))
      (dolist (e (remove-if (lambda (e) (segment-undefined e)) li))
         (format file "~A~%" (concatenate 'string (segment-chars e)))))))
