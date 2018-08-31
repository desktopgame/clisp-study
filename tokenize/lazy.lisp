(defmacro lazy(&body body)
   (let ((forced (gensym))
         (value (gensym)))
      `(let ((,forced nil)
             (,value nil))
          (lambda ()
             (unless ,forced
                (setf ,value (progn ,@body))
                (setf ,forced t))
             ,value))))

(defun force(lazy-value)
   (funcall lazy-value))

(defmacro lazy-cons(a d)
   `(lazy (cons ,a ,d)))

(defun lazy-car(x)
   (car (force x)))

(defun lazy-cdr(x)
   (cdr (force x)))

(defun lazy-nil()
   (lazy nil))

(defun lazy-null(x)
   (not (force x)))

(defun make-lazy(lst)
   (lazy (when lst
            (cons (car lst) (make-lazy (cdr lst))))))

(defun take(n lst)
   (unless (or (zerop n) (lazy-null lst))
      (cons (lazy-car lst) (take (1- n) (lazy-cdr lst)))))

(defun take-all(lst)
   (unless (lazy-null lst)
      (cons (lazy-car lst) (take-all (lazy-cdr lst)))))

(defparameter *integers*
   (labels ((f (n)
              (lazy-cons n (f (1+ n)))))
      (f 1)))

#|
(princ (lazy-car *integers*))
(princ (lazy-car (lazy-cdr *integers*)))
(princ (lazy-car (lazy-cdr (lazy-cdr *integers*))))
(princ (take 10 *integers*))
|#