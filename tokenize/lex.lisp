(defun not-eq(a b)
   (not (eq a b)))

(defstruct segment (chars (list)) (next nil) (id nil) (undefined nil))

(defun debug-print(a)
   (princ ": ")
   (princ a)
   (format t "~%"))


(defun read-while(source cond)
   "cond に source の先頭から一文字ずつ文字を与えて、
    t を返す限り文字列をバッファします。
    t 以外の値が返されると、その時点のバッファと source から segment を作成して返します。"
   (let ((c (car source)) (tail (cdr source)) (buf '()) (len 0) (e nil))
      (loop while (and (not-eq c nil) (funcall cond c)) do
         (setf e c)
         (setf c (car tail))
         (setf tail (cdr tail))
         (incf len)
         (push e buf))  (make-segment :chars (nreverse buf)
                                   :next (make-segment :chars (push c tail))) ))

(defun read-identifier(source)
   "read-while の カバー関数です。
    文字が アルファベット で有る限りバッファします。"
   (read-while source #'isident))

(defun read-digit(source)
   "read-while の カバー関数です。
    文字が 数字 で有る限りバッファします。"
   (read-while source #'isdigit))

(defmacro defun-read-once(name a)
   "`一文字だけ読み取って、それが a と同じなら segment を返す`
    関数を定義するマクロです。"
    `(defun ,name(source)
        (if (char= ,a (car source))
            (make-segment :chars (list (car source))
                       :next (make-segment :chars (cdr source)))
            (make-segment :chars nil
                       :next (make-segment :chars source)))))

(defmacro defun-read-fusion(name a b)
   "`a の戻り値 seg-a を入力として b を呼び出し、
    両方の chars を連結した新しい segment を返す` 関数を定義するマクロです。"
   `(defun ,name(source)
       (let* ((al (funcall ,a source)) (ala (segment-chars al)) (ald (segment-next al)))
          (if ala
             (let* ((bl (funcall ,b  (segment-chars ald))) (bla (segment-chars bl)) (bld (segment-next bl)))
                (make-segment :chars (append ala bla)
                           :next (make-segment :chars bld))
             )
             (make-segment :chars nil
                        :next (make-segment :chars ald)) ))))


(defun read-word(source word)
   "source から一文字ずつ読み取って、 word と前方一致する限りそれを続行します。
    前方一致が途絶えた、あるいは完全に一致することが確認された場合には
    その時点での残りの文字列を返します。"
   (if word
       (if (char= (car source) (car word))
           (read-word (cdr source) (cdr word))
           source)
        source))

(defmacro defun-read-word(name word)
   "`word という文字列との前方一致によって segment を返す` 関数を定義するマクロです。"
   `(defun ,name(source)
       (let ((tail (read-word source ,word)))
          (if (= (+ (length ,word) (length tail)) (length source))
              (make-segment :chars ,word
                            :next (make-segment :chars tail))
              (make-segment :chars nil
                            :next (make-segment :chars source))
          ))))

(defmacro defun-read-string(name word)
    "引数を list へ変換して、defun-read-word へ渡すマクロです。"
   `(defun-read-word ,name (coerce ,word 'list)))

(defun wrap-reader(proxy id)
   "`proxy の戻り値の segment の segment-id を id に変更して返す`
     ラムダを返します。"
   (format t "~a ~a~%" (type-of proxy) (type-of id))
   (lambda (source)
      (let ((seg (funcall proxy source)))
         (setf (segment-id seg) id) seg)))

(defun define-reader(head tailing)
   (let ((id (car tailing)))
      (values (wrap-reader head id) (cdr tailing))))

(defun define-readers-by(values)
   (let ((ret (list)))
      (loop while values do
         (multiple-value-bind (reader tailing) (define-reader (car values) (cdr values))
            (push reader ret)
            (setf values tailing)
         )) ret))

(defmacro define-readers(&body elements)
   `(define-readers-by (list ,@elements)))

;readerの一覧を定義する
(defparameter *readers* (list))
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
(defun-read-fusion read-ident+digit #'read-identifier #'read-digit)
(defun-read-string read-public "public:")

(reader-register #'read-identifier)
(reader-register #'read-public)
(reader-register #'read-digit)
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

(defun read-lex-one(source readers)
   "source に対して 一つづつ readers を適用して、
    有効な segment が返された時点でそれを戻り値とします。"
   (if readers
      (let ((ca (funcall (car readers) source)))
         (if (segment-chars ca)
             ca
             (read-lex-one source (cdr readers))))
      (make-segment :id :none)))

(defun read-lex-all(source readers)
   "source が空になるまで readers によって解析します。
    解析された segment のリストが戻り値となります。"
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