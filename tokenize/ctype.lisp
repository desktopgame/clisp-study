(defparameter *left-paren* #\()
(defparameter *right-paren* #\))
(defparameter *left-brace* #\{)
(defparameter *right-brace* #\})
(defparameter *plus* #\+)
(defparameter *minus* #\-)
(defparameter *multiply* #\*)
(defparameter *divide* #\/)
(defparameter *mod* #\%)
(defparameter *bit-or* #\|)
(defparameter *bit-and* #\&)
(defparameter *colon* #\:)
(defparameter *semi-colon* #\;)
(defparameter *dot* #\.)

(defun isspace (char) (case char ((#\Space #\Return #\Newline #\Tab) t)))

(defun isident(char) (or (isalpha char) (char= #\_ char)))

(defun isalpha (char) (or (char<= #\A char #\Z) (char<= #\a char #\z)))

(defun isdigit (char) (char<= #\0 char #\9))

(defun isalnum (char) (or (isalpha char) (isdigit char)))

(defun isdouble(char) (or (isdigit char) (char= #\. char)))
