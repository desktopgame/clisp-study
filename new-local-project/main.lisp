;https://qiita.com/kedama17/items/528d98b3f858728c1054
;上述の記事を参考に自動でそれを実行するスクリプトを書きました
;

(ql:quickload :cl-project)
(ql:quickload :cl-fad)

;https://stackoverflow.com/questions/44682199/common-lisp-relative-path-to-absolute
(defun absolute-path(path-string)
   (uiop:unix-namestring
    (uiop:merge-pathnames*
     (uiop:parse-unix-namestring path-string))))

(block top
   (let ((name (read)))
      (unless (stringp name)
         (return-from top (princ "input value must be string.")))
      (if (cl-fad:directory-exists-p name)
         (princ "already created project. ")
         (progn
            (cl-project:make-project (pathname name))))))