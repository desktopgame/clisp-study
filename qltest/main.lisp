;個人用テスト
;quicklisp/local-projectにmylibが存在する必要がある
(ql:quickload :mylib :silent t)
(mylib:foo)