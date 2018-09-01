# clisp-work
clisp練習用

# 環境
roswell/sbcl を使用します。
````
dansakaarinoMBP:~ koya$ ros config
setup.time=3744762711
sbcl-bin.version=1.4.4
default.lisp=sbcl-bin
````

# xxx-project
quicklisp/cl-projectを簡単に扱うためのスクリプト  
パスは環境に応じて修正する
## プロジェクトの作成
- 一時的に現在のディレクトリに `cl-project` の雛形を作成
- `quicklisp/local-project` へ 移動
- `quicklisp/system-index` を更新する
````
cd new-local-project
ros -l main.lisp
# プロジェクト名入力

cd move-local-project
ros -l main.lisp
# プロジェクト名を入力

cd update-index-project
ros -l main.lisp
````

## プロジェクトの削除
- `quicklisp/local-project/xxx` を削除
- `quicklisp/system-index` を更新する
````
cd delete-local-project
ros -l main.lisp
# プロジェクト名を入力

cd update-index-project
ros -l main.lisp
````

# 実行方法
vscode の `task` 機能によって実行できるようになってます。