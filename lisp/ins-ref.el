;;--------------------------------------------------------------------------
;; ins-ref.el (Version 1.02)
;;--------------------------------------------------------------------------
;; 説明:
;;   マークされた所からカーソルのある行までの間の行の先頭に文字列を
;;   いれる emacs-lisp プログラムです。メールや GUNS の引用記号を
;;   挿入したり、lisp や C++ のコメントアウトするのに使用できます。 
;;
;; 「.emacs」の設定:
;;    (setq load-path (cons ("プログラムの置場所") load-path))
;;    (load "ins-ref")
;; 
;; 使い方:
;;    [Ctrl]+[c][j] : マークをつけたところから現在のカーソル位置までの
;;                    行の先頭に引用記号(デフォルトは" | ")を挿入します。
;;    [Ctrl]+[c][i] : マークをつけたところから現在のカーソル位置までの
;;                    行の先頭に指定の文字列を挿入します。
;;    [Ctrl]+[c][d] : マークをつけたところから現在のカーソル位置までの
;;                    矩形領域を削除します。
;;    ※ [Ctrl]+[c][j] はコントロールキーを押しながら c を
;;       押したあとに、j のみを押すことを示しています。
;; 
;; 例:
;;   [挿入]
;;     +---------------+    「G」のところで           +---------------+ 
;;     |ABCDEF         |    [Ctrl]+[space] を         |ABCDEF         |
;;     |GHIJKL         |    押してマークをつける      |// GHIJKL      |
;;     |MNOPQR         | → カーソルを「S」に      → |// MNOPQR      |
;;     |STUVWX         |    移動し[Ctrl]+[c][i]を     |// STUVWX      |
;;     |YZ             |    押し「// 」を入力する。   |YZ             |
;;     +---------------+    GUIの環境では、「G」      +---------------+
;;                          から「S」までをドラッグ
;;                          したあとに[Ctrl]+[c][i]
;;                          でも可能です。
;;
;;   [削除]
;;     +---------------+    「G」の行の行頭で         +---------------+ 
;;     |ABCDEF         |    [Ctrl]+[space] を         |ABCDEF         |
;;     |// GHIJKL      |    押してマークをつける      |GHIJKL         |
;;     |// MNOPQR      | → カーソルを「S」に      → |MNOPQR         |
;;     |// STUVWX      |    移動し[Ctrl]+[c][d]を     |STUVWX         |
;;     |YZ             |    押す。                    |YZ             |
;;     +---------------+    GUIの環境では、「G」の    +---------------+
;;                          行頭の「/」から「S」
;;                          までをドラッグしたあとに
;;                          [Ctrl]+[c][d]でも可能です。
;;
;;--------------------------------------------------------------------------
;;                             Copyright (C) 1994-2001 TSURUTA Mitsutoshi
;;--------------------------------------------------------------------------

;;  処理の本体
(defun ins-ref-str (ref-str)
  "行の頭に、引用記号をいれる。"
  (interactive "sinsert string : ")
  (if (> (mark) (point)) (exchange-point-and-mark))
  (save-excursion
    (goto-char (mark))
    (beginning-of-line)
    (setq top_pos (point)))
  (beginning-of-line)
  (while (< top_pos (point))
    (insert ref-str)
    (previous-line 1)
    (beginning-of-line))
  (insert ref-str)
)

;;  何の文字列を入れるかを問い合わせないで実行する。
;;        (ins-ref-str ....) の 文字列部分を書き換えると
;;        その文字列を挿入するようになります。
(defun ins-ref ()
  "何の文字列を入れるかを問い合わせないで行の頭に、引用記号をいれる。"
  (interactive)
  (ins-ref-str "  | ")
)

;; 矩形削除
(defun del-ref ()
  "矩形領域を削除する。"
  (interactive)
  (kill-rectangle (mark) (point))
)

;;  キーの割り当て
;;(global-set-key "\C-cj" 'ins-ref)
(global-set-key "\C-ci" 'ins-ref-str)
(global-set-key "\C-cd" 'del-ref)


;;----------------------------------------------------------------------
;; 以下は、gnus 用です。必要ない場合は削除しても大丈夫です。
;;----------------------------------------------------------------------
;;  ニュースのフォローをする時の参照記事の前に付け加える
;
;  news-reply-yank-message-id : ニュースのメッセージID
;  news-reply-yank-from : そのニュースを投稿した人
;
(defvar news-reply-header-hook
  '(lambda () (insert news-reply-yank-from " さん wrote : \n\n"))
  "ニュースのフォローをする時の参照記事の前に付け加える")
;(defvar news-reply-header-hook
;  '(lambda () (insert (mail-fetch-field "date") " 頃に\n"
;                      news-reply-yank-from " さんは書きました \n\n"))
;  "ニュースのフォローをする時の参照記事の前に付け加える")

;;  ニュースやsendmailのフォローをする時の参照記事の
;;  前に入るスペースの数の設定（デフォルトは３でした）
(defvar mail-indentation-spaces 0 
    "フォローをする時の参照記事の前に入るスペースの数")

;;  rnewspost を 使っている人ようの設定です
;;
(setq news-reply-mode-hook
      '(lambda ()
         (defun news-reply-yank-original (arg)
           "ニュースのフォローをする時の参照記事の挿入"
           (interactive "P")
           (mail-yank-original mail-indentation-spaces)
           (exchange-point-and-mark)
           (run-hooks 'news-reply-header-hook))))
