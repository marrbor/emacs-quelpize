1.各フォントをインストール
  (右クリック→「インストール」または
   右クリック→プログラムから開く→Windowsフォントビューアーで開いて「インストール」)

2.お好みで MacTypeInstaller_2013_1231_0.exe もインストール

3.emacs の設定例
;;; Font
(set-face-attribute 'default nil
                    :family "Ricty Discord"
                    :height 140)
(set-fontset-font (frame-parameter nil 'font)
                  'japanese-jisx0208
                  (cons "Ricty Discord" "iso10646-1"))
(set-fontset-font (frame-parameter nil 'font)
                  'japanese-jisx0212
                  (cons "Ricty Discord" "iso10646-1"))
(set-fontset-font (frame-parameter nil 'font)
                  'katakana-jisx0201
                  (cons "Ricty Discord" "iso10646-1"))

4.参考文献
http://futurismo.biz/archives/1313
