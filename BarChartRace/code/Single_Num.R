
# シングルリリース数の推移を可視化 --------------------------------------------------

# 利用パッケージ
library(tidyverse)
library(lubridate)
library(gganimate)

# チェック用
library(magrittr)
library(ggplot2)


# データの読込 -----------------------------------------------------------------

# フォルダパスを指定
dir_path <- "BarChartRace/data/HP_DB-main/"

# シングル一覧を読み込み
single_df <- readr::read_csv(
  file = paste0(dir_path, "single.csv"), 
  col_types = readr::cols(
    singleID = "i", 
    singleName = "c", 
    releaseDate = readr::col_date(format = "%Y/%m/%d"), 
    singleCategory = "c", 
    artistName = "c"
  )
) %>% 
  dplyr::arrange(releaseDate, singleID) # 昇順に並び替え
single_df


# 期間の設定 -------------------------------------------------------------------

# 期間を指定
date_from <- "1997-10-01"
date_from <- "2010-01-01"
date_to   <- "2022-04-15"
date_to   <- lubridate::today() %>% 
  as.character()

# 値をまとめる
date_vals <- c(date_from, date_to) %>% 
  lubridate::as_date() %>% # Date型に変換
  lubridate::floor_date(unit = "mon") # 月単位に切り捨て
date_vals


# シングルカテゴリの設定 -------------------------------------------------------------

# シングルの種類を確認
unique(single_df[["singleCategory"]])

# シングルの種類を指定
single_category <- unique(single_df[["singleCategory"]])
single_category <- c("インディーズシングル", "シングル")


# 連名作品の編集 -----------------------------------------------------------------

# アーティスト名を確認
unique(single_df[["artistName"]])

# 連名作品の名義を指定
pattern_vec <- c(
  "7AIR/SALT5/11WATER", "セクシーオトナジャン/エレジーズ/プリプリピンク", 
  "ガーディアンズ4/しゅごキャラエッグ!", "Berryz工房×℃-ute", 
  "ピーベリー/ハーベスト", "DIY♡／GREEN FIELDS", 
  "ハロプロ研修生 feat. Juice=Juice", 
  "ダイヤレディー／メロウクアッド／HI-FIN", "さとのあかり／トリプレット／ODATOMO", 
  "カントリー・ガールズ/つばきファクトリー", "つばきファクトリー/ハロプロ研修生", "こぶしファクトリー＆つばきファクトリー"
)

# 分割後のアーティスト名を指定
replace_vec <- c(
  "7AIR", "SALT5", "11WATER", "セクシーオトナジャン", "エレジーズ", "プリプリピンク", 
  "ガーディアンズ4", "しゅごキャラエッグ!", "ガーディアンズ4", "しゅごキャラエッグ!", "Berryz工房", "℃-ute", "Berryz工房", "℃-ute", 
  "ピーベリー", "ハーベスト", "DIY♡", "GREEN FIELDS", 
  "ハロプロ研修生", "Juice=Juice", 
  "ダイヤレディー", "メロウクアッド", "HI-FIN", "さとのあかり", "トリプレット", "ODATOMO", 
  "カントリー・ガールズ", "つばきファクトリー", "つばきファクトリー", "ハロプロ研修生", "こぶしファクトリー", "つばきファクトリー"
)

# 分割する数を指定
n_vec <- c(
  3, 3, 
  2, 2, 2, 2, 
  2, 2, 
  2, 
  3, 3, 
  2, 2, 2
)

# 連名作品を分割
split_df <- single_df %>% 
  dplyr::filter(artistName %in% pattern_vec) %>% # 連名の作品を抽出
  tibble::add_column(n = n_vec) %>% # 複製数を追加
  tidyr::uncount(n) %>% # 作品を複製
  dplyr::mutate(artistName = replace_vec) # 個々のアーティスト名を再設定
split_df


# リリース数の集計 ----------------------------------------------------------------

# アーティスト名を編集してリリース数を集計
release_n_df <- single_df %>% 
  dplyr::filter(!(artistName %in% pattern_vec)) %>% # 連名作品を削除
  dplyr::bind_rows(split_df) %>% # 分割した連名作品を追加
  dplyr::filter(singleCategory %in% single_category) %>% # 指定したカテゴリを抽出
  dplyr::filter(releaseDate >= date_vals[1], releaseDate <= date_vals[2]) %>% # 指定した期間内の作品を抽出
  dplyr::arrange(releaseDate, singleID) %>% # IDの割り当て用に昇順に並び替え
  dplyr::mutate(
    release_date = lubridate::floor_date(releaseDate, unit = "mon"), # 月単位に切り捨て
    artist_name = artistName %>% # グラフ表示名を追加
      #stringr::str_replace(pattern = "℃-ute", replacement = "C-ute") %>% # 作図時に豆腐化するので代用
      stringr::str_replace(pattern = "中澤ゆうこ&高山厳", replacement = "中澤ゆうこ") %>% # 不要な文字列を削除
      stringr::str_replace(pattern = "カントリー娘。に石川梨華（モーニング娘。）", replacement = "カントリー娘。に石川梨華") %>% # グループ名を削除
      stringr::str_replace(pattern = "カントリー娘。に紺野と藤本（モーニング娘。）", replacement = "カントリー娘。に紺野と藤本") %>% # グループ名を削除
      stringr::str_replace(pattern = "ミニハムず/プリンちゃん", replacement = "ミニハムず") %>% # 不要な文字列を削除
      stringr::str_replace(pattern = "バカ殿様とミニモニ姫。", replacement = "ミニモニ。") %>% # 企画名義を元の名前に変更
      stringr::str_replace(pattern = "ミニモニ。と高橋愛＋4KIDS", replacement = "ミニモニ。") %>% # ゲスト名を削除
      stringr::str_replace(pattern = "おけいさんと安倍なつみ", replacement = "安倍なつみ") %>% # 不要な文字列を削除
      stringr::str_replace(pattern = "あややム with エコハムず", replacement = "松浦亜弥") %>% # 企画名義を元の名前に変更
      stringr::str_replace(pattern = "DEF.DIVAと楽天イーグルス応援隊", replacement = "DEF.DIVA") %>% # 不要な文字列を削除
      stringr::str_replace(pattern = "月島きらり.*", replacement = "月島きらり(久住小春)") %>% # 長いので省略
      stringr::str_replace(pattern = "THE ポッシボー.*", replacement = "THE ポッシボー") %>% # メンバー名を削除
      stringr::str_replace(pattern = "里田まい with 藤岡藤巻", replacement = "里田まい") %>% # 不要な文字列を削除
      stringr::str_replace(pattern = "ジンギスカン×Berryz工房", replacement = "Berryz工房") %>% # 不要な文字列を削除
      stringr::str_replace(pattern = "矢口真里/エアバンド", replacement = "矢口真里") %>% # 不要な文字列を削除
      stringr::str_replace(pattern = "北神未海（CV 小川真奈）with MM学園 合唱部／氷室衣舞（声：菅谷梨沙子/Berryz工房）", replacement = "氷室衣舞(菅谷梨沙子)") %>% # 不要な文字列を削除
      stringr::str_replace(pattern = "むてん娘。", replacement = "モーニング娘。") %>% # 企画名義を元の名前に変更
      stringr::str_replace(pattern = "おはガールメープル with スマイレージ", replacement = "スマイレージ") %>% # 不要な文字列を削除
      stringr::str_replace(pattern = "NEXT YOU/Juice=Juice", replacement = "Juice=Juice") %>% # 企画名義を元の名前に変更
      stringr::str_remove(pattern = "\\(モーニング娘。'17\\)") %>% # グループ名を削除
      stringr::str_remove(pattern = "\\(アンジュルム\\)") %>% # グループ名を削除
      stringr::str_remove(pattern = "\\(Juice=Juice\\)") %>% # グループ名を削除
      stringr::str_remove(pattern = "\\(こぶしファクトリー\\)") %>% # グループ名を削除
      stringr::str_remove(pattern = "\\(つばきファクトリー\\)") %>% # グループ名を削除
      stringr::str_remove(pattern = "\\(ハロプロ研修生\\)") %>% # グループ名を削除
      stringr::str_remove(pattern = "\\(アンジュルム/カントリー・ガールズ\\)") %>% # グループ名を削除
      stringr::str_remove(pattern = "\\(LoVendoЯ\\)"), # グループ名を削除
    artist_idname = artist_name %>% # IDの割り当て用に編集
      stringr::str_replace(pattern = "^モーニング娘。.*", replacement = "モーニング娘。") %>% # ナンバリングを削除
      stringr::str_replace(pattern = "中澤ゆうこ", replacement = "中澤裕子") %>% # ソロ用名義を名前に統一
      stringr::str_replace(pattern = "T&Cボンバー", replacement = "太陽とシスコムーン") %>% # 改名前に統一
      stringr::str_replace(pattern = "カントリー娘。.*", replacement = "カントリー娘。") %>% # ゲスト名を削除
      stringr::str_replace(pattern = "ミニハムず", replacement = "ミニモニ。") %>% # 企画名義を元の名前に変更
      stringr::str_replace(pattern = "S/mileage", replacement = "スマイレージ") %>%  # 改名後に統一
      stringr::str_replace(pattern = "アンジュルム", replacement = "スマイレージ") %>% # 改名前に統一
      stringr::str_replace(pattern = "ハロプロ研修生北海道 feat.稲場愛香", replacement = "ハロプロ研修生北海道"), # ゲスト名を削除
    artist_idname = factor(artist_idname, levels = unique(artist_idname)), # レベル設定のため因子型に変換
    artist_id = dplyr::dense_rank(artist_idname) # アーティストIDを追加
  ) %>% # アーティスト名を編集
  dplyr::group_by(release_date, artist_id) %>% # カウント用にグループ化
  dplyr::mutate(release_n = dplyr::row_number()) %>% # リリース数をカウント:(count()で処理できるならしたい)
  dplyr::filter(release_n == max(release_n)) %>% # 同じ月に複数リリースしていると重複するので遅い方を採用
  dplyr::group_by(artist_id) %>% # 累積和の計算用にグループ化
  dplyr::mutate(release_n = cumsum(release_n)) %>% # リリース数の累積和を計算
  dplyr::arrange(release_date, artist_id) %>% # 複製数の追加用に並べ替え
  dplyr::group_by(artist_id) %>% # 複製数の追加用にグループ化
  dplyr::mutate(
    next_release_date = release_date %>% 
      dplyr::lead(n = 1) %>% # 1行前に値をズラす
      tidyr::replace_na(
        replace = lubridate::today() %>% 
          lubridate::rollforward(roll_to_first = TRUE)
      ), # 最後の行を現在の翌月にする
    n = lubridate::interval(start = release_date, end = next_release_date) %>% 
      lubridate::time_length(unit = "mon") # リリース数がない期間の月数を追加
  ) %>% 
  tidyr::uncount(n) %>% # 月数に応じて行を複製
  dplyr::group_by(release_date, artist_id) %>% # 行番号用にグループ化
  dplyr::mutate(idx = dplyr::row_number()) %>% # 行番号を割り当て
  dplyr::group_by(release_date, artist_id, idx) %>% # 1か月刻みの値の作成用にグループ化
  dplyr::mutate(date = seq(from = release_date, to = next_release_date, by = "mon")[idx]) %>% # 複製した行を1か月刻みの値に変更
  dplyr::ungroup() %>% # グループ化を解除
  dplyr::select(date, artist_id, artist_name, release_n) %>% # 利用する列を選択
  dplyr::arrange(date, artist_id) # 昇順に並べ替え
release_n_df

# アーティスト名を確認
unique(release_n_df[["artist_name"]])


# 演出と順位付け -------------------------------------------------------------------

## AlbumNum.Rを参照


# アニメーションの作成 --------------------------------------------------------------

### ・フレーム数の設定 -----

# 遷移フレーム数を指定
t <- 8

# 一時停止フレーム数を指定
s <- 2

# 1秒間に表示する月数を指定:(値が大きいと意図した通りにならない)
mps <- 3

# フレーム数を取得
n <- length(unique(rank_df[["date"]]))
n


### ・バーチャートレース -----

# バーチャートレースを作成:(y軸可変)
anim <- ggplot(rank_df, aes(x = ranking, y = release_n, fill = artist_id, color = artist_id)) + 
  geom_bar(stat = "identity", width = 0.9, alpha = 0.8) + # リリース数のバー
  geom_text(aes(y = 0, label = paste(artist_name, " ")), hjust = 1) + # アーティスト名のラベル
  geom_text(aes(label = paste(" ", round(release_n, 0), "枚")), hjust = 0) + # リリース数のラベル
  gganimate::transition_states(states = date, transition_length = t, state_length = s, wrap = FALSE) + # フレーム
  gganimate::ease_aes("cubic-in-out") + # アニメーションの緩急
  theme(
    axis.title.x = element_blank(), # x軸のラベル
    axis.title.y = element_blank(), # y軸のラベル
    axis.text.x = element_blank(), # x軸の目盛ラベル
    axis.text.y = element_blank(), # y軸の目盛ラベル
    axis.ticks.x = element_blank(), # x軸の目盛指示線
    axis.ticks.y = element_blank(), # y軸の目盛指示線
    #panel.grid.major.x = element_line(color = "grey", size = 0.1), # x軸の主目盛線
    panel.grid.major.y = element_blank(), # y軸の主目盛線
    #panel.grid.minor.x = element_blank(), # x軸の補助目盛線
    panel.grid.minor.y = element_blank(), # y軸の補助目盛線
    panel.border = element_blank(), # グラフ領域の枠線
    #panel.background = element_blank(), # グラフ領域の背景
    plot.title = element_text(color = "black", face = "bold", size = 20, hjust = 0.5), # 全体のタイトル
    plot.subtitle = element_text(color = "black", size = 15, hjust = 0.5), # 全体のサブタイトル
    plot.margin = margin(t = 10, r = 50, b = 10, l = 150, unit = "pt"), # 全体の余白
    legend.position = "none" # 凡例の表示位置
  ) + # 図の体裁
  coord_flip(clip = "off", expand = FALSE) + # 軸の入れ変え
  scale_x_reverse() + # x軸を反転
  gganimate::view_follow(fixed_x = TRUE) + # 表示範囲の調整
  labs(
    title = "ハロプロアーティストのシングルリリース数の推移", 
    subtitle = paste0(
      "カテゴリ：［", paste0(single_category, collapse = ", "), "］\n", 
      lubridate::year(date_from), "年", lubridate::month(date_from), "月～", 
      "{lubridate::year(closest_state)}年{lubridate::month(closest_state)}月"
    ), 
    caption = "データ:「https://github.com/xxgentaroxx/HP_DB」"
  ) # ラベル

# gif画像を作成
g <- gganimate::animate(
  plot = anim, 
  nframes = n*(t+s), fps = (t+s)*mps, 
  width = 900, height = 800
)
g

# gif画像を保存
gganimate::anim_save(filename = "BarChartRace/output/SingleNum.gif", animation = g)


# 動画を作成と保存
m <- gganimate::animate(
  plot = anim, 
  nframes = n*(t+s), fps = (t+s)*mps, 
  width = 900, height = 800, 
  renderer = gganimate::av_renderer(file = "BarChartRace/output/SingleNum.mp4")
)

warnings()


# 月を指定して作図 ----------------------------------------------------------------

# 月を指定
date_val <- "2021-05-01"

# 棒グラフを作成
graph <- rank_df %>% 
  dplyr::filter(date == lubridate::as_date(date_val)) %>% 
  ggplot(aes(x = ranking, y = release_n, fill = artist_id, color = artist_id)) + 
  geom_bar(stat = "identity", width = 0.9, alpha = 0.8) + # リリース数のバー
  geom_text(aes(y = 0, label = paste(artist_name, " ")), hjust = 1) + # グループ名のラベル
  geom_text(aes(y = 0, label = paste(" ", release_n, "枚")), hjust = 0, color = "white") + # リリース数のラベル
  theme(
    axis.title.y = element_blank(), # y軸のラベル
    axis.text.y = element_blank(), # y軸の目盛ラベル
    #panel.grid.major.x = element_line(color = "grey", size = 0.1), # x軸の主目盛線
    panel.grid.major.y = element_blank(), # y軸の主目盛線
    panel.grid.minor.x = element_blank(), # x軸の補助目盛線
    panel.grid.minor.y = element_blank(), # y軸の補助目盛線
    panel.border = element_blank(), # グラフ領域の枠線
    #panel.background = element_blank(), # グラフ領域の背景
    plot.title = element_text(color = "black", face = "bold", size = 20, hjust = 0.5), # 全体のタイトル
    plot.subtitle = element_text(color = "black", size = 15, hjust = 0.5), # 全体のサブタイトル
    plot.margin = margin(t = 10, r = 20, b = 10, l = 125, unit = "pt"), # 全体の余白
    legend.position = "none" # 凡例の表示位置
  ) + # 図の体裁
  coord_flip(clip = "off", expand = FALSE) + # 軸の入れ変え
  scale_x_reverse(breaks = 1:max(rank_df[["ranking"]])) + # x軸を反転
  labs(
    title = "ハロプロアーティストのシングルリリース数", 
    subtitle = paste0(
      "カテゴリ：［", paste0(single_category, collapse = ", "), "］\n", 
      lubridate::year(date_from), "年", lubridate::month(date_from), "月～", 
      lubridate::year(date_val), "年", lubridate::month(date_val), "月"
    ), 
    y = "リリース数", 
    caption = "データ:「https://github.com/xxgentaroxx/HP_DB」"
  ) # ラベル
graph

# 画像を保存
ggplot2::ggsave(
  filename = paste0("BarChartRace/output/SingleNum_", date_val, ".png"), plot = graph, 
  width = 24, height = 18, units = "cm", dpi = 100
)


