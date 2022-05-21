
# アルバムリリース数の推移を可視化 -------------------------------------------------------

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

# アルバム一覧を読み込み
album_df <- readr::read_csv(
  file = paste0(dir_path, "album.csv"), 
  col_types = readr::cols(
    albumID = "i", 
    albumName = "c", 
    releaseDate = readr::col_date(format = "%Y/%m/%d"), 
    albumCategory = "c", 
    artistName = "c"
  )
) %>% 
  dplyr::arrange(releaseDate, albumID) # 昇順に並べ替え
album_df


# 期間の設定 ---------------------------------------------------------------------

# 期間を指定
date_from <- "1998-01-01"
date_to   <- "2022-04-15"
date_to   <- lubridate::today() %>% 
  as.character()

# 値をまとめる
date_vals <- c(date_from, date_to) %>% 
  lubridate::as_date() %>% # Date型に変換
  lubridate::floor_date(unit = "mon") # 月単位に切り捨て
date_vals


# アルバムカテゴリの設定 --------------------------------------------------------------

# アルバムの種類を確認
unique(album_df[["albumCategory"]])

# アルバムの種類を指定
album_category <- c("オリジナルアルバム", "ベストアルバム", "ミニアルバム")


# 連名作品の編集 ---------------------------------------------------------------------

# アーティスト名を確認
unique(album_df[["artistName"]])

# 連名作品(アルバム名義・分割後の名義・分割数)を指定
pattern_vec <- c("モーニング娘。&平家みちよ", "市井紗耶香 with 中澤裕子", "中澤裕子/メロン記念日/松浦亜弥/石井リカ", "タンポポ/プッチモニ", "中澤裕子/後藤真希/藤本美貴", "モーニング娘。'14/スマイレージ")
replace_vec <- c("モーニング娘。", "平家みちよ", "市井紗耶香", "中澤裕子", "中澤裕子", "メロン記念日", "松浦亜弥", "石井リカ", "中澤裕子", "後藤真希", "藤本美貴", "タンポポ", "プッチモニ", "モーニング娘。'14", "スマイレージ")
n_vec <- c(2, 2, 4, 3, 2, 2)

# 連名作品を分割
split_df <- album_df %>% 
  dplyr::filter(artistName %in% pattern_vec) %>% # 連名作品を抽出
  tibble::add_column(n = n_vec) %>% # 複製する数を追加
  tidyr::uncount(n) %>% # 作品を複製
  dplyr::mutate(artistName = replace_vec) # 個々のアーティスト名を設定
split_df


# リリース数の集計 ----------------------------------------------------------------

# アーティスト名を編集してリリース数を集計
release_n_df <- album_df %>% 
  dplyr::filter(!(artistName %in% pattern_vec)) %>% # 連名作品を削除
  dplyr::bind_rows(split_df) %>% # 分割した連名作品を追加
  dplyr::filter(albumCategory %in% album_category) %>% # 指定した種類の作品を抽出
  dplyr::filter(releaseDate >= date_vals[1], releaseDate <= date_vals[2]) %>% # 指定した期間内の作品を抽出
  dplyr::arrange(releaseDate, albumID) %>% # IDの割り当て用に並べ替え
  dplyr::mutate(
    release_date = lubridate::floor_date(releaseDate, unit = "mon"), # 月単位に切り捨て
    artist_name = artistName %>% # グラフ表示名を追加
      #stringr::str_replace(pattern = "℃-ute", replacement = "C-ute") %>% # 作図時に豆腐化するので代用
      stringr::str_replace(pattern = "月島きらり.*", replacement = "月島きらり(久住小春)") %>% # 長いので省略
      stringr::str_replace(pattern = "太陽とシスコムーン/T&Cボンバー", replacement = "太陽とシスコムーン"), # 改名前に変更
    artist_idname = artist_name %>% # IDの割り当て用に編集
      stringr::str_replace(pattern = "モーニング娘。.*", replacement = "モーニング娘。") %>% # ナンバリングを削除
      stringr::str_replace(pattern = "中澤ゆうこ", replacement = "中澤裕子") %>% # ソロ用名義を名前に統一
      stringr::str_replace(pattern = "T&Cボンバー", replacement = "太陽とシスコムーン") %>% # 改名前に統一
      stringr::str_replace(pattern = "アンジュルム", replacement = "スマイレージ"), # 改名前に統一
    artist_idname = factor(artist_idname, levels = unique(artist_idname)), # レベル設定のため因子型に変換
    artist_id = dplyr::dense_rank(artist_idname) # アーティストIDを追加
  ) %>% # アーティスト名を編集
  dplyr::count(release_date, artist_id, artist_name, name = "release_n") %>% # リリース数をカウント:(同じ月にIDが一緒で名前が異なる作品があるとたぶんバグる)
  dplyr::group_by(artist_id) %>% # 累積和の計算用にグループ化
  dplyr::mutate(release_n = cumsum(release_n)) %>% # リリース数の累積和を計算
  dplyr::arrange(release_date, artist_id) %>% # 用に並べ替え
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


# 演出用の処理 ---------------------------------------------------------------------

### ・1枚目の発売前月の作成:(バーの変化を強調したい) -----

# 1枚目のリリース前月のデータを作成
release_0_df <- release_n_df %>% 
  dplyr::group_by(artist_id) %>% # 1枚目の抽出用にグループ化
  dplyr::filter(date == min(date)) %>% # 1枚目を抽出
  dplyr::ungroup() %>% # グループ化を解除
  dplyr::mutate(
    date = date %>% 
      lubridate::rollback() %>% 
      lubridate::floor_date(unit = "mon"), 
    artist_name = " ", 
    release_n = 0
  ) %>% # 1か月前のデータに書き換え
  dplyr::filter(date >= date_vals[1], date <= date_vals[2]) # 指定した期間内のデータを抽出
release_0_df


# リリース数の順位付け --------------------------------------------------------------

# 描画する順位を指定
max_rank <- 50

# リリース数で順位付け
rank_df <- dplyr::bind_rows(release_0_df, release_n_df) %>% # 発売前月のデータを追加:(小細工する場合)
  dplyr::arrange(date, artist_id) %>% # 昇順に並び替え
  dplyr::group_by(date) %>% # 月でグループ化
  dplyr::mutate(
    artist_id = factor(artist_id),  # 作図用に因子型に変換
    ranking = dplyr::row_number(-release_n) # ランキング列を追加
  ) %>% 
  dplyr::ungroup() %>% # グループ化を解除
  dplyr::filter(ranking <= max_rank) %>% # ランク上位グループを抽出
  dplyr::arrange(date, ranking) # 昇順に並べ替え
rank_df


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
    title = paste0(
      "ハロプロアーティストのアルバムリリース数の推移", 
      ":(", lubridate::year(date_from), "年", lubridate::month(date_from), "月以降)"
    ), 
    subtitle = "{lubridate::year(closest_state)}年{lubridate::month(closest_state)}月", 
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
gganimate::anim_save(filename = "BarChartRace/output/AlbumNum.gif", animation = g)


# 動画を作成と保存
m <- gganimate::animate(
  plot = anim, 
  nframes = n*(t+s)+e, end_pause = e, fps = (t+s)*mps, 
  width = 900, height = 800, 
  renderer = gganimate::av_renderer(file = "BarChartRace/output/AlbumNum.mp4")
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
    title = paste0(
      "ハロプロアーティストのアルバムリリース数", 
      ":(", lubridate::year(date_from), "年", lubridate::month(date_from), "月以降)"
    ), 
    subtitle = paste0(lubridate::year(date_val), "年", lubridate::month(date_val), "月時点"), # ラベル
    y = "リリース数", 
    caption = "データ:「https://github.com/xxgentaroxx/HP_DB」"
  ) # ラベル
graph

# 画像を保存
ggplot2::ggsave(
  filename = paste0("BarChartRace/output/AlbumNum_", date_val, ".png"), plot = graph, 
  width = 24, height = 18, units = "cm", dpi = 100
)


