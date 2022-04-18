
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

# アルバム一覧
album_df <- readr::read_csv(
  file = paste0(dir_path, "album.csv"), 
  col_types = readr::cols(
    albumID = "i", 
    albumName = "c", 
    releaseDate = readr::col_date(format = "%Y/%m/%d"), 
    albumCategory = "c", 
    artistName = "c"
  )
)
album_df


# リリース数の集計 ---------------------------------------------------------------------

### ・受け皿の作成 -----

# 期間を指定
date_from <- "1998-01-01"
date_from <- "2014-01-01"
date_to   <- "2022-04-15"
data_to   <- lubridate::now()

# 月ベクトルを作成
date_vec <- seq(
  from = date_from %>% 
    lubridate::as_date() %>% 
    lubridate::floor_date(unit = "mon"), 
  to = date_to %>% 
    lubridate::as_date() %>% 
    lubridate::floor_date(unit = "mon"), 
  by = "mon"
)


### ・アーティスト名の編集 -----

# アルバムの種類を確認
unique(album_df[["albumCategory"]])

# アルバムの種類を指定
album_category <- c("オリジナルアルバム", "ベストアルバム", "ミニアルバム")

# アーティスト名を確認
unique(album_df[["artistName"]])

# 連名作品を分割
tmp_album_df <- album_df %>% 
  dplyr::filter(artistName == "タンポポ/プッチモニ") %>% # 連名の作品を抽出
  tibble::add_column(n = 2) %>% # 複製数を追加
  tidyr::uncount(n) %>% # 作品を複製
  dplyr::mutate(artistName = c("タンポポ", "プッチモニ")) # 個々のアーティスト名に再設定
tmp_album_df

# アーティスト名を編集してリリース数を集計
release_n_df <- album_df %>% 
  dplyr::filter(artistName != "タンポポ/プッチモニ") %>% # 連名作品を削除
  rbind(tmp_album_df) %>% # 分割した連名作品を追加
  dplyr::filter(albumCategory %in% album_category) %>% # 指定したカテゴリを抽出
  dplyr::filter(releaseDate >= min(date_vec), releaseDate <= max(date_vec)) %>% # 指定した期間内の作品を抽出
  dplyr::arrange(releaseDate, albumID) %>% # 昇順に並び替え
  dplyr::mutate(
    release_date = lubridate::floor_date(releaseDate, unit = "mon"), # 月単位に切り捨て
    artist_name = artistName %>% # グラフ表示名を追加
      stringr::str_replace(pattern = "月島きらり.*", replacement = "月島きらり(久住小春)") %>% # 長いので省略
      stringr::str_replace(pattern = "太陽とシスコムーン/T&Cボンバー", replacement = "太陽とシスコムーン"), #%>% # 改名前に変更
      #stringr::str_replace(pattern = "℃-ute", replacement = "C-ute"), # 作図時に豆腐化するので代用
    artist_idname = artist_name %>% # id割り当て用に編集
      stringr::str_replace(pattern = "モーニング娘。.*", replacement = "モーニング娘。") %>% # ナンバリングを削除
      stringr::str_replace(pattern = "T&Cボンバー", replacement = "太陽とシスコムーン") %>% # 改名前に統一
      stringr::str_replace(pattern = "アンジュルム", replacement = "スマイレージ"), # 改名前に統一
    artist_idname = factor(artist_idname, levels = unique(artist_idname)), # レベル設定のため因子型に変換
    artist_id = dplyr::dense_rank(artist_idname) # アーティストIDを追加
  ) %>% 
  dplyr::group_by(release_date, artist_id) %>% # 発売月とアーティストでグループ化
  dplyr::mutate(release_n = dplyr::row_number()) %>% # リリース数をカウント:(count()で処理できるならしたい)
  dplyr::filter(release_n == max(release_n)) %>% # 同じ月に複数リリースしていると重複するので遅い方を採用
  dplyr::ungroup() %>% # グループ化を解除
  #dplyr::select(release_date, artist_id, artist_idname, artist_name, artistName, release_n) %>% # 利用する列を選択:(確認用)
  dplyr::select(release_date, artist_id, artist_name, release_n) %>% # 利用する列を選択:(描画用)
  dplyr::arrange(release_date, artist_id) # 昇順に並び替え
release_n_df

# アーティスト名を確認
unique(release_n_df[["artist_idname"]])
unique(release_n_df[["artist_name"]])


### ・アニメ用の小細工 -----

## バーの変化を強調するためにリリース1か月前のデータを含めたい

# 1枚目のリリース前月のデータを作成
release_0_df <- release_n_df %>% 
  dplyr::group_by(artist_id) %>% # アーティストでグループ化
  dplyr::filter(release_date == min(release_date)) %>% # 1枚目の作品を抽出
  dplyr::ungroup() %>% # グループ化の解除
  dplyr::mutate(
    release_date = release_date %>% 
      lubridate::rollback() %>% 
      lubridate::floor_date(unit = "mon"), 
    artist_name = " ", 
    release_n = 0
  ) %>% # 1か月前のデータに書き換え
  dplyr::filter(release_date >= min(date_vec), release_date <= max(date_vec)) # 期間内のデータを抽出
release_0_df


### ・月情報とアーティスト名の対応 -----

# アーティスト数を取得
artist_size <- max(release_n_df[["artist_id"]])

# 月情報とアーティスト名の対応データフレームを作成
date_df <- tibble::tibble()
for(i in 1:artist_size) {
  # i番目のアーティストのデータを抽出
  tmp_artist_df <- release_n_df %>% 
    dplyr::filter(artist_id == i) %>% # i番目のアーティストを抽出
    dplyr::select(date_from = release_date, artist_name) %>% # 月・アーティスト名の列を選択
    dplyr::distinct(artist_name, .keep_all = TRUE) %>% # 重複を削除
    dplyr::mutate(date_to = dplyr::lead(date_from, n = 1, default = max(date_vec))) # 月をズラして複製
  
  # 月とアーティスト名を対応
  if(nrow(tmp_artist_df) > 1) {
    
    # 改名アーティストの場合
    tmp_date_df <- tibble::tibble()
    for(j in 1:nrow(tmp_artist_df)) {
      
      # j番目のアーティスト名のときの月ベクトルを作成
      tmp_date_vec <- seq(
        from = tmp_artist_df[["date_from"]][j], 
        to = tmp_artist_df[["date_to"]][j], 
        by = "mon"
      )
      
      # 月データフレームを作成
      if(j != nrow(tmp_artist_df)) {
        # 次の名前があるときは最後を含めない
        tmp_df <- tibble::tibble(
          release_date = tmp_date_vec[-length(tmp_date_vec)], 
          artist_id = i, 
          artist_name = tmp_artist_df[["artist_name"]][j]
        )
      } else {
        # 次の名前がないときは最後を含める
        tmp_df <- tibble::tibble(
          release_date = tmp_date_vec, 
          artist_id = i, 
          artist_name = tmp_artist_df[["artist_name"]][j]
        )
      }
      
      # 同じアーティストの月データフレームを結合
      tmp_date_df <- rbind(tmp_date_df, tmp_df)
    }
  } else {
    # 非改名アーティストの場合
    # 月データフレームを作成
    tmp_date_df <- tibble::tibble(
      release_date = seq(
        from = tmp_artist_df[["date_from"]], 
        to = tmp_artist_df[["date_to"]], 
        by = "mon"
      ), 
      artist_id = i, 
      artist_name = tmp_artist_df[["artist_name"]]
    )
  }
  
  # 全てのアーティストの月データフレームを結合
  date_df <- rbind(date_df, tmp_date_df) %>% 
    dplyr::arrange(release_date, artist_id) # 昇順に並び替え
}
date_df


### ・順位付け -----

# 描画する順位を指定
max_rank <- 50

# リリース数で順位付け
rank_df <- release_n_df %>% 
  dplyr::right_join(date_df, by = c("release_date", "artist_id", "artist_name")) %>% # リリースデータを全期間に統合
  rbind(release_0_df) %>% # 発売前月のデータを追加:(小細工する場合)
  dplyr::arrange(release_date, artist_id) %>% # 昇順に並び替え
  dplyr::group_by(artist_id) %>% # アーティストでグループ化
  dplyr::mutate(
    artist_id = factor(artist_id), # 作図用に因子型に変換
    release_n = release_n %>% 
      tidyr::replace_na(replace = 0) %>% 
      cumsum()
  ) %>% # リリース数を集計
  dplyr::group_by(release_date) %>% # 月でグループ化
  dplyr::mutate(ranking = dplyr::row_number(-release_n)) %>% # ランキング列を追加
  dplyr::ungroup() %>% # グループ化の解除
  dplyr::filter(ranking <= max_rank) %>% # ランク上位を抽出
  dplyr::arrange(release_date, ranking) # 昇順に並び替え
rank_df


# アニメーションの作成 --------------------------------------------------------------

### ・バーチャートレース -----

# 遷移フレーム数を指定
t <- 8

# 一時停止フレーム数を指定
s <- 2

# 1秒間に表示する月数を指定:(値が大きいと意図した通りにならない)
mps <- 4

# 最終結果をn秒間表示するフレーム数を計算:(フレーム数が少ないときしかムリっぽい)
n <- 5
e <- (t + s) * mps * n

# フレーム数を取得
frame_n <- length(unique(rank_df[["release_date"]]))

# バーチャートレースを作成:(y軸可変)
anim <- ggplot(rank_df, aes(x = ranking, y = release_n, fill = artist_id, color = artist_id)) + 
  geom_bar(stat = "identity", width = 0.9, alpha = 0.8) + # 出現回数のグラフ
  geom_text(aes(y = 0, label = paste(artist_name, " ")), hjust = 1) + # カテゴリ名
  geom_text(aes(label = paste(" ", round(release_n, 0), "枚")), hjust = 0) + # 出現回数
  gganimate::transition_states(states = release_date, transition_length = t, state_length = s, wrap = FALSE) + # フレーム
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
    panel.grid.minor.x = element_blank(), # x軸の補助目盛線
    panel.grid.minor.y = element_blank(), # y軸の補助目盛線
    panel.border = element_blank(), # グラフ領域の枠線
    #panel.background = element_blank(), # グラフ領域の背景
    plot.title = element_text(color = "black", face = "bold", size = 20, hjust = 0.5), # 全体のタイトル
    plot.subtitle = element_text(color = "black", size = 15, hjust = 0.5), # 全体のサブタイトル
    plot.margin = margin(t = 10, r = 40, b = 10, l = 130, unit = "pt"), # 全体の余白
    legend.position = "none" # 凡例の表示位置
  ) + # 図の体裁
  coord_flip(clip = "off", expand = FALSE) + # 軸の入れ変え
  scale_x_reverse() + # x軸を反転
  gganimate::view_follow(fixed_x = TRUE) + # 表示範囲の調整
  labs(
    title = paste0(
      "ハロプログループのアルバムリリース数の推移", 
      ":(", lubridate::year(date_from), "年", lubridate::month(date_from), "月以降)"
    ), 
    subtitle = "{lubridate::year(closest_state)}年{lubridate::month(closest_state)}月", 
    caption = "データ:「https://github.com/xxgentaroxx/HP_DB」"
  ) # ラベル

# gif画像を作成
g <- gganimate::animate(
  plot = anim, 
  nframes = frame_n*(t+s)+e, end_pause = e, fps = (t+s)*mps, 
  width = 900, height = 600
)
g

# gif画像を保存
gganimate::anim_save(filename = "BarChartRace/output/AlbumNum_2014.gif", animation = g)


# 動画を作成と保存
m <- gganimate::animate(
  plot = anim, 
  nframes = frame_n*(t+s)+e, end_pause = e, fps = (t+s)*mps, 
  width = 900, height = 600, 
  renderer = gganimate::av_renderer(file = "BarChartRace/output/AlbumNum_2014.mp4")
)


warnings()

