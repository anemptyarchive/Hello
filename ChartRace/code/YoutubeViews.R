
# MV再生回数の推移の可視化 -----------------------------------------------------------

# 利用パッケージ
library(tidyverse)
library(lubridate)
library(readxl)
library(gganimate)

# チェック用
library(ggplot2)


# 共通の設定 -------------------------------------------------------------------

### ・データの読込 -----

# ファイルパスを指定
file_path <- "ChartRace/data/youtube-viewcount-logger-python/save.xlsx"


### ・期間の設定 -----

# 期間を指定
date_from <- "2022-01-01"
date_to   <- "2022-12-31"

# 表示する日付を作成
date_vec <- seq(
  from = lubridate::as_date(date_from), 
  to = lubridate::as_date(date_to), 
  by = "week" # 間隔を指定
)


### ・アニメーションの作成 -----

# 遷移フレーム数を指定
t <- 9

# 一時停止フレーム数を指定
s <- 1

# 1秒間に表示する日数を指定:(値が大きいと意図した通りにならない)
mps <- 3

# 終了時の停止フレーム数を指定
e <- 0


# アーティストごとの推移 -------------------------------------------------------------

### ・データの読込 -----

# グループ名(シート名)を指定
artist_name <- "つばきファクトリー"

# データを読み込み
log_df <- readxl::read_excel(
  path = file_path, 
  sheet = artist_name, 
  col_names = TRUE, .name_repair = "unique"
) |>
  dplyr::select(!...1, title = タイトル) |> 
  dplyr::mutate(
    #title = stringr::str_remove(title, pattern = artist_name)
    title = stringr::str_replace(title, pattern = "つばきファクトリー『", replacement = "『") # 個別対応用:(つばき)
  )


### ・再生回数の集計 -----

# ・総再生回数

# 再生回数を集計
rank_df <- log_df |> 
  dplyr::mutate(
    music_id = dplyr::row_number() |> 
      factor() # 楽曲番号
  ) |> 
  tidyr::pivot_longer(
    cols = !c(title, music_id), 
    names_to = "date", # 記録日
    names_transform = list(date = lubridate::as_date), 
    values_to = "count" # 再生回数
  ) |> 
  dplyr::filter(date >= date_from, date <= date_to) |> # 期間内のデータを抽出
  dplyr::filter(count > 0) |> # 公開前・未集計のデータを除去
  dplyr::arrange(date, -count, music_id) |> # ランク付け用
  dplyr::group_by(date) |> # ランク付け用
  dplyr::mutate(
    rank = dplyr::row_number() # 再生回数ランキング
  ) |> 
  dplyr::ungroup()


# ・期間内の再生回数

# 再生回数を集計
rank_df <- log_df |> 
  dplyr::mutate(
    music_id = dplyr::row_number() |> 
      factor() # 楽曲番号
  ) |> 
  tidyr::pivot_longer(
    cols = !c(title, music_id), 
    names_to = "date", # 記録日
    names_transform = list(date = lubridate::as_date), 
    values_to = "count" # 再生回数
  ) |> 
  dplyr::filter(date >= date_from-1, date <= date_to) |> # 期間内のデータを抽出
  dplyr::filter(count > 0) |> # 公開前・未集計のデータを除去
  dplyr::group_by(music_id) |> # 再生回数の計算用
  dplyr::mutate(
    count = count - min(count) # 期間内の再生回数
  ) |> 
  dplyr::ungroup() |> 
  dplyr::arrange(date, -count, music_id) |> # ランク付け用
  dplyr::group_by(date) |> # ランク付け用
  dplyr::mutate(
    rank = dplyr::row_number() # 再生回数ランキング
  ) |> 
  dplyr::ungroup()


### ・アニメーションの作成 -----

## ・バーチャートレース

# フレーム数を取得
n <- rank_df[["date"]] |> 
  unique() |> 
  length()
n

# バーチャートレースを作成:(y軸可変)
graph <- ggplot(data = rank_df, mapping = aes(x = rank, y = count, color = music_id, fill = music_id)) + 
  geom_bar(stat = "identity", width = 0.8) + # 再整数バー
  geom_text(aes(y = 0, label = paste(title, " ")), hjust = 1) + # アーティスト名のラベル
  geom_text(aes(label = paste(" ", format(count, big.mark = ",", scientific = FALSE), "回")), hjust = 0) + # リリース数のラベル
  gganimate::transition_states(states = date, transition_length = t, state_length = s, wrap = FALSE) + # フレーム
  gganimate::ease_aes("cubic-in-out") + # アニメーションの緩急
  scale_x_reverse() + # x軸を反転
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
    plot.margin = margin(t = 10, r = 100, b = 10, l = 200, unit = "pt"), # 全体の余白
    legend.position = "none" # 凡例の表示位置
  ) + # 図の体裁
  coord_flip(clip = "off", expand = FALSE) + # 軸の入れ変え
  labs(
    title = paste0(artist_name, "のMV再生回数"), 
    subtitle = paste0(
      #format(date_from, format = "%Y年%m月%d日"), "～", # (総再生回数の場合は不要)
      "{lubridate::year(closest_state)}年{lubridate::month(closest_state)}月{lubridate::day(closest_state)}日", "時点"
    ), 
    caption = "データ:「https://github.com/yayoimizuha/youtube-viewcount-logger-python」"
  )

# gif画像を作成
anim <- gganimate::animate(
  plot = graph, 
  nframes = n*(t+s)+e, fps = (t+s)*mps, end_pause = e, 
  width = 900, height = 800
)
anim


# 複数アーティストの推移 -------------------------------------------------------------

### ・データの読込 -----
artist_name_vec[15] == "COVERS -One on One- "

# 全てのアーティスト名(シート名)を取得
artist_name_vec <- readxl::excel_sheets(path = file_path)

# アーティスト名(シート名)を指定
artist_name_vec <- c("つばきファクトリー", "BEYOOOOONDS", "OCHA NORMA")
artist_name_vec <- c("モーニング娘。", "アンジュルム", "℃-ute")
artist_name_vec <- "鈴木愛理"

# データを読み込み
music_id_cnt <- 1
all_df <- tibble::tibble()
for(i in seq_along(artist_name_vec)) {
  
  # i番目のシート名を抽出
  artist_name <- artist_name_vec[i]
  
  # データを読み込み
  tmp_wide_df <- readxl::read_excel(
    path = file_path, 
    sheet = artist_name, 
    col_names = TRUE, .name_repair = "unique"
  ) |>
    dplyr::select(!...1, title = タイトル) |> 
    dplyr::mutate(
      title = dplyr::case_when(
        is.na(title) ~ "no title", 
        artist_name == "モーニング娘。" ~ stringr::str_remove(title, pattern = "モーニング娘。'\\d\\d"), 
        artist_name == "アンジュルム" ~ stringr::str_remove(title, pattern = "スマイレージ"), 
        artist_name == "℃-ute" ~ stringr::str_remove(title, pattern = "°C-ute"), 
        artist_name == "鈴木愛理" ~ stringr::str_remove(title, pattern = "-| - "), 
        artist_name == "COVERS - One on One -" ~ stringr::str_remove(title, pattern = "COVERS -One on One- "), 
        TRUE ~ title
      ), 
      title = stringr::str_remove(title, pattern = artist_name)
    )
  
  # 楽曲番号の最大値を計算
  music_id_max <- music_id_cnt + nrow(tmp_wide_df) - 1
  
  # 縦型に変換
  tmp_long_df <- tmp_wide_df|> 
    dplyr::mutate(
      music_id = factor(music_id_cnt:music_id_max), # 楽曲番号
      artist_name = stringr::str_replace(artist_name, pattern = "℃-ute", replacement = "°C-ute") |> 
        factor(levels = stringr::str_replace(artist_name_vec, pattern = "℃-ute", replacement = "°C-ute")), # アーティスト名:(°Cは文字化け対策)
      artist_id = factor(i) # アーティスト番号
    ) |> 
    tidyr::pivot_longer(
      cols = !c(title, music_id, artist_name, artist_id), 
      names_to = "col_name", # 記録日用の文字列
      values_to = "count" # 再生回数
    ) |> 
    dplyr::mutate(
      date = dplyr::case_when(
          nchar(col_name) == 10 ~ col_name |> # yyyy-mm-ddの場合
            lubridate::as_date(), 
          nchar(col_name) == 5 ~ col_name |> # シリアル値の場合
            as.numeric() |> # (シリアル値でない値に一時的に警告文が出る)
            (\(num) {num - 2})() |> # ExcelとRの対応用
            lubridate::as_date(origin = "1900-01-01"), 
          TRUE ~ lubridate::as_date(NA)
        ) # 記録日
    )
  
  # 楽曲番号を更新
  music_id_cnt <- music_id_max + 1
  
  # データを結合
  all_df <- dplyr::bind_rows(all_df, tmp_long_df)
}
warnings()


### ・再生回数の集計 -----

## ・期間内の再生回数

# グラフに表示する楽曲数(順位)を指定
max_rank <- 50

# 再生回数を集計
rank_df <- all_df |> 
  dplyr::filter(!is.na(date)) |> 
  dplyr::arrange(music_id, date) |> # 念のため
  dplyr::group_by(title, music_id, artist_name, artist_id) |> # 日付の補完用
  dplyr::summarise(
    date = seq(from = min(date), to = max(date), by = "day"), .groups = "drop"
  ) |> # 欠損記録日を補完
  dplyr::left_join(
    all_df |> 
      dplyr::select(music_id, date, count), 
    by = c("music_id", "date"), 
  ) |> # 再生回数列が落ちるので戻す
  dplyr::arrange(music_id, date) |> # 1日当たりの再生回数の計算用
  dplyr::group_by(music_id) |> # 1日当たりの再生回数の計算用
  dplyr::mutate(
    count = tidyr::replace_na(count, replace = 0), # 補完した日付の再生回数を0に置換
    count = cummax(count), # 再生回数が0なら前日の回数に変更
    count_before = dplyr::lag(count, n = 1, default = NA), # 前日の再生回数列を追加
    count_before = dplyr::if_else(
      is.na(count_before)|count_before == 0, true = count, false = count_before
    ), # 前日の記録がなければ同じ再生回数に変更
    diff = count - count_before # 1日当たりの再生回数
  ) |> 
  dplyr::filter(date >= min(date_vec), date <= max(date_vec)) |> # 期間中の再生回数の計算用
  dplyr::mutate(
    sum_diff = cumsum(diff) # 期間中の累積再生回数
  ) |> 
  dplyr::ungroup() |> 
  dplyr::filter(date %in% date_vec) |> # 描画する日のデータを抽出
  dplyr::filter(count > 0) |> # 公開前・未集計のデータを除去
  dplyr::arrange(date, -sum_diff, music_id) |> # 念のため
  dplyr::group_by(date) |> # ランク付け用
  dplyr::mutate(
    rank = dplyr::row_number(-sum_diff) # 再生回数ランキング
  ) |> 
  dplyr::ungroup() |> 
  dplyr::filter(rank <= max_rank) # 上位楽曲を抽出


# 最後のデータを抽出
tmp_label_df <- rank_df |> 
  dplyr::filter(date == max(date_vec))

# ランク外の日付を補完
tmp_rank_df <- rank_df |> 
  dplyr::group_by(music_id, date) |> # 日付の補完用
  dplyr::summarise(date = date_vec, .groups = "drop") |> # ランク外の日付を補完
  dplyr::left_join(
    rank_df |> 
      dplyr::select(music_id, date, rank), 
    by = c("music_id", "date")
  ) # ランキング列が落ちるので戻す

# 順位の推移を確認
ggplot()+
  geom_line(data = tmp_rank_df, mapping = aes(x = date, y = rank, color = music_id), 
           alpha = 0.5, size = 1, na.rm = TRUE) + # ランキングの推移
  geom_text(data = tmp_label_df, x = Inf, mapping = aes(y = rank, label = title, color = music_id), 
            hjust = 0) + # ランキングの推移
  scale_x_date(date_breaks = "month", date_labels = "%Y-%m-%d") + 
  scale_y_reverse() + 
  coord_cartesian(clip = "off") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        legend.position = "none", 
        plot.margin = margin(t = 10, r = 150, b = 10, l = 10, unit = "pt")) + # 全体の余白
  labs(x = "date", y = "ranking")


### ・アニメーションの作成 -----

# フレーム数を取得
n <- rank_df[["date"]] |> 
  unique() |> 
  length()
n

# バーチャートレースを作成:(y軸可変)
graph <- ggplot(data = rank_df, mapping = aes(x = rank, y = sum_diff, color = artist_name, fill = artist_name)) + 
  geom_bar(stat = "identity", width = 0.75) + # 再生回数バー
  geom_text(aes(y = 0, label = paste(title, " ")), hjust = 1) + # アーティスト名のラベル
  geom_text(aes(label = paste0(" ", format(sum_diff, big.mark = ",", scientific = FALSE), " 回")), hjust = 0) + # 再生回数のラベル
  gganimate::transition_states(states = date, transition_length = t, state_length = s, wrap = FALSE) + # フレーム
  gganimate::ease_aes("cubic-in-out") + # アニメーションの緩急
  gganimate::view_follow(fixed_x = TRUE) + # 表示範囲の調整
  scale_x_reverse() + # x軸を反転
  coord_flip(clip = "off", expand = TRUE) + # 軸の入れ変え
  theme(
    axis.title.x = element_blank(), # x軸のラベル
    axis.title.y = element_blank(), # y軸のラベル
    axis.text.x = element_blank(), # x軸の目盛ラベル
    axis.text.y = element_blank(), # y軸の目盛ラベル
    axis.ticks.x = element_blank(), # x軸の目盛指示線
    axis.ticks.y = element_blank(), # y軸の目盛指示線
    panel.grid.major.x = element_line(color = "grey", size = 0.1), # x軸の主目盛線
    panel.grid.major.y = element_blank(), # y軸の主目盛線
    panel.grid.minor.x = element_blank(), # x軸の補助目盛線
    panel.grid.minor.y = element_blank(), # y軸の補助目盛線
    panel.border = element_blank(), # グラフ領域の枠線
    panel.background = element_blank(), # グラフ領域の背景
    plot.title = element_text(color = "black", face = "bold", size = 20, hjust = 0.5), # 全体のタイトル
    plot.subtitle = element_text(color = "black", size = 15, hjust = 0.5), # 全体のサブタイトル
    plot.margin = margin(t = 10, r = 90, b = 10, l = 180, unit = "pt"), # 全体の余白
    legend.position = c(1, 0), #"bottom", # 凡例の表示位置
    legend.justification = c(1, 0), # 凡例の表示位置
    legend.direction = "vertical" #"horizontal" # 凡例の向き
  ) + # 図の体裁
  labs(
    title = "期間中のMV再生回数", 
    subtitle = paste0(
      format(lubridate::as_date(date_from), format = "%Y年%m月%d日"), "～", # (総再生回数の場合は不要)
      "{lubridate::year(closest_state)}年{lubridate::month(closest_state)}月{lubridate::day(closest_state)}日", "時点"
    ), 
    color = "アーティスト(チャンネル名)", fill = "アーティスト(チャンネル名)", 
    caption = "データ:「https://github.com/yayoimizuha/youtube-viewcount-logger-python」"
  )

# gif画像を作成
anim <- gganimate::animate(
  plot = graph, 
  nframes = n*(t+s)+e, fps = (t+s)*mps, end_pause = e, 
  width = 750, height = 1250
)
anim

