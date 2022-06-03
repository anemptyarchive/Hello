
# メンバー数の推移をバーチャートレースで可視化 --------------------------------------------------

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

# グループ一覧を読み込み
group_df <- readr::read_csv(
  file = paste0(dir_path, "group.csv"), 
  col_types = readr::cols(
    groupID = "i", 
    groupName = "c", 
    formDate = readr::col_date(format = "%Y/%m/%d"), 
    dissolveDate = readr::col_date(format = "%Y/%m/%d"), 
    isUnit = "l"
  )
) %>% 
  dplyr::arrange(groupID, formDate) # 昇順に並び替え
group_df

# 加入・卒業日一覧を読み込み
join_df <- readr::read_csv(
  file = paste0(dir_path, "join.csv"), 
  col_types = readr::cols(
    memberID = "i", 
    groupID = "i", 
    joinDate = readr::col_date(format = "%Y/%m/%d"), 
    gradDate = readr::col_date(format = "%Y/%m/%d")
  )
) %>% 
  dplyr::arrange(joinDate, memberID, groupID) # 昇順に並び替え
join_df


# 期間の設定 -------------------------------------------------------------------

### ・受け皿の作成 -----

# 期間を指定
date_from <- "1997-09-01"
date_to   <- "2022-05-07"
date_to   <- lubridate::today()

# 月初めのDate型に変換
date_from <- date_from %>% 
  lubridate::as_date() %>% # Date型に変換
  lubridate::floor_date(unit = "mon") # 月単位に切り捨て

# 月ベクトルを作成
date_vec <- seq(
  from = lubridate::as_date("1997-09-01"), # 一番古い月で固定
  to = date_to %>% 
    lubridate::as_date() %>% 
    lubridate::floor_date(unit = "mon"), 
  by = "mon"
)

# グループ数を取得
group_size <- max(group_df[["groupID"]])

# 月データフレームを作成
date_df <- tibble::tibble(
  date = rep(date_vec, each = group_size), 
  groupID = rep(1:group_size, times = length(date_vec))
)
date_df


# 演出用の処理 ------------------------------------------------------------------

### ・グループ名の対応:(改名組の表示名に対応したい) -----

# 月・グループID・グループ名の対応表を作成
group_name_df <- group_df %>% 
  dplyr::mutate(
    formDate = formDate %>% 
      lubridate::floor_date(unit = "mon"), 
    dissolveDate = dissolveDate %>% 
      dplyr::if_else(
        condition = is.na(.), 
        true = lubridate::today(), 
        false = dissolveDate
        ) %>% # 現在活動中なら現在の日付
      lubridate::floor_date(unit = "mon"), 
    n = lubridate::interval(start = formDate, end = dissolveDate) %>% 
      lubridate::time_length(unit = "mon") + 1
  ) %>% # 月単位に切り捨てて月数をカウント
  tidyr::uncount(n) %>% # 月数に応じて行を複製
  dplyr::group_by(groupName) %>% # 行番号用にグループ化
  dplyr::mutate(idx = dplyr::row_number()) %>% # 行番号を割り当て
  dplyr::group_by(groupName, idx) %>% # 1か月刻みの値の作成用にグループ化
  dplyr::mutate(date = seq(from = formDate, to = dissolveDate, by = "mon")[idx]) %>% # 複製した行を1か月刻みの値に変更
  dplyr::group_by(date, groupID) %>% # 重複の除去用にグループ化
  dplyr::slice_max(formDate) %>% # 重複する場合は新しい方を抽出
  dplyr::ungroup() %>% # グループ化を解除
  dplyr::select(date, groupID, groupName) %>% # 利用する列を選択
  dplyr::arrange(date, groupID) # 昇順に並べ替え
group_name_df


### ・結成前月と解散月の追加:(バーの変化を強調したい) -----

# 結成前月・解散月のデータを作成
member_0_df <- group_df %>% 
  dplyr::group_by(groupID) %>% # 日付の再設定用にグループ化
  dplyr::mutate(dissolveDate = dplyr::lead(dissolveDate, n = max(dplyr::n())-1)) %>% # 最後の行を1行目にズラす
  dplyr::slice_head(n = 1) %>% # 1行目を抽出
  dplyr::ungroup() %>% # グループ化を解除
  dplyr::mutate(
    formDate = formDate %>% 
      lubridate::rollback() %>% # 結成1か月前に変更
      lubridate::floor_date(unit = "mon"), 
    dissolveDate = dissolveDate %>% 
      lubridate::floor_date(unit = "mon")
  ) %>% # 月単位に切り捨て
  tidyr::pivot_longer(
    cols = c(formDate, dissolveDate), 
    names_to = "date_type", 
    values_to = "date"
  ) %>% # 結成前月・解散月を同じ列に変形
  dplyr::select(date, groupID) %>% # 利用する列を選択
  dplyr::filter(!is.na(date)) %>% # 現在活動中のグループの解散月を除去
  tibble::add_column(member_n = 0) %>% # メンバー数(0人)を追加
  dplyr::arrange(date, groupID) # 昇順に並び替え
member_0_df


# メンバー数の集計 ----------------------------------------------------------------

### ・順位付け -----

# メンバー数を集計してランク付け
rank_df <- join_df %>% 
  dplyr::mutate(
    joinDate = lubridate::floor_date(joinDate, unit = "mon"), 
    gradDate = lubridate::floor_date(gradDate, unit = "mon")
  ) %>% # 月単位に切り捨て
  tidyr::pivot_longer(
    cols = c(joinDate, gradDate), 
    names_to = "date_type", 
    values_to = "date"
  ) %>% # 加入月・卒業月を同じ列に変形
  dplyr::group_by(date, groupID, date_type) %>% # カウント用にグループ化
  dplyr::count(name = "n") %>% # 変動数をカウント
  dplyr::ungroup() %>% # グループ化を解除
  dplyr::left_join(date_df, ., by = c("date", "groupID")) %>% # 全期間に統合
  tidyr::pivot_wider(
    names_from = date_type, 
    values_from = n, 
    values_fill = 0
  ) %>% # 加入数・卒業数を別の列に変形
  dplyr::select(date, groupID, join_n = joinDate, grad_n = gradDate) %>% # 利用する列を選択
  dplyr::group_by(groupID) %>% # 在籍数の集計用にグループ化
  dplyr::mutate(member_n = cumsum(join_n - grad_n)) %>% # 在籍数を集計
  dplyr::ungroup() %>% # グループ化を解除
  dplyr::filter(member_n > 0) %>% # 活動中のグループを抽出
  dplyr::select(date, groupID, member_n) %>% # 利用する列を選択
  dplyr::bind_rows(member_0_df) %>% # 在籍なし期間を結合
  dplyr::arrange(date, groupID) %>% # ランク付け用に並べ替え
  dplyr::group_by(date) %>% # ランク付け用にグループ化
  dplyr::mutate(ranking = dplyr::row_number(-member_n)) %>% # ランキング列を追加
  dplyr::ungroup() %>% # グループ化を解除
  dplyr::left_join(group_name_df, by = c("date", "groupID")) %>% # グループ名列を追加
  dplyr::mutate(
    groupID = as.factor(groupID), 
    groupName = tidyr::replace_na(groupName, replace = " ")
  ) %>% # 作図用に変換
  dplyr::select(date, groupID, groupName, member_n, ranking) %>% # 列を並べ替え
  dplyr::filter(date >= date_from, date <= max(date_vec)) %>% # 期間内のデータを抽出
  dplyr::arrange(date, ranking) # 昇順に並び替え
rank_df


# アニメーションの作成 --------------------------------------------------------------

### ・フレーム数の設定 -----

# 遷移フレーム数を指定
t <- 8

# 一時停止フレーム数を指定
s <- 2

# 1秒間に表示する月数を指定:(値が大きいと意図した通りにならない)
mps <- 2.5

# フレーム数を取得
n <- length(unique(rank_df[["date"]]))


### ・y軸固定 -----

# バーチャートレースを作成:(y軸固定)
anim <- ggplot(rank_df, aes(x = ranking, y = member_n, fill = groupID, color = groupID)) + 
  geom_bar(stat = "identity", width = 0.9, alpha = 0.8) + # メンバー数のバー
  geom_text(aes(y = 0, label = paste(groupName, " ")), hjust = 1) + # グループ名のラベル
  geom_text(aes(y = 0, label = paste(" ", round(member_n))), hjust = 0, color = "white") + # メンバー数のラベル
  gganimate::transition_states(states = date, transition_length = t, state_length = s, wrap = FALSE) + # フレーム
  gganimate::ease_aes("cubic-in-out") + # アニメーションの緩急
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
  labs(title = "ハロプログループのメンバー数の推移", 
       subtitle = "{lubridate::year(closest_state)}年{lubridate::month(closest_state)}月", # ラベル
       y = "メンバー数", 
       caption = "データ:「https://github.com/xxgentaroxx/HP_DB」") # ラベル

# gif画像を作成
g <- gganimate::animate(
  plot = anim, 
  nframes = n*(t+s), fps = (t+s)*mps, 
  width = 900, height = 600
)
g

# gif画像を保存
gganimate::anim_save(filename = "BarChartRace/output/MemberNum_fixed.gif", animation = g)


# 動画を作成と保存
m <- gganimate::animate(
  plot = anim, 
  nframes = n*(t+s), fps = (t+s)*mps, 
  width = 900, height = 600, 
  renderer = gganimate::av_renderer("BarChartRace/output/MemberNum_fixed.mp4")
  #renderer = gganimate::ffmpeg_renderer(format = "h264")
  #renderer = gganimate::gifski_renderer()
)

warnings()


### ・y軸可変 -----

# バーチャートレースを作成:(y軸可変)
anim <- ggplot(rank_df, aes(x = ranking, y = member_n, fill = groupID, color = groupID)) + 
  geom_bar(stat = "identity", width = 0.9, alpha = 0.8) + # メンバー数のバー
  geom_text(aes(y = 0, label = paste(groupName, " ")), hjust = 1) + # グループ名のラベル
  geom_text(aes(label = paste(" ", round(member_n), "人")), hjust = 0) + # メンバー数のラベル
  gganimate::transition_states(states = date, transition_length = t, state_length = s, wrap = FALSE) + # フレーム
  gganimate::ease_aes("cubic-in-out") + # アニメーションの緩急
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
    plot.margin = margin(t = 10, r = 40, b = 10, l = 130, unit = "pt"), # 全体の余白
    legend.position = "none" # 凡例の表示位置
  ) + # 図の体裁
  coord_flip(clip = "off", expand = FALSE) + # 軸の入れ変え
  scale_x_reverse() + # x軸を反転
  gganimate::view_follow(fixed_x = TRUE) + # 表示範囲の調整
  labs(title = "ハロプログループのメンバー数の推移", 
       subtitle = "{lubridate::year(closest_state)}年{lubridate::month(closest_state)}月", 
       caption = "データ:「https://github.com/xxgentaroxx/HP_DB」") # ラベル

# gif画像を作成
g <- gganimate::animate(
  plot = anim, 
  nframes = n*(t+s), fps = (t+s)*mps, 
  width = 900, height = 600
)
g

# gif画像を保存
gganimate::anim_save(filename = "BarChartRace/output/MemberNum_variable.gif", animation = g)


# 動画を作成と保存
m <- gganimate::animate(
  plot = anim, 
  nframes = n*(t+s), fps = (t+s)*mps, 
  width = 900, height = 600, 
  renderer = gganimate::av_renderer(file = "BarChartRace/output/MemberNum_variable.mp4")
)

warnings()


# 月を指定して作図 ----------------------------------------------------------------

# 月を指定
date_val <- "2021-05-01"

# 棒グラフを作成
rank_df %>% 
  dplyr::filter(date == lubridate::as_date(date_val)) %>% 
  ggplot(aes(x = ranking, y = member_n, fill = groupID, color = groupID)) + 
  geom_bar(stat = "identity", width = 0.9, alpha = 0.8) + # メンバー数のバー
  geom_text(aes(y = 0, label = paste(groupName, " ")), hjust = 1) + # グループ名のラベル
  geom_text(aes(y = 0, label = paste(" ", round(member_n), "人")), hjust = 0, color = "white") + # メンバー数のラベル
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
  labs(title = "ハロプログループのメンバー数", 
       subtitle = paste0(lubridate::year(date_val), "年", lubridate::month(date_val), "月時点"), # ラベル
       y = "メンバー数", 
       caption = "データ:「https://github.com/xxgentaroxx/HP_DB」") # ラベル


# メモ ----------------------------------------------------------------------

## 解散しておらずメンバー数が0人だと表示されない:(ex:北研)

