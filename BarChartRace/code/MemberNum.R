
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

# グループ一覧
group_df <- readr::read_csv(
  file = paste0(dir_path, "group.csv"), 
  col_types = readr::cols(
    groupID = "i", 
    groupName = "c", 
    formDate = readr::col_date(format = "%Y/%m/%d"), 
    dissolveDate = readr::col_date(format = "%Y/%m/%d"), 
    isUnit = "l"
  )
)
group_df

# 加入・卒業日一覧
join_df <- readr::read_csv(
  file = paste0(dir_path, "join.csv"), 
  col_types = readr::cols(
    memberID = "i", 
    groupID = "i", 
    joinDate = readr::col_date(format = "%Y/%m/%d"), 
    gradDate = readr::col_date(format = "%Y/%m/%d")
  )
)
join_df


# メンバー数の集計 ----------------------------------------------------------------

### ・受け皿の作成 -----

# 期間を指定
date_from <- "2015-08-01"
date_to   <- "2022-04-15"
date_to   <- lubridate::now()

# 月単位に切り捨て
date_from <- date_from %>% 
  lubridate::as_date() %>% 
  lubridate::floor_date(unit = "mon")

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


### ・在籍数を集計 -----

# 月ごとの加入・卒業数を集計
join_n_df <- join_df %>% 
  dplyr::mutate(
    joinDate = lubridate::floor_date(joinDate, unit = "mon"), 
    gradDate = lubridate::floor_date(gradDate, unit = "mon")
  ) %>% # 月単位に切り捨て
  tidyr::pivot_longer(
    cols = !c(memberID, groupID), 
    names_to = "date_type", 
    values_to = "date"
  ) %>% # 加入・卒業月を同じ列に変換
  dplyr::group_by(groupID, date_type, date) %>% # グループ化
  dplyr::count() %>% # メンバー変動数をカウント
  dplyr::ungroup() %>% # グループ化の解除
  dplyr::arrange(date, groupID) # 昇順に並び替え
join_n_df

# 月ごとのメンバー数を集計
member_n_df <- dplyr::left_join(date_df, join_n_df, by = c("date", "groupID")) %>% # メンバー変動数を全期間に統合
  tidyr::pivot_wider(
    names_from = date_type, 
    values_from = n, 
    values_fill = 0
  ) %>% # 加入・卒業月を別の列に変形
  dplyr::select(date, groupID, join_n = joinDate, grad_n = gradDate) %>% # 月・グループ・変動数の列を選択
  dplyr::group_by(groupID) %>% # グループごとにグループ化
  dplyr::mutate(active_n = cumsum(join_n - grad_n)) %>% # 在籍数を集計
  dplyr::ungroup() %>% # グループ化の解除
  dplyr::filter(active_n > 0) %>% # 活動中のグループを抽出
  dplyr::select(date, groupID, active_n) %>% # 月・グループ・在籍数の列を選択
  dplyr::arrange(date, groupID) # 昇順に並び替え
member_n_df


### ・アニメ用の小細工 -----

## バーの変化を強調するために結成前月と解散月をデータに含めたい

# 改名したグループを抽出
tmp_rename_group_df <- group_df %>% 
  dplyr::select(!isUnit) %>% # 不要な列を削除
  dplyr::group_by(groupID) %>% # グループごとにグループ化
  dplyr::filter(formDate != min(formDate) | formDate != max(formDate)) %>% # 改名グループを抽出
  dplyr::ungroup() %>% # グループ化の解除
  dplyr::arrange(groupID, formDate) # 昇順に並び替え
tmp_rename_group_df

# 改名したグループの結成・解散月を再設定
rename_group_df <- tibble::tibble()
for(i in unique(tmp_rename_group_df[["groupID"]])) {
  # i番目の改名グループを抽出
  tmp_df1 <- tmp_rename_group_df %>% 
    dplyr::filter(groupID == i)
  
  # 結成・解散月を再設定
  tmp_df2 <- tibble::tibble(
    groupID = i, 
    groupName = tmp_df1[["groupName"]][1], 
    formDate = tmp_df1[["formDate"]][1], 
    dissolveDate = tmp_df1[["dissolveDate"]][nrow(tmp_df1)]
  )
  
  # 結合
  rename_group_df <- rbind(rename_group_df, tmp_df2)
}
rename_group_df

# 改名していないグループを抽出
nonrename_group_df <- group_df %>% 
  dplyr::select(!isUnit) %>% # 不要な列を削除
  dplyr::group_by(groupID) %>% # グループごとにグループ化
  dplyr::filter(formDate == min(formDate), formDate == max(formDate)) %>% # 非改名グループを抽出
  dplyr::ungroup() %>% # グループ化の解除
  dplyr::arrange(groupID, formDate) # 昇順に並び替え
nonrename_group_df

# 結成・解散月を取得
member_0_df <- rbind(rename_group_df, nonrename_group_df) %>% # 改名・非改名グループを結合
  dplyr::mutate(
    formDate = formDate %>% 
      lubridate::rollback() %>% # 結成1か月前に変更
      lubridate::floor_date(unit = "mon"), 
    dissolveDate = lubridate::floor_date(dissolveDate, unit = "mon")
  ) %>% # 月単位に切り捨て
  tidyr::pivot_longer(
    cols = !c(groupID, groupName), 
    names_to = "date_type", 
    values_to = "date"
  ) %>% # 結成・解散月を同じ列に変換
  dplyr::select(date, groupID) %>% # 月・グループの列を選択
  tibble::add_column(active_n = 0) %>% # メンバー数(0人)を追加
  dplyr::filter(!is.na(date)) %>% # 活動中のグループを除去
  dplyr::arrange(date, groupID) # 昇順に並び替え
member_0_df


### ・グループ名の対応 -----

## 改名組の表示名に対応したい

# 月・グループID・グループ名の対応表を作成
group_name_df <- tibble::tibble()
for(i in 1:nrow(group_df)) {
  # i番目のグループの結成日と解散日を取得
  tmp_date_from <- group_df[["formDate"]][i]
  if(is.na(group_df[["dissolveDate"]][i])) {
    tmp_date_to <- lubridate::as_date(lubridate::now()) # 活動中であれば現在の日時
  } else {
    tmp_date_to <- group_df[["dissolveDate"]][i]
  }
  
  # 月ベクトルを作成
  tmp_date_vec <- seq(
    from = lubridate::floor_date(tmp_date_from, unit = "mon"), 
    to = lubridate::floor_date(tmp_date_to, unit = "mon"), 
    by = "mon"
  )
  
  # 月データフレームを作成
  tmp_name_df <- tibble::tibble(
    date = tmp_date_vec, 
    groupID = group_df[["groupID"]][i], 
    groupName = group_df[["groupName"]][i]
  )
  
  # 結合
  group_name_df <- rbind(group_name_df, tmp_name_df)
}

# 改名が月途中だと重複するのでその対応
group_name_df <- group_name_df %>% 
  dplyr::arrange(groupID, date) %>% # 昇順に並び替え
  dplyr::group_by(date, groupID) %>% # 日付とグループでグループ化
  dplyr::mutate(row_num = dplyr::row_number()) %>% # 重複を確認
  dplyr::filter(row_num == max(row_num)) %>% # 重複する場合は新しい方を抽出
  dplyr::select(!row_num) # 不要な列を削除
group_name_df


### ・順位付け -----

# メンバー数で順位付け
rank_df <- member_n_df %>% 
  rbind(member_0_df) %>% # 在籍なし期間を結合:(小細工する場合)
  dplyr::arrange(date, groupID) %>% # 昇順に並び替え
  dplyr::group_by(date) %>% # 月ごとにグループ化
  dplyr::mutate(ranking = dplyr::row_number(-active_n)) %>% # ランキング列を追加
  dplyr::ungroup() %>% # グループ化の解除
  dplyr::left_join(group_name_df, by = c("date", "groupID")) %>% # グループ名列を追加
  dplyr::mutate(
    groupID = as.factor(groupID), 
    groupName = tidyr::replace_na(groupName, replace = " ")
  ) %>% # 作図用に変換
  dplyr::select(date, groupID, groupName, active_n, ranking) %>% # 列を並べ替え
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

# 最終結果をn秒間表示するフレーム数を計算:(フレーム数が少ないときしかムリっぽい)
n <- 0
e <- (t + s) * mps * n

# フレーム数を取得
frame_n <- length(unique(rank_df[["date"]]))


### ・y軸固定 -----

# バーチャートレースを作成:(y軸固定)
anim <- ggplot(rank_df, aes(x = ranking, y = active_n, fill = groupID, color = groupID)) + 
  geom_bar(stat = "identity", width = 0.9, alpha = 0.8) + # 出現回数のグラフ
  geom_text(aes(y = 0, label = paste(groupName, " ")), hjust = 1) + # カテゴリ名
  geom_text(aes(y = 0, label = paste(" ", round(active_n))), hjust = 0, color = "white") + # 出現回数
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
  nframes = frame_n*(t+s)+e, end_pause = e, fps = (t+s)*mps, 
  width = 900, height = 600
)

# gif画像を保存
gganimate::anim_save(filename = "BarChartRace/output/MemberNum_fixed.gif", animation = g)


# 動画を作成と保存
m <- gganimate::animate(
  plot = anim, 
  nframes = frame_n*(t+s)+e, end_pause = e, fps = (t+s)*mps, 
  width = 900, height = 600, 
  renderer = gganimate::av_renderer("BarChartRace/output/MemberNum_fixed.mp4")
  #renderer = gganimate::ffmpeg_renderer(format = "h264")
  #renderer = gganimate::gifski_renderer()
)


warnings()


### ・y軸可変 -----

# バーチャートレースを作成:(y軸可変)
anim <- ggplot(rank_df, aes(x = ranking, y = active_n, fill = groupID, color = groupID)) + 
  geom_bar(stat = "identity", width = 0.9, alpha = 0.8) + # 出現回数のグラフ
  geom_text(aes(y = 0, label = paste(groupName, " ")), hjust = 1) + # カテゴリ名
  geom_text(aes(label = paste(" ", round(active_n), "人")), hjust = 0) + # 出現回数
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
  nframes = frame_n*(t+s)+e, end_pause = e, fps = (t+s)*mps, 
  width = 900, height = 600
)

# gif画像を保存
gganimate::anim_save(filename = "BarChartRace/output/MemberNum_variable.gif", animation = g)


# 動画を作成と保存
m <- gganimate::animate(
  plot = anim, 
  nframes = frame_n*(t+s)+e, end_pause = e, fps = (t+s)*mps, 
  width = 900, height = 600, 
  renderer = gganimate::av_renderer(file = "BarChartRace/output/MemberNum_variable.mp4")
)


warnings()


# メモ ----------------------------------------------------------------------

## 解散しておらずメンバー数が0人だと表示されない:(ex:北研)

