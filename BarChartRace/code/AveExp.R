
# グループごとの平均活動年数の推移を可視化 -------------------------------------------------------------------

# 利用パッケージ
library(tidyverse)
library(lubridate)
library(gganimate)

# チェック用
library(magrittr)
library(ggplot2)


# データの読込 -----------------------------------------------------------------

## AveAge.Rを参照


# 期間の設定 -------------------------------------------------------------------

## AveAge.Rを参照


# 演出用の処理 ------------------------------------------------------------------

## AveAge.Rを参照


# 集計 ----------------------------------------------------------------------

# サイズを取得
date_size   <- length(date_vec)
group_size  <- max(group_df[["groupID"]])
member_size <- max(member_df[["memberID"]])

# 平均活動年数を集計
rank_df <- tibble::tibble(
  date = rep(date_vec, each = group_size*member_size), 
  groupID = rep(rep(1:group_size, times = date_size), each = member_size), 
  memberID = rep(1:member_size, times = date_size*group_size)
) %>% # 全ての組み合わせを作成
  dplyr::left_join(group_name_df, by = c("date", "groupID")) %>% # グループ情報を結合
  dplyr::filter(date >= formDate, date <= dissolveDate) %>% # 活動中のグループを抽出
  dplyr::select(!c(formDate, dissolveDate)) %>% # 不要な列を削除
  dplyr::left_join(
    join_df %>% 
      dplyr::mutate(
        joinDate = lubridate::floor_date(joinDate, unit = "mon"), 
        gradDate = lubridate::floor_date(gradDate, unit = "mon")
      ), # 月単位に切り捨て
    by = c("groupID", "memberID")
  ) %>% # 加入メンバー情報を結合
  dplyr::filter(date >= joinDate, date < gradDate | is.na(gradDate)) %>% # グループ活動中のメンバーを抽出
  dplyr::select(!c(joinDate, gradDate)) %>% # 不要な列を削除
  dplyr::left_join(
    member_df %>% 
      dplyr::distinct(memberID, .keep_all = TRUE) %>% # 重複を除去
      dplyr::mutate(HPjoinDate = lubridate::floor_date(HPjoinDate, unit  = "mon")), # 月単位に切り捨て
    by = "memberID"
  ) %>% # メンバー情報を結合
  dplyr::select(date, groupID, groupName, memberID, memberName, HPjoinDate) %>% # 利用する列を選択
  dplyr::mutate(
    moonage = lubridate::interval(start = HPjoinDate, end = date) %>% 
      lubridate::time_length(unit = "mon")
  ) %>% # メンバーの活動月数を計算
  dplyr::group_by(date, groupID, groupName) %>% # 平均活動月数の計算用にグループ化
  dplyr::summarise(
    moonage = sum(moonage), 
    member_n = dplyr::n(), 
    .groups = "drop"
  ) %>% # グループの総活動月数と(計算に使った)メンバー数を計算
  dplyr::mutate(average_moonage = moonage / member_n) %>% # グループの平均活動月数を計算
  dplyr::bind_rows(member_0_df) %>% # 結成前月・解散月を追加
  dplyr::arrange(date, average_moonage, groupID) %>% # ランク付け用に並べ替え
  dplyr::group_by(date) %>% # ランク付け用にグループ化
  dplyr::mutate(
    groupID = factor(groupID), 
    year = average_moonage %/% 12, 
    month = round(average_moonage %% 12, digits = 1), 
    ranking = dplyr::row_number(-average_moonage), 
  ) %>% # ランク付けとラベル用の値を追加
  dplyr::ungroup() %>% # グループ化の解除
  dplyr::select(date, groupID, groupName, average_moonage, year, month, ranking) %>% # 利用する列を選択
  dplyr::arrange(date, ranking) # 昇順に並べ替え
rank_df


# アニメーションの作成 --------------------------------------------------------------

### ・フレーム数の設定 -----

# 遷移フレーム数を指定
t <- 9

# 一時停止フレーム数を指定
s <- 1

# 1秒間に表示する月数を指定:(値が大きいと意図した通りにならない)
mps <- 3

# フレーム数を取得
n <- length(unique(rank_df[["date"]]))


### ・バーチャートレース -----

# バーチャートレースを作成:(y軸可変)
anim <- ggplot(rank_df, aes(x = ranking, y = average_moonage, fill = groupID, color = groupID)) + 
  geom_bar(stat = "identity", width = 0.9, alpha = 0.8) + # 平均活動月数バー
  geom_text(aes(y = 0, label = paste(groupName, " ")), hjust = 1) + # グループ名ラベル
  geom_text(aes(label = paste(" ", year, "年", month, "か月")), hjust = 0) + # 平均活動年数ラベル
  gganimate::transition_states(states = date, transition_length = t, state_length = s, wrap = FALSE) + # フレーム
  gganimate::ease_aes("cubic-in-out") + # アニメーションの緩急
  gganimate::view_follow(fixed_x = TRUE) + # 表示範囲のフィット
  coord_flip(clip = "off", expand = FALSE) + # 軸の入れ変え
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
    #panel.grid.minor.x = element_line(color = "grey", size = 0.1), # x軸の補助目盛線
    panel.grid.minor.y = element_blank(), # y軸の補助目盛線
    panel.border = element_blank(), # グラフ領域の枠線
    #panel.background = element_blank(), # グラフ領域の背景
    plot.title = element_text(color = "black", face = "bold", size = 20, hjust = 0.5), # 全体のタイトル
    plot.subtitle = element_text(color = "black", size = 15, hjust = 0.5), # 全体のサブタイトル
    plot.margin = margin(t = 10, r = 100, b = 10, l = 150, unit = "pt"), # 全体の余白
    legend.position = "none" # 凡例の表示位置
  ) + # 図の体裁
  labs(
    title = "ハロプログループの平均活動年数の推移", 
    subtitle = "{lubridate::year(closest_state)}年{lubridate::month(closest_state)}月", 
    caption = "データ:「https://github.com/xxgentaroxx/HP_DB」"
  ) # ラベル


# gif画像を作成
g <- gganimate::animate(
  plot = anim, 
  nframes = n*(t+s), fps = (t+s)*mps, 
  width = 900, height = 600
)
g

# gif画像を保存
gganimate::anim_save(filename = "BarChartRace/output/AverageExperience.gif", animation = g)


# 動画を作成と保存
m <- gganimate::animate(
  plot = anim, 
  nframes = n*(t+s), fps = (t+s)*mps, 
  width = 900, height = 600, 
  renderer = gganimate::av_renderer(file = "BarChartRace/output/AverageExperience.mp4")
)


warnings()


# 月を指定して作図 ----------------------------------------------------------------

# 月を指定
date_val <- "2021-05-01"

# 作図用のデータを抽出
tmp_rank_df <- rank_df %>% 
  dplyr::filter(date == lubridate::as_date(date_val))

# 平均活動年数の最大値を取得
y_max <- max(tmp_rank_df[["average_moonage"]]) %/% 12
y_max

# 棒グラフを作成
graph <- ggplot(tmp_rank_df, aes(x = ranking, y = average_moonage, fill = groupID, color = groupID)) + 
  geom_bar(stat = "identity", width = 0.9, alpha = 0.8) + # 平均活動月数バー
  geom_text(aes(y = 0, label = paste(groupName, " ")), hjust = 1) + # グループ名ラベル
  geom_text(aes(y = 0, label = paste(" ", year, "年", month, "か月")), hjust = 0, color = "white") + # 平均活動年数ラベル
  coord_flip(clip = "off", expand = FALSE) + # 軸の入れ変え
  scale_x_reverse(breaks = 1:nrow(tmp_rank_df)) + # x軸(縦軸)を反転
  scale_y_continuous(breaks = 0:y_max*12, labels = 0:y_max) + # y軸(横軸)のラベル
  theme(
    axis.title.y = element_blank(), # y軸のラベル
    axis.text.y = element_blank(), # y軸の目盛ラベル
    axis.ticks.x = element_blank(), # x軸の目盛指示線
    #panel.grid.major.x = element_line(color = "grey", size = 0.1), # x軸の主目盛線
    panel.grid.major.y = element_blank(), # y軸の主目盛線
    panel.grid.minor.x = element_blank(), # x軸の補助目盛線
    panel.grid.minor.y = element_blank(), # y軸の補助目盛線
    panel.border = element_blank(), # グラフ領域の枠線
    #panel.background = element_blank(), # グラフ領域の背景
    plot.title = element_text(color = "black", face = "bold", size = 20, hjust = 0.5), # 全体のタイトル
    plot.subtitle = element_text(color = "black", size = 15, hjust = 0.5), # 全体のサブタイトル
    plot.margin = margin(t = 10, r = 50, b = 10, l = 150, unit = "pt"), # 全体の余白
    legend.position = "none" # 凡例の表示位置
  ) + # 図の体裁
  labs(
    title = "ハロプログループの平均活動年数", 
    subtitle = paste0(lubridate::year(date_val), "年", lubridate::month(date_val), "月時点"), 
    y = "年数", 
    caption = "データ:「https://github.com/xxgentaroxx/HP_DB」"
  ) # ラベル
graph

# 画像を保存
ggplot2::ggsave(
  filename = paste0("BarChartRace/output/AverageExperience_", date_val, ".png"), plot = graph, 
  width = 24, height = 18, units = "cm", dpi = 100
)

