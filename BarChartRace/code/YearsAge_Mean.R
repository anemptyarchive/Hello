
# グループごとの平均年齢の推移を可視化 -------------------------------------------------------------------

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

# メンバー一覧を読み込み
member_df <- readr::read_csv(
  file = paste0(dir_path, "member.csv"), 
  col_types = readr::cols(
    memberID = "i", 
    memberName = "c", 
    HPjoinDate = readr::col_date(format = "%Y/%m/%d"), 
    debutDate = readr::col_date(format = "%Y/%m/%d"), 
    HPgradDate = readr::col_date(format = "%Y/%m/%d"), 
    memberKana = "c", 
    birthDate = readr::col_date(format = "%Y/%m/%d")
  )
) |> 
  dplyr::arrange(memberID) # 昇順に並べ替え
member_df

# 加入・卒業日一覧を読み込み
join_df <- readr::read_csv(
  file = paste0(dir_path, "join.csv"), 
  col_types = readr::cols(
    memberID = "i", 
    groupID = "i", 
    joinDate = readr::col_date(format = "%Y/%m/%d"), 
    gradDate = readr::col_date(format = "%Y/%m/%d")
  )
) |> 
  dplyr::arrange(joinDate, memberID, groupID) # 昇順に並べ替え
join_df

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
) |> 
  dplyr::arrange(groupID, formDate) # 昇順に並べ替え
group_df


# 期間の設定 -------------------------------------------------------------------

# 集計期間を指定
date_from <- "1997-09-01"
date_to   <- "2022-06-20"
date_to   <- lubridate::today()

# 月ベクトルを作成
date_vec <- seq(
  from = date_from |> 
    lubridate::as_date() |> 
    lubridate::floor_date(unit = "mon"), 
  to = date_to |> 
    lubridate::as_date() |> 
    lubridate::floor_date(unit = "mon"), 
  by = "mon"
)
length(date_vec) # フレーム数


# 演出用の処理 ------------------------------------------------------------------

### ・グループ名の対応:(改名組の表示名に対応したい) -----

# 月・グループID・グループ名の対応表を作成
group_name_df <- group_df |> 
  dplyr::mutate(
    formDate = formDate |> 
      lubridate::floor_date(unit = "mon"), 
    dissolveDate = dissolveDate %>% 
      dplyr::if_else(
        condition = is.na(.), 
        true = lubridate::today(), 
        false = dissolveDate
      ) |> # 現在活動中なら現在の日付
      lubridate::floor_date(unit = "mon"), 
    n = lubridate::interval(start = formDate, end = dissolveDate) |> 
      lubridate::time_length(unit = "mon") + 1
  ) |> # 月単位に切り捨てて月数をカウント
  tidyr::uncount(n) |> # 月数に応じて行を複製
  dplyr::group_by(groupName) |> # 行番号用にグループ化
  dplyr::mutate(idx = dplyr::row_number()) |> # 行番号を割り当て
  dplyr::group_by(groupName, idx) |> # 1か月刻みの値の作成用にグループ化
  dplyr::mutate(date = seq(from = formDate, to = dissolveDate, by = "mon")[idx]) |> # 複製した行を1か月刻みの値に変更
  dplyr::group_by(date, groupID) |> # 重複の除去用にグループ化
  dplyr::slice_max(formDate) |> # 重複する場合は新しい方を抽出
  dplyr::ungroup() |> # グループ化を解除
  dplyr::select(date, groupID, groupName, formDate, dissolveDate) |> # 利用する列を選択
  dplyr::arrange(date, groupID) # 昇順に並べ替え
group_name_df


### ・結成前月と解散月の追加:(バーの変化を強調したい) -----

# 結成前月・解散月のデータを作成
member_0_df <- group_df |> 
  dplyr::group_by(groupID) |> # 日付の再設定用にグループ化
  dplyr::mutate(dissolveDate = dplyr::lead(dissolveDate, n = max(dplyr::n())-1)) |> # 最後の行を1行目にズラす
  dplyr::slice_head(n = 1) |> # 1行目を抽出
  dplyr::ungroup() |> # グループ化を解除
  dplyr::mutate(
    formDate = formDate |> 
      lubridate::rollback() |> # 結成1か月前に変更
      lubridate::floor_date(unit = "mon"), 
    dissolveDate = dissolveDate |> 
      lubridate::floor_date(unit = "mon")
  ) |> # 月単位に切り捨て
  tidyr::pivot_longer(
    cols = c(formDate, dissolveDate), 
    names_to = "date_type", 
    values_to = "date"
  ) |> # 結成前月・解散月を同じ列に変形
  dplyr::select(date, groupID) |> # 利用する列を選択
  dplyr::filter(!is.na(date)) |> # 現在活動中のグループの解散月を除去
  tibble::add_column(
    groupName = " ", 
    age_total = 0, 
    member_n = 0, 
    age_mean = 0
  ) |> # メンバー数(0人)を追加
  dplyr::filter(date > min(date_vec), date < max(date_vec)) |> # 指定した期間内のデータを抽出
  dplyr::arrange(date, groupID) # 昇順に並び替え
member_0_df


# 集計と順位付け ----------------------------------------------------------------------

# サイズを取得
group_size  <- max(group_df[["groupID"]])
member_size <- max(member_df[["memberID"]])

# 平均年齢を集計
rank_df <- tidyr::expand_grid(
  date = date_vec, 
  groupID = 1:group_size, 
  memberID = 1:member_size
) |> # 全ての組み合わせを作成
  dplyr::left_join(group_name_df, by = c("date", "groupID")) |> # グループ情報を結合
  dplyr::filter(date >= formDate, date <= dissolveDate) |> # 活動中のグループを抽出
  dplyr::select(!c(formDate, dissolveDate)) |> # 不要な列を削除
  dplyr::left_join(
    join_df |> 
      dplyr::mutate(
        joinDate = lubridate::floor_date(joinDate, unit = "mon"), 
        gradDate = lubridate::floor_date(gradDate, unit = "mon")
      ), # 月単位に切り捨て
    by = c("groupID", "memberID")
  ) |> # 所属メンバー情報を結合
  dplyr::filter(date >= joinDate, date < gradDate | is.na(gradDate)) |> # 活動中のメンバーを抽出
  dplyr::select(!c(joinDate, gradDate)) |> # 不要な列を削除
  dplyr::left_join(
    member_df |> 
      dplyr::distinct(memberID, .keep_all = TRUE), # 重複を除去
    by = "memberID"
  ) |> # メンバー情報を結合
  dplyr::select(date, groupID, groupName, memberID, memberName, birthDate) |> # 利用する列を選択
  dplyr::mutate(
    age = lubridate::interval(start = birthDate, end = date) |> 
      lubridate::time_length(unit = "year") |> 
      floor()
  ) |> # メンバーの年齢を計算
  dplyr::group_by(date, groupID, groupName) |> # 平均年齢の計算用にグループ化
  dplyr::summarise(
    age_total = sum(age, na.rm = TRUE), 
    member_n = sum(!is.na(birthDate)), 
    .groups = "drop"
  ) |> # グループの合計年齢と(計算に使った)メンバー数を計算
  dplyr::mutate(age_mean = age_total / member_n) |> # グループの平均年齢を計算
  dplyr::bind_rows(member_0_df) |> # 結成前月・解散月を追加
  dplyr::arrange(date, age_mean, groupID) |> # 順位付け用に並べ替え
  dplyr::group_by(date) |> # 順位付け用にグループ化
  dplyr::mutate(
    groupID = factor(groupID), 
    ranking = dplyr::row_number(-age_mean), 
  ) |> # 順位を追加
  dplyr::ungroup() |> # グループ化を解除
  dplyr::select(date, groupID, groupName, age_mean, ranking) |> # 利用する列を選択
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


### ・バーチャートレース -----

# バーチャートレースを作成:(y軸可変)
anim <- ggplot(rank_df, aes(x = ranking, y = age_mean, fill = groupID, color = groupID)) + 
  geom_bar(stat = "identity", width = 0.9, alpha = 0.8) + # 平均年齢バー
  geom_text(aes(label = paste(" ", round(age_mean, digits = 1), "歳")), hjust = 0) + # 平均年齢ラベル
  geom_text(aes(y = 0, label = paste(groupName, " ")), hjust = 1) + # グループ名ラベル
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
    plot.margin = margin(t = 10, r = 60, b = 10, l = 120, unit = "pt"), # 全体の余白
    legend.position = "none" # 凡例の表示位置
  ) + # 図の体裁
  labs(
    title = "ハロプログループの平均年齢の推移", 
    subtitle = paste0(
      "{lubridate::year(closest_state)}年", 
      "{stringr::str_pad(lubridate::month(closest_state), width = 2, pad = 0)}月", 
      "01日時点"
    ), 
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
gganimate::anim_save(filename = "BarChartRace/output/YearsAge_Mean.gif", animation = g)


# 動画を作成と保存
m <- gganimate::animate(
  plot = anim, 
  nframes = n*(t+s), fps = (t+s)*mps, 
  width = 900, height = 600, 
  renderer = gganimate::av_renderer(file = "BarChartRace/output/YearsAge_Mean.mp4")
)


warnings()


# 月を指定して作図 ----------------------------------------------------------------

# 月(月初の日付)を指定
date_val <- "2014-01-01"

# 作図用のデータを抽出
mon_rank_df <- rank_df |> 
  dplyr::filter(date == lubridate::as_date(date_val))

# 棒グラフを作成
graph <- ggplot(mon_rank_df, aes(x = ranking, y = age_mean, fill = groupID, color = groupID)) + 
  geom_bar(stat = "identity", width = 0.9, alpha = 0.8) + # 平均年齢バー
  geom_text(aes(y = 0, label = paste(" ", round(age_mean, digits = 1), "歳")), hjust = 0, color = "white") + # 平均年齢ラベル
  geom_text(aes(y = 0, label = paste(groupName, " ")), hjust = 1) + # グループ名ラベル
  coord_flip(clip = "off", expand = FALSE) + # 軸の入れ変え
  scale_x_reverse(breaks = 1:nrow(mon_rank_df)) + # x軸(縦軸)目盛を反転
  theme(
    axis.title.y = element_blank(), # y軸のラベル
    axis.text.y = element_blank(), # y軸の目盛ラベル
    #panel.grid.major.x = element_line(color = "grey", size = 0.1), # x軸の主目盛線
    panel.grid.major.y = element_blank(), # y軸の主目盛線
    #panel.grid.minor.x = element_line(color = "grey", size = 0.1), # x軸の補助目盛線
    panel.grid.minor.y = element_blank(), # y軸の補助目盛線
    panel.border = element_blank(), # グラフ領域の枠線
    #panel.background = element_blank(), # グラフ領域の背景
    plot.title = element_text(color = "black", face = "bold", size = 20, hjust = 0.5), # 全体のタイトル
    plot.subtitle = element_text(color = "black", size = 15, hjust = 0.5), # 全体のサブタイトル
    plot.margin = margin(t = 10, r = 20, b = 10, l = 120, unit = "pt"), # 全体の余白
    legend.position = "none" # 凡例の表示位置
  ) + # 図の体裁
  labs(
    title = "ハロプログループの平均年齢", 
    subtitle = paste0(
      lubridate::year(date_val), "年", lubridate::month(date_val), "月1日時点"), 
    y = "年齢", 
    caption = "データ:「https://github.com/xxgentaroxx/HP_DB」"
  ) # ラベル
graph

# 画像を保存
ggplot2::ggsave(
  filename = paste0("BarChartRace/output/YearsAge_Mean_", date_val, ".png"), plot = graph, 
  width = 24, height = 18, units = "cm", dpi = 100
)


