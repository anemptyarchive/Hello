
# 総メンバー数の推移をラインチャートで可視化 ---------------------------------------------------


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


# 前処理 ---------------------------------------------------------------------

# 
group_df2 <- group_df |> 
  dplyr::filter(isUnit == FALSE) |> # ユニットを除去
  dplyr::mutate(
    formDate = formDate |> 
      lubridate::floor_date(unit = "mon"), 
    dissolveDate = dplyr::case_when(
        is.na(dissolveDate) ~ lubridate::today(), # 現在活動中であれば現在の日付
        groupName == "カントリー娘。" ~ lubridate::as_date("2007-01-28"), # 解散日を変更
        TRUE ~ dissolveDate
      ) |> 
      lubridate::floor_date(unit = "mon"), 
    n = lubridate::interval(start = formDate, end = dissolveDate) |> 
      lubridate::time_length(unit = "mon") + 1
  ) |> # 月数をカウント
  tidyr::uncount(n) |> # 月数に応じて行を複製
  dplyr::group_by(groupName) |> # 行番号用にグループ化
  dplyr::mutate(idx = dplyr::row_number()) |> # 行番号を割り当て
  dplyr::group_by(groupName, idx) |> # 1か月刻みの値の作成用にグループ化
  dplyr::mutate(date = seq(from = formDate, to = dissolveDate, by = "mon")[idx]) |> # 複製した行を1か月刻みの値に変更
  dplyr::group_by(date, groupID) |> # 重複の除去用にグループ化
  dplyr::slice_max(formDate) |> # 重複する場合は新しい方を抽出
  dplyr::ungroup() |> # グループ化を解除
  dplyr::arrange(date, groupID) |> # 昇順に並べ替え
  dplyr::mutate(
    group_idname = groupName |> 
      stringr::str_replace(pattern = "モーニング娘。.*", replacement = "モーニング娘。") |> # ナンバリングを削除
      stringr::str_replace(pattern = "T&Cボンバー", replacement = "太陽とシスコムーン") |> # 改名前に統一
      stringr::str_replace(pattern = "ココナッツ娘$", replacement = "ココナッツ娘。") |> # 。を追加
      stringr::str_replace(pattern = "SI☆NA", replacement = "ハロプロ関西") |> # 改名前に統一
      stringr::str_replace(pattern = "S/mileage", replacement = "スマイレージ") |> # 改名後に統一
      stringr::str_replace(pattern = "アンジュルム", replacement = "スマイレージ") %>% # 改名前に統一
      factor(levels = unique(.)), 
    group_id = dplyr::dense_rank(group_idname) %>% 
      factor()
  ) |> 
  dplyr::select(date, groupID, group_id, groupName) # 利用する列を選択
group_df2



# 期間の設定 -------------------------------------------------------------------

# 期間を指定
date_from <- "1997-09-01"
date_to   <- "2022-07-01"
date_to   <- lubridate::today()

# 月単位に切り捨て
date_from <- date_from |> 
  lubridate::as_date() |> # Date型に変換
  lubridate::floor_date(unit = "mon") # 月単位に切り捨て
date_to <- date_to |> 
  lubridate::as_date() |> # Date型に変換
  lubridate::floor_date(unit = "mon") # 月単位に切り捨て

# メンバー数の集計 ----------------------------------------------------------------

### ・順位付け -----

# グループIDを取得
groupID_vec <- group_df |> 
  dplyr::filter(isUnit == FALSE) |> # ユニットを除去
  dplyr::pull(groupID) |> # ベクトルとして抽出
  unique() # 重複を削除

# ハロプロ研修生
init_kenshusei_n <- join_df |> 
  dplyr::filter(
    groupID == 18, 
    joinDate < lubridate::as_date("2011-12-01"), 
    gradDate >= lubridate::as_date("2011-12-01")
  ) |> 
  nrow()


# メンバー数を集計してランク付け
rank_df <- join_df %>% 
  dplyr::filter(groupID %in% groupID_vec) |> 
  dplyr::mutate(date = lubridate::floor_date(joinDate, unit = "mon")) |> # 月単位に切り捨て
  dplyr::group_by(date, groupID) |> # 加入数の集計用にグループ化
  dplyr::count(name = "join_n") |> # 加入数を集計
  dplyr::ungroup() |> # グループ化を解除
  dplyr::right_join(group_df2, by = c("date", "groupID")) |> # 全期間に統合
  dplyr::mutate(
    join_n = tidyr::replace_na(join_n, replace = 0), 
    join_n = dplyr::if_else(and(groupID == 18, date == lubridate::as_date("2011-12-01")), true = init_kenshusei_n, false = join_n)
  ) |> # 加入なしの場合の欠損値を0に置換
  dplyr::arrange(date) |> # 総メンバー数の集計用に並べ替え
  dplyr::group_by(group_id) |> # 総メンバー数の集計用にグループ化
  dplyr::mutate(member_n = cumsum(join_n)) |> # 総メンバー数を集計
  dplyr::ungroup() #|> # グループ化を解除
  dplyr::filter(date < lubridate::as_date("2010-01-01")) |> 
  #dplyr::filter(member_n > 0) %>% # 活動中のグループを抽出
  dplyr::select(date, groupID, member_n) %>% # 利用する列を選択
  #dplyr::bind_rows(member_0_df) %>% # 在籍なし期間を結合
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




point_df <- rank_df |> 
  dplyr::group_by(group_id) |> 
  dplyr::filter(date == min(date) | date == max(date), date != lubridate::floor_date(lubridate::today(), unit = "mon")) |> 
  dplyr::mutate(date_type = dplyr::if_else(date == min(date), true = "form", false = "dissolve")) |> 
  dplyr::ungroup()
label_df <- rank_df |> 
  dplyr::group_by(group_id) |> 
  dplyr::filter(date == max(date)) |> 
  dplyr::ungroup() |> 
  dplyr::mutate(label = paste0("", groupName, "：", member_n, "人"))

date_vec <- seq(
  from = date_from |> 
    lubridate::floor_date(unit = "year"), 
  to = date_to |> 
    lubridate::ceiling_date(unit = "year"), 
  by = "year"
)
stringr::str_remove(date_vec, pattern = "-01$")

group_size <- max(as.integer(rank_df[["group_id"]]))
ggplot() + 
  geom_line(data = rank_df, mapping = aes(x = date, y = member_n, color = factor(group_id, levels = sample(1:group_size, size = group_size))), 
            size = 1, alpha = 0.5) + # メンバー数の推移
  geom_point(data = point_df, mapping = aes(x = date, y = member_n, color = group_id), 
             size = 2, alpha = 0.5) + # 結成・解散日の点
  #scale_shape_manual(values = c(form = 17, dissolve = 6)) + # 結成・解散日の点
  ggrepel::geom_label_repel(data = label_df, mapping = aes(x = date, y = member_n, color = group_id, label = label), 
                            max.overlaps = Inf, min.segment.length = 0, size = 3, alpha = 0.8, 
                            xlim = c(-Inf, Inf), ylim = c(-Inf, Inf)) + # グループ名・メンバー数ラベル
  scale_x_date(breaks = date_vec, date_labels = "%Y-%m", guide = guide_axis(angle = 45)) + # x軸目盛
  coord_cartesian(clip = "off") + # 表示範囲
  theme(
    plot.title = element_text(color = "black", face = "bold", size = 20, hjust = 0.5), # 全体のタイトル
    plot.subtitle = element_text(color = "black", size = 15, hjust = 0.5), # 全体のサブタイトル
    plot.margin = margin(t = 10, r = 80, b = 10, l = 20, unit = "pt"), # 全体の余白
    legend.position = "none" # 凡例の表示位置
  ) + # 図の体裁
  labs(
    title = "ハロプログループの総メンバー数の推移", 
    subtitle = paste0(format(date_from, format = "%Y年%m月"), "～", format(date_to, format = "%Y年%m月")), 
    x = "年-月", y = "メンバー数", 
    caption = "データ:「https://github.com/xxgentaroxx/HP_DB」"
  ) # ラベル



# フレーム数を取得
n <- length(unique(rank_df[["date"]]))

num_vec <- seq(from = 0, to = max(rank_df[["member_n"]]), by = 10)

# ラインチャートを作成
anim <- ggplot(rank_df, aes(x = date, y = member_n, color = factor(group_id, levels = sample(1:group_size, size = group_size)))) + 
  geom_hline(yintercept = c(0, 10), color = "white", size = 0.5) + 
  geom_line(size = 1, alpha = 0.5) + # メンバー数の推移
  geom_point(size = 2, alpha = 0.5) + # メンバー数の点
  ggrepel::geom_label_repel(mapping = aes(label = paste0("", groupName, "：", member_n, "人")), 
                            max.overlaps = Inf, min.segment.length = 0, alpha = 0.8, 
                            xlim = c(-Inf, Inf), ylim = c(-Inf, Inf), seed = 86) + # グループ名・メンバー数ラベル
  gganimate::transition_reveal(date) + # フレーム
  gganimate::view_follow() + # 表示範囲のフィット
  scale_x_date(breaks = date_vec, date_labels = "%Y-%m", guide = guide_axis(angle = 45)) + # x軸目盛
  scale_y_continuous(breaks = num_vec) + 
  #scale_color_brewer(palette = "Set3") + 
  coord_cartesian(clip = "off") + # 表示範囲
  theme(
    plot.title = element_text(color = "black", face = "bold", size = 20, hjust = 0.5), # 全体のタイトル
    plot.subtitle = element_text(color = "black", size = 15, hjust = 0.5), # 全体のサブタイトル
    plot.margin = margin(t = 10, r = 120, b = 40, l = 40, unit = "pt"), # 全体の余白
    legend.position = "none" # 凡例の表示位置
  ) + # 図の体裁
  labs(
    title = "ハロプログループの総メンバー数の推移", 
    subtitle = paste0(format(date_from, format = "%Y年%m月"), "～", "{format(frame_along, format = '%Y年%m月')}"), 
    x = "年-月", y = "メンバー数", 
    caption = "データ:「https://github.com/xxgentaroxx/HP_DB」"
  ) # ラベル

# gif画像を作成
gganimate::animate(anim, nframes = n+41, end_pause = 10, fps = 9, width = 1200, height = 800)


# 動画を作成と保存
m <- gganimate::animate(
  plot = anim, 
  nframes = n+10, end_pause = 10, fps = 6, 
  width = 1200, height = 800, 
  renderer = gganimate::av_renderer(file = "BarChartRace/output/Member_TotalNum.mp4")
)
rank_df[["date"]] |> 
  unique()


warnings()




fggplot2::ggsave(filename = "totalnum.png", width = 1200, height = 900, units = "px")
format(date_val, format = "%Y年%m月")
