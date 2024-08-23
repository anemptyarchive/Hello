
# グループごとの平均年齢の推移 -------------------------------------------------

# 利用パッケージ
library(tidyverse)
library(gganimate)

# パッケージを読込
library(ggplot2)


# データの読込 -----------------------------------------------------------------

## ReadData.Rを参照

# 利用データを確認
member_df # メンバー一覧
group_df  # グループ一覧
join_df   # 加入・卒業日一覧


# バーチャートレースの作成 -----------------------------------------------------

### 期間の設定 -----

# 集計期間を指定
date_min <- "1997-09-01" |> 
  lubridate::as_date() |> 
  lubridate::floor_date(unit = "month") # 集計開始(最小)月
date_max <- "2024-09-29" |> 
  lubridate::as_date() |> 
  lubridate::floor_date(unit = "month") # 集計終了(最大)月
date_max <- lubridate::today()|> 
  lubridate::floor_date(unit = "month") # 集計終了(最大)月


### データの集計 -----

# 活動月, グループID, グループ名の対応表を作成
group_name_df <- group_df |> 
  dplyr::mutate(
    date_from = formDate |> 
      lubridate::floor_date(unit = "month"), # 結成・改名月
    date_to   = dissolveDate |> 
      is.na() |> 
      dplyr::if_else(
        true  = max(lubridate::today(), date_max), # 活動中なら現在の日付or最大月
        false = dissolveDate
      ) |> 
      lubridate::floor_date(unit = "month") # 解散・改名・現在(最大)月
  ) |> 
  dplyr::reframe(
    date = seq(from = date_from, to = date_to, by = "month"), # 活動月
    .by = dplyr::everything()
  ) |> 
  dplyr::slice_min(formDate, n = 1, with_ties = FALSE, by = c(date, groupID)) |> # 月途中の改名なら重複するので改名前を抽出
  dplyr::select(date, groupID, groupName, formDate, dissolveDate) |> 
  dplyr::arrange(date, groupID) # 昇順
group_name_df

# 結成前月, 解散翌月のデータを作成:(バーの変化の強調用)
outside_df <- group_df |> 
  dplyr::summarise(
    formDate     = min(formDate),     # 結成日
    dissolveDate = max(dissolveDate), # 解散日
    .by = groupID
  ) |> # 改名情報を統合
  dplyr::mutate(
    date_pre = formDate |> 
      lubridate::floor_date(unit = "month"), # 結成月
    date_pre = (date_pre == formDate)|> 
      dplyr::if_else(
        true = date_pre |> 
          lubridate::rollback() |> 
          lubridate::floor_date(unit = "month"), # 結成月が月初なら結成前月
        false = date_pre
      ), 
    date_post = dissolveDate |> 
      lubridate::rollforward(roll_to_first = TRUE)  # 解散翌月
  ) |>　
  tidyr::pivot_longer(
    cols      = c(date_pre, date_post), 
    names_to  = "date_type", 
    values_to = "date"
  ) |> # 月列をまとめる
  dplyr::select(date, groupID) |> 
  dplyr::filter(!is.na(date)) |> # 活動中なら解散月を除去
  tibble::add_column(
    groupName = " ", 
    mean_age  = 0
  ) |> # 疑似集計データを追加
  dplyr::filter(dplyr::between(date, left = date_min, right = date_max)) |> # 集計期間の月を抽出
  dplyr::arrange(groupID, date) # 昇順
outside_df

# 平均年齢, 順位を集計
rank_df <- group_name_df |> # 活動月, グループ名, 結成(改名)月, 解散(改名)月
  dplyr::filter(dplyr::between(date, left = date_min, right = date_max)) |> # 集計期間の月を抽出
  dplyr::left_join(
    join_df |> # メンバーID, 加入日, 卒業日
      dplyr::mutate(
        gradDate = gradDate |> 
          is.na() |> 
          dplyr::if_else(
            true  = max(lubridate::today(), date_max), # 活動中なら現在の日付or最大月
            false = gradDate
          )
      ), 
    by = "groupID", relationship = "many-to-many"
  ) |> 
  dplyr::filter(dplyr::between(date, left = joinDate, right = gradDate)) |> # 活動中のメンバーを抽出
  dplyr::left_join(
    member_df |> # 生年月日
      dplyr::select(memberID, memberName, birthDate) |> 
      dplyr::distinct(memberID, .keep_all = TRUE), # 重複を除去
    by = "memberID"
  ) |> 
  dplyr::mutate(
    member_age = lubridate::interval(start = birthDate, end = date) |> 
      lubridate::time_length(unit = "year") |> 
      floor() # メンバー年齢
  ) |> 
  dplyr::summarise(
    totla_age  = sum(member_age, na.rm = TRUE), # (生年月日非公開を除く)グループ合計年齢
    member_num = sum(!is.na(birthDate)),        # (生年月日非公開を除く)グループメンバー数
    .by = c(date, groupID, groupName)
  ) |> 
  dplyr::mutate(
    mean_age = totla_age / member_num # グループ平均年齢
  ) |> 
  dplyr::bind_rows(
    outside_df # 結成前月, 解散翌月
  ) |> 
  dplyr::arrange(date, mean_age, groupID) |> # 順位付け用
  dplyr::mutate(
    ranking = dplyr::row_number(-mean_age), # 順位
    .by = date
  ) |> 
  dplyr::select(date, groupID, groupName, mean_age, ranking) |> 
  dplyr::arrange(date, ranking) # 昇順
rank_df


### アニメーションの作成 -----

# 遷移フレーム数を指定
t <- 8

# 一時停止フレーム数を指定
s <- 2

# 1秒間に表示する月数を指定:(値が大きいと意図した通りにならない)
mps <- 3

# フレーム数を取得
n <- length(unique(rank_df[["date"]]))

# バーチャートレースを作図:(y軸可変)
anim <- ggplot(
  data = rank_df, 
  mapping = aes(x = ranking, fill = factor(groupID), color = factor(groupID))
) + 
  geom_bar(
    mapping = aes(y = mean_age), 
    stat = "identity", width = 0.9, alpha = 0.8
  ) + # 年齢バー
  geom_text(
    mapping = aes(y = mean_age, label = paste(" ", round(mean_age, digits = 1), "歳")), 
    hjust = 0
  ) + # 年齢ラベル
  geom_text(
    mapping = aes(y = 0, label = paste(groupName, " ")), 
    hjust = 1
  ) + # グループ名ラベル
  gganimate::transition_states(states = date, transition_length = t, state_length = s, wrap = FALSE) + # フレーム切替
  gganimate::ease_aes("cubic-in-out") + # アニメーションの緩急
  gganimate::view_follow(fixed_x = TRUE) + # 表示範囲の可変
  coord_flip(clip = "off", expand = FALSE) + # 軸の入替
  scale_x_reverse() + # ランク軸を反転
  theme(
    axis.title = element_blank(), # 軸ラベル
    axis.text  = element_blank(), # 軸目盛ラベル
    axis.ticks = element_blank(), # 軸目盛指示線
    panel.grid.major.y = element_blank(), # y軸主目盛線
    panel.grid.minor.y = element_blank(), # y軸補助目盛線
    panel.border = element_blank(), # グラフ領域の枠線
    plot.title    = element_text(color = "black", face = "bold", size = 20, hjust = 0.5), # 図タイトル
    plot.subtitle = element_text(color = "black", size = 15, hjust = 0.5), # 図サブタイトル
    plot.margin   = margin(t = 10, r = 60, b = 10, l = 120, unit = "pt"), # 図の余白
    legend.position = "none" # 凡例の位置
  ) + 
  labs(
    title = "ハロプログループの平均年齢の推移", 
    subtitle = paste0(
      "{lubridate::year(closest_state)}年", 
      "{stringr::str_pad(lubridate::month(closest_state), width = 2, pad = 0)}月", 
      "01日時点"
    ), 
    caption = "データ:「https://github.com/xxgentaroxx/HP_DB」"
  )

# 動画を作成
m <- gganimate::animate(
  plot = anim, 
  nframes = (t+s)*n, fps = (t+s)*mps, 
  width = 900, height = 600, 
  renderer = gganimate::av_renderer(file = "ChartRace/output/YearsAge_mean.mp4")
)


# バーチャートの作成 -----------------------------------------------------------

### データの集計 -----

# 集計日を指定
date_val <- "2014-01-01" |> 
  lubridate::as_date()
date_val <- lubridate::today()

# 平均年齢, 順位を集計
rank_month_df <- group_df |> # グループ名, 結成(改名)月, 解散(改名)月
  dplyr::mutate(
    dissolveDate = dissolveDate |> 
      is.na() |> 
      dplyr::if_else(
        true  = max(lubridate::today(), date_val), # 活動中なら現在の日付or集計日
        false = dissolveDate
      ) # 解散・改名・現在(集計)日
  ) |> 
  tibble::add_column(
    date = date_val # 活動月
  ) |> 
  dplyr::filter(dplyr::between(date, left = formDate, right = dissolveDate)) |> # 活動中のグループを抽出
  dplyr::left_join(
    join_df |> # メンバーID, 加入日, 卒業日
      dplyr::mutate(
        gradDate = gradDate |> 
          is.na() |> 
          dplyr::if_else(
            true  = max(lubridate::today(), date_val), # 活動中なら現在の日付or集計日
            false = gradDate
          )
      ), 
    by = "groupID", relationship = "many-to-many"
  ) |> 
  dplyr::filter(dplyr::between(date, left = joinDate, right = gradDate)) |> # 活動中のメンバーを抽出
  dplyr::left_join(
    member_df |> # 生年月日
      dplyr::select(memberID, memberName, birthDate) |> 
      dplyr::distinct(memberID, .keep_all = TRUE), # 重複を除去
    by = "memberID"
  ) |> 
  dplyr::mutate(
    member_age = lubridate::interval(start = birthDate, end = date) |> 
      lubridate::time_length(unit = "year") |> 
      floor() # メンバー年齢
  ) |> 
  dplyr::summarise(
    totla_age  = sum(member_age, na.rm = TRUE), # (生年月日非公開を除く)グループ合計年齢
    member_num = sum(!is.na(birthDate)),        # (生年月日非公開を除く)グループメンバー数
    member_num_all = dplyr::n(),                # グループメンバー数
    .by = c(date, groupID, groupName)
  ) |> 
  dplyr::mutate(
    mean_age = totla_age / member_num # グループ平均年齢
  ) |> 
  dplyr::arrange(date, mean_age, groupID) |> # 順位付け用
  dplyr::mutate(
    ranking = dplyr::row_number(-mean_age) # 順位
  ) |> 
  dplyr::select(date, groupID, groupName, member_num = member_num_all, mean_age, ranking) |> 
  dplyr::arrange(ranking) # 昇順
rank_month_df


### グラフの作成 -----

# バーチャートを作図
graph <- ggplot(
  data = rank_month_df, 
  mapping = aes(x = ranking, fill = factor(groupID), color = factor(groupID))
) + 
  geom_bar(
    mapping = aes(y = mean_age), 
    stat = "identity", width = 0.9, alpha = 0.8
  ) + # 年齢バー
  geom_text(
    mapping = aes(y = 0, label = paste0("  ", round(mean_age, digits = 1), "歳", " (", member_num, "人)")), 
    hjust = 0, color = "white"
  ) + # 年齢ラベル
  geom_text(
    mapping = aes(y = 0, label = paste(groupName, " ")), 
    hjust = 1
  ) + # グループ名ラベル
  coord_flip(clip = "off", expand = FALSE) + # 軸の入替
  scale_x_reverse(breaks = 1:nrow(rank_month_df)) + # ランク軸を反転
  theme(
    axis.title.y = element_blank(), # y軸ラベル
    axis.text.y  = element_blank(), # y軸目盛ラベル
    panel.grid.major.y = element_blank(), # y軸主目盛線
    panel.grid.minor.y = element_blank(), # y軸補助目盛線
    panel.border       = element_blank(), # グラフ領域の枠線
    plot.title    = element_text(color = "black", face = "bold", size = 20, hjust = 0.5), # 図タイトル
    plot.subtitle = element_text(color = "black", size = 15, hjust = 0.5), # 図サブタイトル
    plot.margin   = margin(t = 10, r = 20, b = 10, l = 120, unit = "pt"), # 図の余白
    legend.position = "none" # 凡例の位置
  ) + 
  labs(
    title = "ハロプログループの平均年齢", 
    subtitle = format(date_val, format = "%Y年%m月%d日時点"), 
    y = "年齢", 
    caption = "データ:「https://github.com/xxgentaroxx/HP_DB」"
  )
graph


# 画像を書出
ggplot2::ggsave(
  filename = paste0("ChartRace/output/YearsAge_mean_", date_val, ".png"), plot = graph, 
  width = 24, height = 18, units = "cm", dpi = 100
)


