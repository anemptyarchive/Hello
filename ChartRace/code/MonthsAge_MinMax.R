
# グループごとの最年少・最年長メンバー月齢の推移 -------------------------------

# 利用パッケージ
library(tidyverse)
library(gganimate)

# パッケージを読込
library(ggplot2)


# データの読込 -----------------------------------------------------------------

## ReadData.Rを参照

# 利用データを確認
group_df  # グループ一覧
join_df   # 加入・卒業日一覧
member_df # メンバー一覧

# 前処理データを確認
group_name_df # 活動月, グループID, グループ名の対応表
outside_df    # 結成前月, 解散翌月の一覧:(バーの変化の強調用)

# 集計期間を確認
date_min; date_max


# バーチャートレースの作成 -----------------------------------------------------

### データの集計 -----

# 最年少・最年長を指定:(min, max)
MinMax_flag <- "min"
MinMax_flag <- "max"

# 結成前月, 解散翌月のデータを調整
outside_df <- outside_df |> 
  dplyr::mutate(
    groupName  = " ", 
    member_age = 0
  ) # 疑似集計データを追加
outside_df

# 最小or最大月齢, 順位を集計
rank_df <- group_name_df |> # 活動月, グループ名, 結成(改名)月, 解散(改名)月
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
      lubridate::time_length(unit = "month") |> 
      floor() # メンバー月齢
  ) |> 
  dplyr::summarise(
    member_age = dplyr::if_else(
      condition = MinMax_flag == "min", 
      true  = min(member_age, na.rm = TRUE), # 最小月齢
      false = max(member_age, na.rm = TRUE), # 最大月齢
    ), 
    .by = c(date, groupID, groupName)
  ) |> 
  dplyr::bind_rows(
    outside_df # 結成前月, 解散翌月
  ) |> 
  dplyr::arrange(date, member_age, groupID) |> # 順位付け用
  dplyr::mutate(
    age_years  = member_age %/% 12, # グループ最小・最大年齢
    age_months = member_age %% 12,  # グループ最小・最大月齢 - 最小・最大年齢
    ranking    = dplyr::row_number(-member_age), # 順位
    .by = date
  ) |> 
  dplyr::select(date, groupID, groupName, member_age, age_years, age_months, ranking) |> 
  dplyr::arrange(date, ranking) # 昇順
rank_df


### アニメーションの作成 -----

# 遷移フレーム数を指定
t <- 9

# 一時停止フレーム数を指定
s <- 1

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
    mapping = aes(y = member_age), 
    stat = "identity", width = 0.9, alpha = 0.8
  ) + # 月齢バー
  geom_text(
    mapping = aes(y = member_age, label = paste(" ", age_years, "歳", age_months, "か月")), 
    hjust = 0
  ) + # 月齢ラベル
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
    panel.border       = element_blank(), # グラフ領域の枠線
    plot.title    = element_text(color = "black", face = "bold", size = 20, hjust = 0.5), # 図タイトル
    plot.subtitle = element_text(color = "black", size = 15, hjust = 0.5), # 図サブタイトル
    plot.margin   = margin(t = 10, r = 90, b = 10, l = 120, unit = "pt"), # 図の余白
    legend.position = "none" # 凡例の位置
  ) + 
  labs(
    title = ifelse(
      test = MinMax_flag == "min", 
      yes  = "ハロプログループ・ユニットの最年少メンバーの月齢の推移", 
      no   = "ハロプログループ・ユニットの最年長メンバーの月齢の推移"
    ), 
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
  renderer = gganimate::av_renderer(file = paste0("ChartRace/output/MonthsAge_", MinMax_flag, ".mp4"))
)


# バーチャートの作成 -----------------------------------------------------------

### データの集計 -----

# 集計日を指定
date_val <- "2014-01-01" |> 
  lubridate::as_date()
date_val <- lubridate::today()

# 最年少・最年長を指定:(min, max)
MinMax_flag <- "min"
MinMax_flag <- "max"

# 最小or最大月齢, 順位を集計
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
      lubridate::time_length(unit = "month") |> 
      floor() # メンバー月齢
  ) |> 
  dplyr::summarise(
    member_age = dplyr::if_else(
      condition = MinMax_flag == "min", 
      true  = min(member_age, na.rm = TRUE), # 最小月齢
      false = max(member_age, na.rm = TRUE), # 最大月齢
    ), 
    member_num = dplyr::n(), # グループメンバー数
    .by = c(date, groupID, groupName)
  ) |> 
  dplyr::arrange(date, member_age, groupID) |> # 順位付け用
  dplyr::mutate(
    age_years  = member_age %/% 12, # グループ最小・最大年齢
    age_months = member_age %% 12,  # グループ最小・最大月齢 - 最小・最大年齢
    ranking    = dplyr::row_number(-member_age) # 順位
  ) |> 
  dplyr::select(date, groupID, groupName, member_num, member_age, age_years, age_months, ranking) |> 
  dplyr::arrange(ranking) # 昇順
rank_month_df


### グラフの作成 -----

# 年齢の最大値を取得
years_max <- max(rank_month_df[["member_age"]]) %/% 12

# 軸目盛の間隔を指定
tick_val <- 5

# バーチャートを作図
graph <- ggplot(
  data = rank_month_df, 
  mapping = aes(x = ranking, fill = factor(groupID), color = factor(groupID))
) + 
  geom_bar(
    mapping = aes(y = member_age), 
    stat = "identity", width = 0.9, alpha = 0.8
  ) + # 月齢バー
  geom_text(
    mapping = aes(
      y = 0, 
      label = paste0("  ", age_years, "歳", age_months, "か月", " (", member_num, "人)")
    ), 
    hjust = 0, color = "white"
  ) + # 月齢ラベル
  geom_text(
    mapping = aes(y = 0, label = paste(groupName, " ")), 
    hjust = 1
  ) + # グループ名ラベル
  coord_flip(clip = "off", expand = FALSE) + # 軸の入替
  scale_x_reverse(breaks = 1:nrow(rank_month_df)) + # ランク軸を反転
  scale_y_continuous(
    breaks = seq(from = 0, to = years_max, by = tick_val) * 12, 
    labels = seq(from = 0, to = years_max, by = tick_val)
  ) + # 年齢軸目盛
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
    title = ifelse(
      test = MinMax_flag == "min", 
      yes  = "ハロプログループ・ユニットの最年少メンバーの年齢", 
      no   = "ハロプログループ・ユニットの最年長メンバーの年齢"
    ), 
    subtitle = format(date_val, format = "%Y年%m月%d日時点"), 
    y = "年齢", 
    caption = "データ:「https://github.com/xxgentaroxx/HP_DB」"
  )
graph

# 画像を書出
ggplot2::ggsave(
  filename = paste0("ChartRace/output/MonthsAge_MinMax_", MinMax_flag, "_", date_val, ".png"), plot = graph, 
  width = 24, height = 18, units = "cm", dpi = 100
)


