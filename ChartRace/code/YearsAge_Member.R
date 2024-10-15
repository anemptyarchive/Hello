
# グループメンバーの年齢の推移 -------------------------------------------------

# 利用パッケージ
library(tidyverse)
library(gganimate)

# パッケージを読込
library(ggplot2)


# 編集メモ --------------------------------------------------------------------

## 順位付けに日数を反映したい


# データの読込 -----------------------------------------------------------------

## ReadData.Rを参照

# 利用データを確認
join_df   # 加入・卒業日一覧
member_df # メンバー一覧


# 現役メンバーの年齢 -----------------------------------------------------------

## 加入から卒業までの年齢の推移

### 期間の設定 -----

# 集計期間を指定
date_min <- "1997-09-01" |> 
  lubridate::as_date() |> 
  lubridate::floor_date(unit = "month") # 集計開始(最小)月
date_max <- "2024-10-01" |> 
  lubridate::as_date() |> 
  lubridate::floor_date(unit = "month") # 集計終了(最大)月


### グループの設定 -----

# グループを指定
group_id   <- 1
group_name <- "モーニング娘。"


### 演出用データの作成 -----

# 加入前月, 卒業翌月のデータを作成:(バーの変化の強調用)
outside_df <- join_df |> 
  dplyr::filter(groupID == group_id) |> # グループを抽出
  dplyr::mutate(
    date_pre = (lubridate::day(joinDate) == 1) |> 
      dplyr::if_else(
        true  = joinDate |> 
          lubridate::rollback() |> 
          lubridate::floor_date(unit = "month"), # 加入日が月初なら加入前月
        false = joinDate |> 
          lubridate::floor_date(unit = "month") # 加入月
      ), 
    date_post = gradDate |> 
      lubridate::rollforward(roll_to_first = TRUE) # 卒業翌月
  ) |>　
  tidyr::pivot_longer(
    cols      = c(date_pre, date_post), 
    names_to  = "date_type", 
    values_to = "date"
  ) |> # 月列をまとめる
  dplyr::filter(!is.na(date)) |> # 活動中なら卒業月を除去
  dplyr::filter(dplyr::between(date, left = date_min, right = date_max)) |> # 集計期間の月を抽出
  dplyr::left_join(
    color_df |> # メンバーカラー
      dplyr::select(groupID, memberID, colorCode), 
    by = c("groupID", "memberID")
  ) |> 
  dplyr::mutate(
    memberName = " ", 
    member_age = 0
  ) |> # 疑似集計データを追加
  dplyr::select(date, memberID, memberName, colorCode, member_age) |> 
  dplyr::arrange(memberID, date) # 昇順
outside_df


### データの集計 -----

# 年齢, 順位を集計
rank_df <- join_df |> # メンバーID, 加入日, 卒業日
  dplyr::filter(groupID == group_id) |> # グループを抽出
  dplyr::mutate(
    gradDate = gradDate |> 
      is.na() |> 
      dplyr::if_else(
        true  = max(lubridate::today(), date_max), # 活動中なら現在の日付or最大月
        false = gradDate
      )
  ) |> 
  dplyr::left_join(
    member_df |> # 生年月日
      dplyr::select(memberID, memberName, birthDate) |> 
      dplyr::distinct(memberID, .keep_all = TRUE), # 重複を除去
    by = "memberID"
  ) |> 
  dplyr::left_join(
    color_df |> # メンバーカラー
      dplyr::select(groupID, memberID, colorCode), 
    by = c("groupID", "memberID")
  ) |> 
  dplyr::mutate(
    date_from = (lubridate::day(joinDate) == 1) |> 
      dplyr::if_else(
        true  = joinDate |> 
          lubridate::floor_date(unit = "month"), # 加入月
        false = joinDate |> 
          lubridate::rollforward(roll_to_first = TRUE) # 加入日が月初なら加入翌月
      ), 
    date_to = gradDate |> 
      lubridate::floor_date(unit = "month") # 卒業月
  ) |> 
  dplyr::reframe(
    date = seq(from = date_from, to = date_to, by = "month"), # 活動月
    .by = c(memberID, groupID, joinDate, gradDate, memberName, birthDate, colorCode)
  ) |> 
  dplyr::filter(dplyr::between(date, left = date_min, right = date_max)) |> # 集計期間を抽出
  dplyr::mutate(
    member_age = lubridate::interval(start = birthDate, end = date) |> 
      lubridate::time_length(unit = "year") |> 
      floor() # メンバー年齢
  ) |> 
  dplyr::bind_rows(
    outside_df # 加入前月, 卒業翌月
  ) |> 
  dplyr::arrange(date, -member_age, birthDate, memberID) |> # 順位付け用
  dplyr::mutate(
    ranking = dplyr::row_number(), # 順位
    .by = date
  ) |> 
  dplyr::select(date, memberID, memberName, colorCode, member_age, ranking) |> 
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
  mapping = aes(x = ranking, group = factor(memberID))
) + 
  geom_bar(
    mapping = aes(y = member_age, fill = colorCode, color = colorCode), 
    stat = "identity", width = 0.9, alpha = 0.8
  ) + # 年齢バー
  geom_text(
    mapping = aes(y = member_age, label = paste(" ", member_age, "歳")), 
    hjust = 0
  ) + # 年齢ラベル
  geom_text(
    mapping = aes(y = 0, label = paste(memberName, " ")), 
    hjust = 1
  ) + # メンバー名ラベル
  gganimate::transition_states(states = date, transition_length = t, state_length = s, wrap = FALSE) + # フレーム切替
  gganimate::ease_aes("cubic-in-out") + # アニメーションの緩急
  gganimate::view_follow(fixed_x = TRUE) + # 表示範囲の可変
  coord_flip(clip = "off", expand = FALSE) + # 軸の入替
  scale_x_reverse() + # ランク軸を反転
  scale_fill_identity() + # カラーコードを設定
  scale_color_identity() + # カラーコードを設定
  theme(
    axis.title = element_blank(), # 軸ラベル
    axis.text  = element_blank(), # 軸目盛ラベル
    axis.ticks = element_blank(), # 軸目盛指示線
    panel.grid.major.y = element_blank(), # y軸主目盛線
    panel.grid.minor.y = element_blank(), # y軸補助目盛線
    panel.border       = element_blank(), # グラフ領域の枠線
    plot.title    = element_text(color = "black", face = "bold", size = 20, hjust = 0.5), # 図タイトル
    plot.subtitle = element_text(color = "black", size = 15, hjust = 0.5), # 図サブタイトル
    plot.margin   = margin(t = 10, r = 60, b = 10, l = 120, unit = "pt"), # 図の余白
    legend.position = "none" # 凡例の位置
  ) + 
  labs(
    title = paste0(group_name, "メンバーの年齢の推移"), 
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
  renderer = gganimate::av_renderer(file = paste0("ChartRace/output/YearsAge/YearsAge_Member_", group_id, ".mp4"))
)


# 歴代メンバーの在籍時の年齢 ---------------------------------------------------

## 加入から卒業以降も含めた年齢の推移

### 期間の設定 -----

# 集計期間を指定
date_min <- "1997-09-01" |> 
  lubridate::as_date() |> 
  lubridate::floor_date(unit = "month") # 集計開始(最小)月
date_max <- "2024-10-01" |> 
  lubridate::as_date() |> 
  lubridate::floor_date(unit = "month") # 集計終了(最大)月


### グループの設定 -----

# グループを指定
group_id   <- 1
group_name <- "モーニング娘。"


### 演出用データの作成 -----

# 加入前月のデータを作成:(バーの変化の強調用)
outside_df <- join_df |> 
  dplyr::filter(groupID == group_id) |> # グループを抽出
  dplyr::mutate(
    date = (lubridate::day(joinDate) == 1) |> 
      dplyr::if_else(
        true  = joinDate |> 
          lubridate::rollback() |> 
          lubridate::floor_date(unit = "month"), # 加入日が月初なら加入前月
        false = joinDate |> 
          lubridate::floor_date(unit = "month") # 加入月
      )
  ) |>　
  dplyr::filter(dplyr::between(date, left = date_min, right = date_max)) |> # 集計期間の月を抽出
  dplyr::left_join(
    color_df |> # メンバーカラー
      dplyr::select(groupID, memberID, colorCode), 
    by = c("groupID", "memberID")
  ) |> 
  dplyr::mutate(
    memberName = " ", 
    member_age = 0, 
    add_label  = ""
  ) |> # 疑似集計データを追加
  dplyr::select(date, memberID, memberName, colorCode, member_age, add_label) |> 
  dplyr::arrange(memberID, date) # 昇順
outside_df


### データの集計 -----

# 年齢, 順位を集計
rank_df <- join_df |> # メンバーID, 加入日, 卒業日
  dplyr::filter(groupID == group_id) |> # グループを抽出
  dplyr::mutate(
    gradDate = gradDate |> 
      is.na() |> 
      dplyr::if_else(
        true  = max(lubridate::today(), date_max), # 活動中なら現在の日付or最大月
        false = gradDate
      )
  ) |> 
  dplyr::left_join(
    member_df |> # 生年月日
      dplyr::select(memberID, memberName, birthDate) |> 
      dplyr::distinct(memberID, .keep_all = TRUE), # 重複を除去
    by = "memberID"
  ) |> 
  dplyr::left_join(
    color_df |> # メンバーカラー
      dplyr::select(groupID, memberID, colorCode), 
    by = c("groupID", "memberID")
  ) |> 
  dplyr::mutate(
    date_from = (lubridate::day(joinDate) == 1) |> 
      dplyr::if_else(
        true  = joinDate |> 
          lubridate::floor_date(unit = "month"), # 加入月
        false = joinDate |> 
          lubridate::rollforward(roll_to_first = TRUE) # 加入日が月初なら加入翌月
      ), 
    date_to = date_max |> 
      lubridate::floor_date(unit = "month") # 集計最大月
  ) |> 
  dplyr::reframe(
    date = seq(from = date_from, to = date_to, by = "month"), # 活動・卒業後月
    .by = c(memberID, groupID, joinDate, gradDate, memberName, birthDate, colorCode)
  ) |> 
  dplyr::filter(dplyr::between(date, left = date_min, right = date_max)) |> # 集計期間を抽出
  dplyr::mutate(
    member_age = lubridate::interval(
      start = birthDate, 
      end   = (date <= gradDate) |> 
        dplyr::if_else(
          true  = date,    # 集計日
          false = gradDate # 卒業日
        )
    ) |> 
      lubridate::time_length(unit = "year") |> 
      floor(), # メンバー年齢
    add_label = (date <= gradDate) |> 
      dplyr::if_else(
        true  = "(現)", # 現役ラベル
        false = "(卒)"  # 卒業ラベル
      )
  ) |> 
  dplyr::bind_rows(
    outside_df # 加入前月, 卒業翌月
  ) |> 
  dplyr::arrange(date, -member_age, birthDate, memberID) |> # 順位付け用
  dplyr::mutate(
    ranking = dplyr::row_number(), # 順位
    .by = date
  ) |> 
  dplyr::select(date, memberID, memberName, colorCode, member_age, ranking, add_label) |> 
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
  mapping = aes(x = ranking, group = factor(memberID))
) + 
  geom_bar(
    mapping = aes(y = member_age, fill = colorCode, color = colorCode), 
    stat = "identity", width = 0.9, alpha = 0.8
  ) + # 年齢バー
  geom_text(
    mapping = aes(y = member_age, label = paste(" ", member_age, "歳", add_label)), 
    hjust = 0
  ) + # 年齢ラベル
  geom_text(
    mapping = aes(y = 0, label = paste(memberName, " ")), 
    hjust = 1
  ) + # メンバー名ラベル
  gganimate::transition_states(states = date, transition_length = t, state_length = s, wrap = FALSE) + # フレーム切替
  gganimate::ease_aes("cubic-in-out") + # アニメーションの緩急
  gganimate::view_follow(fixed_x = TRUE) + # 表示範囲の可変
  coord_flip(clip = "off", expand = FALSE) + # 軸の入替
  scale_x_reverse() + # ランク軸を反転
  scale_fill_identity() + # カラーコードを設定
  scale_color_identity() + # カラーコードを設定
  theme(
    axis.title = element_blank(), # 軸ラベル
    axis.text  = element_blank(), # 軸目盛ラベル
    axis.ticks = element_blank(), # 軸目盛指示線
    panel.grid.major.y = element_blank(), # y軸主目盛線
    panel.grid.minor.y = element_blank(), # y軸補助目盛線
    panel.border       = element_blank(), # グラフ領域の枠線
    plot.title    = element_text(color = "black", face = "bold", size = 20, hjust = 0.5), # 図タイトル
    plot.subtitle = element_text(color = "black", size = 15, hjust = 0.5), # 図サブタイトル
    plot.margin   = margin(t = 10, r = 60, b = 10, l = 120, unit = "pt"), # 図の余白
    legend.position = "none" # 凡例の位置
  ) + 
  labs(
    title = paste0(group_name, "歴代メンバーの在籍時の年齢の推移"), 
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
  width = 900, height = 1200, 
  renderer = gganimate::av_renderer(file = paste0("ChartRace/output/YearsAge/YearsAge_AllMember_", group_id, ".mp4"))
)


# 歴代メンバーの年齢 -----------------------------------------------------------

## 加入以前(グループ結成時)から卒業以降も含めた年齢の推移

### 期間の設定 -----

# 集計期間を指定
date_max <- "2024-10-01" |> 
  lubridate::as_date() |> 
  lubridate::floor_date(unit = "month") # 集計終了(最大)月


### グループの設定 -----

# グループを指定
group_id   <- 1
group_name <- "モーニング娘。"


### データの集計 -----

# 年齢, 順位を集計
rank_df <- join_df |> # メンバーID, 加入日, 卒業日
  dplyr::filter(groupID == group_id) |> # グループを抽出
  dplyr::mutate(
    gradDate = gradDate |> 
      is.na() |> 
      dplyr::if_else(
        true  = max(lubridate::today(), date_max), # 活動中なら現在の日付or最大月
        false = gradDate
      ), 
    date_from = min(joinDate) |> 
      lubridate::floor_date(unit = "month") # 結成月
  ) |> 
  dplyr::left_join(
    member_df |> # 生年月日
      dplyr::select(memberID, memberName, birthDate) |> 
      dplyr::distinct(memberID, .keep_all = TRUE), # 重複を除去
    by = "memberID"
  ) |> 
  dplyr::left_join(
    color_df |> # メンバーカラー
      dplyr::select(groupID, memberID, colorCode), 
    by = c("groupID", "memberID")
  ) |> 
  dplyr::reframe(
    date = seq(from = date_from, to = date_max, by = "month"), # (グループ)活動月
    .by = dplyr::everything()
  ) |> 
  dplyr::mutate(
    member_age = lubridate::interval(start = birthDate, end = date) |> 
      lubridate::time_length(unit = "year") |> 
      floor(), # メンバー年齢
    add_label = dplyr::case_when(
      date < joinDate ~ "(未)", # 未加入ラベル
      date > gradDate ~ "(卒)", # 卒業ラベル
      .default        = "(現)"  # 現役ラベル
    ) |> 
      factor(levels = c("(卒)", "(現)", "(未)"))
  ) |> 
  dplyr::arrange(date, add_label, birthDate, memberID) |> # 順位付け用
  dplyr::mutate(
    ranking = dplyr::row_number(), # 順位
    .by = date
  ) |> 
  dplyr::select(date, memberID, memberName, colorCode, member_age, ranking, add_label) |> 
  dplyr::arrange(date, ranking) # 昇順
rank_df


### 演出用データの作成 -----

# 境界線を作成
border_df <- rank_df |> 
  dplyr::select(date, add_label) |> 
  dplyr::reframe(
    member_num = dplyr::n(), # 未加入・現役・卒業メンバー数
    .by = c(date, add_label)
  ) |> 
  tidyr::complete(date, add_label, fill = list(member_num = 0)) |> # ラベルを補完
  dplyr::mutate(
    ranking = cumsum(member_num) + 0.5, # 境界位置
    .by = date
  ) |> 
  dplyr::filter(add_label %in% c("(卒)", "(現)")) # 境界を抽出
border_df

# ラベルを作成
label_df <- rank_df |> 
  dplyr::mutate(
    birth_flag = member_age >= 0, # 誕生フラグ
    name_label = birth_flag |> 
      dplyr::if_else(
        true  = paste(memberName, " "), 
        false = paste(" ", memberName)
      ), # メンバー名ラベル
    age_label = birth_flag |> 
      dplyr::if_else(
        true  = paste(" ", member_age, "歳", add_label), 
        false = paste(member_age, "歳 ")
      ) # 年齢ラベル
  ) |> 
  dplyr::select(date, memberID, memberName, member_age, ranking, birth_flag, name_label, age_label)
label_df


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
anim <- ggplot() + 
  geom_vline(
    data = border_df, 
    mapping = aes(xintercept = ranking, group = factor(add_label)), 
    color = "black", linetype = "dashed"
  ) + # 境界線
  geom_bar(
    data = rank_df, 
    mapping = aes(x = ranking, y = member_age, 
                  fill = colorCode, color = colorCode, group = factor(memberID)), 
    stat = "identity", width = 0.9, alpha = 0.8
  ) + # 年齢バー
  geom_text(
    data = label_df, 
    mapping = aes(x = ranking, y = member_age, label = age_label, 
                  hjust = as.integer(!birth_flag), group = factor(memberID))
  ) + # 年齢ラベル
  geom_text(
    data = label_df, 
    mapping = aes(x = ranking, y = 0, label = name_label, 
                  hjust = as.integer(birth_flag), group = factor(memberID))
  ) + # メンバー名ラベル
  gganimate::transition_states(states = date, transition_length = t, state_length = s, wrap = FALSE) + # フレーム切替
  gganimate::ease_aes("cubic-in-out") + # アニメーションの緩急
  gganimate::view_follow(fixed_x = TRUE) + # 表示範囲の可変
  coord_flip(clip = "off", expand = FALSE) + # 軸の入替
  scale_x_reverse() + # ランク軸を反転
  scale_fill_identity() + # カラーコードを設定
  scale_color_identity() + # カラーコードを設定
  theme(
    axis.title = element_blank(), # 軸ラベル
    axis.text  = element_blank(), # 軸目盛ラベル
    axis.ticks = element_blank(), # 軸目盛指示線
    panel.grid.major.y = element_blank(), # y軸主目盛線
    panel.grid.minor.y = element_blank(), # y軸補助目盛線
    panel.border       = element_blank(), # グラフ領域の枠線
    plot.title    = element_text(color = "black", face = "bold", size = 20, hjust = 0.5), # 図タイトル
    plot.subtitle = element_text(color = "black", size = 15, hjust = 0.5), # 図サブタイトル
    plot.margin   = margin(t = 10, r = 60, b = 10, l = 120, unit = "pt"), # 図の余白
    legend.position = "none" # 凡例の位置
  ) + 
  labs(
    title = paste0(group_name, "歴代メンバーの年齢の推移"), 
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
  width = 900, height = 1200, 
  renderer = gganimate::av_renderer(file = paste0("ChartRace/output/YearsAge/YearsAge_AllTermMember_", group_id, ".mp4"))
)


