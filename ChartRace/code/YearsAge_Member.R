
# 任意のグループのメンバーごとの年齢の推移 -------------------------------------

# 利用パッケージ
library(tidyverse)
library(gganimate)
library(ggtext)

# パッケージ名の省略用
library(ggplot2)


# 編集メモ ---------------------------------------------------------------------



# データの読込 -----------------------------------------------------------------

## ReadData.Rを参照

# 利用データを確認
join_df   # 加入・卒業日一覧
member_df # メンバー一覧
color_df  # メンバーカラー一覧


# 集計の設定 -------------------------------------------------------------------

### グループの設定 -----

# グループを指定:(複数の場合はベクトル)
group_id <- 9

# タイトル用の文字列を指定
group_name <- "ばってん少女隊"


### 期間の設定 -----

# 集計終了(最大)月を指定
date_max <- "2025-07-01" |> # 任意の日
  lubridate::as_date() |> 
  lubridate::floor_date(unit = "month")
date_max <- lubridate::today() |> # 集計日
  lubridate::floor_date(unit = "month")
date_max <- group_df |> 
  dplyr::filter(groupID %in% group_id) |> 
  dplyr::pull(dissolveDate) |> 
  max() |> 
  (\(val) {min(val, lubridate::today(), na.rm = TRUE)})() |> # 解散日or集計日
  lubridate::floor_date(unit = "month")
date_max


# 現役メンバーの年齢 -----------------------------------------------------------

## 加入から卒業までの年齢の推移


### 演出用データの作成 -----

# 加入前月, 卒業翌月のデータを作成:(バーの変化の強調用)
outside_df <- join_df |> 
  dplyr::filter(groupID %in% group_id) |> # グループを抽出
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
  dplyr::left_join(
    color_df |> # メンバーカラー
      dplyr::select(groupID, memberID, colorCode), 
    by = c("groupID", "memberID")
  ) |> 
  dplyr::mutate(
    memberName = " ", 
    member_age = 0, 
    age_label  = "  0 歳", 
    dummy_flag = TRUE # メンバー数の集計用
  ) |> # 疑似データを追加
  dplyr::select(date, memberID, memberName, colorCode, member_age, age_label, dummy_flag) |> 
  dplyr::arrange(memberID, date)
outside_df


### データの集計 -----

# 年齢, 順位を集計
rank_df <- join_df |> # メンバーID, 加入日, 卒業日
  dplyr::filter(groupID %in% group_id) |> # グループを抽出
  dplyr::mutate(
    gradDate = gradDate |> 
      is.na() |> 
      dplyr::if_else(
        true  = max(lubridate::today(), date_max), # 活動中なら現在の日付or将来月
        false = gradDate
      )
  ) |> 
  dplyr::left_join(
    member_df |> # メンバー名, 生年月日
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
        true  = joinDate, # 加入月
        false = joinDate |> 
          lubridate::rollforward(roll_to_first = TRUE) # 加入日が月初でないなら加入翌月
      ), 
    date_to = gradDate |> 
      lubridate::floor_date(unit = "month") # 卒業月
  ) |> 
  dplyr::reframe(
    date = seq(from = date_from, to = date_to, by = "month"), # 活動月
    .by = c(memberID, memberName, colorCode, birthDate)
  ) |> 
  dplyr::mutate(
    member_age = birthDate |> 
      is.na() |> 
      dplyr::if_else(
        true  = 0, # 生年月日非公開メンバー用
        false = lubridate::interval(start = birthDate, end = date) |> 
          lubridate::time_length(unit = "year") |> 
          floor() # メンバー年齢
      ), 
    age_label = birthDate |> 
      is.na() |> 
      dplyr::if_else(
        true  = "  NA 歳", # 生年月日非公開メンバー用
        false = paste(" ", member_age, "歳") # 年齢ラベル
      ), 
    dummy_flag = FALSE # メンバー数の集計用
  ) |> 
  dplyr::bind_rows(
    outside_df # 加入前月, 卒業翌月
  ) |> 
  dplyr::filter(date <= date_max) |> # 集計期間を抽出
  dplyr::arrange(date, birthDate, memberID) |> # 順位付け用
  dplyr::mutate(
    ranking = dplyr::row_number(), # 順位
    .by = date
  ) |> 
  dplyr::select(date, memberID, memberName, colorCode, birthDate, member_age, ranking, age_label, dummy_flag)
rank_df


### 演出用データの作成 -----

# 0より大きい値を設定
dummy_val <- 1

# 描画枠の確保用のデータを作成:(全てのバーが0のときに表示位置が崩れる対応用)
dummy_df <- rank_df |> 
  dplyr::summarise(
    tmp_sum = sum(member_age), # 抽出用
    .by = date
  ) |> 
  dplyr::filter(tmp_sum == 0) |> # 全ての値が0の月を抽出
  dplyr::mutate(
    dummy_y = dummy_val # (初期表示範囲の調整用)
  ) |> 
  dplyr::select(date, dummy_y)
dummy_df

# 現役メンバーの平均年齢を集計
label_df <- rank_df |> 
  dplyr::summarise(
    max_age    = max(member_age, na.rm = TRUE), # (生年月日非公開を除く)グループ最大年齢
    total_age  = sum(member_age, na.rm = TRUE), # (生年月日非公開を除く)グループ合計年齢
    tmp_num    = sum(!is.na(birthDate)),        # (生年月日非公開を除く)グループメンバー数
    member_num = sum(dummy_flag == FALSE), # グループメンバー数
    .by = date
  ) |> # (疑似データを除いて計算)
  dplyr::mutate(
    max_age = (max_age == 0) |> 
      dplyr::if_else(
        true  = dummy_val, 
        false = max_age
      ), # (初期表示範囲の調整用)
    mean_age = total_age / tmp_num, # グループ平均年齢
    mean_age = mean_age |> 
      is.na() |> 
      dplyr::if_else(
        true  = 0, # 全て疑似データor生年月日非公開メンバーなら0に置換
        false = mean_age
      ), 
    mean_label = paste(
      "メンバー数:", member_num, "人<br>", 
      "平均年齢:", round(mean_age, digits = 1), "歳"
    )
  ) |> 
  dplyr::select(date, max_age, mean_age, mean_label)
label_df


### アニメーションの作成 -----

# 遷移フレーム数を指定
t <- 9

# 一時停止フレーム数を指定
s <- 1

# 1秒間に表示する月数を指定:(値が大きいと意図した通りにならない)
mps <- 3

# フレーム数を取得
n <- rank_df[["date"]] |> 
  unique() |> 
  length()

# ラベルの表示位置を設定
max_rank <- rank_df |> 
  dplyr::pull(ranking) |> 
  max()

# バーチャートレースを作図
anim <- ggplot() + 
  geom_point(
    data    = dummy_df, 
    mapping = aes(x = max_rank+0.5, y = dummy_y), 
    color = "grey90", size = 0
  ) + # (初期表示範囲の調整用)
  geom_hline(
    data    = label_df, 
    mapping = aes(yintercept = mean_age), 
    color = "black", linetype = "dashed"
  ) + # 平均年齢線
  ggtext::geom_textbox(
    data    = label_df, 
    mapping = aes(x = max_rank+0.5, y = max_age, label = mean_label), 
    hjust = 1, vjust = 0, color = "black", fill = "white", 
    halign = 0, valign = 0.5, width = unit(1.5, units = "inch"), 
    size = 4
  ) + # 平均年齢ラベル
  geom_bar(
    data    = rank_df, 
    mapping = aes(
      x = ranking, y = member_age, group = factor(memberID), 
      fill = colorCode, color = colorCode
    ), 
    stat = "identity", width = 0.9, alpha = 0.8
  ) + # 年齢バー
  geom_text(
    data    = rank_df, 
    mapping = aes(
      x = ranking, y = member_age, group = factor(memberID), 
      label = age_label
      ), 
    hjust = 0
  ) + # 年齢ラベル
  geom_text(
    data    = rank_df, 
    mapping = aes(
      x = ranking, y = 0, group = factor(memberID), 
      label = paste(memberName, " ")
    ), 
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
    )#, 
    #caption = "データ:「https://github.com/xxgentaroxx/HP_DB」"
  )

# 動画を作成
tmp_id <- paste0(group_id, collapse = "-") # (複数グループ用)
file_path <- paste0("ChartRace/output/YearsAge_Member/YearsAge_Member_", tmp_id, ".mp4")
m <- gganimate::animate(
  plot = anim, 
  nframes = (t+s)*n, fps = (t+s)*mps, 
  width = 900, height = 600, 
  renderer = gganimate::av_renderer(file = file_path)
)


# 歴代メンバーの在籍時の年齢 ---------------------------------------------------

## 加入から卒業以降も含めた年齢の推移


### 演出用データの作成 -----

# 加入前月のデータを作成:(バーの変化の強調用)
outside_df <- join_df |> 
  dplyr::filter(groupID %in% group_id) |> # グループを抽出
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
  dplyr::left_join(
    color_df |> # メンバーカラー
      dplyr::select(groupID, memberID, colorCode), 
    by = c("groupID", "memberID")
  ) |> 
  dplyr::mutate(
    memberName = " ", 
    member_age = 0, 
    age_label  = "  0 歳", 
    add_label  = "(未)", 
    dummy_flag = TRUE # メンバー数の集計用
  ) |> # 疑似データを追加
  dplyr::select(date, memberID, memberName, colorCode, member_age, age_label, add_label, dummy_flag) |> 
  dplyr::arrange(memberID, date)
outside_df


### データの集計 -----

# 年齢, 順位を集計
rank_df <- join_df |> # メンバーID, 加入日, 卒業日
  dplyr::filter(groupID %in% group_id) |> # グループを抽出
  dplyr::mutate(
    gradDate = gradDate |> 
      is.na() |> 
      dplyr::if_else(
        true  = max(lubridate::today(), date_max), # 活動中なら現在の日付or将来月
        false = gradDate
      )
  ) |> 
  dplyr::left_join(
    member_df |> # メンバー名, 生年月日
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
        true  = joinDate, # 加入月
        false = joinDate |> 
          lubridate::rollforward(roll_to_first = TRUE) # 加入日が月初でないなら加入翌月
      ), 
    date_to = date_max |> 
      lubridate::floor_date(unit = "month") # 集計最大月
  ) |> 
  dplyr::filter(date_from <= date_to) |> # 未加入メンバーを除去(エラー回避用)
  dplyr::reframe(
    date = seq(from = date_from, to = date_to, by = "month"), # 活動・卒業後月
    .by = c(memberID, memberName, colorCode, joinDate, gradDate, birthDate)
  ) |> 
  dplyr::mutate(
    member_age = birthDate |> 
      is.na() |> 
      dplyr::if_else(
        true  = 0, # 生年月日非公開メンバー用
        false = lubridate::interval(
          start = birthDate, 
          end   = (date <= gradDate) |> 
            dplyr::if_else(
              true  = date,    # 集計日
              false = gradDate # 卒業日
            )
        ) |> 
          lubridate::time_length(unit = "year") |> 
          floor() # メンバー年齢
      ), 
    age_label = birthDate |> 
      is.na() |> 
      dplyr::if_else(
        true  = "  NA 歳", # 生年月日非公開メンバー用
        false = paste(" ", member_age, "歳") # 年齢ラベル
      ), 
    add_label = (date <= gradDate) |> 
      dplyr::if_else(
        true  = "(現)", # 現役ラベル
        false = "(卒)"  # 卒業ラベル
      ), 
    dummy_flag = FALSE # メンバー数の集計用
  ) |> 
  dplyr::bind_rows(
    outside_df # 加入前月, 卒業翌月
  ) |> 
  dplyr::filter(date <= date_max) |> # 集計期間を抽出
  dplyr::arrange(date, -member_age, birthDate, memberID) |> # 順位付け用
  dplyr::mutate(
    ranking = dplyr::row_number(), # 順位
    .by = date
  ) |> 
  dplyr::select(
    date, memberID, memberName, colorCode, birthDate, member_age, ranking, age_label, add_label, dummy_flag
  )
rank_df


### 演出用データの作成 -----

# 0より大きい値を設定
dummy_val <- 1

# 描画枠の確保用のデータを作成:(全てのバーが0のときに表示位置が崩れる対応用)
dummy_df <- rank_df |> 
  dplyr::summarise(
    tmp_sum = sum(member_age), # 抽出用
    .by = date
  ) |> 
  dplyr::filter(tmp_sum == 0) |> # 全ての値が0の月を抽出
  dplyr::mutate(
    dummy_y = dummy_val # (初期表示範囲の調整用)
  )|> 
  dplyr::select(date, dummy_y)
dummy_df

# 現役・卒業メンバーの数を集計
label_df <- rank_df |> 
  dplyr::mutate(
    max_age = max(member_age, na.rm = TRUE), # グループ最大年齢
    .by = date
  ) |> 
  dplyr::summarise(
    member_num = dplyr::n(), # メンバー数
    .by = c(date, add_label, max_age)
  ) |> 
  tidyr::complete(
    date = rank_df |> 
      dplyr::pull(date) |> 
      unique(), 
    add_label = c("(未)", "(現)", "(卒)"), 
    fill = list(max_age = 0, member_num = 0)
  ) |> # メンバー数0のデータを補完
  dplyr::mutate(
    max_age = (max(max_age) == 0) |> 
      dplyr::if_else(
        true  = dummy_val, 
        false = max(max_age)
      ), # (初期表示範囲の調整用)
    .by = date
  ) |> 
  dplyr::filter(add_label %in% c("(現)", "(卒)")) |> # 疑似データを除去
  dplyr::reframe(
    num_label = paste(
      "現役メンバー数:", member_num[1], "人<br>", 
      "卒業メンバー数:", member_num[2], "人"
    ), 
    .by = c(date, max_age)
  ) |> 
  dplyr::select(date, max_age, num_label)
label_df


### アニメーションの作成 -----

# 遷移フレーム数を指定
t <- 9

# 一時停止フレーム数を指定
s <- 1

# 1秒間に表示する月数を指定:(値が大きいと意図した通りにならない)
mps <- 3

# フレーム数を取得
n <- rank_df[["date"]] |> 
  unique() |> 
  length()

# ラベルの表示位置を設定
max_rank <- rank_df |> 
  dplyr::pull(ranking) |> 
  max()

# バーチャートレースを作図
anim <- ggplot() + 
  geom_point(
    data    = dummy_df, 
    mapping = aes(x = max_rank+0.5, y = dummy_y), 
    color = "grey90", size = 0
  ) + # (初期表示範囲の調整用)
  ggtext::geom_textbox(
    data    = label_df, 
    mapping = aes(x = max_rank+0.5, y = max_age, label = num_label), 
    hjust = 1, vjust = 0, color = "black", fill = "white", 
    halign = 0, valign = 0.5, width = unit(1.5, units = "inch"), 
    size = 4
  ) + # メンバー数ラベル
  geom_bar(
    data    = rank_df, 
    mapping = aes(
      x = ranking, y = member_age, group = factor(memberID), 
      fill = colorCode, color = colorCode
    ), 
    stat = "identity", width = 0.9, alpha = 0.8
  ) + # 年齢バー
  geom_text(
    data    = rank_df, 
    mapping = aes(
      x = ranking, y = member_age, group = factor(memberID), 
      label = paste(age_label, add_label)
    ), 
    hjust = 0
  ) + # 年齢ラベル
  geom_text(
    data    = rank_df, 
    mapping = aes(
      x = ranking, y = 0, group = factor(memberID), 
      label = paste(memberName, " ")
    ), 
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
    plot.margin   = margin(t = 10, r = 90, b = 10, l = 90, unit = "pt"), # 図の余白
    legend.position = "none" # 凡例の位置
  ) + 
  labs(
    title = paste0(group_name, "歴代メンバーの在籍時の年齢の推移"), 
    subtitle = paste0(
      "{lubridate::year(closest_state)}年", 
      "{stringr::str_pad(lubridate::month(closest_state), width = 2, pad = 0)}月", 
      "01日時点"
    )#, 
    #caption = "データ:「https://github.com/xxgentaroxx/HP_DB」"
  )

# 動画を作成
tmp_id <- paste0(group_id, collapse = "-") # (複数グループ用)
file_path <- paste0("ChartRace/output/YearsAge_Member/YearsAge_AllMember_", tmp_id, ".mp4")
m <- gganimate::animate(
  plot = anim, 
  nframes = (t+s)*n, fps = (t+s)*mps, 
  width = 900, height = 900, 
  renderer = gganimate::av_renderer(file = file_path)
)


# 歴代メンバーの年齢 -----------------------------------------------------------

## 加入以前(グループ結成時)から卒業以降も含めた年齢の推移


### データの集計 -----

# 年齢, 順位を集計
rank_df <- join_df |> # メンバーID, 加入日, 卒業日
  dplyr::filter(groupID %in% group_id) |> # グループを抽出
  dplyr::mutate(
    gradDate = gradDate |> 
      is.na() |> 
      dplyr::if_else(
        true  = max(lubridate::today(), date_max), # 活動中なら現在の日付or将来月
        false = gradDate
      ), 
    date_from = (lubridate::day(joinDate) == 1) |> 
      dplyr::if_else(
        true  = joinDate |> 
          lubridate::rollback() |> 
          lubridate::floor_date(unit = "month"), # 加入日が月初なら加入前月
        false = joinDate |> 
          lubridate::floor_date(unit = "month") # 加入月
      ) |> 
      min() # 結成前月or結成月
  ) |> 
  dplyr::left_join(
    member_df |> # メンバー名, 生年月日
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
    date = seq(from = date_from, to = date_max, by = "month"), # グループ活動月
    .by = c(memberID, memberName, colorCode, joinDate, gradDate, birthDate)
  ) |> 
  dplyr::mutate(
    member_age = birthDate |> 
      is.na() |> 
      dplyr::if_else(
        true  = 0, # 生年月日非公開メンバー用
        false = lubridate::interval(start = birthDate, end = date) |> 
          lubridate::time_length(unit = "year") |> 
          floor() # メンバー年齢
      ), 
    add_label = dplyr::case_when(
      date < joinDate ~ "(未)", # 未加入ラベル
      date > gradDate ~ "(卒)", # 卒業ラベル
      .default        = "(現)"  # 現役ラベル
    ) |> 
      factor(levels = c("(卒)", "(現)", "(未)")), # 表示順を指定
    born_flag  = member_age >= 0, # 誕生フラグ
    name_label = born_flag |> 
      dplyr::if_else(
        true  = paste(memberName, " "), 
        false = paste(" ", memberName)
      ), # メンバー名ラベル
    tmp_age   = birthDate |> 
      is.na() |> 
      dplyr::if_else(
        true  = NA, # 生年月日非公開メンバー用
        false = member_age
      ), 
    age_label = born_flag |> 
      dplyr::if_else(
        true  = paste(" ", tmp_age, "歳", add_label), 
        false = paste(tmp_age, "歳 ")
      ) # 年齢ラベル
  ) |> 
  dplyr::arrange(date, add_label, birthDate, memberID) |> # 順位付け用
  dplyr::mutate(
    ranking = dplyr::row_number(), # 順位
    .by = date
  ) |> 
  dplyr::select(
    date, memberID, memberName, colorCode, birthDate, 
    member_age, ranking, name_label, age_label, add_label, born_flag
  )
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
    border_x = cumsum(member_num) + 0.5, # 境界位置
    .by = date
  ) |> 
  dplyr::filter(add_label %in% c("(卒)", "(現)")) # 境界を抽出
border_df

# 未加入・現役・卒業メンバーの平均年齢を集計
label_df <- rank_df |> 
  dplyr::mutate(
    max_age = max(member_age, na.rm = TRUE), # (生年月日非公開を除く)グループ最大年齢
    member_age = born_flag |> 
      dplyr::if_else(
        true  = member_age, 
        false = NA
      ), # 未出生の場合は集計に含めない
    .by = date
  ) |> 
  dplyr::summarise(
    label_x    = max(ranking)　+ 0.5, # 境界位置
    total_age  = sum(member_age, na.rm = TRUE),       # (生年月日非公開・未出生を除く)グループ合計年齢
    tmp_num    = sum(!is.na(member_age)),             # (生年月日非公開・未出生を除く)グループメンバー数
    member_num = dplyr::n() - sum(is.na(member_age)), # (未出生を除く)グループメンバー数
    .by = c(date, add_label, max_age)
  ) |> # (疑似データを除いて計算)
  dplyr::mutate(
    max_age = (max_age == 0) |> 
      dplyr::if_else(
        true  = dummy_val, 
        false = max_age
      ), # (初期表示範囲の調整用)
    mean_age = total_age / tmp_num, # グループ平均年齢
    mean_age = mean_age |> 
      is.na() |> 
      dplyr::if_else(
        true  = 0, # 全て生年月日非公開メンバーなら0に置換
        false = mean_age
      ), 
    mean_label = paste(
      "メンバー数:", member_num, "人<br>", 
      "平均年齢:", round(mean_age, digits = 1), "歳"
    )
  ) |> 
  dplyr::select(date, add_label, label_x, max_age, mean_age, mean_label)
label_df

### アニメーションの作成 -----

# 遷移フレーム数を指定
t <- 9

# 一時停止フレーム数を指定
s <- 1

# 1秒間に表示する月数を指定:(値が大きいと意図した通りにならない)
mps <- 3

# フレーム数を取得
n <- rank_df[["date"]] |> 
  unique() |> 
  length()

# バーチャートレースを作図
anim <- ggplot() + 
  geom_vline(
    data    = border_df, 
    mapping = aes(xintercept = border_x, group = factor(add_label)), 
    color = "black", linetype = "dashed"
  ) + # 境界線
  ggtext::geom_textbox(
    data    = label_df, 
    mapping = aes(x = label_x, y = max_age, label = mean_label, group = add_label), 
    hjust = 1, vjust = 0, color = "black", fill = "white", 
    halign = 0, valign = 0.5, width = unit(1.5, units = "inch"), 
    size = 4
  ) + # 平均年齢ラベル
  geom_bar(
    data    = rank_df, 
    mapping = aes(
      x = ranking, y = member_age, group = factor(memberID), 
      fill = colorCode, color = colorCode
    ), 
    stat = "identity", width = 0.9, alpha = 0.8
  ) + # 年齢バー
  geom_text(
    data    = rank_df, 
    mapping = aes(
      x = ranking, y = member_age, group = factor(memberID), 
      label = age_label, hjust = as.integer(!born_flag)
    )
  ) + # 年齢ラベル
  geom_text(
    data    = rank_df, 
    mapping = aes(
      x = ranking, y = 0, group = factor(memberID), 
      label = name_label, hjust = as.integer(born_flag)
    )
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
    plot.margin   = margin(t = 10, r = 90, b = 10, l = 90, unit = "pt"), # 図の余白
    legend.position = "none" # 凡例の位置
  ) + 
  labs(
    title = paste0(group_name, "歴代メンバーの年齢の推移"), 
    subtitle = paste0(
      "{lubridate::year(closest_state)}年", 
      "{stringr::str_pad(lubridate::month(closest_state), width = 2, pad = 0)}月", 
      "01日時点"
    )#, 
    #caption = "データ:「https://github.com/xxgentaroxx/HP_DB」"
  )

# 動画を作成
tmp_id <- paste0(group_id, collapse = "-") # (複数グループ用)
file_path <- paste0("ChartRace/output/YearsAge_Member/YearsAge_AllTermMember_", tmp_id, ".mp4")
m <- gganimate::animate(
  plot = anim, 
  nframes = (t+s)*n, fps = (t+s)*mps, 
  width = 900, height = 900, 
  renderer = gganimate::av_renderer(file = file_path)
)


