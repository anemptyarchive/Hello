
# 任意のグループの生まれ年ごとのメンバー数の推移 -------------------------------

# 利用パッケージ
library(tidyverse)
library(gganimate)

# パッケージを読込
library(ggplot2)


# 編集メモ --------------------------------------------------------------------

## 生年月日非公開メンバーを含む場合は未対応


# データの読込 -----------------------------------------------------------------

## ReadData.Rを参照

# 利用データを確認
join_df   # 加入・卒業日一覧
member_df # メンバー一覧


# 集計の設定 -------------------------------------------------------------------

### グループの設定 -----

# グループを指定:(複数の場合はベクトル)
group_id <- 2

# タイトル用の文字列を指定
group_name <- "私立恵比寿中学"


### 期間の設定 -----

# 集計終了(最大)月
date_max <- "2024-01-01" |> # 任意の日
  lubridate::as_date() |> 
  lubridate::floor_date(unit = "month")
date_max <- lubridate::today() |> # 集計日
  lubridate::floor_date(unit = "month")
date_max <- group_df |> 
  dplyr::filter(groupID %in% group_id) |> 
  dplyr::pull(dissolveDate) |> 
  (\(val) {max(val, lubridate::today(), na.rm = TRUE)})() |> # 解散日or集計日
  lubridate::floor_date(unit = "month")


# 各誕生年のメンバー数 ---------------------------------------------------------

## 生まれ年ごとのメンバー数の降順の推移


### データの集計 -----

# メンバー数を集計
count_df <- join_df |> # メンバーID, 加入日, 卒業日
  dplyr::filter(groupID %in% group_id) |> # グループを抽出
  dplyr::mutate(
    gradDate = gradDate |> 
      is.na() |> 
      dplyr::if_else(
        true  = max(lubridate::today(), date_max), # 活動中なら現在の日付or最大月
        false = gradDate # 卒業日
      )
  ) |> 
  dplyr::left_join(
    member_df |> # メンバー名, 生年月日
      dplyr::select(memberID, memberName, birthDate) |> 
      dplyr::distinct(memberID, .keep_all = TRUE), # 重複を除去
    by = "memberID"
  ) |> 
  dplyr::mutate(
    date_from = (lubridate::day(joinDate) == 1) |> 
      dplyr::if_else(
        true  = joinDate, # 加入月
        false = joinDate |> 
          lubridate::rollforward(roll_to_first = TRUE) # 加入日が月初でないなら加入翌月
      ), 
    date_to = gradDate |> 
      lubridate::floor_date(unit = "month"), # 卒業月
    birth_year = birthDate |> 
      lubridate::year() |> 
      as.integer() # メンバー誕生年
  ) |> 
  dplyr::reframe(
    date = seq(from = date_from, to = date_to, by = "month"), # 活動月
    .by = c(memberID, groupID, birth_year)
  ) |> 
  dplyr::summarise(
    member_num = dplyr::n(), # 誕生年メンバー数
    .by = c(date, birth_year)
  ) |> 
  dplyr::select(date, birth_year, member_num)
count_df


### 演出用データの作成 -----

# 加入前月, 卒業翌月のデータを作成:(バーの変化の強調用)
outside_df <- tidyr::expand_grid(
  date = seq(
    from = count_df[["date"]] |> 
      min() |> 
      lubridate::rollback() |> 
      lubridate::floor_date(unit = "month"), # 最小前月
    to   = count_df[["date"]] |> 
      max(), # 最大月
    by = "month"
  ), # 活動月
  birth_year = count_df[["birth_year"]] |> 
    unique() |> 
    sort() # 誕生年
) |> # 全ての組み合わせを作成
  dplyr::left_join(
    count_df, # メンバー数
    by = c("date", "birth_year")
  ) |> 
  dplyr::mutate(
    member_num = member_num |> 
      is.na() |> 
      dplyr::if_else(
        true  = 0, 
        false = member_num
      ), # 結合時の欠損値を置換
  ) |> 
  dplyr::arrange(birth_year, date) |> # 変化量の計算用
  dplyr::mutate(
    lead_num = member_num |> 
      dplyr::lead(n = 1, default = NA), # 翌月の値
    lag_num = member_num |> 
      dplyr::lag(n = 1, default = 0), # 前月の値
    lead_diff_num = lead_num - member_num, # 翌月との変化量
    lag_diff_num  = member_num - lag_num,  # 前月との変化量
    .by = birth_year
  ) |> 
  dplyr::filter(member_num == 0, lead_diff_num != 0 | lag_diff_num != 0) |> # 疑似集計データを抽出
  dplyr::select(date, birth_year, member_num)
outside_df


### データの集計 -----

# 順位を集計
rank_df <- count_df |> 
  dplyr::bind_rows(
    outside_df # 加入前月, 卒業翌月
  ) |> 
  dplyr::filter(date <= date_max) |> # 集計期間を抽出
  dplyr::mutate(
    year_id = birth_year |> 
      dplyr::dense_rank() # 誕生年ID
    
  ) |> 
  dplyr::arrange(date, -member_num, birth_year) |> # 順位付け用
  dplyr::mutate(
    ranking = dplyr::row_number(), # 順位
    .by = date
  ) |> 
  dplyr::select(date, year_id, birth_year, member_num, ranking)
rank_df


### 演出用データの作成 -----

# 0より大きい最小値を設定
dummy_val <- 1

# 描画枠の確保用のデータを作成:(全てのバーが0のときに表示位置が崩れる対応用)
dummy_df <- rank_df |> 
  dplyr::summarise(
    tmp_sum = sum(member_num), # 抽出用
    .by = date
  ) |> 
  dplyr::filter(tmp_sum == 0) |> # 全ての値が0の月を抽出
  dplyr::mutate(
    dummy_y = dummy_val # (初期表示範囲の調整用)
  ) |> 
  dplyr::select(date, dummy_y)
dummy_df


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
  geom_bar(
    data    = rank_df, 
    mapping = aes(
      x = ranking, y = member_num, group = factor(year_id), 
      fill = factor(year_id), color = factor(year_id)
    ), 
    stat = "identity", width = 0.9, alpha = 0.8
  ) + # メンバー数バー
  geom_text(
    data    = rank_df, 
    mapping = aes(
      x = ranking, y = member_num, group = factor(year_id), 
      label = paste(" ", member_num, "人")
    ), 
    hjust = 0
  ) + # メンバー数ラベル
  geom_text(
    data    = rank_df, 
    mapping = aes(
      x = ranking, y = 0, group = factor(year_id), 
      label = paste(" ", birth_year, "年生まれ  ") # (なぜか文頭にスペースを入れると表示がバグらない)
    ), 
    hjust = 1
  ) + # 誕生年ラベル
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
    plot.margin   = margin(t = 10, r = 60, b = 10, l = 120, unit = "pt"), # 図の余白
    legend.position = "none" # 凡例の位置
  ) + 
  labs(
    title = paste0(group_name, "メンバーの誕生年の推移"), 
    subtitle = paste0(
      "{lubridate::year(closest_state)}年", 
      "{stringr::str_pad(lubridate::month(closest_state), width = 2, pad = 0)}月", 
      "01日時点"
    ), 
    caption = "データ:「https://github.com/xxgentaroxx/HP_DB」"
  )

# 動画を作成
tmp_id <- paste0(group_id, collapse = "-") # (複数グループ用)
file_path <- paste0("ChartRace/output/YearOfBirth_MemberNum/YearOfBirth_", tmp_id, ".mp4")
m <- gganimate::animate(
  plot = anim, 
  nframes = (t+s)*n, fps = (t+s)*mps, 
  width = 900, height = 450, 
  renderer = gganimate::av_renderer(file = file_path)
)


# 誕生年の分布 -----------------------------------------------------------------

## 生まれ年ごとのメンバー数の全体の推移


### データの集計 -----

## 「各誕生年のメンバー数」を参照

# メンバー数を集計
count_df


### 演出用データの作成 -----

# 加入前月, 卒業翌月, メンバー数0の年齢のデータを作成:(バーの変化の強調用)
hist_df <- tidyr::expand_grid(
  date = seq(
    from = count_df[["date"]] |> 
      min() |> 
      lubridate::rollback() |> 
      lubridate::floor_date(unit = "month"), # 最小前月
    to   = count_df[["date"]] |> 
      max(), # 最大月
    by = "month"
  ), # 活動月
  birth_year = seq(
    from = count_df[["birth_year"]] |> # 最年長
      min(), 
    to   = count_df[["birth_year"]] |> # 最年少
      max(), 
    by = 1
  ) |> 
    as.integer() # 誕生年
) |> # 全ての組み合わせを作成
  dplyr::left_join(
    count_df, # メンバー数
    by = c("date", "birth_year")
  ) |> 
  dplyr::mutate(
    member_num = member_num |> 
      is.na() |> 
      dplyr::if_else(
        true  = 0L, 
        false = member_num
      ), # 結合時の欠損値を置換
  ) |> 
  dplyr::filter(date <= date_max) |> # 集計期間を抽出
  dplyr::mutate(
    year_id = birth_year |> 
      dplyr::dense_rank() # 誕生年ID
  ) |> 
  dplyr::select(date, year_id, birth_year, member_num)
hist_df


### アニメーションの作成 -----

# 遷移フレーム数を指定
t <- 9

# 一時停止フレーム数を指定
s <- 1

# 1秒間に表示する月数を指定:(値が大きいと意図した通りにならない)
mps <- 3

# フレーム数を取得
n <- hist_df[["date"]] |> 
  unique() |> 
  length()

# ラベルの表示位置を設定
max_num <- hist_df |> 
  dplyr::pull(member_num) |> 
  max()
max_year <- hist_df |> 
  dplyr::pull(birth_year) |> 
  max()

# バーチャートレースを作図
anim <- ggplot() + 
  geom_bar(
    data    = hist_df, 
    mapping = aes(
      x = birth_year, y = member_num, group = factor(year_id), 
      fill = factor(year_id), color = factor(year_id)
    ), 
    stat = "identity", width = 0.9, alpha = 0.8
  ) + # メンバー数バー
  geom_text(
    data    = hist_df, 
    mapping = aes(
      x = birth_year, y = 0, group = factor(year_id), 
      label = paste(" ", member_num, "人")
    ), 
    hjust = 0
  ) + # メンバー数ラベル
  geom_text(
    data    = hist_df, 
    mapping = aes(
      x = birth_year, y = 0, group = factor(year_id), 
      label = paste(" ", birth_year, "年生まれ  ") # (なぜか文頭にスペースを入れると表示がバグらない)
    ), 
    hjust = 1
  ) + # 誕生年ラベル
  gganimate::transition_states(states = date, transition_length = t, state_length = s, wrap = FALSE) + # フレーム切替
  gganimate::ease_aes("cubic-in-out") + # アニメーションの緩急
  coord_flip(clip = "off", expand = FALSE) + # 軸の入替
  scale_x_reverse(breaks = sort(unique(hist_df[["member_age"]]))) + 
  theme(
    axis.title = element_blank(), # 軸ラベル
    axis.text  = element_blank(), # 軸目盛ラベル
    axis.ticks = element_blank(), # 軸目盛指示線
    panel.grid.minor.y = element_blank(), # y軸補助目盛線
    panel.border       = element_blank(), # グラフ領域の枠線
    plot.title    = element_text(color = "black", face = "bold", size = 20, hjust = 0.5), # 図タイトル
    plot.subtitle = element_text(color = "black", size = 15, hjust = 0.5), # 図サブタイトル
    plot.margin   = margin(t = 10, r = 60, b = 10, l = 120, unit = "pt"), # 図の余白
    legend.position = "none" # 凡例の位置
  ) + 
  labs(
    title = paste0(group_name, "メンバーの誕生年の推移"), 
    subtitle = paste0(
      "{lubridate::year(closest_state)}年", 
      "{stringr::str_pad(lubridate::month(closest_state), width = 2, pad = 0)}月", 
      "01日時点"
    )#, 
    #caption = "データ:「https://github.com/xxgentaroxx/HP_DB」"
  )

# 動画を作成
tmp_id <- paste0(group_id, collapse = "-") # (複数グループ用)
file_path <- paste0("ChartRace/output/YearOfBirth_MemberNum/YearOfBirth_hist_", tmp_id, ".mp4")
m <- gganimate::animate(
  plot = anim, 
  nframes = (t+s)*n, fps = (t+s)*mps, 
  width = 900, height = 450, 
  renderer = gganimate::av_renderer(file = file_path)
)
