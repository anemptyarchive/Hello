
# グループメンバーごとのグループ加入月数(活動歴)の推移 -------------------------

# 利用パッケージ
library(tidyverse)
library(gganimate)

# パッケージを読込
library(ggplot2)


# 編集メモ --------------------------------------------------------------------



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
group_name <- "モーニング娘。"
group_name <- "私立恵比寿中学"


### 期間の設定 -----

# 集計終了(最大)月
date_max <- "2024-12-01" |> # 任意の日
  lubridate::as_date() |> 
  lubridate::floor_date(unit = "month")
date_max <- lubridate::today() |> # 集計日
  lubridate::floor_date(unit = "month")
date_max <- group_df |> 
  dplyr::filter(groupID %in% group_id) |> 
  dplyr::pull(dissolveDate) |> 
  (\(val) {max(val, lubridate::today(), na.rm = TRUE)})() |> # 解散日or集計日
  lubridate::floor_date(unit = "month")


# 現役メンバーの活動月数 -------------------------------------------------------

## 加入から卒業までの活動月数の推移


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
    memberName    = " ", 
    member_period = 0
  ) |> # 疑似集計データを追加
  dplyr::select(date, memberID, memberName, colorCode, member_period) |> 
  dplyr::arrange(memberID, date)
outside_df


### データの集計 -----

# 活動月数, 順位を集計
rank_df <- join_df |> # メンバーID, 加入日, 卒業日
  dplyr::filter(groupID %in% group_id) |> # グループを抽出
  dplyr::mutate(
    gradDate = gradDate |> 
      is.na() |> 
      dplyr::if_else(
        true  = max(lubridate::today(), date_max), # 活動中なら現在の日付or最大月
        false = gradDate
      )
  ) |> 
  dplyr::left_join(
    member_df |> # メンバー名
      dplyr::select(memberID, memberName) |> 
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
    .by = c(memberID, groupID, joinDate, gradDate, memberName, colorCode)
  ) |> 
  dplyr::mutate(
    member_period = lubridate::interval(start = joinDate, end = date) |> 
      lubridate::time_length(unit = "month") |> 
      floor() # メンバー活動月数
  ) |> 
  dplyr::bind_rows(
    outside_df # 加入前月, 卒業翌月
  ) |> 
  dplyr::filter(date <= date_max) |> # 集計期間を抽出
  dplyr::arrange(date, joinDate, memberID) |> # 順位付け用
  dplyr::mutate(
    period_years  = member_period %/% 12, # メンバー活動年数
    period_months = member_period %% 12,  # メンバー活動月数 - 活動年数
    ranking       = dplyr::row_number(), # 順位
    .by = date
  ) |> 
  dplyr::select(
    date, memberID, memberName, colorCode, joinDate, 
    member_period, period_years, period_months, ranking
  )
rank_df


### 演出用データの作成 -----

# 0より大きい最小値を設定
dummy_val <- 1

# 描画枠の確保用のデータを作成:(全てのバーが0のときに表示位置が崩れる対応用)
dummy_df <- rank_df |> 
  dplyr::summarise(
    tmp_sum = sum(member_period), # 抽出用
    .by = date
  ) |> 
  dplyr::filter(tmp_sum == 0) |> # 全ての値が0の月を抽出
  dplyr::mutate(
    dummy_y = dummy_val # (初期表示範囲の調整用)
  )
dummy_df

# 平均活動月数を集計
mean_df <- rank_df |> 
  dplyr::summarise(
    max_period  = max(member_period, na.rm = TRUE),  # グループ最大活動月数
    mean_period = mean(member_period, na.rm = TRUE), # グループ平均活動月数
    member_num  = sum(memberName != " "),            # グループメンバー数
    .by = date
  ) |> # (疑似集計データを除いて計算)
  dplyr::mutate(
    period_years  = mean_period %/% 12, # グループ平均活動年数
    period_months = mean_period %% 12,  # グループ平均活動月数 - 活動年数
    label = paste(
      "メンバー数:", member_num, "人\n", 
      "平均活動月数:", period_years, "年", round(period_months, digits = 1), "か月"
    ), 
    max_period = (max_period == 0) |> 
      dplyr::if_else(
        true  = dummy_val, 
        false = max_period
      ) # (初期表示範囲の調整用)
  )
mean_df


### アニメーションの作成 -----

# 遷移フレーム数を指定
t <- 9

# 一時停止フレーム数を指定
s <- 1

# 1秒間に表示する月数を指定:(値が大きいと意図した通りにならない)
mps <- 3

# フレーム数を取得
n <- length(unique(rank_df[["date"]]))

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
    data    = mean_df, 
    mapping = aes(yintercept = mean_period), 
    color = "black", linetype = "dashed"
  ) + # 平均活動月数線
  geom_label(
    data    = mean_df, 
    mapping = aes(x = max_rank+0.5, y = max_period, label = label), 
    hjust = 1, vjust = 0, color = "black", fill = "white", 
    size = 4
  ) + # 平均活動月数ラベル
  geom_bar(
    data    = rank_df, 
    mapping = aes(
      x = ranking, y = member_period, group = factor(memberID), 
      fill = colorCode, color = colorCode
    ), 
    stat = "identity", width = 0.9, alpha = 0.8
  ) + # 活動月数バー
  geom_text(
    data    = rank_df, 
    mapping = aes(
      x = ranking, y = member_period, group = factor(memberID), 
      label = paste(" ", period_years, "年", round(period_months, digits = 1), "か月")
    ), 
    hjust = 0
  ) + # 活動月数ラベル
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
    title = paste0(group_name, "メンバーの活動月数の推移"), 
    subtitle = paste0(
      "{lubridate::year(closest_state)} 年 ", 
      "{stringr::str_pad(lubridate::month(closest_state), width = 2, pad = 0)} 月 ", 
      "01 日時点"
    )#, 
    #caption = "データ:「https://github.com/xxgentaroxx/HP_DB」"
  )

# 動画を作成
tmp_id <- paste0(group_id, collapse = "-") # (複数グループ用)
file_path <- paste0("ChartRace/output/PeriodSinceJoiningGroup_Member/PeriodSinceJoiningGroup_Member_", tmp_id, ".mp4")
m <- gganimate::animate(
  plot = anim, 
  nframes = (t+s)*n, fps = (t+s)*mps, 
  width = 900, height = 600, 
  renderer = gganimate::av_renderer(file = file_path)
)


# 歴代メンバーの在籍時の活動月数 -----------------------------------------------

## 加入から卒業以降も含めた活動月数の推移


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
    memberName    = " ", 
    member_period = 0, 
    add_label     = ""
  ) |> # 疑似集計データを追加
  dplyr::select(date, memberID, memberName, colorCode, member_period, add_label) |> 
  dplyr::arrange(memberID, date)
outside_df


### データの集計 -----

# 活動月数, 順位を集計
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
    member_df |> # メンバー名
      dplyr::select(memberID, memberName) |> 
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
  dplyr::reframe(
    date = seq(from = date_from, to = date_to, by = "month"), # 活動・卒業後月
    .by = c(memberID, groupID, joinDate, gradDate, memberName, colorCode)
  ) |> 
  dplyr::mutate(
    member_period = lubridate::interval(
      start = joinDate, 
      end   = (date <= gradDate) |> 
        dplyr::if_else(
          true  = date,    # 集計日
          false = gradDate # 卒業日
        )
    ) |> 
      lubridate::time_length(unit = "month") |> 
      floor(), # メンバー活動月数
    add_label = (date <= gradDate) |> 
      dplyr::if_else(
        true  = "(現)", # 現役ラベル
        false = "(卒)"  # 卒業ラベル
      )
  ) |> 
  dplyr::bind_rows(
    outside_df # 加入前月, 卒業翌月
  ) |> 
  dplyr::filter(date <= date_max) |> # 集計期間を抽出
  dplyr::arrange(date, -member_period, memberID) |> # 順位付け用
  dplyr::mutate(
    period_years  = member_period %/% 12, # メンバー活動年数
    period_months = member_period %% 12,  # メンバー活動月数 - 活動年数
    ranking = dplyr::row_number(), # 順位
    .by = date
  ) |> 
  dplyr::select(
    date, memberID, memberName, colorCode, joinDate, 
    member_period, period_years, period_months, ranking, add_label
  )
rank_df


### 演出用データの作成 -----

# 0より大きい値を設定
dummy_val <- 1

# 描画枠の確保用のデータを作成:(全てのバーが0のときに表示位置が崩れる対応用)
dummy_df <- rank_df |> 
  dplyr::summarise(
    tmp_sum = sum(member_period), # 抽出用
    .by = date
  ) |> 
  dplyr::filter(tmp_sum == 0) |> # 全ての値が0の月を抽出
  dplyr::mutate(
    dummy_y = dummy_val # (表示位置の調整用)
  )
dummy_df


### アニメーションの作成 -----

# 遷移フレーム数を指定
t <- 9

# 一時停止フレーム数を指定
s <- 1

# 1秒間に表示する月数を指定:(値が大きいと意図した通りにならない)
mps <- 3

# フレーム数を取得
n <- length(unique(rank_df[["date"]]))

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
      x = ranking, y = member_period, group = factor(memberID), 
      fill = colorCode, color = colorCode
    ), 
    stat = "identity", width = 0.9, alpha = 0.8
  ) + # 活動月数バー
  geom_text(
    data    = rank_df, 
    mapping = aes(
      x = ranking, y = member_period, group = factor(memberID), 
      label = paste(" ", period_years, "年", round(period_months, digits = 1), "か月", add_label)
    ), 
    hjust = 0
  ) + # 活動月数ラベル
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
    plot.margin   = margin(t = 10, r = 120, b = 10, l = 90, unit = "pt"), # 図の余白
    legend.position = "none" # 凡例の位置
  ) + 
  labs(
    title = paste0(group_name, "歴代メンバーの在籍時の活動月数の推移"), 
    subtitle = paste0(
      "{lubridate::year(closest_state)} 年 ", 
      "{stringr::str_pad(lubridate::month(closest_state), width = 2, pad = 0)} 月 ", 
      "01 日時点"
    )#, 
    #caption = "データ:「https://github.com/xxgentaroxx/HP_DB」"
  )

# 動画を作成
tmp_id <- paste0(group_id, collapse = "-") # (複数グループ用)
file_path <- paste0("ChartRace/output/PeriodSinceJoiningGroup_Member/PeriodSinceJoiningGroup_AllMember_", tmp_id, ".mp4")
m <- gganimate::animate(
  plot = anim, 
  nframes = (t+s)*n, fps = (t+s)*mps, 
  width = 900, height = 900, 
  renderer = gganimate::av_renderer(file = file_path)
)


