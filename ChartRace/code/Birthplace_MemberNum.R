
# 任意のグループの出身地ごとのメンバー数の推移 ---------------------------------


# 編集メモ --------------------------------------------------------------------

## 生年月日非公開メンバーを含むグループは非対応
## 複数グループの指定は非対応

## 歴代メンバーが累積していく図を作りたい
## 地図の作図をsfに置き換えたい


# パッケージの読込 -------------------------------------------------------------

# 利用パッケージ
library(tidyverse)
library(gganimate)
library(ggtext)
library(mapdata)

# パッケージ名の省略用
library(ggplot2)

# データの読込用
library(mapdata)


# データの読込 -----------------------------------------------------------------

## ReadData.Rを参照

# 利用データを確認
join_df   # 加入・卒業日一覧
member_df # メンバー一覧
reg_df    # 都道府県名の対応表

# 地図データを取得
base_map_df <- ggplot2::map_data(map = "japan") |> # 日本地図を取得
  tibble::as_tibble() |> 
  dplyr::mutate(region = stringr::str_to_lower(region)) |> # 表記を小文字に統一
  dplyr::select(group, order, region, long, lat)


# 集計の設定 -------------------------------------------------------------------

### グループの設定 -----

# グループを指定
group_id <- 9

# タイトル用の文字列を指定
group_name <- "ばってん少女隊"


### 期間の設定 -----

# 集計終了月を指定
date_max <- "2025-07-01" |> # 任意の日
  lubridate::as_date() |> 
  lubridate::floor_date(unit = "month")
date_max <- lubridate::today() |> # 実行日
  lubridate::floor_date(unit = "month")
date_max <- group_df |> 
  dplyr::filter(groupID %in% group_id) |> 
  dplyr::pull(dissolveDate) |> 
  (\(vec) {max(vec, lubridate::today(), na.rm = TRUE)})() |> # 解散日or実行日
  lubridate::floor_date(unit = "month")
date_max


# メンバーの出身地数 -----------------------------------------------------------

## 各月における在籍メンバーの出身地ごとのメンバー数の推移
## 出身地の多い順に配置


### データの集計 -----

# 出身地数を集計
count_df <- join_df |> # メンバーID, 加入日, 卒業日
  dplyr::filter(groupID == group_id) |> # グループを抽出
  dplyr::mutate(
    gradDate = gradDate |> 
      is.na() |> 
      dplyr::if_else(
        true  = max(lubridate::today(), date_max), # 活動中なら実行日or集計終了月
        false = gradDate # 卒業日
      )
  ) |> 
  dplyr::left_join(
    member_df |> # 出身地
      dplyr::select(memberID, memberName, birthplace) |> 
      dplyr::distinct(memberID, .keep_all = TRUE), # 重複を除去
    by = "memberID"
  ) |> 
  dplyr::left_join(
    reg_df |> # 都道府県名のアルファベット表記
      dplyr::select(birthplace = prefecture_jp, region = prefecture_en), 
    by = "birthplace"
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
    .by = c(memberID, joinDate, gradDate, memberName, birthplace, region)
  ) |> 
  dplyr::summarise(
    member_num = dplyr::n(), # 出身地ごとのメンバー数
    .by = c(date, birthplace, region)
  ) |> 
  dplyr::arrange(date, region)
count_df


### 演出用データの作成 -----

# 出身地構成が変化する前後月のデータを作成:(バーの変化の強調用)
outside_df <- count_df |> 
  tidyr::complete(
    date = seq(
      from = count_df |> 
        dplyr::pull(date) |> 
        min() |> # 集計開始月
        lubridate::rollback() |> 
        lubridate::floor_date(unit = "month"), # 前月
      to   = count_df |> 
        dplyr::pull(date) |> 
        max(), # 集計終了月
      by = "month"
    ), # 活動月
    birthplace = count_df |> 
      dplyr::pull(birthplace) |> 
      unique() |> 
      sort(), # メンバー出身地
    fill = list(member_num = 0)
  ) |> # メンバー数0のデータを補完
  dplyr::select(-region) |> # 都道府県名のアルファベット表記の再設定用
  dplyr::left_join(
    reg_df |> # 都道府県名のアルファベット表記
      dplyr::select(birthplace = prefecture_jp, region = prefecture_en), 
    by = "birthplace"
  ) |> 
  dplyr::arrange(region, date) |> # 変化量の計算用
  dplyr::mutate(
    lead_num = member_num |> 
      dplyr::lead(n = 1, default = NA), # 翌月の値
    lag_num  = member_num |> 
      dplyr::lag(n = 1, default = 0),   # 前月の値
    lead_diff_num = lead_num - member_num, # 翌月との変化量
    lag_diff_num  = member_num - lag_num,  # 前月との変化量
    .by = region
  ) |> 
  dplyr::filter(member_num == 0, lead_diff_num != 0 | lag_diff_num != 0) |> # 疑似データを抽出
  dplyr::select(date, birthplace, region, member_num)
outside_df


### データの集計 -----

# 順位を集計
rank_df <- count_df |> 
  dplyr::bind_rows(
    outside_df # 出身地構成の変化の前・翌月
  ) |> 
  dplyr::filter(date <= date_max) |> # 集計期間を抽出
  dplyr::arrange(date, -member_num, region) |> # 順位付け用
  dplyr::mutate(
    ranking = dplyr::row_number(), # 順位
    .by = date
  )
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

# 総メンバー数を集計
label_df <- rank_df |> 
  dplyr::left_join(
    reg_df |> # 緯度
      dplyr::select(region = prefecture_en, lat = latitude), 
    by = "region"
  ) |> 
  dplyr::mutate(
    tmp_lat = (member_num > 0) |> 
      dplyr::if_else(
        true  = lat, 
        false = NA # 疑似データの場合
      )
  ) |> 
  dplyr::summarise(
    max_num   = max(member_num), # 最大メンバー数
    total_num = sum(member_num), # 総メンバー数
    max_lat  = max(tmp_lat, na.rm = TRUE), # 最北端
    min_lat  = min(tmp_lat, na.rm = TRUE), # 最南端
    .by = date
  ) |> # (疑似データを除いて計算)
  dplyr::left_join(
    reg_df |> # 都道府県名
      dplyr::select(southern_region = prefecture_jp, max_lat = latitude), 
    by = "max_lat"
  ) |> # 最北端の都道府県名を追加
  dplyr::left_join(
    reg_df |> # 都道府県名
      dplyr::select(northern_region = prefecture_jp, min_lat = latitude), 
    by = "min_lat"
  ) |> # 最南端の都道府県名を追加
  dplyr::mutate(
    max_num = (max_num == 0) |> 
      dplyr::if_else(
        true  = dummy_val, # (初期表示範囲の調整用)
        false = max_num
      ), 
    num_label = paste(
      "メンバー数:", total_num, "人<br>", 
      "最北端:", "<span style='color:#ffffff;'>...</span>", southern_region, "<br>", # (白文字による文字揃え)
      "最南端:", "<span style='color:#ffffff;'>...</span>", northern_region
    )
  ) |> 
  dplyr::select(date, max_num, num_label)
label_df


### アニメーションの作成 -----

# 遷移フレーム数を指定
t <- 9

# 一時停止フレーム数を指定
s <- 1

# 1秒間に表示する月数を指定:(値が大きいと意図した通りにならない)
mps <- 3

# フレーム数を取得
n <- rank_df |> 
  dplyr::pull(date) |> 
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
    mapping = aes(x = max_rank+0.5, y = max_num, label = num_label),
    hjust = 1, vjust = 0, color = "black", fill = "white",
    halign = 0, valign = 0.5, width = unit(1.75, units = "inch"),
    size = 4
  ) + # 総メンバー数ラベル
  geom_bar(
    data    = rank_df, 
    mapping = aes(
      x = ranking, y = member_num, group = factor(region), 
      fill = factor(region), color = factor(region)
    ), 
    stat = "identity", width = 0.9, alpha = 0.8
  ) + # メンバー数バー
  geom_text(
    data    = rank_df, 
    mapping = aes(
      x = ranking, y = member_num, group = factor(region), 
      label = paste(" ", member_num, "人")
    ), 
    hjust = 0
  ) + # メンバー数ラベル
  geom_text(
    data    = rank_df, 
    mapping = aes(
      x = ranking, y = 0, group = factor(region), 
      label = paste(birthplace, "  ")
    ), 
    hjust = 1
  ) + # 出身地ラベル
  gganimate::transition_states(states = date, transition_length = t, state_length = s, wrap = FALSE) + # フレームの制御
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
    plot.margin   = margin(t = 10, r = 60, b = 10, l = 60, unit = "pt"), # 図の余白
    legend.position = "none" # 凡例の位置
  ) + 
  labs(
    title    = paste0(group_name, "：メンバーの出身地構成の推移"), 
    subtitle = paste(
      "{lubridate::year(closest_state)} 年", 
      "{sprintf(fmt = '%02d', lubridate::month(closest_state))} 月", 
      "01 日時点"
    )#, 
    #caption = "データ:「https://github.com/xxgentaroxx/HP_DB」"
  )

# 動画を作成
file_path <- paste0("ChartRace/output/birthplace_MemberNum/birthplace_", agcy_flag, group_id, ".mp4")
m <- gganimate::animate(
  plot = anim, 
  nframes = (t+s)*n, fps = (t+s)*mps, 
  width = 900, height = 600, 
  renderer = gganimate::av_renderer(file = file_path)
)


# メンバーの出身地分布 ---------------------------------------------------------

## 各月における在籍メンバーの出身地ごとの数の推移
## 地図上にヒートマップで可視化

### データの集計 -----

## 「メンバーの出身地数」を参照

# メンバー数を集計
count_df


### 作図用データの作成 -----

# 地図データを取得
map_df <- base_map_df |> 
  dplyr::reframe(
    date = count_df |> # 活動月
      dplyr::filter(date <= date_max) |> # 集計期間を抽出
      dplyr::pull(date) |> 
      unique() |> 
      sort(), 
    .by = dplyr::everything()
  ) |> 
  dplyr::left_join(
    count_df |> # 都道府県名, メンバー数
      dplyr::select(date, region, member_num), 
    by = c("date", "region")
  ) |> 
  dplyr::mutate(
    member_num = member_num |> 
      is.na() |> 
      dplyr::if_else(
        true  = 0, 
        false = member_num
      ) # メンバーなしの欠損値を補完
  )
map_df


### 演出用データの作成 -----

# 総メンバー数を集計
label_df <- count_df |> 
  dplyr::summarise(
    member_num = sum(member_num), # 総メンバー数
    .by = date
  ) |> 
  dplyr::mutate(
    num_label = paste("総メンバー数: ", member_num, "人")
  )
label_df

# ラベルの表示位置を設定
count_label_df <- count_df |> 
  dplyr::left_join(
    reg_df |> # 経度, 緯度
      dplyr::select(region = prefecture_en, long = longitude, lat = latitude), 
    by = "region"
  )
count_label_df


### アニメーションの作成 -----

# 遷移フレーム数を指定
t <- 9

# 一時停止フレーム数を指定
s <- 1

# 1秒間に表示する月数を指定:(値が大きいと意図した通りにならない)
mps <- 3

# フレーム数を取得
n <- map_df |> 
  dplyr::pull(date) |> 
  unique() |> 
  length()

# ヒートマップを作図
anim <- ggplot() +
  geom_polygon(
    data    = map_df, 
    mapping = aes(x = long, y = lat, group = group, fill = member_num), 
    color = "gray", linewidth = 0
  ) + # メンバー数
  geom_text(
    data = count_label_df, 
    mapping = aes(x = long, y = lat, label = member_num), 
    size = 4
  ) + # 出身地数ラベル
  ggtext::geom_textbox(
    data    = label_df, 
    mapping = aes(x = Inf, y = -Inf, label = num_label), 
    hjust = 1, vjust = 0, color = "black", fill = "white", 
    halign = 0, valign = 0.5, width = unit(1.5, units = "inch"), 
    size = 4
  ) + # 総メンバー数ラベル
  gganimate::transition_manual(frames = date) + # フレームの制御
  scale_fill_gradient2(low = NA, mid = "white", high = "hotpink") + # 配色
  coord_quickmap() + # 緯度経度軸
  theme(
    plot.title    = element_text(color = "black", face = "bold", size = 20, hjust = 0.5), # 図タイトル
    plot.subtitle = element_text(color = "black", size = 15, hjust = 0.5) # 図サブタイトル
  ) + 
  labs(
    title = paste0(group_name, "：メンバーの出身地構成の推移"), 
    subtitle = paste(
      "{lubridate::year(current_frame)} 年", 
      "{sprintf(fmt = '%02d', lubridate::month(current_frame))} 月", 
      "01 日時点"
    ), 
    fill = "メンバー数", 
    x = "longitude", 
    y = "latitude"
  )

# 動画を作成
file_path <- paste0("ChartRace/output/birthplace_MemberNum/birthplace_heatmap_", agcy_flag, group_id, ".mp4")
m <- gganimate::animate(
  plot = anim, 
  nframes = n, fps = mps, 
  width = 900, height = 900, 
  renderer = gganimate::av_renderer(file = file_path)
)


