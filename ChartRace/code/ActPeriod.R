
# メンバーの活動期間をタイムラインで可視化 ---------------------------------------------------

# 利用パッケージ
library(tidyverse)
library(lubridate)
library(gganimate)

# チェック用
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
) |> 
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
) |> 
  dplyr::arrange(joinDate, memberID, groupID) # 昇順に並び替え
join_df

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


# 重複データを確認
join_df |> 
  dplyr::group_by(memberID, groupID) |> # 重複のカウント用にグループ化
  dplyr::mutate(n = dplyr::n()) |> # 重複をカウント
  dplyr::ungroup() |> # グループ化を解除
  dplyr::filter(n > 1) # 重複データを抽出

member_df |> 
  dplyr::group_by(memberID) |> # 重複のカウント用にグループ化
  dplyr::mutate(n = dplyr::n()) |> # 重複をカウント
  dplyr::ungroup() |> # グループ化を解除
  dplyr::filter(n > 1) # 重複データを抽出


# データの編集 -----------------------------------------------------------------

# 分割するグループ名を指定
#group_name <- "カントリー・ガールズ"

# グループを分割
#group_df <- 
group_df |> 
  dplyr::mutate(
    groupID = dplyr::if_else(
      groupName == group_name, true = max(groupID) + 1L, false = groupID
    ) # 指定したグループのIDを再設定
  )


# タイムラインによる可視化 ----------------------------------------------------------------------

### ・活動期間の集計 -----

# グループを指定
groupID_val <- 1

# メンバーごとに加入日と卒業日における年齢と活動日数を計算
chart_df <- join_df |> 
  # メンバー情報を追加
  dplyr::filter(groupID == groupID_val) |> # 指定したグループを抽出
  dplyr::arrange(joinDate, memberID) |> # メンバーIDの再設定用に並べ替え
  dplyr::mutate(
    member_id = dplyr::row_number(), # メンバーIDを再設定
    gradDate = dplyr::if_else(
      is.na(gradDate), true = lubridate::today(), false = gradDate
    ), # 現在活動中であれば現在の日付を設定
    joinDate2 = joinDate, # 加入日を複製
  ) |> # 加入・卒業情報を編集
  dplyr::left_join(
    member_df |> 
      dplyr::select(memberID, memberName, birthDate) |> # 利用する列を選択
      dplyr::group_by(memberID) |> # 重複の除去用にグループ化
      dplyr::slice_tail(n = 1) |> # 重複を除去:(slice_headなら改名前、slice_tailなら改名後を抽出)
      dplyr::ungroup(), # グループ化を解除
    by = "memberID"
  ) |> # メンバー情報を結合
  tidyr::pivot_longer(
    cols = c(joinDate, gradDate), 
    names_to = "date_type", 
    values_to = "date"
  ) |> # 加入日・卒業日の列をまとめる
  dplyr::select(date, date_type, member_id, memberName, birthDate, joinDate = joinDate2) |> # 利用するを選択
  dplyr::arrange(date, member_id) |> # 昇順に並べ替え
  # メンバー情報を編集
  dplyr::mutate(
    age = lubridate::interval(start = birthDate, end = date) |> 
      lubridate::time_length(unit = "year") |> 
      floor(), # 年齢を計算
    act_y = lubridate::interval(start = joinDate, end = date) |> 
      lubridate::time_length(unit = "year") |> 
      floor(), # 活動年数を計算
    act_m = lubridate::interval(start = joinDate, end = date) |> 
      lubridate::time_length(unit = "mon") |> 
      floor() %% 12, # 活動月数-年数を計算
    act_d = dplyr::case_when(
      lubridate::day(date) == lubridate::day(joinDate) ~ 0, # 日にちが同じなら、0
      lubridate::day(date) > lubridate::day(joinDate) ~ lubridate::day(date) - lubridate::day(joinDate) |> 
        as.numeric(), # 加入時の日にちが大きいなら、日にちの差
      and(
        lubridate::day(date) < lubridate::day(joinDate), 
        lubridate::day(lubridate::rollback(date)) < lubridate::day(joinDate)
      ) ~ lubridate::interval(
        start = date |> 
          lubridate::rollback(), # 前月の末日に変更
        end = date
      ) |> 
        lubridate::time_length(unit = "day"), # 卒業時の日にちが大きく前月に加入時の日にちが存在しない月なら、前月の末日との差
      lubridate::day(date) < lubridate::day(joinDate) ~ lubridate::interval(
        start = date |> 
          lubridate::rollback() |> # 1か月前の末日に変更
          lubridate::floor_date(unit = "mon") + lubridate::day(joinDate) - 1, # 加入時と同じ日にちに変更
        end = date
      ) |> 
        lubridate::time_length(unit = "day") # 卒業時の日にちが大きいなら、前月の加入時と同じ日にちとの差
    ), # 活動日数-年月数を計算
    act_days = lubridate::interval(start = joinDate, end = date) |> 
      lubridate::time_length(unit = "day"), # 活動日数を計算
    label = paste0(age, "歳：", act_y, "年", act_m, "か月", act_d, "日")
  ) |> # ラベル用の値を計算
  dplyr::select(date, member_id, memberName, age, act_y, act_m, act_d, act_days, label) # 利用する列を選択
chart_df


### ・タイムラインの作図 -----

# グループ名を設定
group_name <- group_df |> 
  dplyr::filter(groupID == groupID_val) |> # 指定したグループを抽出
  dplyr::pull(groupName) |> # ベクトルとして取得
  unique() |> # 重複を削除
  (\(x){x[1]})() # x[n]でn番目の要素を抽出
group_name

# x軸の値(年)を作成
date_vec <- seq(
  from = chart_df[["date"]] |> 
    min() |> # 最小値を取得
    lubridate::floor_date(unit = "year"), # 年単位で切り捨て
  to = chart_df[["date"]] |> 
    max() |> # 最大値を取得
    lubridate::ceiling_date(unit = "year"), # 年単位で切り上げ
  by = "year"
)

# 加入時ラベルを作成
label_join_df <- chart_df |> 
  dplyr::group_by(member_id) |> # データ抽出用にグループ化
  dplyr::filter(date == min(date)) |> # 加入時のデータを抽出
  dplyr::ungroup() |> # グループ化を解除
  dplyr::mutate(label = paste0(memberName, " (", age, "歳) ")) # 年齢ラベルを作成

# 卒業時ラベルを作成
label_grad_df <- chart_df |> 
  dplyr::group_by(member_id) |> # データ抽出用にグループ化
  dplyr::filter(date == max(date)) |> # 卒業時のデータを抽出
  dplyr::ungroup() |> # グループ化を解除
  dplyr::mutate(label = paste0(" (", age, "歳：", act_y, "年", act_m, "か月", act_d, "日)")) # 年齢・活動年数ラベルを作成


# タイムラインを作成:デフォルトの配色
ggplot() + 
  geom_line(data = chart_df, mapping = aes(x = date, y = member_id, color = factor(member_id)), 
            size = 2) + # 活動期間ライン
  geom_text(data = label_join_df, mapping = aes(x = date, y = member_id, label = label, color = factor(member_id)), 
            hjust = 1) + # 加入時ラベル
  geom_text(data = label_grad_df, mapping = aes(x = date, y = member_id, label = label, color = factor(member_id)), 
            hjust = 0) + # 卒業時ラベル
  scale_x_date(breaks = date_vec, date_labels = "%Y-%m", guide = guide_axis(angle = 45), expand = c(0, 0)) + # x軸目盛
  scale_y_reverse(breaks = seq(0, max(chart_df[["member_id"]]), by = 10)) + # y軸を反転
  coord_cartesian(clip = "off") + # 表示範囲
  theme(
    plot.title = element_text(color = "black", face = "bold", size = 20, hjust = 0.5), # 全体のタイトル
    plot.subtitle = element_text(color = "black", size = 15, hjust = 0.5), # 全体のサブタイトル
    plot.margin = margin(t = 10, r = 120, b = 10, l = 80, unit = "pt"), # 全体の余白
    legend.position = "none" # 凡例の表示位置
  ) + # 図の体裁
  labs(
    title = paste0(group_name, "歴代メンバーの活動期間"), 
    subtitle = paste0(
      format(min(chart_df[["date"]]), format = "%Y年%m月%d日"), 
      "～", format(max(chart_df[["date"]]), format = "%Y年%m月%d日"), 
      "：総メンバー", max(chart_df[["member_id"]]), "人"
    ), 
    x = "年-月", y = "メンバー", 
    caption = "データ:「https://github.com/xxgentaroxx/HP_DB」"
  ) # ラベル

# タイムラインを作成:メンバーカラーで配色(color_list.Rを参照)
ggplot() + 
  geom_line(data = chart_df, mapping = aes(x = date, y = member_id, color = memberName), 
            size = 2) + # 活動期間ライン
  geom_label(data = label_join_df, mapping = aes(x = date, y = member_id, label = label, color = memberName), 
             hjust = 1, fill = "gray92", label.size = 0, label.padding = unit(0.09, units = "lines")) + # 加入時ラベル
  geom_label(data = label_grad_df, mapping = aes(x = date, y = member_id, label = label, color = memberName), 
             hjust = 0, fill = "gray92", label.size = 0, label.padding = unit(0.09, units = "lines")) + # 卒業時ラベル
  scale_color_manual(breaks = color_df[["member_name"]], values = color_df[["color_code"]]) + # 線の色
  scale_fill_manual(breaks = color_df[["member_name"]], values = color_df[["color_code"]]) + # 塗りつぶしの色
  scale_x_date(breaks = date_vec, date_labels = "%Y-%m", guide = guide_axis(angle = 45), expand = c(0, 0)) + # x軸目盛
  scale_y_reverse(breaks = seq(0, max(chart_df[["member_id"]]), by = 10)) + # y軸を反転
  coord_cartesian(clip = "off") + # 表示範囲
  theme(
    plot.title = element_text(color = "black", face = "bold", size = 20, hjust = 0.5), # 全体のタイトル
    plot.subtitle = element_text(color = "black", size = 15, hjust = 0.5), # 全体のサブタイトル
    #plot.background = element_rect(fill = "gray"), # 全体の背景
    plot.margin = margin(t = 10, r = 120, b = 10, l = 80, unit = "pt"), # 全体の余白
    legend.position = "none" # 凡例の表示位置
  ) + # 図の体裁
  labs(
    title = paste0(group_name, "歴代メンバーの活動期間"), 
    subtitle = paste0(
      format(min(chart_df[["date"]]), format = "%Y年%m月%d日"), 
      "～", format(max(chart_df[["date"]]), format = "%Y年%m月%d日"), 
      "：総メンバー", max(chart_df[["member_id"]]), "人"
    ), 
    x = "年-月", y = "メンバー", 
    caption = "データ:「https://github.com/xxgentaroxx/HP_DB」"
  ) # ラベル


# 月次アニメーションによる可視化 --------------------------------------------------------------

### ・活動期間の集計 -----

# グループを指定
groupID_val <- 1

# 活動期間を集計
anime_df <- group_df |> 
  # 活動期間に対応した行を作成
  dplyr::filter(groupID == groupID_val) |> # 指定したグループを抽出
  dplyr::mutate(
    formDate = formDate |> 
      lubridate::floor_date(unit = "mon"), 
    dissolveDate = dplyr::if_else(
      is.na(dissolveDate), true = lubridate::today(), false = dissolveDate
    ) |> # 現在活動中であれば現在の日付を設定
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
  dplyr::select(date, groupID, groupName) |> # 利用する列を取得
  # メンバー情報を追加
  tidyr::expand_grid(
    join_df |> 
      dplyr::filter(groupID == groupID_val) |> # 指定したグループを抽出
      dplyr::arrange(joinDate, memberID) |> # メンバーIDの再設定用に並べ替え
      dplyr::mutate(
        member_id = dplyr::row_number(), # メンバーIDを再設定
        gradDate = dplyr::if_else(
          is.na(gradDate), true = lubridate::today(), false = gradDate
        ) # 現在活動中であれば現在の日付を設定
      ) |> 
      dplyr::select(!groupID), # 結合時に重複する列を削除, 
  ) |> # 日付情報を複製してメンバーIDを結合
  dplyr::left_join(
    member_df |> 
      dplyr::select(memberID, memberName, birthDate) |> # 利用する列を取得
      dplyr::group_by(memberID) |> # 
      dplyr::slice_tail(n = 1), # 重複を除去:(slice_headなら改名前、slice_tailなら改名後を抽出)
    by = "memberID"
  ) |> # メンバー情報を結合
  dplyr::arrange(date, member_id) |> # 昇順に並べ替え
  dplyr::filter(date >= joinDate, date <= gradDate) |> # 活動期間中のデータを抽出
  # メンバー情報を編集
  dplyr::mutate(
    age = lubridate::interval(start = birthDate, end = date) |> 
      lubridate::time_length(unit = "year") |> 
      floor(), # 年齢を計算
    act_y = lubridate::interval(start = joinDate, end = date) |> 
      lubridate::time_length(unit = "year") |> 
      floor(), # 活動年数を計算
    act_m = lubridate::interval(start = joinDate, end = date) |> 
      lubridate::time_length(unit = "mon") |> 
      floor() %% 12, # 活動月数-年数を計算
    act_d = dplyr::if_else(
      lubridate::day(lubridate::rollback(date)) >= lubridate::day(joinDate), # 加入日の日にちが存在しない月の場合
      true = lubridate::interval(
        start = date |> 
          lubridate::rollback() |> # 1か月前の末日に変更
          lubridate::floor_date(unit = "mon") + lubridate::day(joinDate) - 1, # 加入日と同じ日にちに変更
        end = date
      ) |> 
        lubridate::time_length(unit = "day"), # 日にちの差を計算
      false = 1
    ), # 活動日数-年月数を計算
    act_days = lubridate::interval(start = joinDate, end = date) |> 
      lubridate::time_length(unit = "day"), # 活動日数を計算
    label = paste0(
      memberName, " (", age, "歳：", 
      act_y, "年", act_m, "か月", stringr::str_pad(act_d, width = 2, pad = 0), "日)"
    )
  ) |> # ラベルを作成
  dplyr::select(date, groupName, member_id, memberName, age, act_y, act_m, act_d, act_days, label) # 利用する列を取得
anime_df


### ・タイムラインの作図 -----

# グループ名を設定
group_name <- anime_df[["groupName"]] |> 
  unique() |> # 重複を削除
  (\(x){x[1]})() # x[n]でn番目の要素を抽出
group_name

# 在籍数を計算
member_n_df <- anime_df |> 
  dplyr::count(date, name = "member_n") |> # メンバー数を集計
  dplyr::mutate(label = paste0("在籍数：", member_n, "人")) # ラベルを作成

# x軸の値(年)を作成
date_vec <- seq(
  from = anime_df[["date"]] |> 
    min() |> # 最小値を取得
    lubridate::floor_date(unit = "year"), # 年単位で切り捨て
  to = anime_df[["date"]] |> 
    max() |> # 最大値を取得
    lubridate::ceiling_date(unit = "year"), # 年単位で切り上げ
  by = "year"
)


# 1秒間に表示する月数を指定:(値が大きいと意図した通りにならない)
mps <- 6

# 最後のグラフでの停止フレーム数を指定
ep <- 30

# フレーム数を取得
n <- length(unique(anime_df[["date"]]))


# タイムラインのアニメーションを作成:デフォルトの配色
anim <- ggplot(data = anime_df, mapping = aes(x = date, y = member_id, color = factor(member_id))) + 
  geom_vline(mapping = aes(xintercept = date), 
             color = "gray56", size = 1, linetype = "dashed") + # 時間経過の垂線
  geom_label(data = member_n_df, mapping = aes(x = date, y = 0, label = label), 
             vjust = 0, color = "gray56") + # 在籍数ラベル
  geom_line(size = 2) + # 活動期間ライン
  geom_point(size = 4) + # 活動期間終点
  geom_text(mapping = aes(label = paste("  ", label)), 
            hjust = 0) + # メンバーラベル
  gganimate::transition_reveal(date) + # フレーム
  scale_x_date(breaks = date_vec, date_labels = "%Y", expand = c(0, 0)) + # x軸目盛
  scale_y_reverse(breaks = seq(0, max(anime_df[["member_id"]]), by = 10)) + # y軸を反転
  coord_cartesian(clip = "off") + # 表示範囲
  theme(
    plot.title = element_text(color = "black", face = "bold", size = 20, hjust = 0.5), # 全体のタイトル
    plot.subtitle = element_text(color = "black", size = 15, hjust = 0.5), # 全体のサブタイトル
    plot.margin = margin(t = 10, r = 200, b = 20, l = 40, unit = "pt"), # 全体の余白
    legend.position = "none" # 凡例の表示位置
  ) + # 図の体裁
  labs(
    title = paste0(group_name, "歴代メンバーの活動期間"), 
    subtitle = paste0("{format(frame_along, format = '%Y年%m月')}01日時点"), 
    x = "年", y = "メンバー", 
    caption = "データ:「https://github.com/xxgentaroxx/HP_DB」"
  ) # ラベル

# タイムラインを作成:メンバーカラーで配色(color_list.Rを参照)
anim <- ggplot(data = anime_df, mapping = aes(x = date, y = member_id, color = memberName)) + 
  geom_vline(mapping = aes(xintercept = date), 
             color = "gray56", size = 1, linetype = "dashed") + # 時間経過の垂線
  geom_label(data = member_n_df, mapping = aes(x = date, y = 0, label = label), 
             vjust = 0, color = "gray56") + # 在籍数ラベル
  geom_label(mapping = aes(label = paste("  ", label)), 
             hjust = 0, fill = "gray92", label.size = 0, label.padding = unit(0.1, units = "lines")) + # メンバーラベル
  geom_line(size = 2) + # 活動期間ライン
  geom_point(size = 4) + # 活動期間終点
  gganimate::transition_reveal(date) + # フレーム
  scale_color_manual(breaks = color_df[["member_name"]], values = color_df[["color_code"]]) + # 線の色
  scale_x_date(breaks = date_vec, date_labels = "%Y", guide = guide_axis(angle = 0), expand = c(0, 0)) + # x軸目盛
  scale_y_reverse(breaks = seq(0, max(anime_df[["member_id"]]), by = 10)) + # y軸を反転
  coord_cartesian(clip = "off") + # 表示範囲
  theme(
    plot.title = element_text(color = "black", face = "bold", size = 20, hjust = 0.5), # 全体のタイトル
    plot.subtitle = element_text(color = "black", size = 15, hjust = 0.5), # 全体のサブタイトル
    plot.margin = margin(t = 10, r = 200, b = 20, l = 40, unit = "pt"), # 全体の余白
    legend.position = "none" # 凡例の表示位置
  ) + # 図の体裁
  labs(
    title = paste0(group_name, "歴代メンバーの活動期間"), 
    subtitle = paste0("{format(lubridate::as_date(frame_along)+3, format = '%Y年%m月')}01日時点"), 
    x = "年", y = "メンバー", 
    caption = "データ:「https://github.com/xxgentaroxx/HP_DB」"
  ) # ラベル


# gif画像を作成
g <- gganimate::animate(
  anim, nframes = n+ep, end_pause = ep, fps = mps, width = 1200, height = 900
)
g

# gif画像を保存
gganimate::anim_save(filename = "BarChartRace/output/Member_Act_beyooooonds.gif", animation = g)

# 動画を作成と保存
m <- gganimate::animate(
  plot = anim, 
  nframes = n+ep, end_pause = ep, fps = mps,  
  width = 1200, height = 900, 
  renderer = gganimate::av_renderer(file = "BarChartRace/output/Member_Act_beyooooonds.mp4")
)



# 日次で活動期間の集計 --------------------------------------------------------------


# グループを指定
groupID_val <- 1

# メンバーごとの活動期間を集計
chart_df <- group_df |> 
  # 活動期間に対応した行を作成
  dplyr::filter(groupID == groupID_val) |> # 指定したグループを抽出
  dplyr::mutate(
    dissolveDate = dplyr::if_else(
      is.na(dissolveDate), true = lubridate::today(), false = dissolveDate
    ), # 現在活動中であれば現在の日付を設定
    n = lubridate::interval(start = formDate, end = dissolveDate) |> 
      lubridate::time_length(unit = "day") + 1
  ) |> # 日数をカウント
  tidyr::uncount(n) |> # 日数に応じて行を複製
  dplyr::group_by(groupName) |> # 行番号用にグループ化
  dplyr::mutate(idx = dplyr::row_number()) |> # 行番号を割り当て
  dplyr::group_by(groupName, idx) |> # 1日刻みの値の作成用にグループ化
  dplyr::mutate(date = seq(from = formDate, to = dissolveDate, by = "day")[idx]) |> # 複製した行を1日刻みの値に変更
  dplyr::ungroup() |> # グループ化を解除
  dplyr::select(date, groupID, groupName) |> # 利用する列を取得
  # メンバー情報を追加
  tidyr::expand_grid(
    join_df |> 
      dplyr::filter(groupID == groupID_val) |> # 指定したグループを抽出
      dplyr::arrange(joinDate, memberID) |> # メンバーIDの再設定用に並べ替え
      dplyr::mutate(
        member_id = dplyr::row_number(), # メンバーIDを再設定
        gradDate = dplyr::if_else(
          is.na(gradDate), true = lubridate::today(), false = gradDate
        ) # 現在活動中であれば現在の日付を設定
      ) |> 
      dplyr::select(!groupID), # 結合時に重複する列を削除
  ) |> # 日付情報を複製してメンバーIDを結合
  dplyr::left_join(
    member_df |> 
      dplyr::select(memberID, memberName, birthDate) |> # 利用する列を取得
      dplyr::group_by(memberID) |> # 
      dplyr::slice_tail(n = 1), # 重複を除去:(slice_headなら改名前、slice_tailなら改名後を抽出)
    by = "memberID"
  ) |> # メンバー情報を結合
  dplyr::filter(date >= joinDate, date <= gradDate) |> # 活動期間中のデータを抽出
  dplyr::arrange(date, member_id) |> # 昇順に並べ替え
  # メンバー情報を編集
  dplyr::mutate(
    age = lubridate::interval(start = birthDate, end = date) |> 
      lubridate::time_length(unit = "year") |> 
      floor(), # 年齢を計算
    act_y = lubridate::interval(start = joinDate, end = date) |> 
      lubridate::time_length(unit = "year") |> 
      floor(), # 活動年数を計算
    act_m = lubridate::interval(start = joinDate, end = date) |> 
      lubridate::time_length(unit = "mon") |> 
      floor() %% 12, # 活動月数-年数を計算
    act_days = lubridate::interval(start = joinDate, end = date) |> 
      lubridate::time_length(unit = "day") # 活動日数を計算
  ) |> # ラベル用の値を計算
  dplyr::group_by(member_id, act_y, act_m) |> # 活動日数-年月数の計算用にグループ化
  dplyr::mutate(
    act_d = dplyr::row_number() - 1, # 活動日数-年月数を計算
    label = paste0(age, "歳：", act_y, "年", act_m, "か月", act_d, "日")
  ) |> # ラベルを作成
  dplyr::ungroup() |> # グループ化を解除
  dplyr::select(date, groupID, groupName, member_id, memberName, age, act_y, act_m, act_d, act_days, label) # 利用する列を取得
chart_df


