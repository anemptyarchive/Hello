
# メンバーの活動期間をタイムラインで可視化 ---------------------------------------------------

# 利用パッケージ
library(tidyverse)
library(lubridate)
library(gganimate)

# チェック用
library(ggplot2)


# データの読込 -----------------------------------------------------------------

### ・利用データ -----

## ReadData.Rを参照

# group.csv
group_df

# join.csv
join_df

# memner.csv
member_df


# 重複を確認
join_df |> 
  dplyr::group_by(memberID, groupID) |> # カウント用
  dplyr::mutate(n = dplyr::n()) |> # 同一データをカウント
  dplyr::ungroup() |> 
  dplyr::filter(n > 1) # 重複データを抽出
member_df |> 
  dplyr::group_by(memberID) |> # カウント用
  dplyr::mutate(n = dplyr::n()) |> # 同一データをカウント
  dplyr::ungroup() |> 
  dplyr::filter(n > 1) # 重複データを抽出


### ・色データ -----

## MemberColorSample.Rを参照

# メンバーカラーデータ
color_df


# データの編集 -----------------------------------------------------------------

# 分割するグループ名を指定
#group_name <- "カントリー・ガールズ"

# グループを分割
#group_df <- group_df |> 
  dplyr::mutate(
    groupID = dplyr::if_else(
      groupName == group_name, true = max(groupID) + 1L, false = groupID
    ) # 指定したグループのIDを再設定
  )


# タイムラインによる可視化 ----------------------------------------------------------------------

### ・活動期間の集計：(加入日・卒業日) -----

# グループを指定
group_id <- 1

# 集計の終了日を指定
date_to <- "2023-02-01" |> 
  lubridate::as_date()
date_to <- lubridate::today()


# メンバーごとに加入日・卒業日における年齢・活動日数を集計
chart_df <- join_df |> 
  # データの整形
  dplyr::filter(groupID == group_id, joinDate <= date_to) |> # 指定したグループ・期間内の在籍メンバーを抽出
  dplyr::arrange(joinDate, memberID) |> # メンバーIDの再設定用
  dplyr::mutate(
    member_id = dplyr::row_number(), # メンバーIDを再設定
    gradDate = dplyr::if_else(is.na(gradDate) | gradDate > date_to, true = date_to, false = gradDate), # 活動中メンバーの卒業日に指定した日付を設定
    tmp_joinDate = joinDate, # 加入日列を複製
  ) |> # 加入・卒業情報を編集
  dplyr::left_join(
    member_df |> 
      dplyr::select(memberID, memberName, birthDate) |> 
      dplyr::group_by(memberID) |> # 重複の除去用
      dplyr::slice_tail(n = 1) |> # 重複を除去:(slice_headなら改名前、slice_tailなら改名後を抽出)
      dplyr::ungroup(), 
    by = "memberID"
  ) |> # メンバー情報を結合
  tidyr::pivot_longer(
    cols = c(tmp_joinDate, gradDate), 
    names_to = "date_type", 
    names_prefix = "tmp_", 
    values_to = "date"
  ) |> # 加入日・卒業日の列をまとめる
  # ラベルの作成
  dplyr::mutate(
    age = lubridate::interval(start = birthDate, end = date) |> 
      lubridate::time_length(unit = "year") |> 
      floor(), # 年齢を計算
    act_y = lubridate::interval(start = joinDate, end = date) |> 
      lubridate::time_length(unit = "year") |> 
      floor(), # 活動年数を計算
    act_m = lubridate::interval(start = joinDate, end = date) |> 
      lubridate::time_length(unit = "month") |> 
      floor() %% 12, # 活動月数-年数を計算
    date_day = date |> 
      lubridate::day(), # 基準日(加入日・卒業日)の日にち
    last_day = date |> 
      lubridate::rollback() |> 
      lubridate::day(), # 基準日の前月末の日にち
    join_day = joinDate |> 
      lubridate::day(), # 加入日の日にち
    act_d = dplyr::case_when(
      date_day >= join_day ~ (date_day - join_day) |> 
        as.numeric(), # 基準日の日にちが加入日の日にち以上の場合「加入日の日にちとの差」
      all(date_day < join_day, last_day < join_day) ~ lubridate::interval(
        start = date |> 
          lubridate::rollback(), # 前月の末日に変更
        end = date
      ) |> 
        lubridate::time_length(unit = "day"), # 加入日の日にちが無い月の場合は「前月末の日にちとの差」
      all(date_day < join_day, last_day >= join_day) ~ lubridate::interval(
        start = date |> 
          lubridate::rollback() |> 
          lubridate::floor_date(unit = "month") + join_day - 1, # 前月の加入日に変更
        end = date
      ) |> 
        lubridate::time_length(unit = "day") # 加入日の日にちが有る月の場合は「前月の加入時と同じ日にちとの差」
    ), # 活動日数-年月数を計算
    act_days = lubridate::interval(start = joinDate, end = date) |> 
      lubridate::time_length(unit = "day"), # 活動日数を計算
    label = paste0(age, "歳：", act_y, "年", act_m, "か月", act_d, "日") # 年齢・活動年数ラベル
  ) |> 
  dplyr::select(date, member_id, memberName, age, act_y, act_m, act_d, act_days, label) |> 
  dplyr::arrange(date, member_id)
chart_df


### ・タイムラインの作図 -----

# グループ名を設定
group_name <- group_df |> 
  dplyr::filter(groupID == group_id) |> # 指定したグループを抽出
  dplyr::pull(groupName) |> # グループ名列を取得
  unique() |> # 重複を削除
  (\(.) {.[1]})() # 最初の要素を抽出
  #(\(.) {.[length(.)]})() # 最後の名前を抽出
group_name

# 加入時・卒業時ラベルを作成
label_df <- chart_df |> 
  dplyr::group_by(member_id) |> # データの抽出用
  dplyr::filter(date == min(date) | date == max(date)) |> # ハロプロ加入時・卒業時のデータを抽出
  dplyr::mutate(
    label = dplyr::case_when(
      date == min(date) ~ paste0(memberName, " (", age, "歳) "), # 加入時の年齢ラベル
      date == max(date) ~ paste0(" (", age, "歳：", act_y, "年", act_m, "か月", act_d, "日)") # 卒業時の年齢・活動年数ラベル
    ), 
    h = dplyr::case_when(
      date == min(date) ~ 1, # 加入時ラベルのプロット位置
      date == max(date) ~ 0  # 卒業時ラベルのプロット位置
    )
  ) |> # ラベル用の値を作成
  dplyr::ungroup() |> 
  dplyr::select(date, member_id, memberName, label, h)

# x軸ラベル用の値を作成
date_vec <- seq(
  from = chart_df[["date"]] |> 
    min() |> # 結成日を取得
    lubridate::floor_date(unit = "year"), # 年単位で切り捨て
  to = chart_df[["date"]] |> 
    max() |> # 終了日を取得
    lubridate::ceiling_date(unit = "year"), # 年単位で切り上げ
  by = "year"
)


# タイムラインを作成:デフォルト配色
ggplot() + 
  geom_line(data = chart_df, 
            mapping = aes(x = date, y = member_id, color = factor(member_id)), 
            size = 2) + # 活動期間ライン
  geom_text(data = label_df, 
            mapping = aes(x = date, y = member_id, label = label, hjust = h, color = factor(member_id)), 
            size = 3, show.legend = FALSE) + # 加入時・卒業時ラベル
  scale_x_date(breaks = date_vec, date_labels = "%Y-%m", 
               guide = guide_axis(angle = 45), expand = c(0, 0)) + # x軸目盛
  scale_y_reverse(breaks = seq(from = 0, to = max(chart_df[["member_id"]]), by = 10)) + # y軸を反転
  coord_cartesian(clip = "off") + # 表示範囲
  theme(
    axis.title.y = element_blank(), # 縦軸ラベル
    axis.text.y = element_blank(), # 縦軸の目盛ラベル
    axis.ticks.y = element_blank(), # 縦軸の目盛指示線
    plot.title = element_text(color = "black", face = "bold", size = 20, hjust = 0.5), # 全体のタイトル
    plot.subtitle = element_text(color = "black", size = 15, hjust = 0.5), # 全体のサブタイトル
    plot.margin = margin(t = 10, r = 120, b = 10, l = 100, unit = "pt"), # 全体の余白
    legend.position = "none" # 凡例の表示位置
  ) + 
  labs(
    title = paste0(group_name, "歴代メンバーの活動期間"), 
    subtitle = paste0(
      format(min(chart_df[["date"]]), format = "%Y年%m月%d日"), 
      "～", format(max(chart_df[["date"]]), format = "%Y年%m月%d日"), 
      "：総メンバー", max(chart_df[["member_id"]]), "人"
    ), 
    x = "年-月", y = "メンバー", 
    caption = "データ:「https://github.com/xxgentaroxx/HP_DB」"
  )

# タイムラインを作成:メンバーカラー配色(MemberColorSample.Rを参照)
ggplot() + 
  geom_line(data = chart_df, 
            mapping = aes(x = date, y = member_id, color = memberName), 
            size = 2) + # 活動期間ライン
  geom_label(data = label_df, 
             mapping = aes(x = date, y = member_id, label = label, hjust = h, color = memberName), 
             fill = "gray92", label.size = 0, label.padding = unit(0.09, units = "lines"), 
             size = 3, show.legend = FALSE) + # 加入時・卒業時ラベル
  scale_color_manual(breaks = color_df[["member_name"]], values = color_df[["color_code"]]) + # 線の色
  scale_fill_manual(breaks = color_df[["member_name"]], values = color_df[["color_code"]]) + # 塗りつぶしの色
  scale_x_date(breaks = date_vec, date_labels = "%Y-%m", 
               guide = guide_axis(angle = 45), expand = c(0, 0)) + # x軸目盛
  scale_y_reverse(breaks = seq(from = 0, to = max(chart_df[["member_id"]]), by = 10)) + # y軸を反転
  coord_cartesian(clip = "off") + # 表示範囲
  theme(
    axis.title.y = element_blank(), # 縦軸ラベル
    axis.text.y = element_blank(), # 縦軸の目盛ラベル
    axis.ticks.y = element_blank(), # 縦軸の目盛指示線
    plot.title = element_text(color = "black", face = "bold", size = 20, hjust = 0.5), # 全体のタイトル
    plot.subtitle = element_text(color = "black", size = 15, hjust = 0.5), # 全体のサブタイトル
    #plot.background = element_rect(fill = "gray"), # 全体の背景
    plot.margin = margin(t = 10, r = 120, b = 10, l = 80, unit = "pt"), # 全体の余白
    legend.position = "none" # 凡例の表示位置
  ) + 
  labs(
    title = paste0(group_name, "歴代メンバーの活動期間"), 
    subtitle = paste0(
      format(min(chart_df[["date"]]), format = "%Y年%m月%d日"), 
      "～", format(max(chart_df[["date"]]), format = "%Y年%m月%d日"), 
      "：総メンバー", max(chart_df[["member_id"]]), "人"
    ), 
    x = "年-月", y = "メンバー", 
    caption = "データ:「https://github.com/xxgentaroxx/HP_DB」"
  )


# 月次アニメーションによる可視化 --------------------------------------------------------------

### ・活動期間の集計：(月別) -----

# グループを指定
group_id <- 44

# 集計の終了日を指定
date_to <- "2020-06-15" |> 
  lubridate::as_date()
date_to <- lubridate::today()


# 月ごとに活動期間を集計
anime_df <- group_df |> 
  # 活動期間の日付の作成
  dplyr::filter(groupID == group_id, formDate <= date_to) |> # 指定したグループを抽出
  dplyr::mutate(
    formDate = formDate |> 
      lubridate::floor_date(unit = "month"), 
    dissolveDate = dplyr::if_else(is.na(dissolveDate) | dissolveDate > date_to, true = date_to, false = dissolveDate) |> # 活動中グループの解散日に指定した日付を設定
      lubridate::floor_date(unit = "month")
  ) |> # 結成日・解散日を月単位に切り捨て
  dplyr::group_by(groupID, groupName, formDate) |> # 日付の作成用
  dplyr::summarise(
    date = seq(from = formDate, to = dissolveDate, by = "month"), .groups = "drop"
  ) |> # 活動期間中の月ごとの日付を作成
  dplyr::group_by(date, groupID) |> # 重複の削除用
  dplyr::slice_max(formDate) |> # (改名日が月途中の場合)改名後のデータを抽出
  dplyr::ungroup() |> 
  dplyr::select(date, groupID, groupName) |> 
  # メンバー情報の追加
  tidyr::expand_grid(
    join_df |> 
      dplyr::filter(groupID == group_id, joinDate <= date_to) |> # 指定したグループ・期間内の在籍メンバーを抽出
      dplyr::arrange(joinDate, memberID) |> # メンバーIDの再設定用
      dplyr::mutate(
        member_id = dplyr::row_number(), # メンバーIDを再設定
        gradDate = dplyr::if_else(is.na(gradDate), true = lubridate::today(), false = gradDate) # 活動中メンバーの卒業日に現在の日付を設定
      ) |> 
      dplyr::select(member_id, memberID, joinDate, gradDate), # 結合時に重複する列を削除, 
  ) |> # 月ごとにメンバーIDを複製して結合
  dplyr::left_join(
    member_df |> 
      dplyr::select(memberID, memberName, birthDate) |> 
      dplyr::group_by(memberID) |> # 重複の除去用
      dplyr::slice_tail(n = 1), # 重複を削除:(slice_headなら改名前、slice_tailなら改名後を抽出)
    by = "memberID"
  ) |> # メンバー情報を結合
  dplyr::filter(date >= joinDate, date <= gradDate) |> # 活動期間中のデータを抽出
  # ラベルの作成
  dplyr::mutate(
    age = lubridate::interval(start = birthDate, end = date) |> 
      lubridate::time_length(unit = "year") |> 
      floor(), # 年齢を計算
    act_y = lubridate::interval(start = joinDate, end = date) |> 
      lubridate::time_length(unit = "year") |> 
      floor(), # 活動年数を計算
    act_m = lubridate::interval(start = joinDate, end = date) |> 
      lubridate::time_length(unit = "month") |> 
      floor() %% 12, # 活動月数-年数を計算
    last_day = date |> 
      lubridate::rollback() |> 
      lubridate::day() |> 
      as.numeric(), # 基準日の前月末の日にち
    join_day = joinDate |> 
      lubridate::day() |> 
      as.numeric(), # 加入日の日にち
    act_d = dplyr::case_when(
      join_day == 1 ~ 0, # 加入日が月初の場合は「0にち」
      join_day >= last_day ~ 1, # 加入日の日にちが無い月の場合は「1にち」
      join_day <= last_day ~ lubridate::interval(
        start = date |> 
          lubridate::rollback() |> # 前月の末日に変更
          lubridate::floor_date(unit = "mon") + lubridate::day(joinDate) - 1, # 加入日と同じ日にちに変更
        end = date
      ) |> 
        lubridate::time_length(unit = "day") # 加入日の日にちが有る月の場合は「前月の加入時と同じ日にちとの差」
    ), # 活動日数-年月数を計算
    act_days = lubridate::interval(start = joinDate, end = date) |> 
      lubridate::time_length(unit = "day"), # 活動日数を計算
    label = paste0(
      memberName, " (", age, "歳：", 
      act_y, "年", act_m, "か月", stringr::str_pad(act_d, width = 2, pad = 0), "日)"
    ) # 年齢・活動年数ラベル
  ) |> 
  dplyr::select(date, groupName, member_id, memberName, age, act_y, act_m, act_d, act_days, label) |> 
  dplyr::arrange(date, member_id)
anime_df


### ・タイムラインの作図 -----

# グループ名を設定
group_name <- anime_df[["groupName"]] |> 
  unique() |> # 重複を削除
  #(\(.) {.[1]})() # 最初の名前を抽出
  (\(. ){.[length(.)]})() # 最後の名前を抽出
group_name

# 月ごとの在籍数を計算
member_num_df <- anime_df |> 
  dplyr::count(date, name = "member_n") |> # メンバー数を集計
  dplyr::mutate(label = paste0("在籍数：", member_n, "人")) # 人数ラベルを作成

# x軸ラベル用の値を作成
date_vec <- seq(
  from = anime_df[["date"]] |> 
    min() |> # 結成月を取得
    lubridate::floor_date(unit = "year"), # 年単位で切り捨て
  to = anime_df[["date"]] |> 
    max() |> # 指定した日付を取得
    lubridate::ceiling_date(unit = "year"), # 年単位で切り上げ
  by = "year"
)


# タイムラインのアニメーション作成:デフォルト配色
anim <- ggplot() + 
  geom_vline(data = anime_df, 
             mapping = aes(xintercept = date), 
             color = "gray56", size = 1, linetype = "dashed") + # 時間経過
  geom_label(data = member_num_df, 
             mapping = aes(x = date, y = 0, label = label), 
             size = 3, vjust = 0, color = "gray56") + # 在籍数ラベル
  geom_line(data = anime_df, 
            mapping = aes(x = date, y = member_id, color = factor(member_id)), 
            size = 2) + # 活動期間ライン
  geom_point(data = anime_df, 
             mapping = aes(x = date, y = member_id, color = factor(member_id)), 
             size = 4) + # 活動期間終点
  geom_text(data = anime_df, 
            mapping = aes(x = date, y = member_id, label = paste("  ", label), color = factor(member_id)), 
            size = 3, hjust = 0) + # メンバーラベル
  gganimate::transition_reveal(along = date) + # フレーム
  scale_x_date(breaks = date_vec, date_labels = "%Y", expand = c(0, 0)) + # x軸目盛
  scale_y_reverse(breaks = seq(0, max(anime_df[["member_id"]]), by = 10)) + # y軸を反転
  coord_cartesian(clip = "off") + # 表示範囲
  theme(
    plot.title = element_text(color = "black", face = "bold", size = 20, hjust = 0.5), # 全体のタイトル
    plot.subtitle = element_text(color = "black", size = 15, hjust = 0.5), # 全体のサブタイトル
    plot.margin = margin(t = 10, r = 200, b = 20, l = 40, unit = "pt"), # 全体の余白
    legend.position = "none" # 凡例の表示位置
  ) + 
  labs(
    title = paste0(group_name, "歴代メンバーの活動期間"), 
    #subtitle = paste0("{format(frame_along, format = '%Y年%m月')}01日時点"), 
    subtitle = paste0("{format(lubridate::as_date(frame_along)+5, format = '%Y年%m月')}01日時点"), # (+5はtransition_reveal()の補正対策)
    x = "年", y = "メンバー", 
    caption = "データ:「https://github.com/xxgentaroxx/HP_DB」"
  )

# タイムラインを作成:メンバーカラー配色(MemberColorSample.Rを参照)
anim <- ggplot() + 
  geom_vline(anime_df, 
             mapping = aes(xintercept = date), 
             color = "gray56", size = 1, linetype = "dashed") + # 時間経過
  geom_label(data = member_num_df, 
             mapping = aes(x = date, y = 0, label = label), 
             size = 3, vjust = 0, color = "gray56") + # 在籍数ラベル
  geom_label(data = anime_df, 
             mapping = aes(x = date, y = member_id, label = paste("  ", label), color = memberName), 
             fill = "gray92", label.size = 0, label.padding = unit(0.1, units = "lines"), 
             size = 3, hjust = 0, show.legend = FALSE) + # メンバーラベル
  geom_line(data = anime_df, 
            mapping = aes(x = date, y = member_id, color = memberName), 
            size = 2) + # 活動期間ライン
  geom_point(data = anime_df, 
             mapping = aes(x = date, y = member_id, color = memberName), 
             size = 4) + # 活動期間終点
  gganimate::transition_reveal(along = date) + # フレーム
  scale_color_manual(breaks = color_df[["member_name"]], values = color_df[["color_code"]]) + # 線の色
  scale_x_date(breaks = date_vec, date_labels = "%Y", expand = c(0, 0)) + # x軸目盛
  scale_y_reverse() + # y軸を反転
  coord_cartesian(clip = "off") + # 表示範囲
  theme(
    plot.title = element_text(color = "black", face = "bold", size = 20, hjust = 0.5), # 全体のタイトル
    plot.subtitle = element_text(color = "black", size = 15, hjust = 0.5), # 全体のサブタイトル
    plot.margin = margin(t = 10, r = 200, b = 20, l = 40, unit = "pt"), # 全体の余白
    legend.position = "none" # 凡例の表示位置
  ) + 
  labs(
    title = paste0(group_name, "歴代メンバーの活動期間"), 
    subtitle = paste0("{format(lubridate::as_date(frame_along)+5, format = '%Y年%m月')}01日時点"), # (+5はtransition_reveal()の補正対策)
    x = "年", y = "メンバー", 
    caption = "データ:「https://github.com/xxgentaroxx/HP_DB」"
  )


# 1秒間に表示する月数を指定:(値が大きいと意図した通りにならない)
month_per_second <- 6

# 最後のグラフで一時停止する月数を指定
pause_month <- 5

# 最後のグラフで一時停止するフレーム数を計算
pause_frame <- pause_month * month_per_second

# グラフ遷移するフレーム数を設定
frame_num <- anime_df[["date"]] |> 
  unique() |> 
  length()
frame_num


# gif画像を作成
g <- gganimate::animate(
  plot = anim, 
  nframes = frame_num+pause_frame, end_pause = pause_frame, fps = month_per_second, 
  width = 1200, height = 900
)
g

# gif画像を保存
gganimate::anim_save(filename = "ChartRace/output/ActPeriod.gif", animation = g)


# 動画を保存
m <- gganimate::animate(
  plot = anim, 
  nframes = frame_num, fps = month_per_second,  
  width = 1200, height = 900, 
  renderer = gganimate::av_renderer(file = "ChartRace/output/ActPeriod.mp4")
)


