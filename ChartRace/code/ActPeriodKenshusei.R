
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


# イレギュラーなデータを確認
join_df |> 
  dplyr::filter(groupID %in% c(10, 18, 23, 39)) |> # 研修生のデータを抽出
  dplyr::left_join(member_df, by = "memberID") |> # メンバー情報を結合
  dplyr::filter(joinDate != HPjoinDate) # 研修生加入日とハロプロ加入日が異なるメンバーを抽出


### ・色データ -----

# 加入先グループを確認
chart_df |> 
  dplyr::pull(groupName) |> 
  unique() |> 
  as.character()

# グループカラーを指定
color_df <- tibble::tibble(
  groupName = c(
    "ハロー!プロジェクト・キッズ", "ハロプロエッグ", "ハロプロ研修生", "ハロプロ関西", "ハロプロ研修生北海道", 
    "モーニング娘。", "Berryz工房", "°C-ute", "THEポッシボー", 
    "スマイレージ", "アンジュルム", "Juice=Juice", "カントリー・ガールズ", 
    "こぶしファクトリー", "つばきファクトリー", "BEYOOOOONDS", "OCHA NORMA"
  ), 
  color_code = c(
    "cyan1", "#33D6AD", "#33D6AD", "orange", "#1CBDBA", 
    "hotpink", "yellow", "red", "pink", 
    "blue", "blue", "green", "limegreen", 
    "turquoise1", "greenyellow", "lightblue1", "skyblue1"
  )
) |> 
  dplyr::mutate(id = dplyr::row_number()) # プロット用

# グループカラーを確認
ggplot() + 
  geom_bar(data = color_df, 
           mapping = aes(x = id, y = 1, fill = groupName), stat = "identity") + 
  geom_text(data = color_df, 
            mapping = aes(x = id, y = 0, label = groupName), 
            hjust = 0) + 
  scale_fill_manual(breaks = color_df[["groupName"]], 
                    values = color_df[["color_code"]], guide = "none") + 
  coord_flip() + 
  scale_x_reverse() + 
  labs(title = "group color", 
       y = "")


# 期間の設定 -------------------------------------------------------------------

# 集計の開始日を指定
date_from <- lubridate::as_date("2002-06-30") # キッズ誕生
date_from <- lubridate::as_date("2004-08-10") # エッグ誕生
date_from <- lubridate::as_date("2011-12-01") # 研修生誕生
date_from <- lubridate::as_date("2017-01-01")

# 集計の終了日を指定
date_to <- lubridate::as_date("2005-06-10") # キッズ終了
date_to <- lubridate::as_date("2011-11-30") # エッグ終了
date_to <- lubridate::as_date("2016-12-31")
date_to <- lubridate::today()


# データの編集 ---------------------------------------------------------------------

# 研修生など(キッズ・エッグ・関西・北海道)のグループIDを指定
kenshu_id_vec <- c(10, 18, 23, 39)

# グループ情報を編集
group_unique_df <- group_df |> 
  dplyr::filter(
    isUnit == FALSE, # ユニットを除去
    groupName != "カントリー娘。" # カントリー・ガールズと別グループ扱いにしたいので除去
  ) |> # グループ情報を抽出
  dplyr::mutate(
    groupName = groupName |> 
      stringr::str_replace(pattern = "℃", replacement = "°C") |>  # 文字化け対策
      #stringr::str_replace(pattern = "ハロプロエッグ", replacement = "ハロプロ研修生") |> # 改名後に変更
      #stringr::str_replace(pattern = "ハロプロ研修生", replacement = "ハロプロエッグ") |> # 改名前に変更
      stringr::str_remove(pattern = " '\\d*") |> # ナンバリングを除去
      stringr::str_replace(pattern = "S/mileage", replacement = "スマイレージ") |> # 改名後に変更
      #stringr::str_replace(pattern = "スマイレージ", replacement = "アンジュルム") |> # 改名後に変更
      stringr::str_replace(pattern = "SI☆NA", replacement = "ハロプロ関西"), # 改名前に変更
    dissolveDate = dplyr::if_else(is.na(dissolveDate), true = lubridate::today(), false = dissolveDate) # 活動中グループの解散日に現在の日付を設定
  ) |> # # 改名グループ情報の統合用に編集
  dplyr::group_by(groupID) |> # 改名グループ情報の統合用
  dplyr::mutate(
    formDate = min(formDate), 
    dissolveDate = max(dissolveDate)
  ) |> # 改名日を結成日・解散日に変更
  dplyr::slice_tail(n = 1) |> # 改名データを除去:(headなら改名前、tailなら改名後を抽出)
  dplyr::ungroup() |> 
  dplyr::arrange(groupID, formDate) |> 
  dplyr::select(groupID, groupName)
group_unique_df

# メンバー情報を編集
member_unique_df <- member_df |> 
  dplyr::select(memberID, memberName, HPjoinDate, birthDate) |> 
  dplyr::group_by(memberID) |> # 重複の除去用
  dplyr::mutate(HPjoinDate = min(HPjoinDate)) |> # 改名日を加入日に変更
  dplyr::slice_tail(n = 1) |> # 改名データを除去:(headなら改名前、tailなら改名後を抽出)
  dplyr::ungroup() |> 
  dplyr::select(memberID, memberName, HPjoinDate, birthDate) |> 
  dplyr::arrange(memberID, HPjoinDate)
member_unique_df


# 研修生の加入・卒業情報を作成
join_kenshu_df <- join_df |> 
  dplyr::filter(
    groupID %in% kenshu_id_vec, # 研修生メンバーを抽出
    gradDate >= date_from | is.na(gradDate), joinDate <= date_to # 指定した期間内の在籍メンバーを抽出
  ) |> 
  dplyr::mutate(
    gradDate = dplyr::if_else(is.na(gradDate), true = lubridate::today(), false = gradDate), # 活動中メンバーの卒業日に現在の日付を設定
    member_id = dplyr::row_number() # メンバーIDを再設定
  ) |>
  dplyr::left_join(member_unique_df, by = "memberID") |> # メンバー情報を結合
  dplyr::left_join(group_unique_df, by = "groupID") |> # グループ名を結合
  dplyr::select(member_id, memberID, memberName, birthDate, HPjoinDate, groupID, groupName, joinDate, gradDate) |> 
  dplyr::arrange(member_id)
join_kenshu_df

# 研修生の昇格・卒業情報を作成
join_debut_df <- join_df |> 
  dplyr::filter(
    memberID %in% join_kenshu_df[["memberID"]], # 指定した期間の研修生メンバーを抽出
    !(groupID %in% kenshu_id_vec), groupID %in% group_unique_df[["groupID"]] # 昇格先グループを抽出
  ) |> 
  dplyr::mutate(
    gradDate = dplyr::if_else(is.na(gradDate), true = lubridate::today(), false = gradDate) # 活動中メンバーの卒業日を現在の日付を設定
  ) |> 
  dplyr::left_join(
    join_kenshu_df |> 
      dplyr::select(memberID, member_id), 
    by = "memberID"
  ) |> # 再設定したメンバーIDを結合
  dplyr::left_join(member_unique_df, by = "memberID") |> # メンバー情報を結合
  dplyr::left_join(group_unique_df, by = "groupID") |> # グループ名を結合
  dplyr::select(member_id, memberID, memberName, birthDate, HPjoinDate, groupID, groupName, joinDate, gradDate) |> 
  dplyr::arrange(member_id)
join_debut_df


# タイムラインによる可視化 ----------------------------------------------------------------------

### ・活動期間の集計：(加入日・卒業日) -----

# メンバーごとに加入日・卒業日における年齢・活動日数を計算
chart_df <- dplyr::bind_rows(join_kenshu_df, join_debut_df) |> # 昇格前後のデータを結合
  # データの整形
  tidyr::pivot_longer(
    cols = c(joinDate, gradDate), 
    names_to = "date_type", 
    values_to = "date"
  ) |> # グループ加入日・卒業日の列をまとめる
  # ラベルの作成
  dplyr::arrange(date, member_id) |> # 因子レベルの設定用
  dplyr::mutate(
    groupName = factor(groupName, levels = unique(groupName)), # 凡例の表示順を設定
    age = lubridate::interval(start = birthDate, end = date) |> 
      lubridate::time_length(unit = "year") |> 
      floor(), # 年齢を計算
    act_y = lubridate::interval(start = HPjoinDate, end = date) |> 
      lubridate::time_length(unit = "year") |> 
      floor(), # 活動年数を計算
    act_m = lubridate::interval(start = HPjoinDate, end = date) |> 
      lubridate::time_length(unit = "mon") |> 
      floor() %% 12, # 活動月数-年数を計算
    date_day = date |> 
      lubridate::day(), # 基準日(加入日・卒業日)の日にち
    last_day = date |> 
      lubridate::rollback() |> 
      lubridate::day(), # 基準日の前月末の日にち
    join_day = HPjoinDate |> 
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
          lubridate::floor_date(unit = "mon") + join_day - 1, # 前月の加入日に変更
        end = date
      ) |> 
        lubridate::time_length(unit = "day") # 加入日の日にちが有る月の場合は「前月の加入時と同じ日にちとの差」
    ), # 活動日数-年月数を計算
    act_days = lubridate::interval(start = HPjoinDate, end = date) |> 
      lubridate::time_length(unit = "day"), # 活動日数を計算
    label = paste0(age, "歳：", act_y, "年", act_m, "か月", act_d, "日") # 年齢・活動年数ラベル
  ) |> 
  # プロット時の調整
  dplyr::group_by(member_id) |> # プロット位置の調整用
  dplyr::mutate(
    join_sort = factor(groupID, levels = unique(groupID)) |> # 兼任の判定用
      dplyr::dense_rank(), # 加入先・卒業元順を割り当て
    diff_flg = any(join_sort != sort(join_sort)), # 兼任期間の有無を判定
    diff = dplyr::if_else(
      diff_flg, true = dplyr::dense_rank(factor(groupID, levels = unique(groupID))) - 1, false = 0
    ), # 兼任期間がある場合はプロット位置を調整
    diff = diff - median(diff), # プロット位置の調整用の値を設定
    id_label = stringr::str_c("member ", member_id, "：group ", groupID) # 線の描き分け用
  ) |> 
  dplyr::ungroup() |> 
  dplyr::select(date, member_id, memberName, groupID, groupName, age, act_y, act_m, act_d, act_days, label, id_label, diff) |> 
  dplyr::arrange(date, member_id, groupID)
chart_df


### ・ラインチャートの作成 -----

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
  dplyr::group_by(member_id, date) |> # 重複の削除用
  dplyr::slice_head(n = 1) |> # 重複を削除
  dplyr::ungroup() |> 
  dplyr::select(date, member_id, memberName, groupID, groupName, label, h)

# 指定期間の描画用のデータフレームを作成
range_df <- tibble::tibble(
  x = c(date_from, date_to), 
  y_min = -Inf, 
  y_max = Inf
)

# x軸ラベル用の値を作成
date_vec <- seq(
  from = chart_df[["date"]] |> 
    min() |> # 一番古い加入日を取得
    lubridate::floor_date(unit = "year"), # 年単位で切り捨て
  to = chart_df[["date"]] |> 
    max() |> # 一番新しい卒業日または現在の日付を取得
    lubridate::ceiling_date(unit = "year"), # 年単位で切り上げ
  by = "year"
)


# ラインチャートを作成
ggplot() + 
  geom_ribbon(data = range_df, 
              mapping = aes(x = x, ymin = y_min, ymax = y_max), 
              color = "gray86", alpha = 0.1, size = 1, linetype = "dashed") + # 指定期間の範囲
  geom_vline(xintercept = c(date_from, date_to), 
             color = "gray86", size = 1, linetype = "dashed") + # 指定期間の開始日・終了日
  geom_line(data = chart_df, 
            mapping = aes(x = date, y = member_id+0.2*diff, color = groupName, group = id_label), 
            size = 2, alpha = 0.8) + # 活動期間ライン
  geom_text(data = label_df, 
            mapping = aes(x = date, y = member_id, label = label, hjust = h, color = groupName), 
            size = 3, show.legend = FALSE) + # 加入時・卒業時ラベル
  #scale_color_manual(breaks = color_df[["groupName"]], values = color_df[["color_code"]]) + # グループカラー配色
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
    legend.position = "top" # 凡例の表示位置
  ) + 
  labs(
    title = paste0("歴代ハロプロキッズ・エッグ・研修生の活動期間"), 
    subtitle = paste0(
      format(date_from, format = "%Y年%m月%d日"), "～", format(date_to, format = "%Y年%m月%d日"), 
      "の在籍メンバー：", length(unique(chart_df[["member_id"]])), "人"
    ), 
    x = "年-月", y = "メンバー", 
    color = "グループ", 
    caption = "データ:「https://github.com/xxgentaroxx/HP_DB」"
  )


# 月次アニメーションによる可視化 --------------------------------------------------------------

### ・活動期間の集計：(月別) -----

# 月ごとに活動期間を集計
anime_df <- dplyr::bind_rows(join_kenshu_df, join_debut_df) |> # 昇格前後のデータを結合
  # 活動期間の日付の作成
  dplyr::mutate(
    joinDate = joinDate |> 
      lubridate::floor_date(unit = "month"), 
    gradDate = gradDate |> 
      lubridate::floor_date(unit = "month")
  ) |> # 加入日・卒業日を月単位に切り捨て
  dplyr::group_by(member_id, memberID, memberName, birthDate, HPjoinDate, groupID, groupName, joinDate) |> # 日付の作成用
  dplyr::summarise(
    date = seq(from = joinDate, to = gradDate, by = "month"), .groups = "drop"
  ) |> # 活動期間中の月ごとの日付を作成
  dplyr::filter(dplyr::between(date, left = date_from, right = date_to)) |> # 指定期間内のデータを抽出
  # ラベルを作成
  dplyr::arrange(date, member_id) |> # 因子レベルの設定用
  dplyr::mutate(
    groupName = factor(groupName, levels = unique(groupName)), # 凡例の表示順を設定
    age = lubridate::interval(start = birthDate, end = date) |> 
      lubridate::time_length(unit = "year") |> 
      floor(), # 年齢を計算
    act_y = lubridate::interval(start = HPjoinDate, end = date) |> 
      lubridate::time_length(unit = "year") |> 
      floor(), # 活動年数を計算
    act_m = lubridate::interval(start = HPjoinDate, end = date) |> 
      lubridate::time_length(unit = "mon") |> 
      floor() %% 12, # 活動月数-年数を計算
    last_day = date |> 
      lubridate::rollback() |> 
      lubridate::day(), # 基準日の前月末の日にち
    join_day = HPjoinDate |> 
      lubridate::day(), # 加入日の日にち
    act_d = date |> 
      lubridate::rollback() |> 
      lubridate::floor_date(unit = "mon") + lubridate::day(HPjoinDate) - 1, # 計算用に前月の加入日の日にちを作成
    act_d = dplyr::case_when(
      join_day == 1 ~ 0, # 加入日が月初の場合は「0にち」
      join_day >= last_day ~ 1, # 加入日の日にちが無い月の場合は「1にち」
      join_day <= last_day ~ lubridate::interval(
        start = date |> 
          lubridate::rollback() |> # 前月の末日に変更
          lubridate::floor_date(unit = "mon") + join_day - 1, # 加入日と同じ日にちに変更
        end = date
      ) |> 
        lubridate::time_length(unit = "day") # 加入日の日にちが有る月の場合は「前月の加入時と同じ日にちとの差」
    ), # 活動日数-年月数を計算
    act_days = lubridate::interval(start = HPjoinDate, end = date) |> 
      lubridate::time_length(unit = "day"), # 活動日数を計算
    label = paste0(
      memberName, " (", age, "歳：", 
      act_y, "年", act_m, "か月", stringr::str_pad(act_d, width = 2, pad = 0), "日)"
    )
  ) |> 
  # プロット時の調整
  dplyr::arrange(member_id, date, joinDate) |> # プロット位置の調整値用
  dplyr::group_by(member_id, date) |> # プロット位置の調整値用
  dplyr::mutate(
    n = dplyr::row_number(joinDate), # 兼任期間の有無を判定
    label = dplyr::if_else(n == max(n), true = label, false = NA_character_), # 兼任期間がある場合は1つのラベル以外を非表示
    diff = n - 1, # 兼任期間がある場合はプロット位置を調整
    diff = diff - median(diff), # プロット位置の調整用の値を設定
    id_label = stringr::str_c("member ", member_id, "：group ", groupID) # 線の描き分け用
  ) |> 
  dplyr::ungroup() |> 
  dplyr::select(date, member_id, memberName, groupID, groupName, age, act_y, act_m, act_d, act_days, label, id_label, diff)
anime_df


### ・タイムラインの作図 -----

# 月ごとの在籍数を計算
member_num_df <- anime_df |> 
  dplyr::filter(groupID %in% kenshu_id_vec) |> # 研修生を抽出
  dplyr::count(date, name = "member_n") |> # メンバー数を集計
  dplyr::mutate(label = paste0("在籍数：", member_n, "人")) # 人数ラベルを作成

# x軸ラベル用の値を作成
date_vec <- seq(
  from = date_from |> 
    lubridate::floor_date(unit = "year"), # 年単位で切り捨て
  to = date_to |> 
    lubridate::ceiling_date(unit = "year"), # 年単位で切り上げ
  by = "year"
)


# ラインチャートを作成
anim <- ggplot() + 
  geom_vline(anime_df, 
             mapping = aes(xintercept = date), 
             color = "gray56", size = 1, linetype = "dashed") + # 時間経過
  geom_label(data = member_num_df, 
             mapping = aes(x = date, y = 0, label = label), 
             color = "gray56", vjust = 0) + # 在籍数ラベル
  geom_point(data = anime_df, 
             mapping = aes(x = date, y = member_id+0.2*diff, color = factor(groupName), group = factor(id_label)), 
             size = 4, alpha = 0.8) + # 活動期間終点
  geom_line(data = anime_df, 
            mapping = aes(x = date, y = member_id+0.2*diff, color = factor(groupName), group = factor(id_label)), 
            size = 2, alpha = 0.8) + # 活動期間ライン
  geom_text(data = anime_df, 
            mapping = aes(x = date, y = member_id, label = paste("  ", label), color = factor(groupName), group = factor(member_id)), 
            size = 4, hjust = 0, show.legend = FALSE) + # メンバ－ラベル
  gganimate::transition_reveal(along = date) + # フレーム
  scale_color_manual(breaks = color_df[["groupName"]], values = color_df[["color_code"]]) + # グループカラー配色
  scale_x_date(breaks = date_vec, date_labels = "%Y", expand = c(0, 0)) + # x軸目盛
  scale_y_reverse() + # y軸を反転
  coord_cartesian(clip = "off") + # 表示範囲
  theme(
    plot.title = element_text(color = "black", face = "bold", size = 20, hjust = 0.5), # 全体のタイトル
    plot.subtitle = element_text(color = "black", size = 15, hjust = 0.5), # 全体のサブタイトル
    plot.margin = margin(t = 10, r = 200, b = 10, l = 10, unit = "pt"), # 全体の余白
    legend.position = "left" # 凡例の表示位置
  ) + 
  labs(
    title = paste0("歴代ハロプロキッズ・エッグ・研修生の活動期間"), 
    subtitle = paste0("{format(lubridate::as_date(frame_along)+5, format = '%Y年%m月')}01日時点"), # (+5はtransition_reveal()の補正対策)
    x = "年", y = "メンバー", 
    color = "グループ", 
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
  width = 1200, height = 1200
)
g

# gif画像を保存
gganimate::anim_save(filename = "ChartRace/output/ActPeriodKenshusei.gif", animation = g)


# 動画を保存
m <- gganimate::animate(
  plot = anim, 
  nframes = frame_num, fps = month_per_second,  
  width = 1200, height = 1200, 
  renderer = gganimate::av_renderer(file = "ChartRace/output/ActPeriodKenshusei.mp4")
)


