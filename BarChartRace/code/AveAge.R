
# グループごとの平均年齢の推移を可視化 -------------------------------------------------------------------

# 利用パッケージ
library(tidyverse)
library(lubridate)
library(gganimate)

# チェック用
library(magrittr)
library(ggplot2)


# データの読込 -----------------------------------------------------------------

# フォルダパスを指定
dir_path <- "BarChartRace/data/HP_DB-main/"

# メンバー一覧
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
)
member_df

# 加入・卒業日一覧
join_df <- readr::read_csv(
  file = paste0(dir_path, "join.csv"), 
  col_types = readr::cols(
    memberID = "i", 
    groupID = "i", 
    joinDate = readr::col_date(format = "%Y/%m/%d"), 
    gradDate = readr::col_date(format = "%Y/%m/%d")
  )
)
join_df

# グループ一覧
group_df <- readr::read_csv(
  file = paste0(dir_path, "group.csv"), 
  col_types = readr::cols(
    groupID = "i", 
    groupName = "c", 
    formDate = readr::col_date(format = "%Y/%m/%d"), 
    dissolveDate = readr::col_date(format = "%Y/%m/%d"), 
    isUnit = "l"
  )
)
group_df


# 期間の指定 -------------------------------------------------------------------

# 期間を指定
date_from <- "1997-09-01"
date_to   <- "2022-04-15"
date_to   <- lubridate::now()

# 月ベクトルを作成
date_vec <- seq(
  from = date_from %>% 
    lubridate::as_date() %>% 
    lubridate::floor_date(unit = "mon"), 
  to = date_to %>% 
    lubridate::as_date() %>% 
    lubridate::floor_date(unit = "mon"), 
  by = "mon"
)


# アニメーションの演出用 ------------------------------------------------------------------

### ・グループ名の対応 -----

## 改名組の表示名に対応したい

# 月・グループID・グループ名の対応表を作成
group_name_df <- tibble::tibble()
for(i in 1:nrow(group_df)) {
  # i番目のグループの結成日と解散日を取得
  tmp_date_from <- group_df[["formDate"]][i]
  if(is.na(group_df[["dissolveDate"]][i])) {
    tmp_date_to <- lubridate::as_date(lubridate::now()) # 活動中であれば現在の日時
  } else {
    tmp_date_to <- group_df[["dissolveDate"]][i]
  }
  
  # 月ベクトルを作成
  tmp_date_vec <- seq(
    from = lubridate::floor_date(tmp_date_from, unit = "mon"), 
    to = lubridate::floor_date(tmp_date_to, unit = "mon"), 
    by = "mon"
  )
  
  # 月データフレームを作成
  tmp_name_df <- tibble::tibble(
    date = tmp_date_vec, 
    groupID = group_df[["groupID"]][i], 
    groupName = group_df[["groupName"]][i], 
    formDate = group_df[["formDate"]][i], 
    dissolveDate = group_df[["dissolveDate"]][i]
  )
  
  # 結合
  group_name_df <- rbind(group_name_df, tmp_name_df)
}

# 改名が月途中だと重複するのでその対応
group_name_df <- group_name_df %>% 
  dplyr::arrange(groupID, date) %>% # 昇順に並び替え
  dplyr::group_by(date, groupID) %>% # 日付とグループでグループ化
  dplyr::mutate(row_num = dplyr::row_number()) %>% # 重複を確認
  dplyr::filter(row_num == max(row_num)) %>% # 重複する場合は新しい方を抽出
  dplyr::ungroup() %>% # 
  dplyr::select(!row_num) # 不要な列を削除
group_name_df


### ・アニメ用の小細工 -----

## バーの変化を強調するために結成前月と解散月をデータに含めたい

# 改名グループを抽出
tmp_rename_group_df <- group_df %>% 
  dplyr::select(groupID, groupName, formDate, dissolveDate) %>% # 利用する列を選択
  dplyr::group_by(groupID) %>% # グループごとにグループ化
  dplyr::filter(formDate != min(formDate) | formDate != max(formDate)) %>% # 改名グループを抽出
  dplyr::ungroup() %>% # グループ化の解除
  dplyr::arrange(groupID, formDate) # 昇順に並び替え
tmp_rename_group_df

# 改名グループの結成月と解散月を再設定
rename_group_df <- tibble::tibble()
for(i in unique(tmp_rename_group_df[["groupID"]])) {
  # i番目の改名グループを抽出
  tmp_df1 <- tmp_rename_group_df %>% 
    dplyr::filter(groupID == i)
  
  # 結成・解散月を再設定
  tmp_df2 <- tibble::tibble(
    groupID = i, 
    groupName = tmp_df1[["groupName"]][1], 
    formDate = tmp_df1[["formDate"]][1], 
    dissolveDate = tmp_df1[["dissolveDate"]][nrow(tmp_df1)]
  )
  
  # 結合
  rename_group_df <- rbind(rename_group_df, tmp_df2)
}
rename_group_df

# 非改名グループを抽出
nonrename_group_df <- group_df %>% 
  dplyr::select(groupID, groupName, formDate, dissolveDate) %>% # 利用する列を選択
  dplyr::group_by(groupID) %>% # グループごとにグループ化
  dplyr::filter(formDate == min(formDate), formDate == max(formDate)) %>% # 非改名グループを抽出
  dplyr::ungroup() %>% # グループ化の解除
  dplyr::arrange(groupID, formDate) # 昇順に並び替え
nonrename_group_df

# 結成・解散月を取得
member_0_df <- dplyr::bind_rows(rename_group_df, nonrename_group_df) %>% # 改名・非改名グループを結合
  dplyr::mutate(
    formDate = formDate %>% 
      lubridate::rollback() %>% # 結成1か月前に変更
      lubridate::floor_date(unit = "mon"), 
    dissolveDate = lubridate::floor_date(dissolveDate, unit = "mon")
  ) %>% # 月単位に切り捨て
  tidyr::pivot_longer(
    cols = c(formDate, dissolveDate), 
    names_to = "date_type", 
    values_to = "date"
  ) %>% # 結成・解散月を同じ列に変換
  dplyr::select(date, groupID, groupName) %>% # 月・グループの列を選択
  dplyr::mutate(
    groupName = " ", 
    moonage = 0, 
    member_n = 0, 
    average_moonage = 0
  ) %>% # メンバー数(0人)を追加
  dplyr::filter(!is.na(date)) %>% # 現在活動中のグループの解散月を除去
  dplyr::arrange(date, groupID) %>% # 昇順に並び替え
  dplyr::filter(date > min(date_vec), date < max(date_vec))
member_0_df


# 集計 ----------------------------------------------------------------------

# サイズを取得
date_size   <- length(date_vec)
group_size  <- max(group_df[["groupID"]])
member_size <- max(member_df[["memberID"]])

# 受け皿を作成
date_df <- tibble::tibble(
  date = rep(date_vec, each = group_size*member_size), 
  groupID = rep(rep(1:group_size, times = date_size), each = member_size), 
  memberID = rep(1:member_size, times = date_size*group_size)
)
date_df

# 集計
rank_df <- date_df %>% 
  dplyr::left_join(
    group_name_df %>% 
      dplyr::mutate(
        formDate = lubridate::floor_date(formDate, unit = "mon"), 
        dissolveDate = lubridate::floor_date(dissolveDate, unit = "mon")
      ), 
    by = c("date", "groupID")
  ) %>% # グループ情報を結合
  dplyr::filter(date >= formDate, date <= dissolveDate | is.na(dissolveDate)) %>% # 活動中のグループを抽出
  dplyr::select(!c(formDate, dissolveDate)) %>% # 不要な列を削除
  dplyr::left_join(
    join_df %>% 
      dplyr::mutate(
        joinDate = lubridate::floor_date(joinDate, unit = "mon"), 
        gradDate = lubridate::floor_date(gradDate, unit = "mon")
      ), 
    by = c("groupID", "memberID")
  ) %>% # 加入メンバー情報を結合
  dplyr::filter(date >= joinDate, date < gradDate | is.na(gradDate)) %>% # グループ活動中のメンバーを抽出
  dplyr::select(!c(joinDate, gradDate)) %>% # 不要な列を削除
  dplyr::left_join(
    member_df %>% 
      dplyr::distinct(memberID, .keep_all = TRUE), # 重複を除去
    by = "memberID"
  ) %>% # メンバー情報を結合
  dplyr::select(date, groupID, groupName, memberID, memberName, birthDate) %>% # 利用する列を選択
  dplyr::mutate(
    moonage = lubridate::interval(start = birthDate, end = date) %>% 
      lubridate::time_length(unit = "mon")
  ) %>% 
  dplyr::group_by(date, groupID, groupName) %>% # 平均月齢の計算用にグループ化
  dplyr::summarise(
    moonage = sum(moonage, na.rm = TRUE), 
    member_n = sum(!is.na(birthDate)), 
    .groups = "drop"
  ) %>% # 総月齢とメンバー数を計算
  dplyr::mutate(average_moonage = moonage / member_n) %>% # 平均月齢を計算
  dplyr::bind_rows(member_0_df) %>% # 結成前月・解散月を追加
  dplyr::arrange(date, average_moonage, groupID) %>% # ランク付け用に昇順に並べ替え
  dplyr::group_by(date) %>% # ランク付け用にグループ化
  dplyr::mutate(
    groupID = factor(groupID), 
    year = average_moonage %/% 12, 
    month = round(average_moonage %% 12, digits = 1), 
    ranking = dplyr::row_number(-average_moonage), 
  ) %>% # 
  dplyr::ungroup() %>% # グループ化の解除
  #dplyr::select(date, groupID, groupName, moonage, member_n, average_moonage, year, month, ranking) %>% # 利用する列を選択:(確認用)
  dplyr::select(date, groupID, groupName, average_moonage, year, month, ranking) %>% # 利用する列を選択:(集計用)
  dplyr::arrange(date, ranking) # 昇順に並べ替え
rank_df


# 検証 ----------------------------------------------------------------------

## rank_num_dfはMemberNum.Rのrank_df

# メンバー数を照合
for(i in 1:max(group_df[["groupID"]])) {
  # メンバー数を抽出
  member_n <- rank_df %>% 
    dplyr::filter(groupID == i) %>% 
    .[["member_n"]]
  
  # メンバー数を抽出
  activ_n <- rank_num_df %>% 
    dplyr::filter(groupID == i) %>% 
    .[["active_n"]]
  
  # メンバー数を照合
  if(sum(member_n == activ_n) != length(member_n)) print(i)
}


# アニメーションの作成 --------------------------------------------------------------

### ・フレーム数の設定 -----

# 遷移フレーム数を指定
t <- 9

# 一時停止フレーム数を指定
s <- 1

# 1秒間に表示する月数を指定:(値が大きいと意図した通りにならない)
mps <- 3

# フレーム数を取得
n <- length(unique(rank_df[["date"]]))


### ・y軸可変 -----

# バーチャートレースを作成:(y軸可変)
anim <- ggplot(rank_df, aes(x = ranking, y = average_moonage, fill = groupID, color = groupID)) + 
  geom_bar(stat = "identity", width = 0.9, alpha = 0.8) + # 平均月齢バー
  geom_text(aes(y = 0, label = paste(groupName, " ")), hjust = 1) + # グループ名ラベル
  geom_text(aes(label = paste(" ", year, "歳", month, "か月")), hjust = 0) + # 平均年齢ラベル
  gganimate::transition_states(states = date, transition_length = t, state_length = s, wrap = FALSE) + # フレーム
  gganimate::ease_aes("cubic-in-out") + # アニメーションの緩急
  gganimate::view_follow(fixed_x = TRUE) + # 表示範囲のフィット
  coord_flip(clip = "off", expand = FALSE) + # 軸の入れ変え
  scale_x_reverse() + # x軸を反転
  theme(
    axis.title.x = element_blank(), # x軸のラベル
    axis.title.y = element_blank(), # y軸のラベル
    axis.text.x = element_blank(), # x軸の目盛ラベル
    axis.text.y = element_blank(), # y軸の目盛ラベル
    axis.ticks.x = element_blank(), # x軸の目盛指示線
    axis.ticks.y = element_blank(), # y軸の目盛指示線
    #panel.grid.major.x = element_line(color = "grey", size = 0.1), # x軸の主目盛線
    panel.grid.major.y = element_blank(), # y軸の主目盛線
    panel.grid.minor.x = element_blank(), # x軸の補助目盛線
    panel.grid.minor.y = element_blank(), # y軸の補助目盛線
    panel.border = element_blank(), # グラフ領域の枠線
    #panel.background = element_blank(), # グラフ領域の背景
    plot.title = element_text(color = "black", face = "bold", size = 20, hjust = 0.5), # 全体のタイトル
    plot.subtitle = element_text(color = "black", size = 15, hjust = 0.5), # 全体のサブタイトル
    plot.margin = margin(t = 10, r = 100, b = 10, l = 150, unit = "pt"), # 全体の余白
    legend.position = "none" # 凡例の表示位置
  ) + # 図の体裁
  labs(title = "ハロプログループの平均年齢の推移", 
       subtitle = "{lubridate::year(closest_state)}年{lubridate::month(closest_state)}月", 
       caption = "データ:「https://github.com/xxgentaroxx/HP_DB」") # ラベル


# gif画像を作成
g <- gganimate::animate(
  plot = anim, 
  nframes = n*(t+s), fps = (t+s)*mps, 
  width = 900, height = 600
)
g

warnings()

# gif画像を保存
gganimate::anim_save(filename = "BarChartRace/output/AverageAge.gif", animation = g)


# 動画を作成と保存
m <- gganimate::animate(
  plot = anim, 
  nframes = n*(t+s), fps = (t+s)*mps, 
  width = 900, height = 600, 
  renderer = gganimate::av_renderer(file = "BarChartRace/output/AverageAge.mp4")
)

