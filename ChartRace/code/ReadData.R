
# データベースの読込 -----------------------------------------------------------


# パッケージの読込 -------------------------------------------------------------

# 利用パッケージ
library(tidyverse)


# データの読込 -----------------------------------------------------------------

# ハロプロorスタプラを指定
agcy_flag <- "hello"
agcy_flag <- "star"

# データフォルダを設定
dat_path <- "ChartRace/data/"
if(agcy_flag == "hello") {
  
  # ハロプロの場合
  agcy_path <- paste0(dat_path, "HP_DB/")
  
} else if(agcy_flag == "star") {
  
  # スタプラの場合
  agcy_path <- paste0(dat_path, "SP_DB/")
  
}

# グループ一覧を読込
group_df <- readr::read_csv(
  file = paste0(agcy_path, "group.csv"), 
  col_types = readr::cols(
    groupID      = "i", 
    groupName    = "c", 
    formDate     = readr::col_date(format = "%Y/%m/%d"), 
    dissolveDate = readr::col_date(format = "%Y/%m/%d"), 
    isUnit       = "l"
  )
) |> 
  dplyr::arrange(groupID, formDate) |> # 昇順
  dplyr::mutate(
    groupName = groupName |> 
      stringr::str_replace(pattern = "℃", replacement = "°C") # 豆腐化回避用の小細工
  )
group_df

# 加入・卒業日一覧を読込
join_df <- readr::read_csv(
  file = paste0(agcy_path, "join.csv"), 
  col_types = readr::cols(
    memberID = "i", 
    groupID  = "i", 
    joinDate = readr::col_date(format = "%Y/%m/%d"), 
    gradDate = readr::col_date(format = "%Y/%m/%d")
  )
) |> 
  dplyr::arrange(joinDate, memberID, groupID) # 昇順
join_df

# メンバー一覧を読込
member_df <- readr::read_csv(
  file = paste0(agcy_path, "member.csv"), 
  col_types = readr::cols(
    memberID   = "i", 
    memberName = "c", 
    HPjoinDate = readr::col_date(format = "%Y/%m/%d"), 
    debutDate  = readr::col_date(format = "%Y/%m/%d"), 
    HPgradDate = readr::col_date(format = "%Y/%m/%d"), 
    memberKana = "c", 
    birthDate  = readr::col_date(format = "%Y/%m/%d")
  )
) |> 
  dplyr::arrange(memberID, HPjoinDate) # 昇順
member_df <- readr::read_csv(
  file = paste0(agcy_path, "member.csv"), 
  col_types = readr::cols(
    memberID   = "i", 
    memberName = "c", 
    SPjoinDate = readr::col_date(format = "%Y/%m/%d"), # (スタプラ用)
    debutDate  = readr::col_date(format = "%Y/%m/%d"), 
    sPgradDate = readr::col_date(format = "%Y/%m/%d"), # (スタプラ用)
    memberKana = "c", 
    birthDate  = readr::col_date(format = "%Y/%m/%d"), 
    birthplace = "c"
  )
) |> 
  dplyr::arrange(memberID, SPjoinDate) # 昇順
member_df

# メンバーカラー一覧を読込
color_df <- readr::read_csv(
  file = paste0(agcy_path, "color.csv"), 
  #locale = readr::locale(encoding = "CP932"), 
  col_types = readr::cols(
    groupID     = "i", 
    groupName   = "c", 
    memberID    = "i", 
    memberName  = "c", 
    memberColor = "c"
  )
) |> 
  dplyr::mutate(
    memberColor = memberColor |> 
      is.na() |> 
      dplyr::if_else(
        true  = "gray", # 色を指定
        false = memberColor
      )
  ) |> # メンカラなしの配色を設定
  dplyr::arrange(groupID, memberID) # 昇順
color_df

# 都道府県名の対応表を読込
reg_df <- readr::read_csv(
  file = paste0(dat_path, "common/region.csv"), 
  col_types = readr::cols(
    prefecture_en = "c", 
    prefecture_jp = "c", 
    capital_en    = "c", 
    capital_jp    = "c", 
    latitude      = "d", 
    longitude     = "d"
  )
)
reg_df


# データの前処理 ---------------------------------------------------------------

### 期間の設定 -----

# 集計期間を指定
date_min <- "1997-09-01" |> 
  lubridate::as_date() |> 
  lubridate::floor_date(unit = "month") # 集計開始(最小)月
date_max <- "2024-10-01" |> 
  lubridate::as_date() |> 
  lubridate::floor_date(unit = "month") # 集計終了(最大)月
#date_max <- lubridate::today()|> 
#  lubridate::floor_date(unit = "month") # 集計終了(最大)月


### 演出用データの作成 -----

# 活動月, グループID, グループ名の対応表を作成:(改名の対応用)
group_name_df <- group_df |> 
  dplyr::mutate(
    date_from = formDate |> 
      lubridate::floor_date(unit = "month"), # 結成・改名月
    date_to   = dissolveDate |> 
      is.na() |> 
      dplyr::if_else(
        true  = max(lubridate::today(), date_max), # 活動中なら現在の日付or最大月
        false = dissolveDate
      ) |> 
      lubridate::floor_date(unit = "month") # 解散・改名・現在(最大)月
  ) |> 
  dplyr::reframe(
    date = seq(from = date_from, to = date_to, by = "month"), # 活動月
    .by = c(groupID, groupName, formDate, dissolveDate)
  ) |> 
  dplyr::slice_min(formDate, n = 1, with_ties = FALSE, by = c(date, groupID)) |> # 月途中の改名なら重複するので改名前を抽出
  dplyr::filter(dplyr::between(date, left = date_min, right = date_max)) |> # 集計期間の月を抽出
  dplyr::select(date, groupID, groupName, formDate, dissolveDate) |> 
  dplyr::arrange(date, groupID) # 昇順
group_name_df

# 結成前月, 解散翌月のデータを作成:(バーの変化の強調用)
outside_df <- group_df |> 
  dplyr::summarise(
    formDate     = min(formDate),     # 結成日
    dissolveDate = max(dissolveDate), # 解散日
    .by = groupID
  ) |> # 改名情報を統合
  dplyr::mutate(
    date_pre = formDate |> 
      lubridate::floor_date(unit = "month"), # 結成月
    date_pre = (date_pre == formDate)|> 
      dplyr::if_else(
        true = date_pre |> 
          lubridate::rollback() |> 
          lubridate::floor_date(unit = "month"), # 結成日が月初なら結成前月
        false = date_pre
      ), 
    date_post = dissolveDate |> 
      lubridate::rollforward(roll_to_first = TRUE) # 解散翌月
  ) |>　
  tidyr::pivot_longer(
    cols      = c(date_pre, date_post), 
    names_to  = "date_type", 
    values_to = "date"
  ) |> # 月列をまとめる
  dplyr::filter(!is.na(date)) |> # 活動中なら解散月を除去
  dplyr::filter(dplyr::between(date, left = date_min, right = date_max)) |> # 集計期間の月を抽出
  dplyr::select(date, groupID) |> 
  dplyr::arrange(groupID, date) # 昇順
outside_df


# データの確認 ------------------------------------------------------------------

# 重複を確認
member_df |> 
  dplyr::mutate(
    n = dplyr::n(), .by = memberID
  ) |> # 重複をカウント
  dplyr::filter(n > 1) # 重複データを抽出
member_df |> 
  dplyr::filter(HPjoinDate > debutDate)

# 生年月日非公開メンバー
member_df |> 
  dplyr::filter(is.na(birthDate))

df <- join_df |> 
  dplyr::left_join(member_df, by = "memberID", relationship = "many-to-many") |> 
  dplyr::mutate(n = dplyr::n(), .by = memberID) |> 
  dplyr::filter(n > 1)


