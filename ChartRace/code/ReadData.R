
# ハロプロデータベースの読込 -----------------------------------------------------------

# 利用パッケージ
library(tidyverse)


# データの読込 -----------------------------------------------------------------

# フォルダパスを指定
dir_path <- "ChartRace/data/HP_DB-main/"

# グループ一覧を読込
group_df <- readr::read_csv(
  file = paste0(dir_path, "group.csv"), 
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
  file = paste0(dir_path, "join.csv"), 
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
  file = paste0(dir_path, "member.csv"), 
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
member_df


# データの前処理 -----------------------------------------------------------------

# 活動月, グループID, グループ名の対応表を作成
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
    .by = dplyr::everything()
  ) |> 
  dplyr::slice_min(formDate, n = 1, with_ties = FALSE, by = c(date, groupID)) |> # 月途中の改名なら重複するので改名前を抽出
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
          lubridate::floor_date(unit = "month"), # 結成月が月初なら結成前月
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
  dplyr::select(date, groupID) |> 
  dplyr::filter(!is.na(date)) |> # 活動中なら解散月を除去
  tibble::add_column(
    groupName = " ", 
    target    = 0
  ) |> # 疑似集計データを追加
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




