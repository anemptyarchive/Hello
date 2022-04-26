
# シングルリリース数の推移を可視化 --------------------------------------------------

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

# シングル一覧
single_df <- readr::read_csv(
  file = paste0(dir_path, "single.csv"), 
  col_types = readr::cols(
    singleID = "i", 
    singleName = "c", 
    releaseDate = readr::col_date(format = "%Y/%m/%d"), 
    singleCategory = "c", 
    artistName = "c"
  )
)
single_df


# 期間とアーティスト名の設定 ------------------------------------------------------------

### ・期間の指定 -----

# 期間を指定
date_from <- "1997-10-01"
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
length(date_vec)


### ・アーティスト名の編集 -----

# シングルの種類を確認
unique(single_df[["singleCategory"]])

# シングルの種類を指定
single_category <- c("インディーズシングル", "シングル")

# アーティスト名を確認
unique(single_df[["artistName"]])

# 連名作品を指定
pattern_vec <- c("ガーディアンズ4/しゅごキャラエッグ!", "Berryz工房×℃-ute", "ハロプロ研修生 feat. Juice=Juice", "こぶしファクトリー＆つばきファクトリー")
replace_vec <- c("ガーディアンズ4", "しゅごキャラエッグ!", "ガーディアンズ4", "しゅごキャラエッグ!", "Berryz工房", "℃-ute","Berryz工房", "℃-ute", "ハロプロ研修生", "Juice=Juice", "こぶしファクトリー", "つばきファクトリー")
n_vec <- c(2, 2, 2, 2, 2, 2)

# 連名作品を分割
tmp_single_df <- single_df %>% 
  dplyr::filter(artistName %in% pattern_vec) %>% # 連名の作品を抽出
  tibble::add_column(n = n_vec) %>% # 複製数を追加
  tidyr::uncount(n) %>% # 作品を複製
  dplyr::mutate(artistName = replace_vec) # 個々のアーティスト名を再設定
tmp_single_df

# アーティスト名を編集してリリース数を集計
release_n_df <- single_df %>% 
  dplyr::filter(!(artistName %in% pattern_vec)) %>% # 連名作品を削除
  rbind(tmp_single_df) %>% # 分割した連名作品を追加
  dplyr::filter(singleCategory %in% single_category) %>% # 指定したカテゴリを抽出
  dplyr::filter(releaseDate >= min(date_vec), releaseDate <= max(date_vec)) %>% # 指定した期間内の作品を抽出
  dplyr::arrange(releaseDate, singleID) %>% # 昇順に並び替え
  dplyr::mutate(
    release_date = lubridate::floor_date(releaseDate, unit = "mon"), # 月単位に切り捨て
    artist_name = artistName %>% # グラフ表示名を追加
      #stringr::str_replace(pattern = "℃-ute", replacement = "C-ute") %>% # 作図時に豆腐化するので代用
      stringr::str_replace(pattern = "中澤ゆうこ&高山厳", replacement = "中澤ゆうこ") %>% # 不要な文字列を削除
      stringr::str_replace(pattern = "カントリー娘。に石川梨華（モーニング娘。）", replacement = "カントリー娘。に石川梨華") %>% # 不要な文字列を削除
      stringr::str_replace(pattern = "カントリー娘。に紺野と藤本（モーニング娘。）", replacement = "カントリー娘。に紺野と藤本") %>% # 不要な文字列を削除
      stringr::str_replace(pattern = "ミニハムず/プリンちゃん", replacement = "ミニハムず") %>% # 不要な文字列を削除
      stringr::str_replace(pattern = "バカ殿様とミニモニ姫。", replacement = "ミニモニ。") %>% # 企画名義を元の名前に変更
      stringr::str_replace(pattern = "ミニモニ。と高橋愛＋4KIDS", replacement = "ミニモニ。") %>% # ゲスト名を削除
      stringr::str_replace(pattern = "おけいさんと安倍なつみ", replacement = "安倍なつみ") %>% # 不要な文字列を削除
      stringr::str_replace(pattern = "あややム with エコハムず", replacement = "松浦亜弥") %>% # 企画名義を元の名前に変更
      stringr::str_replace(pattern = "DEF.DIVAと楽天イーグルス応援隊", replacement = "DEF.DIVA") %>% # 不要な文字列を削除
      stringr::str_replace(pattern = "月島きらり.*", replacement = "月島きらり(久住小春)") %>% # 長いので省略
      stringr::str_replace(pattern = "THE ポッシボー.*", replacement = "THE ポッシボー") %>% # メンバー名を削除
      stringr::str_replace(pattern = "里田まい with 藤岡藤巻", replacement = "里田まい") %>% # 不要な文字列を削除
      stringr::str_replace(pattern = "ジンギスカン×Berryz工房", replacement = "Berryz工房") %>% # 不要な文字列を削除
      stringr::str_replace(pattern = "矢口真里/エアバンド", replacement = "矢口真里") %>% # 不要な文字列を削除
      stringr::str_replace(pattern = "おはガールメープル with スマイレージ", replacement = "スマイレージ") %>% # 不要な文字列を削除
      stringr::str_replace(pattern = "NEXT YOU.*", replacement = "Juice=Juice"), # 企画名義を元の名前に変更
    artist_idname = artist_name %>% # id割り当て用に編集
      stringr::str_replace(pattern = "モーニング娘。.*", replacement = "モーニング娘。") %>% # ナンバリングを削除:(「飯窪春菜(モーニング娘。'17)、金澤朋子(Juice=Juice)」などと衝突するけど実害がないので放置)
      stringr::str_replace(pattern = "中澤ゆうこ", replacement = "中澤裕子") %>% # ソロ用名義を名前に統一
      stringr::str_replace(pattern = "T&Cボンバー", replacement = "太陽とシスコムーン") %>% # 改名前に統一
      stringr::str_replace(pattern = "カントリー娘。.*", replacement = "カントリー娘。") %>% # ゲスト名を削除
      stringr::str_replace(pattern = "ミニハムず", replacement = "ミニモニ。") %>% # 企画名義を元の名前に変更
      stringr::str_replace(pattern = "S/mileage", replacement = "スマイレージ") %>%  # 改名後に統一
      stringr::str_replace(pattern = "アンジュルム", replacement = "スマイレージ") %>% # 改名前に統一
      stringr::str_replace(pattern = "ハロプロ研修生北海道 feat.稲場愛香", replacement = "ハロプロ研修生北海道"), # ゲスト名を削除
    artist_idname = factor(artist_idname, levels = unique(artist_idname)), # レベル設定のため因子型に変換
    artist_id = dplyr::dense_rank(artist_idname) # アーティストIDを追加
  ) %>% 
  dplyr::group_by(release_date, artist_id) %>% # 発売月とアーティストでグループ化
  dplyr::mutate(release_n = dplyr::row_number()) %>% # リリース数をカウント:(count()で処理できるならしたい)
  dplyr::filter(release_n == max(release_n)) %>% # 同じ月に複数リリースしていると重複するので遅い方を採用
  dplyr::ungroup() %>% # グループ化を解除
  dplyr::select(release_date, artist_id, artist_idname, artist_name, artistName, release_n) %>% # 利用する列を選択:(確認用)
  #dplyr::select(release_date, artist_id, artist_name, release_n) %>% # 利用する列を選択:(集計用)
  dplyr::arrange(release_date, artist_id) # 昇順に並び替え
release_n_df

# アーティスト名を確認
unique(release_n_df[["artist_idname"]])
unique(release_n_df[["artist_name"]])


# 集計と作図 -------------------------------------------------------------------

## AlbumNum.Rを参照


