
# メンバーカラーの色見本 -------------------------------------------------------------

# 利用パッケージ
library(tidyverse)

# チェック用
library(ggplot2)


# メンバーカラーの設定 --------------------------------------------------------------

### ・モーニング娘。 -----
morning_df <- tibble::tibble(
  group_name = "モーニング娘。", 
  member_gen = c(
    1, 1, 1, 1, 1, 
    2, 2, 2, 
    3, 
    4, 4, 4, 4, 
    5, 5, 5, 5, 
    6, 6, 6, 6, 
    7, 
    8, 8, 8, 
    9, 9, 9, 9, 
    10, 10, 10, 10, 
    11, 
    12, 12, 12, 12, 
    13, 13, 
    14, 
    15, 15, 15, 
    16
  ), 
  member_name = c(
    "福田明日香", "石黒彩", "中澤裕子", "安倍なつみ", "飯田圭織", 
    "市井紗耶香", "保田圭", "矢口真里", 
    "後藤真希", 
    "辻希美", "加護亜依", "石川梨華", "吉澤ひとみ", 
    "紺野あさ美", "小川麻琴", "高橋愛", "新垣里沙", 
    "藤本美貴", "亀井絵里", "田中れいな", "道重さゆみ", 
    "久住小春", 
    "光井愛佳", "ジュンジュン", "リンリン", 
    "譜久村聖", "生田衣梨奈", "鞘師里保", "鈴木香音", 
    "飯窪春菜", "石田亜佑美", "佐藤優樹", "工藤遥", 
    "小田さくら", 
    "尾形春水", "野中美希", "牧野真莉愛", "羽賀朱音", 
    "加賀楓", "横山玲奈", 
    "森戸知沙希", 
    "北川莉央", "岡村ほまれ", "山﨑愛生", 
    "櫻井梨央"
  ), 
  color_code = c(
    NA, NA, "#006400", "#FF0000", "#000080", 
    "#87CEEB", "#FFB6C1", "#9370DB", 
    "#FFA500", 
    NA, NA, "#FF1493", "#800080", 
    "#FFB6C1", "#0000FF", "#FFFF00", "#ADFF2F", 
    "#FF0000", "#FFA500", "#00BFFF", "#FFB6C1", 
    "#FF0000", 
    "#985BA1", "#0000FF", "#00B379", 
    "#E5007F", "#ADFF2F", "#FF0000", "#008000", 
    "#FFF33F", "#0233CB", "#00A59E", "#FFA500", 
    "#985BA1", 
    "#56BED9", "#572A7B", "#FFC0CB", "#FFA500", 
    "#DB092C", "#EDAE3C", 
    "#FFFFFF", 
    "#56BED9", "#FFDD00", "#02A23E", 
    "#E8D3CA"
  )
) |> 
  dplyr::mutate(member_id = dplyr::row_number(), .before = member_gen)
morning_df


### ・アンジュルム -----
angerme_df <- tibble::tibble(
  group_name = "アンジュルム", 
  member_gen = c(
    1, 1, 1, 1, 
    2, 2, 2, 2, 2, 
    3, 3, 3, 
    4, 
    5, 
    6, 6, 
    7, 7, 
    8, 
    9, 9, 9, 
    10
  ), 
  member_name = c(
    "和田彩花", "前田憂佳", "福田花音", "小川紗季", 
    "中西香菜", "小数賀芙由香", "竹内朱莉", "勝田里奈", "田村芽実", 
    "室田瑞希", "相川茉穂", "佐々木莉佳子", 
    "上國料萌衣", 
    "笠原桃奈", 
    "船木結", "川村文乃", 
    "太田遥香", "伊勢鈴蘭", 
    "橋迫鈴", 
    "川名凜", "為永幸音", "松本わかな", 
    "平山遊季"
  ), 
  color_code = c(
    "#FF0000", "#FFB6C1", "#E5007F", "#ACDD4D", 
    "#F1D1D4", "#FFA500", "#0F2D9E", "#FFA500", "#7D4A94", 
    "#007EC7", "#228B22", "#FFD700", 
    "#7FFFD4", 
    "#E5007F", 
    "#ACDD4D", "#DDA0DD", 
    "#00B379", "#FFA500", 
    "#FF0000", 
    "#008000", "#FFC0CB", "#FFFFFF", 
    "#ACDD4D"
  )
) |> 
  dplyr::mutate(member_id = dplyr::row_number(), .before = member_gen)
angerme_df


### ・Juice=Juice -----
juice_df <- tibble::tibble(
  group_name = "Juice=Jice", 
  member_gen = c(
    1, 1, 1, 1, 1, 1, 
    2, 2, 
    3, 
    4, 4, 
    5, 
    6, 6, 6, 
    7, 7
  ), 
  member_name = c(
    "宮崎由加", "金澤朋子", "高木紗友希", "大塚愛菜", "宮本佳林", "植村あかり", 
    "梁川奈々美", "段原瑠々", 
    "稲場愛香", 
    "工藤由愛", "松永里愛", 
    "井上玲音", 
    "有澤一華", "入江里咲", "江端妃咲", 
    "石山咲良", "遠藤彩加里"
  ), 
  color_code = c(
    "#FFC0CB", "#FF0000", "#FFFF00", "#FFA500", "#8F48C9", "#7FFF00", 
    "#1B81C1", "#FFA500", 
    "#E5007F", 
    "#FFC0CB", "#0402FD", 
    "#FFFFFF", 
    "#9BDCFA", "#E0ADFC", "#FEDC01", 
    "#9966FF", "#49B9AB"
  )
) |> 
  dplyr::mutate(member_id = dplyr::row_number(), .before = member_gen)


### ・つばきファクトリー -----
tsubaki_df <- tibble::tibble(
  group_name = "つばきファクトリー", 
  member_gen = c(
    1, 1, 1, 1, 1, 1, 
    2, 2, 2, 
    3, 3, 3, 3
  ), 
  member_name = c(
    "山岸理子", "小片リサ", "新沼希空", "谷本安美", "岸本ゆめの", "浅倉樹々", 
    "小野瑞歩", "小野田紗栞", "秋山眞緒", 
    "河西結心", "八木栞", "福田真琳", "豫風瑠乃"
  ), 
  color_code = c(
    "#BDF53A", "#FD9556", "#90F2FB", "#E0ADFC", "#FDEB3F", "#FEC1BE", 
    "#1BC6AE", "#FEB4F8", "#FF6361", 
    "#9966FF", "#F36806", "#0402FD", "#FEBD01"
  )
) |> 
  dplyr::mutate(member_id = dplyr::row_number(), .before = member_gen)
tsubaki_df


### ・BEYOOOOONDS -----
beyonds_df <- tibble::tibble(
  group_name = "BEYOOOOONDS", 
  member_gen = c(
    1, 1, 1, 1, 
    1, 1, 1, 1, 1, 
    1, 1, 1
  ), 
  member_name = c(
    "一岡伶奈", "島倉りか", "西田汐里", "江口紗耶", 
    "高瀬くるみ", "前田こころ", "山﨑夢羽", "岡村美波", "清野桃々姫", 
    "平井美葉", "小林萌花", "里吉うたの"
  ), 
  color_code = c(
    "#9BECFF", "#975AA0", "#E3197F", "#FEDC01", 
    "#49B9AB", "#48BBEC", "#EE282A", "#FF80AD", "#F36806", 
    "#491B7E", "#01824C", "#0069B5"
  )
) |> 
  dplyr::mutate(member_id = dplyr::row_number(), .before = member_gen)
beyonds_df


### ・OCHA NORMA -----
ocha_df <- tibble::tibble(
  group_name = "OCHA NORMA", 
  member_gen = c(
    1, 1, 1, 1, 
    1, 1, 1, 1, 
    1, 1
  ), 
  member_name = c(
    "米村姫良々", "石栗奏美", "窪田七海", "斉藤円香", 
    "中山夏月姫", "広本瑠璃", "西﨑美空", "北原もも", 
    "田代すみれ", "筒井澪心"
  ), 
  color_code = c(
    "#EE282A", "#F36806", "#FF80AD", "#48BBEC", 
    "#FFFFFF", "#FDEB3F", "#491B7E", "#BDF53A", 
    "#E0ADFC", "#0402FD"
  )
) |> 
  dplyr::mutate(member_id = dplyr::row_number(), .before = member_gen)
ocha_df


# ファイルの読み書き ---------------------------------------------------------------

# グループごとのメンバーカラー一覧を結合
all_df <- dplyr::bind_rows(
  morning_df, 
  angerme_df, 
  juice_df, 
  tsubaki_df, 
  beyonds_df, 
  ocha_df
) |> 
  dplyr::mutate(group_name = factor(group_name, levels = unique(group_name))) |> 
  dplyr::group_by(group_name) |> 
  dplyr::mutate(
    group_id = dplyr::row_number(), .before = group_name, 
    group_name = as.character(group_name)
  ) |> 
  dplyr::ungroup()
all_df


# 保存先のパスを指定
dir_path <- "ColorPalette/data/"

# UTF8のcsvファイルを書き出し
readr::write_csv(df, file = paste0(dir_path, "membercolor_utf8.csv"))

# Shift-JISのcsvファイルを書き出し
write.csv(df, file = paste0(dir_path, "membercolor_cp932.csv"), fileEncoding = "CP932", row.names = FALSE)


# UTF8のcsvファイルを読み込み
all_df <- readr::read_csv(
  file = paste0(dir_path, "membercolor_utf8.csv"), 
  col_types = readr::cols(
    group_id = "i", 
    group_name = "c", 
    member_id = "i", 
    member_gen = "i", 
    member_name = "c", 
    color_code = "c"
  )
)

# Shift-JISのcsvファイルを読み込み
all_df <- readr::read_csv(
  file = paste0(dir_path, "membercolor_cp932.csv"), 
  locale = readr::locale(encoding = "CP932"), 
  col_types = readr::cols(
    group_id = "i", 
    group_name = "c", 
    member_id = "i", 
    member_gen = "i", 
    member_name = "c", 
    color_code = "c"
  )
)


# 色見本の作成 ------------------------------------------------------------------

# グループを指定
color_df <- morning_df
color_df <- angerme_df
color_df <- juice_df
color_df <- tsubaki_df
color_df <- beyonds_df
color_df <- ocha_df

# グループ名を抽出
group_name_val <- color_df[["group_name"]][1]


# グループを指定
group_name_val <- "モーニング娘。"

# 指定したグループを抽出
color_df <- all_df |> 
  dplyr::filter(group_name == group_name_val)


# 色見本の作成:横方向
ggplot(color_df, aes(x = member_id, color = member_name, fill = member_name)) + 
  geom_bar(mapping = aes(y = 1), stat = "identity") + # カラーバー
  geom_text(mapping = aes(y = 0, label = paste(member_name, " ")), 
            hjust = 1) + # メンバー名ラベル
  geom_text(mapping = aes(y = 0, label = paste(" ", color_code)), 
            hjust = 0, color = "black") + # カラーコードラベル
  scale_color_manual(breaks = color_df[["member_name"]], values = color_df[["color_code"]]) + # 線の色
  scale_fill_manual(breaks = color_df[["member_name"]], values = color_df[["color_code"]]) + # バーの色
  scale_x_reverse(breaks = color_df[["member_id"]], expand = c(0, 0)) + # x軸目盛
  scale_y_continuous(expand = c(0, 0)) + # y軸目盛
  coord_flip(clip = "off") + # 軸の入れ替え
  theme(
    axis.title.x = element_blank(), # 横軸のラベル
    axis.title.y = element_blank(), # 縦軸のラベル
    axis.text.x = element_blank(), # 横軸の目盛ラベル
    axis.text.y = element_blank(), # 縦軸の目盛ラベル
    axis.ticks.x = element_blank(), # 横軸の目盛指示線
    panel.grid.major.x = element_blank(), # 横軸の主目盛線
    panel.grid.minor.x = element_blank(), # 横軸の補助目盛線
    panel.grid.minor.y = element_blank(), # 縦軸の補助目盛線
    panel.border = element_blank(), # グラフ領域の枠線
    panel.background = element_blank(), # グラフ領域の背景
    plot.title = element_text(color = "black", face = "bold", size = 20, hjust = 0.5), # 全体のタイトル
    plot.subtitle = element_text(color = "black", size = 15, hjust = 0.5), # 全体のサブタイトル
    plot.background = element_rect(fill = "gray92"), # 全体の背景
    plot.margin = margin(t = 10, r = 20, b = 10, l = 120, unit = "pt"), # 全体の余白
    legend.position = "none" # 凡例の位置
  ) + # 図の体裁
  labs(
    title = paste0(group_name_val, "のメンバーカラー一覧"), 
    subtitle = paste0("総メンバー数：", max(color_df[["member_id"]]), "人")
  ) # ラベル

# 色見本の作成:縦方向
ggplot(color_df, aes(x = member_id, color = member_name, fill = member_name)) + 
  geom_bar(mapping = aes(y = 1), stat = "identity") + # カラーバー
  geom_text(mapping = aes(y = 0, label = paste(member_name, " ")), 
            hjust = 1, angle = 90) + # メンバー名ラベル
  geom_text(mapping = aes(y = 0, label = paste(" ", color_code)), 
            hjust = 0, angle = 90, color = "black") + # カラーコードラベル
  scale_color_manual(breaks = color_df[["member_name"]], values = color_df[["color_code"]]) + # 線の色
  scale_fill_manual(breaks = color_df[["member_name"]], values = color_df[["color_code"]]) + # バーの色
  scale_x_continuous(breaks = color_df[["member_id"]], expand = c(0, 0)) + # x軸目盛
  scale_y_continuous(expand = c(0, 0)) + # y軸目盛
  coord_cartesian(clip = "off") + 
  theme(
    axis.title.x = element_blank(), # 横軸のラベル
    axis.title.y = element_blank(), # 縦軸のラベル
    axis.text.x = element_blank(), # 横軸の目盛ラベル
    axis.text.y = element_blank(), # 縦軸の目盛ラベル
    axis.ticks.y = element_blank(), # 縦軸の目盛指示線
    panel.grid.major.y = element_blank(), # 縦軸の主目盛線
    panel.grid.minor.x = element_blank(), # 横軸の補助目盛線
    panel.grid.minor.y = element_blank(), # 縦軸の補助目盛線
    panel.border = element_blank(), # グラフ領域の枠線
    panel.background = element_blank(), # グラフ領域の背景
    plot.title = element_text(color = "black", face = "bold", size = 20, hjust = 0.5), # 全体のタイトル
    plot.subtitle = element_text(color = "black", size = 15, hjust = 0.5), # 全体のサブタイトル
    plot.background = element_rect(fill = "gray92"), # 全体の背景
    plot.margin = margin(t = 10, r = 20, b = 90, l = 20, unit = "pt"), # 全体の余白
    legend.position = "none" # 凡例の位置
  ) + # 図の体裁
  labs(
    title = paste0(group_name_val, "のメンバーカラー一覧"), 
    subtitle = paste0("総メンバー数：", max(color_df[["member_id"]]), "人")
  ) # ラベル


# カラーパレット(試作) -----------------------------------------------------------------

# カラーパレットを作成
color_vec <- color_df |> 
  dplyr::filter(member_gen == 10) |> # 期を指定
  dplyr::pull(color_code) |> # ベクトルとして取得
  (\(x){x[c(2, 3, 1, 4)]})() # 色順を並べ替え

# 値を作成
res_df <- tidyr::expand_grid(x = 1:100, y = 1:100) |> # 2次元の点を作成
  dplyr::mutate(z = x^2 + y^2, z = z / max(z)) # 2乗和を最大値で割る

# ヒートマップを作成
ggplot(res_df, aes(x = x, y = y, color = z, fill = z)) + 
  geom_tile() + # ヒートマップ
  scale_color_gradientn(colours = color_vec) + # グラデーション
  scale_fill_gradientn(colours = color_vec) + # タイルの色
  coord_fixed(ratio = 1) # アスペクト比

