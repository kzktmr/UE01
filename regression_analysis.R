# COVID-19の感染率と人口密度の関係

# ライブラリの読み込み ====
library(tidyverse)
library(readxl)

# データの読み込み ====
# 感染者数のデータを読み込む ----
covid_data <- read_csv("data/confirmed_cases_cumulative_daily.csv")
View(covid_data)

covid_data <- covid_data |> filter(Date == "2023/5/7") # 最新のデータを抽出
View(covid_data)

covid_data <- covid_data |> select(-c(ALL)) # 全国のデータを除外
View(covid_data)

covid_data <- covid_data |> 
  pivot_longer(                   # データの整形（縦持ちに変換）
    cols = -Date,                 # Date列以外のすべての列を対象 
    names_to = "Prefecture",      # 都道府県名の列名を"Prefecture"に
    values_to = "Confirmed_Cases" # 感染者数の列名を"Confirmed_Cases"に
  ) 
View(covid_data)

covid_data <- covid_data |> select(-Date) # Date列を削除
View(covid_data)

# すべての処理をパイプでつなげると…
# covid_data <- 
#   read_csv("data/confirmed_cases_cumulative_daily.csv") |> 
#   filter(Date == "2023/5/7") |> 
#   select(-c(ALL)) 
#   pivot_longer(
#     cols = -Date,
#     names_to = "Prefecture",
#     values_to = "Confirmed_Cases" 
#   ) |> 
#   select(-Date)
# View(covid_data)

# 人口密度のデータを読み込む ----
population_data <- list() # 空のリストを作成
population_data[[1]] <- 
  read_excel(
    path = "data/a001.xls",                # Excelファイル名
    range = "I13:J59",                     # 読み込む範囲
    col_names = c("都道府県", "Prefecture") # 
  )
View(population_data[[1]])

population_data[[1]] <- population_data[[1]] |> 
  mutate(Prefecture = str_remove(Prefecture, "-ken$|-to$|-fu$")) # 末尾の「-ken」「-to」「-fu」を削除
View(population_data[[1]])

population_data[[2]] <- 
  read_excel(
    path = "data/a001.xls",                             # Excelファイル名
    range = "CZ13:DC59",                                # 読み込む範囲
    col_names = c("DID_Population", "DID_Area"),        # 列名を指定
    col_types = c("numeric", "skip", "skip", "numeric") # 列の型を指定（不要な列はスキップ）
  )
View(population_data[[2]])

population_data <- bind_cols(population_data) # リストの要素を結合
View(population_data)

population_data <- population_data |>            
  mutate(DID_Density = DID_Population / DID_Area) # DID人口密度を計算
View(population_data)

population_data <- population_data |>
  select(都道府県, Prefecture, DID_Density) # 必要な列のみを抽出
View(population_data)

