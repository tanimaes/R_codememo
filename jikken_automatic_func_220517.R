# 2022年度海洋生物科学実験III
# Greg Nishihara
# 2022-05-18

# パッケージの読み込み ---------------------------------------------------------
library(tidyverse)
library(lubridate)
library(readxl)
library(stringi)

# ファイルパスを取り出す. ------------------------------------------------------
files = dir("~/Lab_Data/学生実験/2022データ/", full = T)


# データの書き込み方が班によって様々なので、１つ１つ整形していく. --------------
# 関数の定義 
 
sheet1_func = function(file, sheet = 1){
  # 酸素データの読み込み
  read_xlsx(file, sheet) |> 
    rename("han" = matches("班"),
           "sample" = matches("サンプル|番号"),
           "time" = matches("時間"),
           "mgl" = matches("酸素"),
           "temp" = matches("水温"),
           "light_c" = matches("光(環境|条件)"),
           "remarks" = matches("備考")) |> 
    mutate(light_c = as.character(stri_trans_nfkd(light_c))) |> 
    mutate(light_c = case_when(str_detect(light_c, pattern = "[アAa]") ~ "アルミホイル",
                               str_detect(light_c, pattern = "6") ~ "6ネット",
                               str_detect(light_c, pattern = "4") ~ "4ネット",
                               str_detect(light_c, pattern = "2") ~ "2ネット",
                               str_detect(light_c, pattern = "0") ~ "0ネット")) |> 
    mutate(across(c(han, sample, light_c), as.factor)) |> 
    drop_na(han)
}

sheet2_func = function(file, sheet = 2){
  # 光環境の読み込み
  df = read_xlsx(file, sheet) |> 
    rename("han" = matches("班"),
           "sample" = matches("サンプル|番号"),
           "light_c" = matches("光環境"),
           "remarks" = matches("備考"),
           "light" = matches("光量子量")) |> 
    mutate(light_c = as.character(stri_trans_nfkd(light_c))) |> 
    mutate(light_c = case_when(str_detect(light_c, pattern = "[アAa]") ~ "アルミホイル",
                               str_detect(light_c, pattern = "6") ~ "6ネット",
                               str_detect(light_c, pattern = "4") ~ "4ネット",
                               str_detect(light_c, pattern = "2") ~ "2ネット",
                               str_detect(light_c, pattern = "0") ~ "0ネット")) |> 
    mutate(across(c(han, sample, light_c), as.factor))
  
  A = df |> drop_na(light_c)
  # N = unique(A$light_c) |> length()
  N = A |> pull(light_c) |> unique() |> length()
  
  if(N != 5){
    df = df |> 
      add_row(light_c = factor("アルミホイル"),
              light = 0) |> 
      fill(c(han, sample))
  }
  
  # HAN = df$han |> unique()
  HAN = df |> pull(han) |> unique()
  
  if(HAN == "C"){
    df = df |> 
      # mutate(sample = ifelse(str_detect(file, pattern = "C班5月12日"), 
      #                        factor(1), factor(2))) 
      mutate(sample = ifelse(str_detect(file, pattern = "C班5月12日"), 1, 2))
  }
  
  df |> 
    fill(c("han", "light_c", "sample"), .direction = "down") |> 
    group_by(han, light_c, sample) |> 
    summarise(light = mean(light), .groups = "drop") |> 
    mutate(sample = as.factor(sample))
}

sheet3_func = function(file, sheet = 3){
  # 海藻と酸素瓶のデータ
  df = read_xlsx(file, sheet) |>  
    rename("han" = matches("班"),
           "datetime" = matches("実験日"),
           "species" = matches("海藻"),
           "sample" = matches("サンプル"),
           "weight" = matches("湿重量"),
           "vol" = matches("瓶")) |> 
    mutate(across(c(han, sample, species), as.factor),
           weight =  str_remove_all(weight, pattern = "[a-z]"),
           vol = str_remove_all(vol, pattern = "[a-z]")) |> 
    mutate(across(c(weight, vol), as.double))
  
  # HAN = df$han |> unique()
  HAN = df |> pull(han) |> unique()
  
  if(HAN == "C"){
    if(str_detect(file, pattern = "C班5月12日")){
      df |> slice(1) |> return()
    } else {
       df |> slice(2) |> return()
    }
  }
}

func_all = function(file){
  
  s1 = sheet1_func(file)
  s2 = sheet2_func(file)
  s3 = sheet3_func(file)
  
  alldata = s1 |> 
    left_join(s2, by = c("han", "sample", "light_c")) |>
    left_join(s3, by = c("han", "sample"))
  
  alldata |> 
    mutate(mgl = ifelse(han == 2 & mgl < 2, mgl*10, mgl),
           mgl = ifelse(han == 2 & mgl > 50, mgl/10, mgl))
}
################################################################################
# データの読み込みと結合

dall = tibble(file = files) |> 
  # slice_head(n = 10) |>
  filter(!str_detect(file, "実験結果")) |>  
  mutate(data = map(file, func_all)) |> 
  select(-file)

dall = dall |> unnest(data) |> select(-remarks)
dall
dall |> filter(is.na(light)) |> print(n = Inf)
# dall |> unnest(data) |> print(n = Inf)

