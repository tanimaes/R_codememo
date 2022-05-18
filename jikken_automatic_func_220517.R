# 学生実験用のコード.
# Shinichiro Tanimae
# 2022-05-18

library(tidyverse)
library(lubridate)
library(readxl)
library(stringi)

# ファイルパスを取り出す. ------------------------------------------------------
files = tibble(fpath = dir("~/Lab_Data/学生実験/2022データ/", full = T))

# エクセルファイルのシートごとに読み込む関数を作る. ------------------------------

sheet1_func = function(file){
  
  # 1つの班だけエクセルファイルのまとめ方が異なるので, 個別に読む.
  if(str_detect(file, pattern = "実験結果.xlsx")){
    df = read_xlsx(file, range = "B3:H163") 
  }else{
    df = read_xlsx(file, sheet = 1)  
  }
  
  df = df |> 
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
  
  # 6 班の 2 日目の班名が D 班になっているので変える.
  if(str_detect(file, pattern = "13_6班")){
    df = df |> mutate(han = factor(6)) 
  }
  return(df)
}

sheet2_func = function(file){
  
  # 1つの班だけエクセルファイルのまとめ方が異なるので, 個別に読む.
  if(str_detect(file, pattern = "実験結果.xlsx")){
    df = read_xlsx(file, range = "J7:L57")  |> mutate(han = 5)
  }else{
    df = read_xlsx(file, sheet = 2)
  }
  
  df =  df |>
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
    mutate(across(c(han, sample), as.factor)) 
  
  # A 班の記入ミスを書き換え.
  if(str_detect(file, pattern = "A班5月13")){
    df = df |> mutate(light_c = ifelse(row_number() > 18, "2ネット", light_c))
  }
  
  N = df |> drop_na(light_c) |> pull(light_c) |> unique() |> length()

  # アルミホイル使用時の光量子量を書いていない班のデータを書き換え.
  if(N != 5){
    df = df |> 
      add_row(light_c = c("アルミホイル"),
              light = 0) |> 
      fill(c(han, sample))
  }
  
  # C 班の sample を書き換え.
  if(str_detect(file, pattern = "C班")){
    if(str_detect(file, pattern = "5月12")){
      df = df |> mutate(sample = factor(1))
    }else{
      df = df |> mutate(sample = factor(2))
    }
  }
  
  df |> 
    fill(c("han", "light_c", "sample"), .direction = "down") |> 
    mutate(light_c = as.factor(light_c)) |> 
    group_by(han, light_c, sample) |> 
    summarise(light = mean(light), .groups = "drop")
}

sheet3_func = function(file){
  
  # 1つの班だけエクセルファイルのまとめ方が異なるので, 個別に読む.
  if(str_detect(file, pattern = "実験結果.xlsx")){
    df = read_xlsx(file, range = "J3:N5") |> mutate(han = 5)
  }else{
    df = read_xlsx(file, sheet = 3)  
  }
  
  df = df |> 
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
  
  # C 班の データフレームを分割.
  if(str_detect(file, pattern = "C班")){
    if(str_detect(file, pattern = "5月12")){
      df = df |> filter(datetime == ymd_hms("2022-05-12 00:00:00"))
    }else{
      df = df |> filter(datetime == ymd_hms("2022-05-13 00:00:00"))
    }
  }
  return(df)
}

func_all = function(file){
  
  s1 = sheet1_func(file)
  s2 = sheet2_func(file)
  s3 = sheet3_func(file)
  
  s1 |> 
    left_join(s2, by = c("han", "sample", "light_c")) |> 
    left_join(s3, by = c("han", "sample")) |> 
    mutate(mgl = ifelse(han == 2 & mgl < 2, mgl*10, mgl),
           mgl = ifelse(han == 2 & mgl > 50, mgl/10, mgl))
}

# データの読み込み. ------------------------------------------------------------
alldata = files |> mutate(data = map(fpath, func_all)) |> unnest()

# 作図.
alldata |> 
  ggplot() +
  geom_line(aes(x = time, y = mgl, color = light_c, group = light_c)) +
  facet_wrap(vars(han, species), scales = "free")

