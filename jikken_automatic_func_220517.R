# 学生実験用のコード.
# Shinichiro Tanimae
# 2022-05-18

library(tidyverse)
library(lubridate)
library(readxl)
library(stringi)

# ファイルパスを取り出す. ------------------------------------------------------
files = tibble(fpath = dir("~/Lab_Data/学生実験/2022データ/", full = T))

# 標準誤差計算用の関数.
# 
se = function (x, na.rm = FALSE ) { 
  # 標準誤差の関数 
  n = length ( ! is.na (x)) 
  sd (x, na.rm) / sqrt (n -1 ) 
}

# エクセルファイルのシートごとに読み込むための関数. 

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
           "light" = matches("光(環境|条件)"),
           "remarks" = matches("備考")) |> 
    mutate(light = as.character(stri_trans_nfkd(light))) |> 
    mutate(light = case_when(str_detect(light, pattern = "[アAa]") ~ "アルミホイル",
                             str_detect(light, pattern = "6") ~ "6ネット",
                             str_detect(light, pattern = "4") ~ "4ネット",
                             str_detect(light, pattern = "2") ~ "2ネット",
                             str_detect(light, pattern = "0") ~ "0ネット")) |> 
    mutate(across(c(han, sample, light), as.factor)) |> 
    drop_na(han)
  
  # 6 班の 2 日目の班名が D 班になっているので変える.
  if(str_detect(file, pattern = "13_6班")){
    df = df |> mutate(han = factor(6)) 
  }
  df |> 
    mutate(mgl = ifelse(han == 2 & mgl < 2, mgl*10, mgl),
           mgl = ifelse(han == 2 & mgl > 50, mgl/10, mgl))
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
           "light" = matches("光環境"),
           "remarks" = matches("備考"),
           "ppfd" = matches("光量子量")) |> 
    mutate(light = as.character(stri_trans_nfkd(light))) |> 
    mutate(light = case_when(str_detect(light, pattern = "[アAa]") ~ "アルミホイル",
                             str_detect(light, pattern = "6") ~ "6ネット",
                             str_detect(light, pattern = "4") ~ "4ネット",
                             str_detect(light, pattern = "2") ~ "2ネット",
                             str_detect(light, pattern = "0") ~ "0ネット")) |> 
    mutate(across(c(han, sample), as.factor)) 
  
  # A 班の記入ミスを書き換え.
  if(str_detect(file, pattern = "A班5月13")){
    df = df |> mutate(light = ifelse(row_number() > 18, "2ネット", light))
  }
  
  N = df |> drop_na(light) |> pull(light) |> unique() |> length()
  
  # アルミホイル使用時の光量子量を書いていない班のデータを書き換え.
  if(N != 5){
    df = df |> 
      add_row(light = c("アルミホイル"),
              ppfd = 0) |> 
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
    fill(c("han", "light", "sample"), .direction = "down") |> 
    mutate(light = as.factor(light)) |> 
    group_by(han, light, sample) |> 
    summarise(ppfd_mean = mean(ppfd),
              # ppfd_se = se(ppfd),
              .groups = "drop")
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
           "gww" = matches("湿重量"),
           "vol" = matches("瓶")) |> 
    mutate(across(c(han, sample, species), as.factor),
           gww =  str_remove_all(gww, pattern = "[a-z]"),
           vol = str_remove_all(vol, pattern = "[a-z]")) |> 
    mutate(across(c(gww, vol), as.double))
  
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

# 一括作業用の関数.

func_all = function(file){
  
  s1 = sheet1_func(file)
  s2 = sheet2_func(file)
  s3 = sheet3_func(file)
  
  s1 |> 
    left_join(s2, by = c("han", "sample", "light")) |> 
    left_join(s3, by = c("han", "sample"))
}


mgldata = files |> mutate(data = map(fpath, sheet1_func)) |> unnest() |> select(-fpath)
mgldata = mgldata |> select(-c(remarks))
lightdata = files |> mutate(data = map(fpath, sheet2_func)) |> unnest() |> select(-fpath)
seaweeddata = files |> mutate(data = map(fpath, sheet3_func)) |> unnest() |> select(-fpath)
