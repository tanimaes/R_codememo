library(tidyverse)
library(lubridate)
library(readxl)
library(stringi)

# ファイルパスを取り出す. ------------------------------------------------------
files = dir("~/Lab_Data/学生実験/2022データ/", full = T)

# データの書き込み方が班によって様々なので、１つ１つ整形していく. --------------

# 列名書き換える関数を用意.
sheet1_func = function(df){
  df |> 
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
    mutate(across(c(han, sample, light_c), as.factor))
}

sheet2_func = function(df){
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
    mutate(across(c(han, sample, light_c), as.factor))
  
  A = df |> drop_na(light_c)
  N = unique(A$light_c) |> length()
  
  if(N != 5){
    df = df |> 
      add_row(light_c = c("アルミホイル"),
              light = 0) |> 
      fill(c(han, sample))
  }
  df
}

sheet3_func = function(df){
  df |> 
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
}

# 1班. -------------------------------------------------------------------------
# 1日目.
file = files |>  str_subset("1班 1")

han1_day1_sheet1 = read_xlsx(file, sheet = 1) |> sheet1_func()
han1_day1_sheet2 = read_xlsx(file, sheet = 2) |> sheet2_func()
han1_day1_sheet3 = read_xlsx(file, sheet = 3) |> sheet3_func()

# 2日目.
file = files |>  str_subset("1班2")

han1_day2_sheet1 = read_xlsx(file, sheet = 1) |> sheet1_func() |> 
  drop_na(han)
han1_day2_sheet2 = read_xlsx(file, sheet = 2) |> sheet2_func()
han1_day2_sheet3 = read_xlsx(file, sheet = 3) |> sheet3_func()

# 2(A)班. -------------------------------------------------------------------------
# 1日目.
file = files |>  str_subset("A班5月12")

han2_day1_sheet1 = read_xlsx(file, sheet = 1) |> sheet1_func()
han2_day1_sheet2 = read_xlsx(file, sheet = 2) |> sheet2_func()
han2_day1_sheet3 = read_xlsx(file, sheet = 3) |> sheet3_func()

# 2日目.
file = files |>  str_subset("A班5月13")

han2_day2_sheet1 = read_xlsx(file, sheet = 1) |> sheet1_func()
han2_day2_sheet2 = read_xlsx(file, sheet = 2) |> sheet2_func() |> 
  mutate(light_c = ifelse(row_number() > 18, "2ネット", light_c))
han2_day2_sheet3 = read_xlsx(file, sheet = 3) |> sheet3_func()

# C班. -------------------------------------------------------------------------
# 1日目.
file = files |>  str_subset("C班5月12")

hanC_day1_sheet1 = read_xlsx(file, sheet = 1) |> sheet1_func()
hanC_day1_sheet2 = read_xlsx(file, sheet = 2) |> sheet2_func() |> 
  mutate(sample = factor(1))
hanC_day1_sheet3 = read_xlsx(file, sheet = 3) |> sheet3_func() |> 
  filter(datetime == ymd_hms("2022-05-12 00:00:00"))

# 2日目.
file = files |>  str_subset("C班5月13")

hanC_day2_sheet1 = read_xlsx(file, sheet = 1) |> sheet1_func()
hanC_day2_sheet2 = read_xlsx(file, sheet = 2) |> sheet2_func() |> 
  mutate(sample = factor(2))
hanC_day2_sheet3 = read_xlsx(file, sheet = 3) |> sheet3_func() |> 
  filter(datetime == ymd_hms("2022-05-13 00:00:00"))

# 4班. -------------------------------------------------------------------------
# 1日目.
file = files |>  str_subset("4班サンプル番号1")

han4_day1_sheet1 = read_xlsx(file, sheet = 1) |> sheet1_func() 
han4_day1_sheet2 = read_xlsx(file, sheet = 2) |> sheet2_func()
han4_day1_sheet3 = read_xlsx(file, sheet = 3) |> sheet3_func()

# 2日目.
file = files |>  str_subset("4班サンプル番号２")

han4_day2_sheet1 = read_xlsx(file, sheet = 1) |> sheet1_func()
han4_day2_sheet2 = read_xlsx(file, sheet = 2) |> sheet2_func()
han4_day2_sheet3 = read_xlsx(file, sheet = 3) |> sheet3_func()

# 班名が無かった班(ここでは X 班とします). -------------------------------------
file = files |>  str_subset("実験結果.xlsx")

hanX_sheet1 = read_xlsx(file, range = "B3:H163") |> sheet1_func() |> 
  mutate(han = "X")
hanX_sheet2 = read_xlsx(file, range = "J7:L57") |>
  mutate(han = "X") |> 
  sheet2_func()
hanX_sheet3 = read_xlsx(file, range = "J3:N5") |> 
  mutate(han = "X") |> 
  sheet3_func()

# 6班. -------------------------------------------------------------------------
# 1日目.
file = files |>  str_subset("12_6班")

han6_day1_sheet1 = read_xlsx(file, sheet = 1) |> sheet1_func() |> 
  drop_na(han)
han6_day1_sheet2 = read_xlsx(file, sheet = 2) |> sheet2_func() |> 
  fill(c("han", "light_c"), .direction = "down")
han6_day1_sheet3 = read_xlsx(file, sheet = 3) |> sheet3_func()

# 2日目.
file = files |>  str_subset("13_6班")

han6_day2_sheet1 = read_xlsx(file, sheet = 1) |> sheet1_func() |>
  drop_na(han) |> 
  mutate(han = factor(6))
han6_day2_sheet2 = read_xlsx(file, sheet = 2) |> sheet2_func() |> 
  fill(c("han", "light_c", "sample"), .direction = "down")
han6_day2_sheet3 = read_xlsx(file, sheet = 3) |> sheet3_func()

# データをまとめる. ------------------------------------------------------------

mgl_data = bind_rows(han1_day1_sheet1, han1_day2_sheet1,
                    han2_day1_sheet1, han2_day2_sheet1,
                    hanC_day1_sheet1, hanC_day2_sheet1,
                    han4_day1_sheet1, han4_day2_sheet1,
                    hanX_sheet1,
                    han6_day1_sheet1, han6_day2_sheet1) |> 
  select(-remarks)

light_data = bind_rows(han1_day1_sheet2, han1_day2_sheet2,
                      han2_day1_sheet2, han2_day2_sheet2,
                      hanC_day1_sheet2, hanC_day2_sheet2,
                      han4_day1_sheet2, han4_day2_sheet2,
                      hanX_sheet2,
                      han6_day1_sheet2, han6_day2_sheet2) |> 
  select(-remarks)

light_data = light_data |> 
  group_by(han, light_c, sample) |> 
  summarise(light = mean(light), .groups = "drop")

basic_info = bind_rows(han1_day1_sheet3, han1_day2_sheet3,
                       han2_day1_sheet3, han2_day2_sheet3,
                       hanC_day1_sheet3, hanC_day2_sheet3,
                       han4_day1_sheet3, han4_day2_sheet3,
                       hanX_sheet3,
                       han6_day1_sheet3, han6_day2_sheet3)

alldata = mgl_data |> 
  left_join(light_data, by = c("han", "sample", "light_c")) |> 
  left_join(basic_info, by = c("han", "sample"))

# 作図.
alldata |> 
  ggplot() +
  geom_line(aes(x = time, y = mgl, color = light_c, group = light_c)) +
  facet_wrap(vars(han, species), scales = "free")

# 2 班の数値の入力ミスを直す.
alldata = alldata |> 
  mutate(mgl = ifelse(han == 2 & mgl < 2, mgl*10, mgl),
         mgl = ifelse(han == 2 & mgl > 50, mgl/10, mgl))

alldata |> 
  ggplot() +
  geom_line(aes(x = time, y = mgl, color = light_c, group = light_c)) +
  facet_wrap(vars(han, species), scales = "free")


