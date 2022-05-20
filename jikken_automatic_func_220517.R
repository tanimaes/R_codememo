# 学生実験用のコード.
# Shinichiro Tanimae
# 2022-05-18

library(tidyverse)
library(lubridate)
library(readxl)
library(stringi)
library(minpack.lm)
library(ggrepel)

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

# データの読み込み. ------------------------------------------------------------
# alldata = files |> mutate(data = map(fpath, func_all)) |> unnest()
# 
# alldata |> select(-remarks) |> print(n= Inf)
# 
# # 作図.
# alldata |> 
#   ggplot() +
#   geom_point(aes(x = time, y = mgl, color = species, group = species)) +
#   facet_grid(han ~ light, scales = "free")
# 
# alldata = alldata |> select(-c(fpath, remarks))
# 
# # ちょっと遊びます.
# # [DO] と [時間＊班＊サンプル＊光条件]の関係性を線形で表現.
# m1 = lm(mgl ~ time * han * sample * light, data = alldata)
# 
# summary(m1)
# 
# valuehat = predict(m1)
# valuese = predict(m1, se.fit = T)$se.fit
# dset = alldata |> 
#   mutate(valuehat,
#          valuese) |> 
#   mutate(l95 = valuehat - 1.96 * valuese,
#          u95 = valuehat + 1.96 * valuese) |> 
#   mutate(across(c(valuehat, l95, u95)))
# 
# # 作図.
# dset |> 
#   ggplot(aes(x = time, group = sample)) +
#   geom_point(aes(y = mgl, color = species), size = 3) +
#   geom_line(aes(y = valuehat, color = species),size = 1) +
#   facet_wrap(han ~ light, ncol = 5)
# 
# 授業用データ. ----------------------------------------------------------------
mgldata = files |>
  mutate(data = map(fpath, sheet1_func)) |> 
  unnest() |> 
  select(-c(fpath, remarks))

lightdata = files |> 
  mutate(data = map(fpath, sheet2_func)) |> 
  unnest() |> 
  select(-fpath)

seaweeddata = files |> 
  mutate(data = map(fpath, sheet3_func)) |> 
  unnest() |>
  select(-fpath)

df0 = mgldata |> 
  left_join(seaweeddata, by = c("han", "sample")) |> 
  left_join(lightdata, by = c("han", "light", "sample"))

# 線形モデルへの当てはめ. ------------------------------------------------------

# [DO] と [時間]の関係性を線形で表現.
fit_lm = function(df){
  lm(mgl ~ time, data = df)
} 

df1 = df0 |> group_nest(han, species, light)
df1 = df1 |> mutate(mout = map(data, fit_lm))

# 統計量.
df_tosave = df1 |> 
  mutate(stats = map(mout, broom::glance)) |> 
  unnest(stats) |>
  select(han, species, light, adj.r.squared, statistic, df, df.residual, p.value)

# 推定結果.
df_tosave2 = df1 |> 
  mutate(cfs = map(mout, broom::tidy)) |> 
  unnest(cfs) |> 
  select(han, species, light, term, estimate, std.error, statistic, p.value)

# 期待値の算出.
get_fit = function(data, mout){
  data |> mutate(fit = predict(mout))
}

df2 = df1 |> mutate(fit = map2(data, mout, get_fit))

# 作図.
df2 |> 
  select(han, species, light, fit) |> 
  unnest(fit) |> 
  ggplot() +
  geom_point(aes(time, mgl, color = species), size = 3) +
  geom_line(aes(time, fit, group = species), color = "black", size = 1) +
  facet_grid(light~han)

# 傾き(変化速度)の抽出.
get_slope = function (model){
  coef(model)[2]
}

df3 = df2 |> 
  mutate(slope = map_dbl(mout, get_slope)) |>
  unnest(data) |> 
  mutate(rate = slope*vol/gww) 

# 非線形モデルへの当てはめ. ----------------------------------------------------

# 光合成-光曲線の式.
pecurve = function(pmax, alpha, rd, ppfd){
  pmax*(1 - exp(-alpha / pmax*ppfd)) - rd
}

# 光合成光曲線の当てはめ.
fit_nls = function(data){
  nlsLM(rate ~ pecurve(pmax = pmax, alpha = alpha, rd = rd, ppfd = ppfd_mean),
        data = data, start = START)
}

# 初期値.
START = list(pmax = 5, alpha = 0.1, rd = 0.1)

df3 = df3 |> 
  select(-c(fit, mout)) |> 
  group_nest(species) |> 
  mutate(peout = map(data, fit_nls)) |> 
  mutate(pefit = map2(data, peout, get_fit))

# 光合成-光曲線の図.
g1 = df3 |> 
  unnest(pefit) |> 
  ggplot() +
  geom_point(aes(x = ppfd_mean, y = rate, color = species), size = 3) +
  geom_line(aes(x = ppfd_mean, y = fit, group = species, color = species), size = 1) +
  scale_color_viridis_d(end = 0.7) +
  facet_wrap("species")

g1

# 係数の抽出. ------------------------------------------------------------------
# 光飽和点.
ik_fn = function(model){
  cfs = coef(model)
  cfs["pmax"]/cfs["alpha"]
}

# 光補償点.
ic_fn = function(model){
  cfs = coef(model)
  cfs["pmax"]/cfs["alpha"]*log(cfs["pmax"]/(cfs["pmax"] - cfs["rd"]))
}

ik_ic = df3 |>
  mutate(ik = map_dbl(peout, ik_fn),
         ic = map_dbl(peout, ic_fn)) |> 
  select(species, ik, ic)

ik_ic = ik_ic |> 
  mutate(label_ic = paste("光補償点", sprintf("%.0f",ic)),
         label_ik = paste("光飽和点", sprintf("%.0f",ik)))

coef = df3 |> 
  mutate(coef = map(peout, coef)) |> 
  unnest(coef) |> 
  mutate(label = c("pmax", "alpha", "rd", "pmax", "alpha", "rd")) |> 
  pivot_wider(names_from = label, values_from = coef) |> 
  select(species, pmax, alpha, rd)

# cdata = coef |> left_join(ik_ic, by = "species")

g2 = g1 + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  # geom_point(aes(x = ic, y = 0), data = ik_ic, size = 3) +
  # geom_point(aes(x = ik, y = 0), data = ik_ic, size = 3) +
  geom_text_repel(aes(x = ic, y = 0, label = label_ic),
                  data = ik_ic, box.padding = 1, nudge_x = 100, nudge_y = -1) +
  geom_text_repel(aes(x = ic, y = 0, label = label_ik),
            data = ik_ic, box.padding = 1,
            nudge_x = 450, nudge_y = -1, min.segment.length = 10000) +
  ggpubr::theme_pubr()

g2





















