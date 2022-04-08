# Analysis of MO2 (oxygen metabolic volume) of Bathynomus doederleinii.
# Data are provided by Shogo Tanaka.
# 2021-03-13, Shinichiro Tanimae.
library(tidyverse)
library(lubridate)
library(readxl)
library(mgcv)

# データの整備.
df = read_xlsx("~/Lab_Data/tanimaes/share_files/mo_model.xlsx",
               skip = 0, col_names = paste0("col", 1:34)) |> 
  mutate(across(everything(), ~ as.character(.x)))

df = df |>
  mutate(cname = c("date", "expA_id", "expB_id", "expA_weight", "expB_weight",
                   "t1_expA", "t1_expB", "t2_expA", "t2_expB", "t3_expA", "t3_expB"),
         .before = col1) |> 
  select(-c(col1, col2)) |> 
  pivot_longer(col3:col34, names_to = "name",values_to = "val") |>
  pivot_wider(names_from = cname, values_from = val)

df = df |> 
  mutate(date = as.numeric(date)) |> 
  mutate(date = as.Date.numeric(date, origin = "1900-01-01")) |>
  mutate(across(matches("_[w, e]"), ~ as.double(.x))) |> 
  pivot_longer(matches("_exp[A, B]"), names_to = "type", values_to = "MO2val") |>
  separate(type, into = c("trial", "exp_id")) 

df1 = df |> 
  mutate(elapsed_day = as.double(date - ymd("2021-07-08"))) |> 
  filter(MO2val > 0)
  
#確認の図.
df1 |> 
  ggplot() +
  geom_point(aes(elapsed_day, MO2val)) +
  geom_smooth(aes(elapsed_day, MO2val), formula = y~log(x), method = "lm")

# ここからモデリング. ----------------------------------------------------------
# 頻度論的なモデリングの練習.

# glm, gam 検討.
m0 = glm(MO2val ~ 1, data = df1, family = Gamma(link = "log")) # 帰無モデル
m1 = glm(MO2val ~ elapsed_day, data = df1, family = Gamma(link = "log")) # 一般化線形.
m2 = gam(MO2val ~ s(elapsed_day, k = 4), data = df1, family = Gamma(link = "log")) # 作業モデル. k = 4
m3 = gam(MO2val ~ s(elapsed_day, k = 5), data = df1, family = Gamma(link = "log")) # k = 5.
m4 = glm(MO2val ~ elapsed_day*trial, data = df1, family = Gamma(link = "log")) # 交互作用.

AIC(m0, m1, m2, m3, m4) # 検討したモデルの中から一番低いのを選ぶ.

summary(m2) # model summary

# 期待値と信頼区間の算出.
hat = predict(m2, interval="confidence", level=0.95)
se = predict(m2, se.fit = T)$se.fit
df2 = df1 |> mutate(hat, se) |> 
  mutate(l95 = hat - 1.96 * se,
         u95 = hat + 1.96 * se) |> 
  mutate(across(c(hat, l95, u95), exp))

df2 |> 
  ggplot() + 
  geom_point(aes(x = elapsed_day, y = MO2val), size = 3) +
  geom_line(aes(x = elapsed_day, y = hat), color = "turquoise4", size = 2) +
  geom_ribbon(aes(x = elapsed_day, ymin = l95, ymax = u95), fill = "turquoise4", alpha = 0.3) +
  ggtitle("オオグソクムシの絶食に対する代謝速度変化") +
  scale_x_continuous("絶食日数", ) +
  scale_y_continuous(expression("MO"[2]~"(mg"~"H"^{-1}~"Kg"^{-1}~")")) +
  ggpubr::theme_pubr()
