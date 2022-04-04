# ムラサキダコの出現確率を解析.
library(tidyverse)
library(lubridate)
library(readxl)
library(mgcv)

# データはWEB上から集めた.
df = read_xlsx("~/Lab_Data/tanimaes/share_files/example_data/murasakidako_sns_data.xlsx") |> 
  mutate(date = ymd(date),
         ex = 1) |> 
  mutate(year = year(date),
         month = month(date)) |> 
  group_by(year, month) |> 
  summarise(n = n(), .groups = "drop") |> 
  group_by(year) |> 
  mutate(scl = n/sum(n)) |> 
  ungroup()

df |> 
  ggplot() +
  geom_point(aes(month, scl))

# ここからモデリング. ----------------------------------------------------------

m0 = gam(scl ~ s(month, bs = "cp", k = 5), data = df, family = gaussian(link = "log"))
m1 = gam(scl ~ s(month, bs = "cp", k = 5), data = df, family = gaussian(link = "log")) 
m2 = gam(scl ~ s(month, bs = "cc", k = 5), data = df, family = gaussian(link = "log"))

AIC(m0, m1, m2) # 検討したモデルの中から一番低いのを選ぶ.

summary(m2) # model summary

# 期待値と信頼区間の算出.
# # 区間算出のためのデータ.
dset = df |> expand(month = seq(min(month), max(month), by = 0.1))

# 期待値.
pdata = dset |> 
  bind_cols(predict(m2, 
                    newdata = dset,
                    interval = 'confidence',
                    lvel = 0.95,
                    se.fit = T) |>
              as_tibble())

df |> 
  ggplot() + 
  geom_point(aes(x = month, y = scl), size = 3) +
  geom_line(aes(x = month, y = exp(fit)),
            data = pdata, color = "turquoise4", size = 2) +
  geom_ribbon(aes(x = month, ymin = exp(fit - se.fit), ymax = exp(fit + se.fit)),
              data = pdata, fill = "turquoise4", alpha = 0.3) +
  scale_x_continuous(name = "月", breaks = 1:12) +
  scale_y_continuous(name = "ムラサキダコの出現確率") +
  ggpubr::theme_pubr()
