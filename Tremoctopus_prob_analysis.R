# ムラサキダコの出現確率を解析.
library(tidyverse)
library(lubridate)
library(readxl)
library(mgcv)

# データ.
dako2 = read_xlsx("~/Lab_Data/tanimaes/share_files/example_data/murasakidako_sns_data.xlsx") |> 
  mutate(date=as_date(date)) |> 
  mutate(month=month(date),
         year = year(date)) |> 
  group_by(year, month) |> 
  summarise(n=n(), .groups = "drop")

dako2 = dako2 |>
  group_by(year) |> 
  mutate(scaled_n = n/sum(n)) |> # 年でグループ化し, 月ごとの観測割合を算出. 
  ungroup()

dako2 |> 
  ggplot() +
  geom_point(aes(x = month, y = scaled_n)) 

# 頻度論的にモデリング.
m0 = gam(scaled_n ~ s(month, bs = "cp", k = 5), data = dako2, family = gaussian(link = "log"))
m1 = gam(scaled_n ~ s(month, bs = "cp", k = 5), data = dako2, family = gaussian(link = "log")) 
m2 = gam(scaled_n ~ s(month, bs = "cc", k = 5), data = dako2, family = gaussian(link = "log"))

AIC(m0, m1, m2) # 検討したモデルの中から一番低いのを選ぶ.

summary(m2) # model summary

# 期待値と信頼区間の算出.
dset = dako2 |> expand(month = seq(min(month), max(month), by = 0.05))

pdata1 = dset |> 
  bind_cols(predict(m2, 
                    newdata = dset,
                    interval = 'confidence',
                    lvel = 0.95,
                    se.fit = T) |>
              as_tibble())

g1 = ggplot() + 
  geom_ribbon(aes(x = month, ymin = exp(fit - se.fit), ymax = exp(fit + se.fit)),
              data = pdata1, fill = "turquoise4", alpha = 0.5)+
  geom_line(aes(x = month, y = exp(fit)),
            data = pdata1, color = "black",linetype = "dashed", size = 1) +
  geom_point(aes(x = month, y = scaled_n), 
             data = dako2, size = 3) +
  ylim(0,1) +
  ggpubr::theme_pubr()

# ベイズモデリング. ------------------------------------------------------------
library(tidybayes)
b1 = bf(scaled_n ~ s(month, bs="cp", k = 5)) + hurdle_gamma(link = "log",
                                                            link_shape = "log",
                                                            link_hu = "logit")

get_prior(b1, data = dako2)

prior = c(prior(student_t(3, -2.3, 2.5), class = Intercept),
          prior(beta(1, 1), class = hu),
          prior(student_t(3, 0, 2.5), class = sds),
          prior(gamma(0.01, 0.01), class = shape))

# サンプラーの設定.
ctrl = list(adapt_delta = 0.99, # ステップの大きさ. 1に近い方が小さく,細かくウォークする. 
            max_treedepth = 10) # 調べる深さ.
chains = 4
cores = 4 # CPU のコアの数.
iterations = 2000
seed = 2022 # 始まりの位置.

# 当てはめ直し：file_refit = "always" もわたす
bout = brm(b1,
           data = dako2,
           control = ctrl,
           chains = chains,
           cores = iterations,
           seed = seed,
           prior = prior,
           file = "dako",
           file_refit = "on_change")

loo(bout)
summary(bout)
pp_check(bout, ndraws = 50, resp = "value")

y = bout$data$scaled_n
yrep = posterior_predict(bout, ndraws = 200, resp = "value")
ppc_hist(y, yrep[1:8,], binwidth = 1)
ppc_stat(y, yrep, stat = "mean")
ppc_stat(y, yrep, stat = "var")

pdata2 = dset |> 
  tidybayes::add_epred_draws(bout, seed = 2020) |> 
  group_by(month) |> 
  mean_hdci(.epred)

qdata2 = dset |>  
  tidybayes::add_predicted_draws(bout, seed = 2022) |> 
  group_by(month) |>
  tidybayes::mean_hdci(.prediction)

g2 = ggplot() +
  # geom_ribbon(aes(x = month,
  #                 ymin = .lower,
  #                 ymax = .upper),
  #             fill =  "turquoise4",
  #             alpha = 0.2,
  #             data = qdata2) +
  geom_ribbon(aes(x = month, ymin = .lower, ymax = .upper),
              fill =  "turquoise4", alpha = 0.5, data = pdata2) +
  geom_line(aes(x = month, y = .epred),
            data = pdata2, linetype = "dashed", size = 1) +
  geom_point(aes(x = month, y = scaled_n),
             data = dako2, size = 3) +
  ylim(0, 1) +
  theme_pubr() 

library(patchwork)
g1+g2
