library(tidyverse)
library(lubridate)
library(bayesplot)
library(brms)
library(ggpubr)
library(patchwork)
library(ggvegan)
library(showtext)
library(tidybayes)
# library(rstanarm)

# データの読み込み. ------------------------------------------------------------
data = read_rds("~/Labs_work_tanimae_2020/test.rds") |> 
  dplyr::select(Time, ST, Hour, Month, egg, lat, lon) |> 
  group_by(ST, lon, lat, Month) |> 
  summarise(count = n()) |> # 月, ST ごとの受信回数.
  ungroup()

# 受信しなかった場所のデータもある方が良い.
ST_info = data |> 
  group_by(ST, lon, lat) |> 
  summarise() |> 
  ungroup() |> 
  bind_rows(tibble(ST = c("V8", "V10"),
                   lon = c(129.327, 129.3275),
                   lat = c(32.9986, 33.002))) |> 
  expand_grid(Month = 5:9)

data = full_join(data, ST_info, by = c("ST", "Month", "lon", "lat")) |> 
  mutate(count = ifelse(is.na(count), 0, count)) |> print(n = Inf)

# とりあえず作図. 
g1 = data |>
  ggplot() +geom_point(aes(lon, lat, color = count, size = count))  +
  scale_y_continuous(limits = c(32.997, 33.002)) +
  scale_color_viridis_c(end = 0.8, option = "D") +
  facet_wrap(vars(Month))

# Bayesian GAM. ----------------------------------------------------------------
# ベイズ一般化加法モデルの定義.
bmodel = bf(count ~ s(lon, bs = "ts", k = 5) + s(lat, bs = "ts", k = 5)) + 
  poisson(link = "log")

# ベイズモデルの事前分布の確認.ここでは確認のみ.
get_prior(bmodel, data = data)

# 事前分布は自分で設定.
prior = c(prior(student_t(3, 2.2, 2.5), class = Intercept),
          prior(student_t(3, 0, 2.5), class = sds))

# サンプラーの設定.
ctrl = list(adapt_delta = 0.99, # ステップの大きさ. 1に近い方が小さく,細かくウォークする. 
            max_treedepth = 10) # 調べる深さ.
chains = 4 # マルコフ連鎖の数
cores = 4 # CPU のコアの数.
iterations = 2000 # サンプリング数.
seed = 2021 # 始まりの位置.
# parallel::detectCores(all.tests = F, logical = T) # CPU のコアの数を調べる.

# 事後分布からサンプルを抽出する.
# モデルの当てはめ直し：file_refit = "always" もわたす
bout = brm(bmodel,
           data = data,
           control = ctrl,
           chains = chains,
           cores = iterations,
           seed = seed,
           prior = prior,
           file_refit = "always",
           file = "iseebi_lon_lat") # rds ファイルの名前.


# 毎回 MCMC を走らせるのはしんどいので, 書き出した rds ファイルを読みましょう.
# bout = read_rds("~/イセエビ/iseebi_lon_lat.rds")

# 推定結果の確認. RHat は 1 が良い.
summary(bout)

# 事後予測チェック (Posterior Predictive Check)
pp_check(bout, ndraws = 50, resp = "value")
# pp_check(bout, ndraws = 150, resp = "temp")

# 統計量チェック, データと予測値のヒストグラムの確認.

y = bout$data$count
yrep = posterior_predict(bout, ndraws = 200, resp = "value") # 予測値をサンプリング.

ppc_stat(y, yrep, stat = "mean")
ppc_stat(y, yrep, stat = "var")

#################################################################################

# bout$data$lat |> unique() |> sort()

# モデルの期待値
pdata = bout$data |> 
  tidyr::expand(lon = seq(129.3260, 129.3325, length = 14), # length を細かくすれば geom_tile も細かくなります.
                lat = seq(32.9975, 33.0025, length = 11))

pdata = pdata |> tidybayes::add_epred_draws(bout, seed = 2020) 

pdata = pdata |> 
  group_by(lon, lat) |> 
  mean_hdci(.epred)

# モデル予測値.
qdata2 = bout$data |> 
  tidyr::expand(lon = seq(129.3260, 129.3325, length = 14),
                lat = seq(32.9975, 33.0025, length = 11)) |> 
  tidybayes::add_predicted_draws(bout, seed = 2020) 

qdata2 = qdata2 |> group_by(lon, lat) |>
  tidybayes::mean_hdci(.prediction)

qdata2 = qdata2 |> mutate(lat2 = factor(lat),
                          lon2 = factor(lon))

pdata2 = pdata |> mutate(lat2 = factor(lat),
                         lon2 = factor(lon))

# 作図. ------------------------------------------------------------------------

label_lon =  "'Longitude ('~degree~')'"
label_count = "'受信回数'"
label_lat = "'Latitude ('~degree~')'"

g2 = ggplot() +
  geom_line(aes(x = lon, y = .epred, color = lat2),
            data = pdata2, size = 1) +
  geom_point(aes(x = lon, y = count),data = bout$data) +
  scale_x_continuous(name = parse(text = label_lon)) +
  scale_y_continuous(name = parse(text = label_count)) +
  scale_color_viridis_d(end = 0.9, option = "D", name = parse(text = label_lat)) +
  # geom_ribbon(aes(x = lon, # 予測区間の描画. 煩雑になるので消去.
  #                 ymin = .lower,
  #                 ymax = .upper,
  #                 fill = lat2),
  #             alpha = 0.1, data = qdata2) +
  # geom_ribbon(aes(x = lon, # 信用区間の描画. 煩雑になるので消去.
  #                 ymin = .lower,
  #                 ymax = .upper,
  #                 fill = lat2),
  #             alpha = 0.1, data = pdata2) +
#   scale_fill_viridis_d(end = 0.9, option = "D") +
theme_pubr() +
  theme(legend.position = c(0, 1),
        legend.justification = c(0, 1),
        legend.key.size = unit(0, 'lines'),
        axis.title = element_text(size = 20),
        legend.background = element_blank())


g3 = ggplot() +
  geom_line(aes(x = lat, y = .epred, color = lon2),
            data = pdata2, size = 1) +
  geom_point(aes(x = lat, y = count), data = bout$data) +
  scale_x_continuous(name = parse(text = label_lat)) +
  scale_y_continuous(name = parse(text = label_count)) +
  scale_color_viridis_d(end = 0.9, name = parse(text = label_lon), option = "D") +
  # geom_ribbon(aes(x = lat, # 予測区間の描画. 煩雑になるので消去.
  #                 ymin = .lower,
  #                 ymax = .upper,
  #                 fill = lon2),
  #             alpha = 0.1, data = qdata2) +
  # geom_ribbon(aes(x = lat, # 信用区間の描画. 煩雑になるので消去.
  #                 ymin = .lower,
  #                 ymax = .upper,
  #                 fill = lon2),
  #             alpha = 0.1, data = pdata2) +
# scale_fill_viridis_d(end = 0.9, option = "D") +
theme_pubr() +
  theme(legend.position = c(0, 1),
        legend.justification = c(0, 1),
        legend.key.size = unit(0, 'lines'),
        axis.title = element_text(size = 20),
        legend.background = element_blank())

g4 = ggplot() +
  geom_tile(aes(lon, lat, fill = .epred), 
            data = pdata2) +
  geom_point(aes(lon, lat), 
             data = ST_info, color = "white") +
  scale_fill_viridis_c(end = 0.8, option = "B") +
  theme_pubr() +
  scale_x_continuous(name = parse(text = label_lon)) +
  scale_y_continuous(name = parse(text = label_lat)) +
  guides(fill = guide_colourbar(title.position="top", title.hjust = 0.5)) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size = 18),
        axis.text.y = element_text(size = 20),
        axis.text.x = element_text(size = 20),
        strip.text = element_text(size = 20),
        legend.position = "top",
        legend.title = element_text(size = 15),
        legend.key.width = unit(40, "points"),
        legend.text = element_text(size = 15))

c1 = (g2+g3)/(g1+g4)

c1

# g2 と g3 はモデルの期待値の線が重なって見にくい. x軸, z軸方向に山形の関係性なので,
# g4 を使用した方が解釈しやすいと思います.

# 画像の保存方法. 一旦 pdf に書き出し, そのあとで png にした方が鮮明で, かつバイト数が少ないはず.
# ggsave(plot = c1, "~/Labs_work_tanimae_2020/out.pdf", width = 4000, height = 4000, units = "px")
# magick::image_read_pdf("~/Labs_work_tanimae_2020/out.pdf") |> 
#   magick::image_write("~/Labs_work_tanimae_2020/out.png")







