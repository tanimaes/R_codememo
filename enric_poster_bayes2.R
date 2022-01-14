# analyze seaweed and environmental data by using bayes model.
# Shinichiro Tanimae
# 2021-11-10

# Packages.
library(tidyverse)
library(lubridate)
library(bayesplot)
library(brms)
library(ggpubr)
library(lemon)
library(patchwork)
library(tidybayes)
library(showtext)
library(rstanarm)

Sys.setlocale("LC_TIME", "en_US.UTF-8") # システムロケールはアメリカ英語に設定
font_add_google("Noto Sans JP","notosans")
theme_pubr(base_family = "notosans") |> theme_set()
showtext_auto()

# data. ------------------------------------------------------------------------
species = read_rds("~/Lab_Data/tanimaes/seaweed_data/rds_write_out/species_writeout_arikawa_2021.rds")
quadrat = read_rds("~/Lab_Data/tanimaes/seaweed_data/rds_write_out/quadrat_writeout_arikawa_2021.rds")
plaster = read_rds("~/Lab_Data/tanimaes/seaweed_data/rds_write_out/plaster_writeout_arikawa_2021.rds")
sediment = read_rds("~/Lab_Data/tanimaes/seaweed_data/rds_write_out/sediment_writeout_arikawa_2021.rds")
temperature = read_rds("~/Lab_Data/tanimaes/seaweed_data/rds_write_out/temp_writeout_arikawa_2021.rds")
jmadata = read_rds("~/Lab_Data/tanimaes/seaweed_data/info/arikawa_JMA_2021.rds")

W = as.character(-pi/2)
E = as.character(pi/2)
station_info = read_csv("~/Lab_Data/tanimaes/seaweed_data/info/station_info.csv") |> 
  mutate(station = as.factor(station)) |> 
  mutate(facing = as.double(recode(facing, N = "0", W = W, E = E)))

species = species |> 
  group_by(date, month,  station, phylum) |> 
  summarise(n = sum(existence)) |> 
  ungroup()

plaster = plaster |> 
  group_by(station) |> 
  summarise(pla = mean(pla_100g_day)) |> 
  ungroup()

temperature = temperature |>
  mutate(D = as.Date(datetime),
         month = month(datetime)) |> 
  group_by(D, month, station) |> 
  summarise(across(temp, list(min = min, max = max))) |> 
  ungroup() |> 
  mutate(temp_range = temp_max - temp_min) |> 
  group_by(month, station) |> 
  summarise(temp_range_mean = mean(temp_range)) |> 
  ungroup()

edata = temperature |> 
  full_join(plaster, by = "station") |> 
  full_join(station_info, by = "station")

alldata = full_join(edata, species, by = c("month", "station")) |> 
  drop_na() |> 
  rename(value = n,
         temp = temp_range_mean)

alldata |> 
  ggplot() +
  geom_line(aes(pla, value, color = as.factor(month)), height = 0, width = 0.1) +
  facet_wrap("phylum")

# ベイズモデルの定義. ----------------------------------------------------------
# ベイズモデルの定義.

mgcv::t2()
bmodel = bf(value ~ s(pla, bs = "ts", by = phylum, k = 4) + s(temp, bs = "ts", by = phylum, k = 4) + phylum) + 
  poisson(link = "log")
# bmodel2 = bf(temp ~ month) + Gamma(link = "log")
# bmodel = bmodel1 + bmodel2

# ベイズモデルの事前分布の確認.ここでは確認のみ.

get_prior(bmodel, data = alldata)

# 事前分布は自分で設定.
# resp : 応答変数.

prior = c(prior(normal(0, 5), class = b),
          prior(normal(0, 5), class = Intercept),
          prior(student_t(3, 0, 2.5), class = sds))
# prior = c(prior(normal(0, 5), class = b, coef = pla, resp = value),
#           prior(normal(0, 5), class = b, coef = temp, resp = value),
#           prior(normal(0, 5), class = b, coef = month, resp = temp),
#           prior(normal(0, 5), class = b, resp = temp),
#           prior(normal(0, 5), class = Intercept, resp = temp),
#           prior(normal(0, 5), class = b, resp = value),
#           prior(normal(0, 5), class = Intercept, resp = value),
#           prior(exponential(1), class = shape, resp = temp))

# サンプラーの設定.

ctrl = list(adapt_delta = 0.99, # ステップの大きさ. 1に近い方が小さく,細かくウォークする. 
            max_treedepth = 10) # 調べる深さ.
chains = 4
cores = 4 # CPU のコアの数.
iterations = 2000
seed = 2021 # 始まりの位置.
# parallel::detectCores(all.tests = F, logical = T) # CPU のコアの数を調べる.

# 事後分布からサンプルを抽出する.

# 当てはめ直し：file_refit = "always" もわたす

bout = brm(bmodel,
           data = alldata,
           control = ctrl,
           chains = chains,
           cores = iterations,
           seed = seed,
           prior = prior,
           file = "alldata_pla_temp",
           file_refit = "always")

# write_rds(bout, "~/seaweed_vegetation_tanimae_2021/result_box/Rhodophyta_pla_temp.rds")
# bout = read_rds("~/seaweed_vegetation_tanimae_2021/result_box/Rhodophyta_pla_temp.rds")

summary(bout)

# 事後予測チェック (Posterior Predictive Check)
pp_check(bout, ndraws = 50, resp = "value")
# pp_check(bout, ndraws = 150, resp = "temp")

# 統計量チェック, データと予測値のヒストグラムの確認.

y = bout$data$value
yrep = posterior_predict(bout, ndraws = 200, resp = "value") # 予測値をサンプリング.

ppc_hist(y, yrep[1:8,], binwidth = 1)
ppc_stat(y, yrep, stat = "mean")
ppc_stat(y, yrep, stat = "var")

ppc_stat_grouped(y = y,
                 yrep = yrep,
                 group = bout$data$phylum,
                 stat = "mean")

ppc_stat_grouped(y = y,
                 yrep = yrep,
                 group = bout$data$phylum,
                 stat = "var")

ppc_dens_overlay_grouped(y = y,
                         yrep = yrep[1:50,],
                         group = bout$data$phylum) 



#################################################################################


bout$data$temp |> unique() |> sort()

pdata = bout$data |> 
  tidyr::expand(pla = seq(10, 20, length = 11),
                temp = seq(0.5, 2.5, length = 11),
                phylum)

pdata = pdata |> tidybayes::add_epred_draws(bout, seed = 2020) # モデルの期待値

pdata = pdata |> 
  group_by(pla, temp, phylum) |> 
  mean_hdci(.epred)

# モデル予測値.

qdata2 = bout$data |> 
  tidyr::expand(pla = seq(10, 20, length = 11),
                temp = seq(0.5, 2.5, length = 11),
                phylum) |> 
  tidybayes::add_predicted_draws(bout, seed = 2020)

qdata2 = qdata2 |> group_by(pla, temp, phylum) |>
  tidybayes::mean_hdci(.prediction)

qdata2 = qdata2 |> mutate(temp2 = factor(temp),
                          pla2 = factor(pla))
pdata2 = pdata |> mutate(temp2 = factor(temp),
                         pla2 = factor(pla))

# 作図. ------------------------------------------------------------------------

label_pla =  "'Plaster dissolution ( g 100g'^{-1}~'day'^{-1}~' )'"
label_sp = "'Species Richness'"
label_temp = "'Diurnal temperature range ('~degree*C~')'"

# a1 = 
g1 = ggplot() +
  # geom_ribbon(aes(x = pla,
  #                 ymin = .lower,
  #                 ymax = .upper,
  #                 fill = (temp2)),
  #             alpha = 0.5,
  #             data = qdata2) +
  # geom_ribbon(aes(x = pla,
  #                 ymin = .lower,
  #                 ymax = .upper,
  #                 fill = (temp2)),
  #             alpha = 0.5,
#             data = pdata2) +
geom_line(aes(x = pla, 
              y = .epred,
              color = temp2),
          data = pdata2,
          size = 2) +
  geom_jitter(aes(x = pla, y = value),
              width = 0.1,
              height = 0,
              data = bout$data) +
  scale_x_continuous(name = parse(text = label_pla)) +
  scale_y_continuous(name = parse(text = label_sp)) +
  scale_color_viridis_d(end = 0.9, name = parse(text = label_temp)) +
  scale_fill_viridis_d(end = 0.9) +
  theme_pubr() +
  facet_wrap("phylum") +
  labs(tag = "1") +
  theme(legend.position = c(0, 1),
        legend.justification = c(0, 1),
        axis.title = element_text(size = 20),
        legend.background = element_blank())


g2 = ggplot() +
  # geom_ribbon(aes(x = pla,
  #                 ymin = .lower,
  #                 ymax = .upper,
  #                 fill = (temp2)),
  #             alpha = 0.5,
  #             data = qdata2) +
  # geom_ribbon(aes(x = pla,
  #                 ymin = .lower,
  #                 ymax = .upper,
  #                 fill = (temp2)),
  #             alpha = 0.5,
#             data = pdata2) +
geom_line(aes(x = temp, 
              y = .epred,
              color = pla2),
          data = pdata2,
          size = 2) +
  geom_jitter(aes(x = temp, y = value),
              width = 0.1,
              height = 0,
              data = bout$data) +
  scale_x_continuous(name = parse(text = label_temp)) +
  scale_y_continuous(name = parse(text = label_sp)) +
  scale_color_viridis_d(end = 0.9, name = parse(text = label_pla)) +
  scale_fill_viridis_d(end = 0.9) +
  theme_pubr() +
  facet_wrap("phylum") +
  labs(tag = "2") +
  theme(legend.position = c(0, 1),
        legend.justification = c(0, 1),
        axis.title = element_text(size = 20),
        legend.background = element_blank())

g0 = g1/g2

# ggsave(plot = g0, "~/seaweed_vegetation_tanimae_2021/result_box/enric_out02_211111.pdf",
#        width = 4000, height = 4000, units = "px")


COL = "gray85"

p1 = ggplot() +
  geom_tile(aes(pla, temp, fill = .epred), 
            data = pdata2 |> filter(str_detect(phylum, "^Ch"))) +
  scale_fill_viridis_c(end = 0.8) +
  scale_x_continuous(breaks = seq(10, 20, by = 2)) + 
  theme_pubr() +
  labs(fill  = "Predicted species richness") +
  ylab(label = expression("Diurnal temperature range ("~degree*"C"~")")) +
  facet_wrap("phylum") +
  guides(fill = guide_colourbar(title.position="top", title.hjust = 0.5)) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size = 18),
        axis.text.y = element_text(size = 20),
        axis.text.x = element_text(size = 20),
        axis.line = element_line(color = COL),
        axis.ticks = element_line(size = 1, color = COL),
        strip.background = element_rect(color = COL, fill = COL),
        strip.text = element_text(size = 20),
        legend.position = "top",
        legend.title = element_text(size = 15),
        legend.key.width = unit(40, "points"),
        legend.text = element_text(size = 15))

p2 = ggplot() +
  geom_tile(aes(pla, temp, fill = .epred), 
            data = pdata2 |> filter(str_detect(phylum, "^Oc"))) +
  scale_fill_viridis_c(end = 0.8) +
  scale_x_continuous(breaks = seq(10, 20, by = 2)) +
  theme_pubr() +
  labs(fill  = "") +
  facet_wrap("phylum") +
  guides(fill = guide_colourbar(title.position="top", title.hjust = 0.5)) +
  theme(axis.title = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 20),
        axis.line = element_line(color = COL),
        axis.ticks = element_line(size = 1,color = COL),
        strip.background = element_rect(color = COL, fill = COL),
        strip.text = element_text(size = 20),
        legend.position = "top",
        legend.title = element_text(size = 15),
        legend.key.width = unit(40, "points"),
        legend.text = element_text(size = 15))

p3 = ggplot() +
  geom_tile(aes(pla, temp, fill = .epred), 
            data = pdata2 |> filter(str_detect(phylum, "^Rh"))) +
  scale_fill_viridis_c(end = 0.8) +
  scale_x_continuous(breaks = seq(10, 20, by = 2)) +
  theme_pubr() +
  labs(fill  = "") + 
  facet_wrap("phylum") +
  xlab(label = expression("Plaster dissolution ( g 100g"^{-1}~"day"^{-1}~ ")")) +
  guides(fill = guide_colourbar(title.position="top", title.hjust = 0.5)) +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_text(size = 18),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 20),
        axis.line = element_line(color = COL),
        axis.ticks = element_line(size = 1,color = COL),
        strip.background = element_rect(color = COL, fill = COL),
        strip.text = element_text(size = 20),
        legend.position = "top",
        legend.title = element_text(size = 15),
        legend.key.width = unit(40, "points"),
        legend.text = element_text(size = 15))

p0 = p1+p3+p2

library(magick)

# ggsave(plot = p0, filename = "~/seaweed_vegetation_tanimae_2021/result_box/enric_out03_211111.pdf",
#        width = 3999, height = 1800, units = "px")
# image_read_pdf("~/seaweed_vegetation_tanimae_2021/result_box/enric_out03_211111.pdf") |>
#   image_transparent("white") |>
#   image_write("~/seaweed_vegetation_tanimae_2021/result_box/enric_out03_211111.png")




