# Seaweed species occurring sympatrically with Sargassums.
# Shinichiro Tanimae
# 2022-02-07

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
# quadrat = read_rds("~/Lab_Data/tanimaes/seaweed_data/rds_write_out/quadrat_writeout_arikawa_2021.rds")
# plaster = read_rds("~/Lab_Data/tanimaes/seaweed_data/rds_write_out/plaster_writeout_arikawa_2021.rds")
# sediment = read_rds("~/Lab_Data/tanimaes/seaweed_data/rds_write_out/sediment_writeout_arikawa_2021.rds")
# temperature = read_rds("~/Lab_Data/tanimaes/seaweed_data/rds_write_out/temp_writeout_arikawa_2021.rds")
# jmadata = read_rds("~/Lab_Data/tanimaes/seaweed_data/info/arikawa_JMA_2021.rds")
# 
# W = as.character(-pi/2)
# E = as.character(pi/2)
# station_info = read_csv("~/Lab_Data/tanimaes/seaweed_data/info/station_info.csv") |> 
#   mutate(station = as.factor(station)) |> 
#   mutate(facing = as.double(recode(facing, N = "0", W = W, E = E)))

# 調べるホンダワラ科の種.
sargassum = "ノコギリモク"

# データフレームの整形.
noko = species |>
  filter(str_detect(species_j, pattern = sargassum)) |> 
  rename(existence_noko = existence) |> 
  select(date, station, existence_noko)

other = species |>
  select(date, station, species_j, type, existence) |> 
  filter(str_detect(species_j, pattern = sargassum, negate = T)) |> 
  rename(existence_other = existence)

other = other |> 
  group_by(species_j) |> 
  mutate(count = sum(existence_other)) |> 
  filter(count > 9) |> # 10 回以上出現した種のみ調べる.
  select(-count)

dateinfo = noko |> 
  filter(existence_noko == 1) |> 
  nest_by(date) |> 
  summarise(date = date, .groups = "drop")

alldata = full_join(noko, other, by = c("date", "station")) |> 
  right_join(dateinfo, by = "date") |> 
  mutate(existence_other = ifelse(existence_other == 1, "exist", "absent")) |> 
  mutate(species_j = as.factor(species_j),
         existence_other = as.factor(existence_other))

# ベイズモデルの定義. ----------------------------------------------------------
# m1 = bf(existence_noko ~ existence_other + species_j) + bernoulli(link = "logit")
m2 = bf(existence_noko ~ existence_other * species_j) + bernoulli(link = "logit")

# ベイズモデルの事前分布の確認.ここでは確認のみ.
get_prior(m2, data = alldata)

# 事前分布は自分で設定.
prior = c(prior("student_t(3, 0, 1)", class = "b"),
          prior("student_t(3, 0, 1)", class = "Intercept"))

ctrl = list(adapt_delta = 0.99,
            max_treedepth = 10)
chains = 4
cores = 4
iterations = 2000
seed = 2021

bout = brm(m2,
           data = alldata,
           control = ctrl,
           chains = chains,
           cores = iterations,
           seed = seed,
           prior = prior,
           file = "noko_sympa",
           file_refit = "always")

summary(bout)

# 事後予測チェック (Posterior Predictive Check)
pp_check(bout, ndraws = 50,resp = "value")

loo(bout)

# y = bout$data$existence_noko
# yrep = posterior_predict(bout, ndraws = 200, resp = "value") # 予測値をサンプリング.
# ppc_stat(y, yrep, stat = "mean")
# ppc_stat(y, yrep, stat = "var")
# emmeans::emmeans(bout, pairwise ~ species_j)

df3 = bout |>
  spread_draws(`b_*.*`, regex = T) |>
  # gather() |> nest_by(key) |> print(n = Inf)
  rename_with(.fn = \(x) {sprintf("exist_sp%02d", 1:length(x))},
              .cols = matches("^b_existence")) |> 
  rename_with(.fn = \(x) {sprintf("absent_sp%02d", 1:length(x))},
              .cols = matches("^b_I|b_species"))

df4 = df3 |> 
  pivot_longer(cols = -c(".chain", ".draw", ".iteration"),
               names_to = "name", values_to = "value") |> 
  separate(name, into = c("existence", "sp"), sep = "_")
  
df4 = df4 |> 
  pivot_wider(names_from = existence, values_from = value) |> 
  mutate(diff = exist - absent)


df4 = df4 |> 
  group_by(sp) |> 
  mutate(mean = mean(diff)) |> 
  group_by(mean) |> 
  nest_by() |>
  ungroup() |> 
  arrange(-mean) |> 
  mutate(rank = row_number()) |>
  # filter(rank < 11) |> 
  unnest()

df4 = df4 |> 
  pivot_longer(absent:exist,
               names_to = "existence", values_to = "value")

df4 |> 
  ggplot() +
  geom_histogram(aes(value, fill = existence),
                 bins = 100, binwidth = 0.5) +
  facet_wrap("sp", scales = "free") +
  scale_fill_viridis_d(end = 0.7) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank())

# 違う作図.
# モデルの期待値
pdata =  bout$data |> add_epred_draws(bout, seed = 2020) 
pdata = pdata |> 
  group_by(existence_other, species_j) |> 
  mean_hdci(.epred)

# モデル予測値.
qdata = bout$data |> add_predicted_draws(bout, seed = 2020)
qdata = qdata |> group_by(existence_other, species_j) |>
  tidybayes::mean_hdci(.prediction)

sp_type = selected_sp |> 
  ungroup() |> 
  select(species_j, type) |> 
  mutate(type = recode(type, EB = "Encrusting Brown"),
         type = recode(type, EC = "Encrusting Coralline"),
         type = recode(type, FR = "Filamentous Red"),
         type = recode(type, FT = "Freshly Turf"),
         type = recode(type, GC = "Geniculate Coralline"),
         type = recode(type, LBB = "Large Branching Brown"),
         type = recode(type, LBG = "Large Branching Green"),
         type = recode(type, LBR = "Large Branching Red"),
         type = recode(type, LFR = "Large Foliose Red"))

pdata |> 
  # filter(existence_other == "exist") |> 
  left_join(sp_type, by = "species_j") |>
  mutate(species_j = recode(species_j, "マギレソゾ" = "Laurencia-sp1")) |> 
  ggplot() +
  geom_pointrange(aes(x = reorder(species_j, .epred), y = .epred, 
                      ymin = .lower, ymax = .upper, color = type),
                  size = 2) +
  facet_wrap("existence_other", scales = "free_x",
             ncol = 1) +
  scale_color_viridis_d(end = 0.8) 
  # theme(axis.text.x = element_text(size = 40, angle = 45,hjust = 1, vjust = 1),
  #       axis.text.y = element_text(size = 40),
  #       axis.title.x = element_blank(),
  #       title = element_text(size = 40),
  #       legend.title = element_blank(),
  #       legend.text = element_text(size = 40),
  #       legend.position = c(0.01,1),
  #       legend.justification = c(0,1),
  #       legend.background = element_blank())

qdata |> 
  # filter(existence_other == "exist") |>
  left_join(sp_type, by = "species_j") |>
  mutate(species_j = recode(species_j, "マギレソゾ" = "Laurencia-sp1")) |> 
  ggplot() +
  geom_pointrange(aes(x = reorder(species_j, .prediction), y = .prediction, 
                      ymin = .lower, ymax = .upper, color = type),
                  size = 2) +
  facet_wrap("existence_other", scales = "free_x",
             ncol = 1) +
  scale_color_viridis_d(end = 0.8) 
# theme(axis.text.x = element_text(size = 40, angle = 45,hjust = 1, vjust = 1),
#       axis.text.y = element_text(size = 40),
#       axis.title.x = element_blank(),
#       title = element_text(size = 40),
#       legend.title = element_blank(),
#       legend.text = element_text(size = 40),
#       legend.position = c(0.01,1),
#       legend.justification = c(0,1),
#       legend.background = element_blank())

