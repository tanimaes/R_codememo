# オオグソクムシ, 水温別酸素消費量.
# Shinichiro Tanimae
# 2022-05-31

library(tidyverse)
library(lubridate)
library(readxl)
library(stringi)
library(showtext)

Sys.setlocale("LC_TIME", "en_US.UTF-8") # システムロケールはアメリカ英語に設定
font_add_google("Noto Sans JP","notosans")
ggpubr::theme_pubr(base_family = "notosans") |> theme_set()
showtext_auto()

theme_me = function() { 
  theme(axis.text = element_text(size = 60),
        title = element_text(size = 60),
        text = element_text(size = 60),
        legend.title = element_text(size = 60),
        legend.text = element_text(size = 60),
        legend.position = c(0,1),
        legend.justification = c(0,1),
        legend.direction = "horizontal",
        legend.background = element_blank(),
        legend.spacing.x = unit(1, 'mm'),
        legend.spacing.y = unit(1, 'mm'),
        legend.box.margin =  margin(c(0, 0, 0, 0)),
        legend.key.width = unit(1.1, 'cm'))
}

# ------------------------------------------------------------------------------
# 水温と代謝速度の関係性.
df1 = read_xlsx("~/field_survey_tanimae/Bathynomus_MO2_analysis2/temp_mo2_220512.xlsx",
                skip = 0,
                col_names = paste0("col", 1:25)) |> 
  mutate(cname = c("date", "temp", "A_id", "B_id", "A_weight", "B_weight",
                   "A_O2", "A_O2edit1", "A_O2edit2",
                   "B_O2", "B_O2edit1", "B_O2edit2"),
         .before = col1) |> 
  select(-col1) |> 
  pivot_longer(col2:col25, names_to = "name",values_to = "val") |>
  pivot_wider(names_from = cname, values_from = val)

df1 = df1 |> 
  mutate(across(everything(), ~stri_trans_general(.x, "Fullwidth-Halfwidth"))) |>
  select(-c(name, date)) |> 
  pivot_longer(cols = matches("[A, B]"),
               names_to = c("Exp", ".value"),
               names_sep = "_") |> 
  drop_na(O2)

df2 = df1 |> 
  mutate(across(c(temp, weight, O2, O2edit1, O2edit2), ~ as.double(.x))) |> 
  mutate(mg_h_kg = O2edit1*60*1000)

xlabel = "'Temperature'~'('~degree*'C)'"
ylabel = "'MO'[2]~'(mg'~'hr'^{-1}~'kg'^{-1}~')'"

df2 |>
  ggplot(aes(x = temp, y = mg_h_kg)) +
  geom_jitter(width = 0.1,height = 0, size = 2) +
  geom_smooth(formula = y ~ x, method = "glm", 
              method.args = list(family = "Gamma"),
              size = 2, color = "turquoise4", fill = "turquoise4", alpha = 0.3) 

# 頻度論的なモデリング.
# glm, gam 検討.

m0 = glm(mg_h_kg ~ 1, data = df2, family = Gamma(link = "log")) # 帰無モデル
m1 = lm(mg_h_kg ~ temp, data = df2) # 一般化線形.
m2 = glm(mg_h_kg ~ temp, data = df2, family = Gamma(link = "log")) # 一般化線形.

AIC(m0, m1, m2) # 検討したモデルの中から一番低いのを選ぶ.

summary(m2) # model summary

# 期待値と信頼区間の算出.
dset = expand_grid(temp = seq(min(df2$temp), max(df2$temp), by = 0.1))
hat = predict(m2, interval="confidence", newdata = dset, level=0.95)
se = predict(m2, se.fit = T, newdata = dset)$se.fit
dset = dset |> mutate(hat, se) |> 
  mutate(l95 = hat - 1.96 * se,
         u95 = hat + 1.96 * se) |> 
  mutate(across(c(hat, l95, u95), exp))

g1 = df2 |> 
  ggplot() + 
  geom_point(aes(x = temp, y = mg_h_kg), size = 3) +
  geom_line(aes(x = temp, y = hat), color = "turquoise4", size = 2, data = dset) +
  geom_ribbon(aes(x = temp, ymin = l95, ymax = u95), fill = "turquoise4", alpha = 0.3, data = dset) +
  # ggtitle("水温とオオグソクムシの代謝速度の関係") +
  scale_x_continuous(name = parse(text = xlabel),
                     breaks = c(6, 9, 12, 15)) +
  scale_y_continuous(name = parse(text = ylabel),
                     breaks = seq(0, 70, by = 10),
                     limits = c(0, 70)) +
  ggpubr::theme_pubr() +
  theme_me()

# ggsave(plot = g1, "~/field_survey_tanimae/Bathynomus_MO2_analysis2/out1.png",
#        width = 1500, height = 2000, units = "px")

# ------------------------------------------------------------------------------
ts1 = read_xlsx("~/field_survey_tanimae/Bathynomus_MO2_analysis2/NEWSDA.xlsx",
                skip = 0,
                sheet = 1,
                col_names = paste0("col", 1:154)) |> 
  mutate(cname = c("date", "A1", "B1", "A2", "B2", "A3", "B3", "A4", "B4"),
         .before = col1) |> 
  select(-col1) |> 
  pivot_longer(col2:col154, names_to = "name",values_to = "val") |>
  pivot_wider(names_from = cname, values_from = val)

ts1 |> print(n = Inf)

ts1 = ts1 |> 
  pivot_longer(A1:B4, names_to = "Exp",
               values_to = "val") |> 
  mutate(val = ifelse(val == 0, NA, val))

g2 = ts1 |> 
  ggplot() +
  geom_point(aes(x = date, y = val, color = Exp), size = 2) +
  geom_line(aes(x = date, y = val, color = Exp, group = Exp)) +
  scale_color_viridis_d(end = 0.8) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40") +
  scale_x_continuous(name = "経過時間 (hr)",
                     breaks = seq(0, 80, by = 20),
                     limits = c(-25, 80)) +
  scale_y_continuous(name = parse(text = ylabel),
                     breaks = seq(0, 110, by = 20),
                     limits = c(0, 110)) +
  facet_wrap(vars(Exp), ncol = 4) +
  ggpubr::theme_pubr() +
  theme_me() +
  theme(legend.position = "none")

# ggsave(plot = g2, "~/field_survey_tanimae/Bathynomus_MO2_analysis2/out2.png",
#        width = 4000, height = 2000, units = "px")

g3 = ts1 |>
  filter(between(date, -12,12)) |>
  ggplot(aes(x = date, y = val)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40") +
  geom_point(aes(color = Exp), size = 2) +
  # geom_text(aes(x = 2, y = 115, label = "← 給餌"), size = 18, check_overlap = T) +
  # geom_line(aes(x = date, y = val, color = Exp, group = Exp)) +
  scale_color_viridis_d(end = 0.8) +
  scale_x_continuous(name = "経過時間 (hr)",
                     breaks = seq(-12, 12, by = 4),
                     limits = c(-12, 12)) +
  scale_y_continuous(name = parse(text = ylabel),
                     breaks = seq(0, 110, by = 20), 
                     limits = c(0, 115)) +
  # facet_wrap(vars(Exp), ncol = 5) +
  ggpubr::theme_pubr() +
  theme_me() +
  theme(legend.position = "none")

# ggsave(plot = g3, "~/field_survey_tanimae/Bathynomus_MO2_analysis2/out3.png",
#        width = 2000, height = 2000, units = "px")

# 箱ひげ.

g4 = ts1 |>
  # filter(between(date, -12,12)) |> 
  ggplot(aes(x = date, y = val)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40") +
  geom_boxplot(aes(color = as.factor(date >= 0), group = date)) +
  # geom_text(aes(x = 10, y = 115, label = "← 給餌"), size = 18, check_overlap = T) +
  # geom_line(aes(x = date, y = val, color = Exp, group = Exp)) +
  scale_color_viridis_d(end = 0.6) +
  scale_x_continuous(name = "経過時間 (hr)",
                     breaks = seq(-24, 168, by = 24),
                     limits = c(-24, 168)) +
  scale_y_continuous(name = parse(text = ylabel),
                     breaks = seq(0, 110, by = 20), 
                     limits = c(0, 115)) +
  # facet_wrap(vars(Exp), ncol = 5) +
  ggpubr::theme_pubr() +
  theme_me() +
  theme(legend.position = "none")

# ggsave(plot = g4, "~/field_survey_tanimae/Bathynomus_MO2_analysis2/out4.png",
#        width = 4000, height = 2000, units = "px")

# point-range.

g5 = ts1 |>
  # filter(between(date, -12,12)) |> 
  group_by(date) |> 
  summarise(mean = mean(val, na.rm = T),
            sd = sd(val, na.rm = T),
            n = n(),
            .groups = "drop") |> 
  mutate(col = ifelse(date < 0, "給餌前", "給餌後")) |> 
  ggplot() +
  geom_pointrange(aes(x = date, y = mean,
                      ymin = mean - sd,
                      ymax = mean + sd,
                      color = col),
                  position = position_dodge(width = 1)) +
  scale_color_viridis_d(end = 0.6) +
  scale_x_continuous(name = "経過時間 (hr)",
                     breaks = seq(-24, 168, by = 24),
                     limits = c(-24, 168)) +
  scale_y_continuous(name = parse(text = ylabel),
                     breaks = seq(0, 80, by = 20), 
                     limits = c(-10, 90)) +
  # facet_wrap(vars(Exp), ncol = 5) +
  ggpubr::theme_pubr() +
  theme_me() +
  theme(legend.title = element_blank(),
        legend.position = c(1,1),
        legend.justification = c(1,1))

# ggsave(plot = g5, "~/field_survey_tanimae/Bathynomus_MO2_analysis2/out5.png",
#        width = 4000, height = 2000, units = "px")

ts2 = ts1 |>
  group_by(date) |> 
  summarise(val = mean(val), .groups = "drop")

ts_aft = ts2 |> filter(date >= 0) |> mutate(col = "給餌後")
ts_bef = ts2 |> filter(date < 0) |> mutate(col = "給餌前")

# 作図.
ts_aft |> 
  mutate(H = date%%24) |> 
  ggplot() +
  geom_line(aes(H, val))

ts_aft |> 
  ggplot() +
  geom_line(aes(date, val))

# ARIMA ------------------------------------------------------------------------
#対数差分系列の可視化
ts_aft %>% 
  mutate(log_diff = c(NA, diff(log(val)))) %>% 
  ggplot()+
  geom_line(aes(x = date, y = log_diff))

#48期までの自己相関コレログラム
acf(diff(log(ts_aft$val)),lag.max = 72*2)

#auto.arima
mod = auto.arima(log(ts_aft$val), 
                 ic = "aic",
                 stepwise = F,
                 parallel = TRUE,
                 num.cores = 4)

#予測する測定回数.
plus = 300
pdate = seq(max(ts_aft$date)+1.25, by = 1.25, length.out = plus)

pred = forecast(mod, plus, level = c(50, 95)) |> 
  as_tibble() |> 
  rename(val =  `Point Forecast`,
         l95 = `Lo 95`,
         u95 = `Hi 95`,
         l50 = `Lo 50`,
         u50 = `Hi 50`) |> 
  mutate(across(c(val, l95, u95, l50, u50), ~ exp(.x))) |> 
  mutate(date = pdate,
         col = "ARIMA モデルによる予測")

arima = bind_rows(ts_bef, ts_aft, pred) |> 
  mutate(col = as.factor(col),
         grp = "A")

g6 = arima %>% 
  ggplot(aes(x = date))+
  geom_line(aes(y = val, color = col, group = grp), size = 1)+
  geom_ribbon(aes(ymax = u95, ymin = l95), fill = "purple4", alpha = 0.2) +
  geom_ribbon(aes(ymax = u50, ymin = l50), fill = "purple4", alpha = 0.2) +
  geom_hline(yintercept = 12, linetype = "dashed") +
  scale_color_viridis_d(end = 0.6) +
  # scale_x_continuous(name = "経過時間 (hr)",
  #                    breaks = c(seq(-24, 168, by = 24), 504),
  #                    limits = c(-24, 550)) +
  scale_y_continuous(name = parse(text = ylabel),
                     breaks = seq(0, 60, by = 10), 
                     limits = c(0, 60)) +
  ggpubr::theme_pubr() +
  theme_me() +
  theme(legend.title = element_blank(),
        legend.position = c(1,1),
        legend.justification = c(1,1))

# ggsave(plot = g6, "~/field_survey_tanimae/Bathynomus_MO2_analysis2/out6.png",
#        width = 4000, height = 2000, units = "px")


