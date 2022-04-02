# 水温データを用いて時系列解析.
# 2022-04-01
# Tanimae
library(tidyverse)
library(lubridate)
library(readxl)
library(showtext)

Sys.setlocale("LC_TIME", "en_US.UTF-8") # システムロケールはアメリカ英語に設定

font_add_google("Noto Sans JP","notosans")
# １）theme_set() をつかってデフォルトのフォントをかえる
# ２）ggplot() の theme() からフォントの指定をはずす。
theme_pubr(base_family = "notosans") |> theme_set()
showtext_auto()


# data. ------------------------------------------------------------------------
# 有川の水温データ(ガラモ場底層のDOロガーのデータを使用).
temp_ari = tibble(fpath = dir("~/Lab_Data/kawatea/Oxygen/",
                              pattern = "arikawagaramo_0m_2[12][01]",
                              full = T)) |> 
  mutate(data = map(fpath, read_onset)) |> 
  unnest() |> 
  select(datetime, temperature) |> 
  mutate(location = "arikawa",
         station = as.factor("garamo")) |> 
  rename(temp = temperature)

period = read_csv("~/Lab_Data/kawatea/period_info_220329.csv") |> 
  mutate(datetime = map2(start_date, end_date, \(x,y){
    seq(x, y, by = "10 min")
  })) |> unnest() |> 
  filter(location == "arikawagaramo") |> 
  select(datetime)

temp_ari = temp_ari |> 
  inner_join(period, by = c("datetime"))

# 七目郷の水温データ.
temp_na = read_rds("~/Lab_Data/Naname/rds_write_out/temp_writeout_naname_2021.rds")

# 気象庁のデータ.
jma = read_rds("~/Lab_Data/Naname/rds_write_out/JMA_writeout_arikawa.rds") |> 
  filter(between(datetime, 
                 ymd_hms("2021-01-28 00:00:00"), 
                 ymd_hms("2022-03-25 00:00:00")))

# 1日の降水量が 30 mm 以上の日を探す.
rain_extract = jma |> 
  mutate(date = as.Date(datetime)) |> 
  group_by(date) |> 
  summarise(rain_total = sum(rain, na.rm = T), .groups = "drop") |> 
  filter(rain_total > 30) |>  
  mutate(id = row_number())

# 雨の日を起点として, 3日間の日付データを作製.
after_3day = rain_extract |> 
  mutate(test = map(date, function(x){
    seq(x, x+2, by = "1 day")
  })) |> 
  unnest() |> 
  select(test, id)

df_aft3 = bind_rows(temp_ari, temp_na) |> 
  filter(between(datetime, 
                 ymd_hms("2021-10-28 00:00:00"), 
                 ymd_hms("2022-03-25 00:00:00"))) |> 
  mutate(location = str_to_sentence(location),
         test = as.Date(datetime)) |> 
  right_join(after_3day, by = "test") |> 
  drop_na()

# 水深data.
# depth = tibble(fpath = dir("~/Lab_Data/kawatea/Depth/",
#                            pattern = "arikawagaramo_0m_2[12]",
#                            full = T)) |> 
#   mutate(data = map(fpath, read_onset)) |> 
#   unnest()
# 
# depth = depth |> 
#   select(-fpath)


# 作図.-------------------------------------------------------------------------
# 降水後の水温変動を見る.
df_aft3 |> 
  ggplot() +
  # geom_col(aes(test, rain_total)) +
  geom_line(aes(x = datetime, y = temp, 
                color = interaction(location, station),
                group = interaction(location, station))) +
  # annotate(geom = "text", x = , y = 5, rain_t) +
  scale_color_viridis_d(end = 0.7) +
  scale_x_datetime(date_breaks = "1 day", date_labels = "%d") +
  facet_grid(.~id, scales = "free") +
  # ylim(0, 20) +
  theme(legend.title = element_blank())

# 時系列解析. ------------------------------------------------------------------

# 七目郷ステーション１．
test_N1 = temp_na |> filter(station == 1) |> select(-station, -location)

#  有川郷ガラモ場.
test_Ag = temp_ari |> select(-station, -location)

##### 七目郷の解析. 
test_N1 |> 
  filter(between(datetime, 
                 ymd_hms("2021-11-26 00:00:00"), 
                 ymd_hms("2021-12-13 00:00:00"))) |>
  ggplot() +
  geom_line(aes(datetime, temp)) +
  scale_x_datetime(date_breaks = "1 day", date_labels = "%d")

test_N1 = test_N1 |> 
  filter(between(datetime, 
                 ymd_hms("2021-11-26 00:00:00"), 
                 ymd_hms("2021-12-13 00:00:00"))) |>
  mutate(date = as.Date(datetime),
         time = hour(datetime) + minute(datetime)/60,
         n = row_number()) 

time_series_N1 = test_N1 |> pull(temp)
time_series_N1 = ts(as.numeric(time_series_N1), frequency = 144) # 必ずnumeric型.
# frequencyで周期の情報をインプット.

# stl() では, Loess による平滑化が行われます。
# locally weighted scatterplot smoother (局所重み付き散布図平滑化）.
stl_N1 = stl(time_series_N1, s.window="periodic")

# STL分解結果のデータ
stl_N1_o = rowSums(stl_N1$time.series) #観測データ（STL分解前の元のデータ）＝トレンド＋季節性＋残差
stl_N1_t = stl_N1$time.series[,2]      #トレンド（Trend）
stl_N1_s = stl_N1$time.series[,1]      #季節性（Seasonal）
stl_N1_r = stl_N1$time.series[,3]      #残差（Remainder）

out_N1 = tibble(trend = stl_N1_t,
                seasonal = stl_N1_s,
                remainder = stl_N1_r) |> 
  mutate(n = row_number())

out_N1 = out_N1 |> 
  left_join(test_N1, by = "n") |>
  left_join(jma, by = "datetime") |> 
  select(datetime, temp, seasonal, trend, remainder, rain) 

out_N1 = out_N1 |> 
  pivot_longer(temp:rain, names_to = "key", values_to = "val") |> 
  mutate(key = factor(key, 
                      levels = c("temp", "seasonal", "trend", "remainder", "rain"),
                      labels = c("水温の観測値", "日周成分", "トレンド成分", "残差", "降水量")))

sr1 = out_N1 |>
  mutate(date = as.Date(datetime)) |> 
  mutate(A = ifelse(date %in% after_3day$test, T, F)) |>
  left_join(depth, by = "datetime") |> 
  ggplot() +
  geom_line(aes(datetime, val, color = A, group = key), size = 1) +
  facet_grid(key~., scales = "free") +
  scale_color_viridis_d(end = 0.6, option = "E", direction = -1) +
  scale_x_datetime(date_breaks = "1 day", date_labels = "%d") +
  theme_pubr() +
  theme(legend.position = "none",
        axis.title = element_blank())



##### 有川郷の解析. 
test_Ag |> 
  filter(between(datetime, 
                 ymd_hms("2021-11-26 00:00:00"), 
                 ymd_hms("2021-12-13 00:00:00"))) |>
  ggplot() +
  geom_line(aes(datetime, temp)) +
  scale_x_datetime(date_breaks = "1 day", date_labels = "%d")

test_Ag = test_Ag |> 
  filter(between(datetime, 
                 ymd_hms("2021-11-26 00:00:00"), 
                 ymd_hms("2021-12-13 00:00:00"))) |> 
  mutate(date = as.Date(datetime),
         time = hour(datetime) + minute(datetime)/60,
         n = row_number()) 

time_series_Ag = test_Ag |> pull(temp)
time_series_Ag = ts(as.numeric(time_series_Ag), frequency = 144) # 必ずnumeric型.
# frequencyで周期の情報をインプット.

stl_Ag = stl(time_series_Ag, s.window="periodic")

# STL分解結果のデータ
stl_Ag_o = rowSums(stl_Ag$time.series) #観測データ（STL分解前の元のデータ）＝トレンド＋季節性＋残差
stl_Ag_t = stl_Ag$time.series[,2]      #トレンド（Trend）
stl_Ag_s = stl_Ag$time.series[,1]      #季節性（Seasonal）
stl_Ag_r = stl_Ag$time.series[,3]      #残差（Remainder）

out_Ag = tibble(trend = stl_Ag_t,
                seasonal = stl_Ag_s,
                remainder = stl_Ag_r) |> 
  mutate(n = row_number())

out_Ag = out_Ag |> 
  left_join(test_Ag, by = "n") |>
  left_join(jma, by = "datetime") |> 
  select(datetime, temp, seasonal, trend, remainder, rain) 

out_Ag = out_Ag |> 
  pivot_longer(temp:rain, names_to = "key", values_to = "val") |> 
  mutate(key = factor(key, 
                      levels = c("temp", "seasonal", "trend", "remainder", "rain"),
                      labels = c("水温の観測値", "日周成分", "トレンド成分", "残差", "降水量")))

sr2 = out_Ag |>
  mutate(date = as.Date(datetime)) |> 
  mutate(A = ifelse(date %in% after_3day$test, T, F)) |>
  left_join(depth, by = "datetime") |> 
  filter(between(datetime, 
                 ymd_hms("2021-10-28 00:00:00"), 
                 ymd_hms("2021-12-25 00:00:00"))) |>
  ggplot() +
  geom_line(aes(datetime, val, color = A, group = key), size = 1) +
  facet_grid(key~., scales = "free") +
  scale_color_viridis_d(end = 0.6, option = "E", direction = -1) +
  scale_x_datetime(date_breaks = "1 day", date_labels = "%d") +
  theme_pubr() +
  theme(legend.position = "none",
        axis.title = element_blank())


sr1+sr2


# GAM (2022-04-01). ------------------------------------------------------------
# stl() の loess 以外にも試したいので, GAM を構築.
# 七目郷の水温データ.
temp_na = read_rds("~/Lab_Data/Naname/rds_write_out/temp_writeout_naname_2021.rds")

temp_na = temp_na |> 
  filter(near(as.numeric(station), 1, tol = 0.1)) |> 
  mutate(time = hour(datetime) + minute(datetime)/60,
         date = as.factor(as.Date(datetime)),
         tau = as.numeric(datetime)) |> 
  mutate(tau = (tau - min(tau))/3600)

###################################################################### modeling.
library(mgcv)

gam1 = gam(temp ~ s(tau, bs = "ps", k = 200) + s(time, bs = "cp", k = 4), data = temp_na)
# gam2 = gam(temp ~ s(tau, bs = "ps", k = 400) + s(time, bs = "cp", k = 4), data = temp_na)
# gam3 = gam(temp ~ s(tau, bs = "ps", k = 800) + s(time, bs = "cp", k = 4), data = temp_na)

# AIC(gam1, gam2, gam3)
# 
# temp_na |>
#   mutate(res1 = residuals(gam1),
#          res2 = residuals(gam3)) |>
#   ggplot() +
#   geom_line(aes(x = datetime, y = res1, color = "res1")) +
#   geom_line(aes(x = datetime, y = res2, color = "res2"))
#   
# model summary
# summary(gam3)

hat = predict(gam1)
se = predict(gam1, se.fit = T)$se.fit
df2 = temp_na |> mutate(hat, se) |> 
  mutate(l95 = hat - 1.96 * se,
         u95 = hat + 1.96 * se)

# GAM確認.
df2 |> 
  ggplot() + 
  geom_point(aes(x = datetime, y = temp),
             color = "dodgerblue4", alpha = 0.5, size = 1) +
  geom_line(aes(x = datetime, y = hat),
            color = "black") +
  ggpubr::theme_pubr()


############################################################## 手動で成分に分解.
# trend 計算用データ.
for_trend = temp_na |>
  expand(tau = tau,
         time = 9.9526377041) # これは何？

# season 計算用データ.
for_season = temp_na |>
  expand(time = time,
         tau = 0)

# 各成分を算出.
trend_season_resid = for_trend |>
  bind_cols(predict(gam1,
                    newdata = for_trend,
                    # type = "response",
                    interval = 'confidence',
                    lvel = 0.95,
                    se.fit = T) |>
              as_tibble()) |> 
  mutate(datetime = temp_na$datetime,
         temp = temp_na$temp,
         time = temp_na$time) |> # もとの time に置き換え.
  rename(trend = fit,
         se.trend = se.fit) |> 
  mutate(diff = temp - trend) |>
  nest_by(time) |> 
  bind_cols(predict(gam1,
                    newdata = for_season,
                    # type = "response",
                    interval = 'confidence',
                    lvel = 0.95,
                    se.fit = T) |>
              as_tibble()) |> 
  unnest() |> 
  ungroup() |> 
  arrange(datetime) |> 
  mutate(season = fit - mean(fit)) |> 
  mutate(resid = diff - season) |> 
  mutate(value_check = trend + season + resid) |> 
  select(datetime, tau, time, temp, trend, season, resid, value_check)

trend_season_resid |> 
  mutate(diff = temp - trend) |> 
  ggplot() +
  geom_point(aes(datetime, diff)) +
  geom_smooth(aes(datetime, diff), formula = y ~ x) 


# 作図.
trend_season_resid |> 
  pivot_longer(temp:value_check, names_to = "key", values_to = "value") |> 
  mutate(key = factor(key, 
                      levels = c("temp", "value_check", "trend", "season", "resid"),
                      labels = c("Temperature", "value_check", "Trend", "Season", "Residuals"))) |> 
  ggplot() +
  geom_line(aes(datetime, value, group = key, color = key)) +
  facet_grid(key~., scales = "free") +
  scale_color_viridis_d(end = 0.7)

trend_season_resid |> 
  mutate(c = sum(resid))
