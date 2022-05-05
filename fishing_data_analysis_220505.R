# 釣果データを用いて RDA してみます.
# Shinichiro Tanimae
# 2022-05-05

# データ整形, 可視化用.
library(tidyverse)
library(stringi) # 全角半角変換.
library(patchwork)
library(ggrepel)
library(ggpubr)
library(showtext)

# RDA用.
library(vegan)
library(ggvegan)

# スクレイピング用.
library(rvest)
library(xml2)

Sys.setlocale("LC_TIME", "en_US.UTF-8") # システムロケールはアメリカ英語に設定
font_add_google("Noto Sans JP","notosans")
theme_pubr(base_family = "notosans") |> theme_set()
showtext_auto()

theme_me = function() { 
  theme(axis.text = element_text(size = 60),
        title = element_text(size = 60),
        text = element_text(size = 60),
        legend.title = element_text(size = 40),
        legend.text = element_text(size = 40),
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
# スクレイピング用関数1 (魚種ごと).

get_fish_point01 = function(key, page, id){
  
  Sys.sleep(10)
  URL = str_glue("https://www.point-i.jp/fishing_spot_guides?&free_word=",key,"&page=",page,"&prefecture_id=", id)
  out = read_html(URL)
  
  out1 = out |> 
    html_nodes(xpath = '//*[@class="card__layout--fishing-item-outer"]/span') |> 
    xml_text() |> 
    as_tibble() |> 
    mutate(n = rep(c("sp_title","species_j","size_title","size"), 15)) |> 
    pivot_wider(names_from = n, values_from = value, values_fn = list) |>
    unnest() |> 
    select(species_j, size)
  
  out2 = out |>
    html_nodes(xpath = '//*/span[@class="card__date"]') |> 
    xml_text() |> 
    as_tibble() |> 
    mutate(value = str_remove_all(value, pattern = "[\\s, '\n']")) |> 
    mutate(value = as.Date(value)) |> 
    rename(date = value)
  
  out3 = out |>
    html_nodes(xpath = '//*/h1[@class="card__title card__title--fishing-spot"]') |> 
    xml_text() |> 
    as_tibble() |> 
    mutate(value = str_remove_all(value, pattern = "[\\s, '\n']")) |> 
    rename(location = value)
  
  bind_cols(out1, out2, out3)
  
}

# 例 (注意).
# test = tibble(page = 1:5, key = "カサゴ", id = 42) |>
#   mutate(data = pmap(list(key, page, id), 
#                      get_fish_point)) |>
#   select(data) |>
#   unnest(data)

# ------------------------------------------------------------------------------
# スクレイピング用関数2 (station ごと).

get_fish_point02 = function(spot_id){
  
  sec = sample(x = 5:15, size = 1)
  Sys.sleep(sec)
  
  URL = str_glue("https://www.point-i.jp/fishing_spots/", spot_id)
  out = read_html(URL)
  
  out1 = out |> 
    html_nodes(xpath = '//tbody') |>
    html_table(fill = TRUE)
  
  n1 = out1[[1]] |> names()
  n2 = c("station", "type", "location", "shop", "date", "species_j",
         "size", "weight", "method", "comments")
  names = setNames(n1, n2)
  
  out1[[1]] |> 
    rename(!!!names)
  
}

spot_info = tibble(spot = c("茂木港", "時津八工区", "新長崎漁港", "伊王島", "牧島弁天", 
                     "有喜", "千々石", "長崎港", "大瀬戸地磯", "結の浜", "長与",
                     "小江港", "神の島", "時津七工区", "時津港", "為石港", 
                     "神の島（マリア像）", "樺島", "脇岬港", "高島釣り公園", "深堀", 
                     "相浦川", "尾戸", "江の浦漁港", "福島", "木指", "池下", "木津", 
                     "国﨑半島", "横瀬港", "佐世保川河口", "三重漁港", "南串（京泊）",
                     "子々川", "小浜", "飛子", "網場", "神の島（鼠島）", "崎戸大島",
                     "東浜港", "東大川", "加津佐漁港"),
            id = c(1127, 674, 545, 6433, 1610, 71, 83, 581, 1148, 5520, 3925, 
                   341, 2859, 552, 1440, 1641, 357, 272, 307, 1703, 344, 3254,
                   5879, 1192, 2616, 73, 1931, 9475, 5285, 2839, 7338, 2752, 2869,
                   2986, 72, 5442, 3206, 382, 986, 3482, 5630, 5628))

# 既に読み込んであるので, 下でRDSファイルを読み込んでください.
# 例 (注意).
# df = df |> mutate(data = map(id, get_fish_point02))

spot_info |> print(n = Inf)

# 地点. ------------------------------------------------------------------------

mogi = tibble(long = 129.9153635749726, lat = 32.70623687738632, spot = "茂木港")
hachikoku = tibble(long = 129.8383693424696, lat = 32.84731449061039, spot = "時津八工区")
shinnagasaki = tibble(long = 129.76273726531167, lat = 32.81348550319948, spot = "新長崎漁港")
iojima = tibble(long = 129.7821657616903, lat = 32.703793584891876, spot = "伊王島")
makishima = tibble(long = 129.965811722672, lat = 32.75280526296073, spot = "牧島弁天")
uki = tibble(long = 130.08539712666882, lat = 32.79186359942752, spot = "有喜")
chijiwa = tibble(long = 130.18963213745377, lat = 32.78110330164613, spot = "千々石")
nagasakigyoko = tibble(long = 129.8655064038331, lat = 32.74467008358571, spot =  "長崎港")
oseto = tibble(long = 129.63277961757416, lat = 32.93102986237914, spot = "大瀬戸地磯")
yuinohama = tibble(long = 129.99744215828974, lat = 32.75999373754844, spot = "結の浜")
nagayo = tibble(long = 129.865934845824, lat = 32.84614488892818, spot = "長与")
koe = tibble(long = 129.81160493756988, lat = 32.75672238458309, spot = "小江港")
kaminoshima = tibble(long = 129.8195940446883, lat = 32.72418777847413, spot = "神の島")
nanakoku = tibble(long = 129.84686360804426, lat = 32.83951168442669, spot = "時津七工区")
togitsuko = tibble(long = 129.84779161587724, lat = 32.83386270229517, spot = "時津港")
tameshi = tibble(long = 129.84086842907925, lat = 32.63768592814823, spot = "為石港")
maria = tibble(long = 129.82697052559877, lat = 32.71645046633131, spot = "神の島（マリア像）")
kabashima = tibble(long = 129.78207587005704, lat = 32.56964417259288, spot = "樺島")
wakimisaki = tibble(long = 129.7844175224698, lat = 32.57935098714508, spot = "脇岬港")
takashima = tibble(long = 129.75940675600606, lat = 32.6701574982406, spot = "高島釣り公園")
fukahori = tibble(long = 129.81655615391824, lat = 32.684034723520824, spot = "深堀")
ainoura = tibble(long = 129.65998414190727, lat = 33.17044115356162, spot = "相浦川")
odo = tibble(long = 129.80164858578118, lat = 32.91795820807477, spot = "尾戸")
enoura = tibble(long = 130.03532906360107, lat = 32.76477883221611, spot = "江の浦漁港")
fukushima = tibble(long = 129.82510401615977, lat = 33.361672678116534, spot = "福島")
kisashi = tibble(long = 130.19682599039155, lat = 32.71797951737513, spot = "木指")
ikeshimo = tibble(long = 129.98698524182933, lat = 32.757089717364224, spot = "池下")
kidu = tibble(long = 130.1877412283317, lat = 32.76624792288015, spot = "木津")
kunisaki = tibble(long = 130.13379349135502, lat = 32.6835771032792, spot = "国﨑半島")
yokose = tibble(long = 129.70488196433504, lat = 33.08595494898572, spot = "横瀬港")
sasebogawa = tibble(long = 129.72194554448632, lat = 33.16169028908325, spot = "佐世保川河口")
mie = tibble(long = 129.74786663770905, lat = 32.820159276236936, spot = "三重漁港")
nagushi = tibble(long = 130.1424470044555, lat = 32.681644023378894, spot = "南串（京泊）")
shishigawa = tibble(long = 129.80598029708491, lat = 32.86486310040935, spot = "子々川")
obama = tibble(long = 130.2055019801174, lat = 32.73220809832507, spot = "小浜")
tobiko = tibble(long = 130.1668200832182, lat = 32.697084316063425, spot = "飛子")
aba = tibble(long = 129.94851786984017, lat = 32.755822723429574, spot = "網場")
nezumishima = tibble(long = 129.83509963298388, lat = 32.7200235294155, spot = "神の島（鼠島）")
sakito = tibble(long = 129.64031098930263, lat = 33.041195585109726, spot = "崎戸大島")
higashihama = tibble(long = 129.7434881947802, lat = 33.133498396367585, spot = "東浜港")
higashiookawa = tibble(long = 130.00122186927044, lat = 32.84251398315686, spot = "東大川")
kadusa = tibble(long = 130.16169860577867, lat = 32.62263363021445, spot = "加津佐漁港")

spot_info = bind_rows(mogi, hachikoku, shinnagasaki, iojima, makishima, uki, chijiwa, nagasakigyoko,
                oseto, yuinohama, nagayo, koe, kaminoshima, nanakoku, togitsuko, tameshi,
                maria, kabashima, wakimisaki, takashima, fukahori, ainoura, odo,
                enoura, fukushima, kisashi, ikeshimo, kidu, kunisaki, yokose, sasebogawa,
                mie, nagushi, shishigawa, obama, tobiko, aba, nezumishima, sakito, 
                higashihama, higashiookawa, kadusa) |> 
  left_join(spot_info, by = "spot")



# 釣果のデータ. ----------------------------------------------------------------
fish_data0 = read_rds("~/Lab_Data/tanimaes/share_files/example_data/nagasaki_fishing_spot_fishdata.rds")

fish_data1 = fish_data0 |> 
  unnest(data) |>
  mutate(species_j = ifelse(str_detect(species_j, "^アジゴ?$"), "マアジ", species_j)) |> 
  mutate(species_j = ifelse(str_detect(species_j, "^アラカブ+"), "カサゴ", species_j)) |> 
  mutate(species_j = ifelse(str_detect(species_j, "エギング"), "アオリイカ", species_j)) |> 
  mutate(species_j = ifelse(str_detect(species_j, "^クロ$"), "メジナ", species_j)) |>
  mutate(species_j = ifelse(str_detect(species_j, "^サゴシ$"), "サワラ", species_j)) |>
  mutate(species_j = ifelse(str_detect(species_j, "^サバゴ?$"), "サバ", species_j)) |>
  mutate(species_j = ifelse(str_detect(species_j, "シーバス|セイゴ"), "スズキ", species_j)) |>
  mutate(species_j = ifelse(str_detect(species_j, "^チヌ$"), "クロダイ", species_j)) |>
  mutate(species_j = ifelse(str_detect(species_j, "^ネリゴ$"), "カンパチ", species_j)) |>
  mutate(species_j = ifelse(str_detect(species_j, "^ハタ$"), "キジハタ", species_j)) |>
  mutate(species_j = ifelse(str_detect(species_j, "ハマチ|ヤズ|ブリ"), "ブリ", species_j)) |>
  mutate(species_j = ifelse(str_detect(species_j, "^ヒラス$"), "ヒラマサ", species_j)) |>
  mutate(species_j = ifelse(str_detect(species_j, "^タコ$"), "マダコ", species_j)) |>
  mutate(species_j = ifelse(str_detect(species_j, "^メッキアジ$"), "メッキ", species_j)) |>
  mutate(species_j = ifelse(str_detect(species_j, "^メバリング$"), "メバル", species_j)) |>
  mutate(species_j = ifelse(str_detect(species_j, "^モンゴウイカ$"), "カミナリイカ", species_j)) |>
  mutate(species_j = ifelse(str_detect(species_j, "^太刀魚$"), "タチウオ", species_j)) |>
  mutate(species_j = ifelse(str_detect(species_j, "^真鯛$"), "マダイ", species_j)) |> 
  mutate(species_j = ifelse(str_detect(species_j, "^キス$"), "シロギス", species_j)) |> 
  filter(species_j != "木指") |> 
  filter(size != "1杯")

fish_data2 = fish_data1 |> 
  mutate(size = ifelse(size == "", NA, noquote(size)),
         weight = ifelse(weight == "", NA, noquote(weight)),
         date = ymd(date)) |> # 引用符を消す.
  mutate(size = ifelse(is.na(size), weight, size)) |> # サイズの情報を１列にまとめる.
  mutate(size = ifelse(str_detect(size, pattern = "手|掌"), "20cm", size), # 「手のひら」は 20cm.
         size = ifelse(str_detect(size, pattern = "コロッケ"), "10cm", size), # 「コロッケサイズ」は 10cm.
         size = str_replace(size, pattern = "三", replacement = "3"), 
         size = str_replace(size, pattern = "四", replacement = "4"),
         size = str_replace(size, pattern = "本半", replacement = ".5")) |> # 漢数字をアラビア数字に.
  mutate(size = stri_trans_general(size, "Fullwidth-Halfwidth")) |> # 半角変換.
  mutate(type = ifelse(str_detect(size, pattern = "[Cc]?[Mm㎝]|ｾﾝﾁ"), "length", "width")) |> # 評価基準.
  mutate(type = ifelse(str_detect(size, pattern = "[Kk]?[Gg㎏ℊ]|ｷﾛ"), "weight", type)) |> # 評価基準.
  mutate(size = str_remove(size, pattern = ".*[~∼]")) |> 
  mutate(size = as.double(str_extract(size, pattern = "[0-9,.]+"))) |> 
  mutate(type = ifelse(type == "width" & size > 180, "weight", type), # 重量っぽいものを重量評価に.
         type = ifelse(type == "width" & between(size, 9, 180), "length", type)) |> # 長さっぽいものを長さ評価に.
  mutate(size = ifelse(type == "weight" & size < 30, size*1000, size)) |> # キロはグラムに.
  select(spot, long, lat, date, species_j, type, size, method) |> 
  drop_na()

fish_data2 = fish_data2 |> 
  group_by(type) |> 
  mutate(size_scl = scale(size)[,1]) |> 
  ungroup() 

# 作図.
fish_data2 |> 
  ggplot() +
  geom_jitter(aes(reorder(species_j, size_scl), size_scl, color = type), width = 0.1) +
  scale_color_viridis_d(end = 0.8) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

fish_data2 |>  
  mutate(month = month(date)) |> 
  ggplot() +
  geom_point(aes(month, size_scl)) +
  scale_x_continuous(breaks = 1:12) +
  facet_wrap("species_j")


# その他の変数. ----------------------------------------------------------------
# 種.
sp_info = fish_data2 |> 
  group_by(spot) |> 
  summarise(n = length(species_j),
            max_size = max(size_scl),
            mean_size = mean(size_scl))

# fetch.
fetch_info = read_rds("~/Lab_Data/tanimaes/share_files/example_data/nagasaki_fishing_spot_fetch.rds") |> 
  unnest(length) |> 
  group_by(spot) |>
  summarise(mean_f = mean(length, na.rm = T),
            .groups = "drop") |> print(n = Inf)

# 1 ㎞ になる緯度幅・経度幅
equator_r = 6378137
long_degree = (360*1000)/(2*pi*(equator_r*cos(32.786294986775076*pi/180)))
pole_r = 6356752.314
lat_degree = (360*1000)/(2*pi*pole_r)

# 水産学部棟から釣り場までの直線距離（km）.
sui_info = df1 |> 
  mutate(nlong = 129.86544761304938,
         nlat = 32.786294986775076) |> 
  mutate(diff_h_km = (long - nlong)/long_degree,
         diff_v_km = (lat - nlat)/lat_degree) |> 
  mutate(dist_from_sui = sqrt((diff_h_km)^2 + (diff_v_km)^2)) |>
  select(spot, dist_from_sui)

# 川.
# river = read_xml("~/Labs_work_tanimae_2020/W05-07_42.xml")
# # 川のID
# rid = river |>
#   xml_nodes(xpath = '///jps:GM_LineString') |> 
#   xml_attrs() |> 
#   map_df(~as.list(.))
# 
# # 支流のID
# subid = river |>
#   xml_nodes(xpath = '///GM_PointRef.point') |> 
#   xml_attrs() |> 
#   map_df(~as.list(.))
# 
# river |> xml_nodes(xpath = '///jps:GM_LineString') 

river = read_xml("~/Labs_work_tanimae_2020/W05-07_42.xml") |> 
  xml_nodes(xpath = '///GM_PointArray.column') |> 
  xml_children() |>
  xml_text() |> 
  as_tibble()

river = river |> 
  mutate(value = ifelse(value == "", "river_sep", value)) |>
  mutate(river_id = ifelse(str_detect(value, pattern = "river_sep"), 0.5, 0)) |> 
  mutate(river_id = cumsum(river_id) + 0.5) |> 
  filter(value != "river_sep") |> 
  separate(value, into = c("lat", "long"), sep = " ") |> 
  mutate(across(c("lat", "long"),  ~ as.double(.x)))

# 作図. 河口と川の合流地点に色付け.
river |> 
  group_by(river_id) |> 
  mutate(n = row_number()) |> 
  mutate(status = ifelse(n == max(n), T, F)) |> 
  ungroup() |> 
  filter(between(long, 129.5, 130)) |>
  filter(between(lat, 32.5, 33.1)) |>
  ggplot() +
  geom_point(aes(x = long, y = lat, color = status))

# 河口からの距離.
river_mouth_info = river |> 
  group_by(river_id) |> 
  mutate(n = row_number()) |> 
  slice_max(n) |> 
  ungroup() |> 
  select(rlong = long, rlat = lat)

river_mouth_info = spot_info |> 
  mutate(data = list(river_mouth_info)) |> 
  unnest() |> 
  mutate(diff_h_km = (long - rlong)/long_degree,
         diff_v_km = (lat - rlat)/lat_degree) |> 
  mutate(dist_from_river = sqrt((long-rlong)^2+(lat - rlat)^2)) |> # km に.
  group_by(spot) |> 
  slice_min(dist_from_river) |> 
  ungroup() |> 
  select(spot, dist_from_river) 


# RDA用のデータ. ---------------------------------------------------------------
Y = fish_data2 |>
  group_by(spot, species_j) |> 
  summarise(n = n(), .groups = "drop") |> 
  pivot_wider(names_from = species_j, values_from = n, values_fill = 0) |> 
  select(-spot)

RS = Y |> rowSums()

Y = Y |> mutate(across(everything(), ~sqrt(.x / RS)))

X = sp_info |> 
  left_join(fetch_info, by = "spot") |> 
  left_join(sui_info, by = "spot") |> 
  left_join(river_mouth_info, by = "spot") |> 
  print(n = Inf)

X = X |> mutate(mean_f = scale(mean_f)[,1],
                dist_from_sui = scale(dist_from_sui)[,1],
                dist_from_river = scale(dist_from_river)[,1])

# Bray-Curtis distance.
# Yd = vegdist(Y, method = "bray")

# モデル選択.
r1 = rda(Y ~ mean_f, data = X)
r2 = rda(Y ~ dist_from_sui, data = X)
r3 = rda(Y ~ dist_from_river, data = X)

r4 = rda(Y ~ dist_from_river + mean_f, data = X)

anova(r1, by = "terms",  permutations = 999)
anova(r2, by = "terms",  permutations = 999)
anova(r3, by = "terms",  permutations = 999)

anova(r4, by = "terms",  permutations = 999)

summary(r4)

RsquareAdj(r1)$r.squared # 調整済みR^2
RsquareAdj(r2)$r.squared
RsquareAdj(r3)$r.squared
RsquareAdj(r4)$r.squared

# ggvegan::fortify() を使えば rda() の結果から必要な情報を抽出できます。
rout1 = fortify(r4, axes = 1:2, scaling = 1)
rout2 = fortify(r4, axes = 1:2, scaling = 2)

calculate_equilibrium = function(X) {
  # vegan scales output with a constant.
  p = length(X$CA$eig)
  tot = sum(X$CA$eig)
  n = nrow(X$CA$u)
  sqrt(2 / p) * ((n-1)*tot)^0.25
}

# biplot.
biplot1 = rout1 |> filter(str_detect(Score, "biplot")) |> 
  mutate(sRDA1 = 0.8*RDA1 / sqrt((RDA1^2+RDA2^2))) |> 
  mutate(sRDA2 = 0.8*RDA2 / sqrt((RDA1^2+RDA2^2))) |> 
  mutate(theta = atan(sRDA2/ sRDA1)) |> 
  mutate(theta = 180 * theta / pi + ifelse(RDA1 < 0, theta , theta),
         hjust = ifelse(RDA1 > 0, 1, 0),
         vjust = 0.5) |> 
  as_tibble() |> 
  mutate(Label = recode(Label, dist_from_river = "河口からの距離"),
         Label = recode(Label, mean_f = "平均フェッチ"))

biplot2 = rout2 |> filter(str_detect(Score, "biplot")) |> 
  mutate(sRDA1 = 2.2*RDA1 / sqrt((RDA1^2+RDA2^2))) |> 
  mutate(sRDA2 = 2.2*RDA2 / sqrt((RDA1^2+RDA2^2))) |> 
  mutate(theta = atan(sRDA2/ sRDA1)) |> 
  mutate(theta = 180 * theta / pi + ifelse(RDA1 < 0, theta , theta),
         hjust = ifelse(RDA1 > 0, 1, 0),
         vjust = 0.5) |> 
  as_tibble() |> 
  mutate(Label = recode(Label, dist_from_river = "河口からの距離"),
         Label = recode(Label, mean_f = "平均フェッチ"))

# サイト.
sites1 = rout1 |>
  filter(str_detect(Score, "sites")) |> 
  bind_cols(X) |> 
  as_tibble() 

sites2 = rout2 |>
  filter(str_detect(Score, "sites")) |> 
  bind_cols(X) |> 
  as_tibble()

# 生物種.
species1 = rout1 |>
  filter(str_detect(Score, "species"))

species2 = rout2 |>
  filter(str_detect(Score, "species"))

# 表示したい生物種.
SP_LABEL = c("アオリイカ", "シロギス", "ブリ", "スズキ", "タチウオ", "イイダコ", "メバル", "カサゴ", 
             "メジナ", "マダイ", "クロダイ", "マアジ")

# 作図.
p1 =
  ggplot() +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  ggforce::geom_circle(aes(x0 = 0, y0 = 0, r = 0), color = "grey50") +
  # geom_point(aes(x = RDA1, y = RDA2, color = size_scl_mean),
  #            data = sites1, size = 5) +
  # 生物種.
  geom_text_repel(aes(x = RDA1, y = RDA2, label = Label),
                  verbose = T, force = 1, box.padding = 0, point.padding = 0,
                  data = species1 |> filter(Label %in% SP_LABEL), size = 10, color = "red")  +
  # 矢印
  geom_segment(aes(x = 0, y = 0, xend = RDA1, yend = RDA2),
               data = biplot1,
               arrow = arrow(10, unit(5, "mm"), type = "closed"),
               color = "black", size  = 1) +
  # 矢印の延長線.
  geom_segment(aes(x = 0, y = 0, xend = sRDA1, yend = sRDA2),
               alpha = 0.25, size = 7, data = biplot1, color = "gray70") +
  # 説明変数の文字.
  geom_text(aes(x = sRDA1, y = sRDA2, label = Label, angle = theta, hjust = hjust, vjust = vjust),
            data = biplot1, size = 10, color = "black")  +
  # サイト.
  # geom_label_repel(aes(x = RDA1, y = RDA2, label = spot),
  #                  verbose = T, force = 1, box.padding = 0, point.padding = 0,
  #                  data = sites1, size = 3)  +
  scale_color_viridis_c(begin = 0.1, end = 0.8, option = "B") +
  coord_equal(xlim = c(-1.3, 1.3), ylim = c(-1.3, 1.3)) +
  labs(title = "Distance biplot (scaling = 1)") +
  theme(legend.position = "none") +
  theme_me()

p2 = ggplot() +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  # geom_point(aes(x = RDA1, y = RDA2, color = n),
  #            data = sites2, size = 5) +
  # 矢印
  geom_segment(aes(x = 0, y = 0, xend = RDA1, yend = RDA2),
               data = biplot2,
               arrow = arrow(10, unit(5, "mm"), type = "closed"),
               color = "black", size  = 1) + 
  # 矢印の延長線.
  geom_segment(aes(x = 0, y = 0, xend = sRDA1, yend = sRDA2),
               alpha = 0.25, size = 7, data = biplot2, color = "gray70") + 
  # 説明変数の文字.
  geom_text(aes(x = sRDA1, y = sRDA2, label = Label, angle = theta, hjust = hjust, vjust = vjust),
            data = biplot2, size = 10, color = "black")  +
  # サイト.
  # geom_label_repel(aes(x = RDA1, y = RDA2, label = spot), 
  #                  verbose = T, force = 1, box.padding = 0.4, point.padding = 0,
  #                  data = sites2, size = 3) +
  # 生物種.
  geom_text_repel(aes(x = RDA1, y = RDA2, label = Label),
                  verbose = T, force = 1, box.padding = 0, point.padding = 0,
                  data = species2 |> filter(Label %in% SP_LABEL), size = 10, color = "red") +
  scale_color_viridis_c(begin = 0.1, end = 0.8, option = "B",
                        guide = guide_colorbar(title = "種数", 
                                               title.position = "top", title.hjust = 0.5, 
                                               ticks = F, label = T)) +
  coord_equal(xlim = c(-1, 1), ylim = c(-1, 1)) +
  labs(title = "Distance biplot (scaling = 2)") +
  theme_me()


p3 = ggplot() +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_point(aes(x = RDA1, y = RDA2, color = n),
             data = sites2, size = 5) +
  # 矢印
  geom_segment(aes(x = 0, y = 0, xend = RDA1, yend = RDA2),
               data = biplot2,
               arrow = arrow(10, unit(5, "mm"), type = "closed"),
               color = "black", size  = 1) + 
  # 矢印の延長線.
  geom_segment(aes(x = 0, y = 0, xend = sRDA1, yend = sRDA2),
               alpha = 0.25, size = 7, data = biplot2, color = "gray70") + 
  # 説明変数の文字.
  geom_text(aes(x = sRDA1, y = sRDA2, label = Label, angle = theta, hjust = hjust, vjust = vjust),
            data = biplot2, size = 10, color = "black")  +
  # サイト.
  geom_text_repel(aes(x = RDA1, y = RDA2, label = spot),
                   verbose = T, force = 1, box.padding = 0.4, point.padding = 0,
                   data = sites2, size = 10, color = "steelblue4") +
  # 生物種.
  # geom_text_repel(aes(x = RDA1, y = RDA2, label = Label),
  #                 verbose = T, force = 1, box.padding = 0, point.padding = 0,
  #                 data = species2 |> filter(Label %in% SP_LABEL), size = 3, color = "dodgerblue4") +
  scale_color_viridis_c(end = 0.9, option = "B", 
                        guide = guide_colorbar(title = "種数", 
                                               title.position = "top", title.hjust = 0.5, 
                                               ticks = F, label = T)) +
  coord_equal(xlim = c(-2, 2), ylim = c(-2, 2)) +
  labs(title = "Distance biplot (scaling = 2)") +
  theme_me()
# guides(fill = guide_legend(title.position = "top"))

ggout = p1 + p3

# ggsave(plot = ggout, "~/seaweed_vegetation_tanimae_2021/images/rda_fishing_scl3.png",
#        width = 4000, height = 2100, units = "px")






