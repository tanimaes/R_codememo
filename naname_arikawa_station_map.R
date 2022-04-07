# 七目郷と有川郷の観測ステーションの地図を作成する.
# 2022-04-07
# Shinichiro Tanimae

# Packages. --------------------------------------------------------------------
library(tidyverse)
library(lubridate)
library(readxl)
library(ggpubr)
library(lemon)
library(patchwork)
library(showtext)

Sys.setlocale("LC_TIME", "en_US.UTF-8") # システムロケールはアメリカ英語に設定

font_add_google("Noto Sans JP","notosans")
# １）theme_set() をつかってデフォルトのフォントをかえる
# ２）ggplot() の theme() からフォントの指定をはずす。
theme_pubr(base_family = "notosans") |> theme_set()
showtext_auto()

# map. -------------------------------------------------------------------------
st_naname = tibble(location = "naname",
                   station = c(1:5),
                   long = c(129.08872195319162, 129.08843004711716, 129.08861466490035, 
                            129.08870539045057, 129.0882770342249),
                   lat = c(32.98303974857359, 32.98378847903156, 32.983800218961335, 
                           32.98381533539792, 32.984165466454336))

st_arikawa = tibble(location = "arikawa",
                    station = c(1:7),
                    long = c(129.11801934274172, 129.1184700774443, 129.11904476419005,
                             129.1184954312713, 129.11888982413603, 129.1190982889792, 129.12016878394536),
                    lat = c(32.98801913636207, 32.98802149929502, 32.987945885409275,
                            32.988449189116935, 32.9885744238659, 32.98911553046403, 32.98760798505502))

st_info = bind_rows(st_naname, st_arikawa) |> 
  mutate(station = as.factor(station)) |> 
  mutate(label = paste("St.", station, sep = ""))


# df1 = full_join(df0, st_info, by = c("location","station"))

library(tidyverse)
library(sf)
library(ggpubr)
library(ggspatial)
library(magick)
library(ggrepel)

st_mean = st_info |> 
  group_by(location) |> 
  summarise(long = mean(long),
            lat = mean(lat)) |> 
  mutate(width = c(0.3, 0.2),
         height = c(0.3, 0.2))

# 箱.
gsi_re = c()
coord = c()

for (i in 1:2) {
  
  long = st_mean$long[i]
  lat = st_mean$lat[i]
  width = st_mean$width[i] # 横 km.
  height = st_mean$height[i] # 縦 km.
  map_type = "WA" # 地図の種類.
  
  # 1 ㎞ になる緯度幅・経度幅を, 赤道半径, 極半径, 円周率を使って各地点ごとに算出.
  # 1 km to longitude.  
  equator_r = 6378137
  long_degree = (360*1000)/(2*pi*(equator_r*cos(lat*pi/180)))
  
  # 1 km to latitude.
  pole_r = 6356752.314
  lat_degree = (360*1000)/(2*pi*pole_r)
  
  # 図の緯度経度の幅.
  longA = long - long_degree*width/2
  longB = long + long_degree*width/2
  latA = lat - lat_degree*height/2
  latB = lat + lat_degree*height/2
  
  # xml ファイルがたくさんあるので, 不要なファイルは読まないようにする.
  # 四隅をとり, 緯度・経度 0.01 刻みの行列を作る. 
  # coords_to_mesh()で gml id が一致したファイルのみ残す.
  corner = tibble(LONG = c(longA, longA, longB, longB),
                  LAT = c(latA, latB, latA, latB))
  
  c = tibble(LONG = c(seq(longA, longB, by = 0.01), longB)) |> distinct() |> 
    expand_grid(tibble(LAT = c(seq(latA, latB, by = 0.01), latB)) |> distinct()) |> 
    mutate(gml_id = jpmesh::coords_to_mesh(LONG, LAT, mesh_size = 10)) |> 
    summarise(gml_id = as.character(unique(gml_id))) |> 
    pull(gml_id)
  
  # 長崎のマップデータ.
  gsi = tibble(xml = dir("~/Lab_Data/Japan_map_data/FG/",
                         full.names = TRUE)) |>
    mutate(fname = basename(xml)) |>
    separate(fname, into = c("fg", "gml", "id", "type", "date", "num"))
  
  c = str_extract(c, pattern = "^.{6}")
  
  gsi = gsi |>
    filter(id %in% c) |> # gml id.
    filter(str_detect(type, map_type)) # map type.
  
  try_read_fgd = possibly(fgdr::read_fgd, NULL)
  
  gsi = gsi |>
    mutate(data = map(xml, try_read_fgd)) |>
    pull(data) |> 
    bind_rows() |> 
    select(type, geometry) |> 
    mutate(location = st_mean$location[i])
  
  gsi_re = gsi_re |> 
    bind_rows(gsi) 
  
  coord = coord |> 
    bind_rows(tibble(longA, longB, latA, latB, 
                     station = st_mean$location[i]))
}

# 完成したデータフレーム.
gsi_re
coord

# df2 = df1 |> 
#   group_by(date) |> 
#   mutate(scl = pla_100g_day/max(pla_100g_day), .after = pla_100g_day) |> 
#   ungroup()


# st_union.
map1 = gsi_re |>st_union() 

################# 七目の図.
mg1 = map1 |> 
  ggplot() +
  geom_sf(fill = "white", color = "black", size = 0.8) +
  coord_sf(xlim = c(coord$longA[2], coord$longB[2]),
           ylim = c(coord$latA[2], coord$latB[2]), expand = F) +
  geom_point(aes(long, lat), data =　st_info, size = 7) +
  geom_label_repel(aes(long, lat, label = label),
                   # nudge_x = 0.0002, nudge_y = -0.0001,
                   label.size = 0.5, label.padding = 0.5, label.r = 0.5,# 枠線の太さ, 枠の大きさ, 角の縁取り.
                   point.padding = 0, size = 10, # segment と point の距離, ラベル全体の大きさ.
                   box.padding = 1, force = 1, # ややこしい.
                   verbose = T, # 何らかのアルゴリズムが走り, labelの位置を自動調節してくれます.
                   data =　st_info |> filter(location == "naname")) + 
  # facet_grid(.~date) +
  scale_color_viridis_c(end = 0.8) +
  geom_text(aes(x = coord$longB[2]-0.0008*2/3, coord$latB[2]-0.0001*2/3, 
                      label = "七目の観測ステーション"), size = 9) +
  guides(color = guide_colourbar(title.position = "top",
                                 title.hjust = 0.5,title.vjust = 0,
                                 label.vjust = 2,
                                 title = "Min-Max normalization")) +
  theme_pubr() +
  theme(title = element_text(size = 30),
        panel.background = element_rect(fill = "gray80", color = "gray80"),
        # panel.grid = element_blank(),
        # panel.border = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.line = element_blank(),
        strip.text = element_text(size= 30),
        legend.text = element_text(size = 30),
        legend.key.width = unit(2, "cm"),
        legend.margin = unit(-2, "cm"))

################# 有川の図.
mg2 = map1 |>
  ggplot() +
  geom_sf(fill = "white", color = "black", size = 0.8) +
  coord_sf(xlim = c(coord$longA[1], coord$longB[1]),
           ylim = c(coord$latA[1]+0.0004, coord$latB[1]+0.0004), expand = F) +
  geom_point(aes(long, lat), data = st_info, size = 7) +
  geom_label_repel(aes(long, lat, label = label),
                   # nudge_x = 0.0002, nudge_y = -0.0001,
                   label.size = 0.5, label.padding = 0.5, label.r = 0.5,# 枠線の太さ, 枠の大きさ, 角の縁取り.
                   point.padding = 0, size = 10,# segment と point の距離, ラベル全体の大きさ.
                   box.padding = 1, force = 1, # ややこしい.
                   verbose = T, # 何らかのアルゴリズムが走り, labelの位置を自動調節してくれます.
                   data =　st_info |> filter(location == "arikawa")) + 
  # facet_grid(.~date) +
  scale_color_viridis_c(end = 0.8) +
  geom_text(aes(x = coord$longB[1]-0.0008, coord$latB[1]+0.0003, 
                label = "有川の観測ステーション"), size = 9) +
  # guides(color = guide_colourbar(title.position = "top", 
  #                                title.hjust = 0.5, title.vjust = 0,
  #                                label.vjust = 6,
  #                                title = "Min-Max normalization")) +
  theme_pubr() +
  theme(title = element_text(size = 50),
        panel.background = element_rect(fill = "gray80", color = "gray80"),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        axis.line = element_blank(),
        legend.position = "none"
        # strip.text = element_text(size = 50),
        # legend.text = element_text(size = 50),
        # legend.key.width = unit(2, "cm"),
        # legend.margin = unit(-2, "cm")
  )


mg3 = mg1+mg2


ggsave(plot = mg3, "~/Lab_Data/Naname/result_box/station_map_naname_arikawa.png",
       width = 5000, height =2500, units = "px")
