library(tidyverse)
library(sf)
library(ggpubr)
library(ggspatial)
library(magick)
library(ggrepel)
library(showtext)
library(ggsflabel)

Sys.setlocale("LC_TIME", "en_US.UTF-8") 
font_add_google("Noto Sans JP","notosans")
theme_gray(base_size = 30, base_family = "notosans") |> theme_set()
showtext_auto()

# 範囲的に狭くて、地形の細かい地図をつくる手順. ################################

ghigh = read_rds("~/Lab_Data/Japan_map_data/Japan/todofuken.rds") |> 
  st_transform(st_crs(2450)) # CRS は 2450 で.

toget = "長崎|熊本|鹿児島|佐賀"

ghigh = ghigh |> filter(str_detect(N03_001, toget))

ghigh |> ggplot() + geom_sf(fill = "red", color = "blue")

# 地点の緯度経度. ##############################################################
# 調査地点の緯度経度は、グーグルマップにピンをたてて調べた.
# このとき CRS は一旦 4326 にしよう.

st01 = c(st = "01", long = 129.12055555, lat = 32.69305555)
st02 = c(st = "02", long = 129.12055555, lat = 32.56944444)
st03 = c(st = "03", long = 129.12055555, lat = 32.48638888)
st04 = c(st = "04", long = 129.12055555, lat = 32.40305555)
st05 = c(st = "05", long = 128.95388888, lat = 32.40305555)
st06 = c(st = "06", long = 128.7872222,  lat = 32.40305555)
st07 = c(st = "07", long = 129.3166666,  lat = 32.4250000)
st08 = c(st = "08", long = 129.3166666,  lat = 32.2583333)
st09 = c(st = "09", long = 129.3166666,  lat = 32.2583333)
st10 = c(st = "10", long = 129.1166666,  lat = 32.2583333)
st11 = c(st = "11", long = 129.0668333,  lat = 32.32227)
st12 = c(st = "12", long = 129.1205555,  lat = 32.2155555)
st13 = c(st = "13", long = 129.12055555, lat = 32.1494444)
st14 = c(st = "14", long = 129.374444,   lat = 31.941111)
st15 = c(st = "15", long = 129.35972222, lat = 31.94194444)

spot = bind_rows(st01, st02, 
                 st03, st04,
                 st05, st06,
                 st07, st08,
                 st09, st10,
                 st11, st12,
                 st13, st14, 
                 st15) |> 
  mutate(across(!st, as.double)) |> 
  st_as_sf(coords = c("long", "lat"), crs = st_crs(4326), agr = "constant")

spot = spot |> 
  mutate(st = as.double(st)) |> 
  mutate(st = str_c("St.", st))

# マップを作図するとき、面積や距離を調べるときなどは、 CRS を 2450 に変える.
spot2450 = spot |> st_transform(crs = st_crs(2450))

# 地図の範囲を決定. ------------------------------------------------------------

# 調査地点の位置情報を１つのポリゴンにまとめ、そのポリゴンの重心を求める.
spot2450cent = spot2450 |> st_union() |> st_centroid()

# 調査地点の重心にバッファーをもたせる.
spot2450cbf = spot2450cent |> st_buffer(dist = 80000)

# バッファーの領域から、図の幅をもとめる.
spot2450box = spot2450cbf |> st_bbox()

# 作図. ------------------------------------------------------------------------

SEACOLOR = "deepskyblue4"
LANDCOLOR = "palegreen"

ghigh |>
  ggplot() + geom_sf(fill = LANDCOLOR, color = "black") +
  geom_sf(data = spot, color = "white", fill = "red", shape = 21, size = 3) +
  geom_sf_label_repel(aes(label = st), 
                      data = spot, min.segment.length = 0, size = 5) +
  coord_sf(xlim = c(spot2450box[1], spot2450box[3]),
           ylim = c(spot2450box[2], spot2450box[4])) +
  theme_minimal() + 
  theme(panel.background = element_rect(fill = SEACOLOR),
        panel.border = element_rect(color = "black", fill = "transparent"),
        panel.grid = element_line(color = SEACOLOR), 
        axis.text = element_text(face = "bold", size = 14),
        axis.title = element_blank())
