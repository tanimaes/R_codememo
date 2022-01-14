# Create maps of the GEP study site.
# 2022-01-10
# Shinichiro Tanimae

library(tidyverse)
library(sf)
library(ggpubr)
library(ggspatial)
library(magick)
library(ggrepel)

# gps. 
shiogamagps = matrix(rev(c(38.34549669653925, 141.0807915733725)), ncol = 2)
hirotagps   = matrix(rev(c(38.98708356030321, 141.64173832460008)), ncol = 2)
bisegps     = matrix(rev(c(26.704302654710496, 127.85974269102186)), ncol = 2)
arikawagps  = matrix(rev(c(32.98827976845565, 129.11838896005543)), ncol = 2)
tainouragps = matrix(rev(c(32.95134175383013, 129.1096027426365)), ncol = 2)
omuragps = matrix(rev(c(32+52/60+11.9/60/60, 129+58/60+24.5/60/60)), ncol = 2)
arikawa_tainouragps = matrix(c((arikawagps[1,1] + tainouragps[1,1])/2, (arikawagps[1,2] + tainouragps[1,2])/2), ncol = 2)

gps_info = rbind(shiogamagps, hirotagps, bisegps, arikawagps, tainouragps, omuragps, arikawa_tainouragps) |> 
  as_tibble() |> 
  mutate(name = c("shiogamagps", "hirotagps", "bisegps", "arikawagps", "tainouragps", "omuragps", "arikawa_tainouragps"),
         num = c(5, 6, 4, 1, 2, 3, 7)) |> 
  rename(long = V1, lat = V2)

gps_info  = gps_info |> 
  mutate(label = toupper(str_remove(name, pattern = "(gps)"))) |> 
  mutate(label = str_replace(label, pattern = "_", replacement = " & "))

gps_info

# small-scale map. -------------------------------------------------------------

gsi_low = st_read("~/Lab_Data/Japan_map_data/GSI/coastl_jpn.shp")

longA = 120
longB = 150
latA = 25
latB = 50

# 軸の区切り幅
dist_long = 10
dist_lat  = 10

# 10進法表記から60進法表記への変換.
hms = seq(longA, longB, by = dist_long)
dms = seq(latA, latB, by = dist_lat)

# 軸ラベルの作成
hmsa = vector(mode = "character", length = length(hms))
for (i in 1:length(hms)) {
  hmsa[i] = parse(text = paste(hms[i], "^", "degree", "*", "E", sep = ""))
}

dmsa = vector(mode = "character", length = length(dms))
for (i in 1:length(dms)) {
  dmsa[i] = parse(text = paste(dms[i], "^", "degree", "*", "N",  sep = ""))
}


jp = gsi_low  |> 
  ggplot() +
  geom_sf(fill = "white", color = "black", size = 0.5) +
  geom_text_repel(aes(long, lat, label = label), box.padding = 2,
                  data = gps_info |> filter(!str_detect(name, pattern = "(arikawa_)")), size = 10) +
  geom_point(aes(long, lat), data = gps_info, size = 5) +
  annotation_north_arrow(style = north_arrow_minimal(text_size = 30, text_face = "bold",line_width = 2),
                         height = unit(50, "mm"), width = unit(45, "mm"),
                         pad_x = unit(25, "mm"), pad_y = unit(40, "mm"),
                         # width = unit(20, "mm"),
                         location = "tl") +
  # ggsn::scalebar(gadm, location="topleft", dist = 250, dist_unit = "km",
  #                transform=TRUE, model="ITRF94", height=0.01) +
  coord_sf(xlim = c(longA, longB), ylim = c(latA, latB)) +
  annotation_scale(location = "tl", # スケールが不確かだと警告が出ます...
                   pad_x =  unit(15, "mm"), pad_y =  unit(15, "mm"), text_pad = unit(10, "mm"), 
                   height = unit(10, "mm"), 
                   line_width = 1, 
                   text_cex = 2) +
  scale_x_continuous(breaks = seq(longA, longB, by = dist_long), labels = c(hmsa)) +
  scale_y_continuous(breaks = seq(latA, latB, by = dist_lat), labels = c(dmsa)) +
  theme(panel.background = element_rect(fill = "white", color = "black"),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.text = element_text(size = 25, color = "black"),
        axis.ticks = element_line(color = "black"),
        axis.title = element_blank())

ggsave(plot = jp, "~/Lab_Data/tanimaes/share_files/map_220110/jp_map.png", 
       width = 4000, height = 4000, units = "px")

# large-scale map. -------------------------------------------------------------

# i = 1

for (i in 1:7) {
  
  newfilename = str_glue("map", gps_info$num[i], str_remove(gps_info$name[i], pattern = "(gps)"), .sep = "_")
  
  long = gps_info$long[i]
  lat = gps_info$lat[i]
  
  width = 10 # 横 km.
  height = 10 # 縦 km.
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
  gsi_ngsk = tibble(xml = dir("~/Lab_Data/Japan_map_data/FG/",
                              full.names = TRUE)) |>
    mutate(fname = basename(xml)) |>
    separate(fname, into = c("fg", "gml", "id", "type", "date", "num"))
  
  # 長崎以外のマップデータ.  
  gsi_otherpref = tibble(xml = dir("~/Lab_Data/Japan_map_data/kaken_GEP_2022/FG/",
                                   full.names = TRUE)) |>
    mutate(fname = basename(xml)) |>
    separate(fname, into = c("fg", "gml", "id", "type", "date", "num"))
  
  gsi = bind_rows(gsi_ngsk, gsi_otherpref)
  
  gsi = gsi |>
    filter(id %in% c) |> # gml id.
    filter(str_detect(type, map_type)) # map type.
  
  try_read_fgd = possibly(fgdr::read_fgd, NULL)
  
  gsi = gsi |>
    mutate(data = map(xml, try_read_fgd)) |>
    pull(data) |> 
    bind_rows()
  
  # fgdr::read_fgd("~/Lab_Data/Japan_map_data/kaken_GEP_2022/FG/FG-GML-574130-AdmArea-20210701-0001.xml")
  # は実行できません. そのため、Admarea の地図に欠けができます.
  # なので、今回は WA の地図で作りました.

  if(i != 7){
    p1 = gsi |>
      ggplot() +
      geom_sf(fill = "slategray1", color = "slategray1", size = 0.05) +
      coord_sf(xlim = c(longA, longB), ylim = c(latA, latB), expand = F) +
      geom_point(aes(long, lat+0.003), 
                 data = gps_info[i,], size = 15, shape = 25, fill = "black") +
      labs(title = gps_info$label[i]) +
      theme(title = element_text(size = 50),
            panel.background = element_rect(fill = "gray60", color = "black"),
            panel.grid = element_blank(),
            panel.border = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            axis.title = element_blank())
  }else{
    p1 = gsi |>
      ggplot() +
      geom_sf(fill = "slategray1", color = "slategray1", size = 0.05) +
      coord_sf(xlim = c(longA, longB), ylim = c(latA, latB), expand = F) +
      geom_point(aes(long, lat+0.003),
                 data = gps_info |> filter(!str_detect(name, pattern = "arikawa_tainoura")), 
                 size = 15, shape = 25, fill = "black") +
      labs(title = gps_info$label[i]) +
      theme(title = element_text(size = 50),
            panel.background = element_rect(fill = "gray60", color = "black"),
            panel.grid = element_blank(),
            panel.border = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            axis.title = element_blank())
  }
  
  ggsave(plot = p1, str_glue("~/Lab_Data/tanimaes/share_files/map_220110/", newfilename, ".png"),
         width = 4000, height = 4000, units = "px")
  
}

maps = tibble(fpath = dir("~/Lab_Data/tanimaes/share_files/map_220110/", pattern = "^map", full.names = T)) |> 
  mutate(img = map(fpath, image_read)) 
imgs = maps |> pull(img)
# imgs = do.call(c, unlist(imgs)) ??

# 上段.
map1 = image_read("~/Lab_Data/tanimaes/share_files/map_220110/jp_map.png") |> 
  image_border(geometry = "100x100", color = "white") |> 
  image_resize("4000x4000")
map2 = image_append(image = c(imgs[[6]], imgs[[5]]), stack = T) |> image_resize("x4000")
map3 = image_append(image = c(map1, map2), stack = F)

# 下段.
map4 = image_append(image = c(imgs[[4]], imgs[[7]], imgs[[3]]), stack = F) |> image_resize("6000x")
map5 = image_append(image = c(map3, map4), stack = T)

# 図の確認.
map5 |> 
  image_resize("x800") 


# map5 |> 
#   image_resize("x6000") |> 
#   image_write("~/Lab_Data/tanimaes/share_files/map_220110/out02.pdf", format = "pdf")
# 
# image_read_pdf("~/Lab_Data/tanimaes/share_files/map_220110/out02.pdf") |> 
#   image_write("~/Lab_Data/tanimaes/share_files/map_220110/out02.png")

