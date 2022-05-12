library(tidyverse)
library(lubridate)
library(ggpubr)
library(magick)
library(showtext)
library(patchwork)
# library(sf)
# library(ggrepel)
# library(exifr)

if(!any(str_detect(font_families(), "notosans"))) {
  font_add_google("Noto Sans","notosans")
}
showtext_auto()
Sys.setlocale("LC_TIME", "en_US.UTF-8")

# コードラート枠の検出用関数.
detect_quadrat = function(img, analysis_W = 1000) {
  
  # ここで処理用の画像をつくる
  conversion_rate = magick::image_info(img)$width / analysis_W
  imgs = img |> magick::image_resize(str_glue("{analysis_W}x")) 
  H = magick::image_info(imgs)$height
  W = magick::image_info(imgs)$width
  
  # Canny 法によるエッジ検出
  canny_radius = 0
  canny_sigma = 1
  canny_lower = 5
  canny_upper = 10
  CANNY = str_glue("{canny_radius}x{canny_sigma}+{canny_lower}%+{canny_upper}%")
  img2 = imgs |> 
    magick::image_channel("GRAY") |> 
    magick::image_equalize()  |> 
    magick::image_canny(CANNY) 
  
  # Hough 変換 (ハフ変換) に渡すエッジを減らしたいので、画像の縁と中央を黒く塗る
  box = magick::image_blank(0.6*H, 0.6*H, color = "black")
  z = magick::image_blank(W, H, "transparent")
  z = magick::image_border(z,color = "black", geometry = "10x10")
  z = z |> magick::image_resize(str_glue("{W}x"))
  
  img2 = magick::image_composite(img2, z, gravity = "center")
  img2 = magick::image_composite(img2, box, gravity = "center")
  img2 = img2 |> 
    magick::image_morphology("Thinning", "LineJunctions", iterations = 15) |> 
    magick::image_morphology("Thinning", "LineEnds", iterations = 10) |> 
    magick::image_despeckle(10)
  
  # Hough変換で直線を検出する
  hough_width   = 30
  hough_height  = 30
  hough_threshold = 110
  HOUGH  = str_glue("{hough_width}x{hough_height}+{hough_threshold}")
  tmp = img2 |> magick::image_hough_txt(geometry = HOUGH, format = "mvg") 
  
  # Hough変換で検出線の情報はここで集めました。鉛直と水平の線だけ残すようにしています。
  hdata = tibble(z = read_lines(tmp, skip = 3)) |> 
    separate(z, sep = " ", into = str_glue("v{1:8}")) |> 
    separate(v2, into = c("x1", "y1"), sep = ",") |> 
    separate(v3, into = c("x2", "y2"), sep = ",") |> 
    dplyr::select(x1:y2, count = v6, angle = v7, distance = v8) |> 
    mutate(across(everything(), as.numeric)) |> 
    mutate(group = ifelse(between(angle, 85, 95), "horizontal", "vertical")) |> 
    filter(!between(angle, 95, 175)) |>
    filter(!between(angle, 5, 85)) |>
    mutate(across(c(y1,y2), ~ H - .))
  hdata = hdata |> mutate(across(c(x1,x2,y1,y2), ~conversion_rate * .))
  
  # 画像のよって、縦と横線は複数検出されます。もっといい方法を考えるまでは、
  # 平均をとレンジでまとめる。
  hdata |> 
    group_nest(group) |> 
    mutate(data = map2(data, group, function(X,G) {
      if(str_detect(G, "vertical")) {
        z = X |> mutate(x = (x1 + x2)/2) |> pull(x) |> range()
        tibble(xintercept = z)
      } else {
        z = X |> mutate(y = (y1 + y2)/2) |> pull(y) |> range()
        tibble(yintercept = z)
      }
    })) |> 
    unnest(data)
}

# 岩の面積計算.
fpath = dir("~/Lab_Data/tanimaes/share_files/example_data/onchidium_photo_quadrat/",
            full = T, pattern = "[0-9].[JjPp]")

img1 = image_read(fpath[1])
g1 = image_ggplot(img1)
int = detect_quadrat(img = img1) |> 
  pivot_longer(-group, names_to = "name", values_to = "value") |> 
  drop_na() |> 
  mutate(AB = c("A", "B", "A", "B")) |> 
  select(-group)

canny_radius = 0
canny_sigma = 0.1
canny_lower = 5
canny_upper = 10
CANNY = str_glue("{canny_radius}x{canny_sigma}+{canny_lower}%+{canny_upper}%")

img2 = img1 |>
  image_channel("GRAY") |> 
  image_equalize() |>
  image_canny(CANNY) |> 
  image_morphology('CloseI', 'Octagon', iter = 1) |> 
  image_morphology('OpenI', 'Octagon', iter = 1) #  黒強めに.

img2 |> image_resize("x500")

cimg = img2 |> imager::magick2cimg()
cimg1 = cimg |> as.data.frame() |> tibble()  

# コードラート枠の近くは陰になってしまうので, 50 px 内側に寄せる.
delta = 50 # px.
int1 = int |> mutate(value = ifelse(AB == "A", value + delta, value - delta))

PERCENT = cimg1 |> 
  filter(between(x, int1[3,]$value, int1[4,]$value)) |> 
  filter(between(y, int1[1,]$value, int1[2,]$value)) |> 
  summarise(mud = sum(value)/n(),
            rock = (n()-sum(value))/n(),
            delta = delta) |> 
  mutate(across(c(mud, rock), ~ sprintf("%.3f", .x))) |>
  mutate(label = paste("'Mud :'~", mud, "~'m'^{2}", "~', Rock :'~", rock, "~'m'^{2}", sep = ""))

out1 = img1 |>  
  image_ggplot() + 
  geom_vline(xintercept = c(int1[3,]$value, int1[4,]$value), color = "red", size = 4) +
  geom_hline(yintercept = c(int1[1,]$value, int1[2,]$value), color = "red", size = 4) +
  labs(title = parse(text = PERCENT$label)) +
  theme(title = element_text(size = 100),
        plot.background = element_rect(fill = "white", color = NA))

out2 = img2 |> 
  image_ggplot() + 
  geom_vline(xintercept = c(int1[3,]$value, int1[4,]$value), color = "red", size = 4) +
  geom_hline(yintercept = c(int1[1,]$value, int1[2,]$value), color = "red", size = 4) +
  theme(title = element_text(size = 100),
        plot.background = element_rect(fill = "white", color = NA))

out3 = out1/out2

ggsave(plot = out3, filename = "~/your_folder/test.png", 
       width = 4000, height = 6000, units = "px")
