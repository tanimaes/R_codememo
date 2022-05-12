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



# 画像処理. --------------------------------------------------------------------

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

# コードラート枠内にランダムポイントを打つ関数.
# trimed でコードラート画像をトリミング済みかどうかを指定.

point_quadrat = function(file, new_file = NULL, trimed = T) {
  
  img = magick::image_read(file)
  imgggplot = img |> magick::image_ggplot()
  
  if(isTRUE(trimed)){
    hdata = magick::image_info(img) |> 
      select(vertical = width, horizontal = height)
    xint = c(0, hdata$vertical)
    yint = c(0, hdata$horizontal)
  }else{
    hdata = img |> detect_quadrat()
    yint = hdata |> drop_na(yintercept) |> pull(yintercept) |> as.integer()
    xint = hdata |> drop_na(xintercept) |> pull(xintercept) |> as.integer()
  }
  
  # Off set is in pixels.
  offset = (yint[2] - yint[1])/20
  yint = round(yint + c(offset,-offset), -0.1)
  xint = round(xint + c(offset,-offset), -0.1)
  
  # The sampling point will be chosen randomly over an evenly spaced grid.
  y = round(seq(yint[1], yint[2], length = 10), -0.1)
  x = round(seq(xint[1], xint[2], length = 10), -0.1)
  z = expand_grid(x,y) |> sample_n(50)
  
  z = z |> 
    arrange(desc(y),x) |>
    mutate(id = 1:n(),
           group = as.factor(rep(1:5, each = 10))) 
  
  # This is to determine the squares
  # The image coordinates are flipped in the detect_quadrat()
  # so, we need to flip it back when squares are sampled.
  # s: is the size of the square in pixels.
  HEIGHT = img |> magick::image_info() |> pull(height)
  
  z = z |> 
    mutate(y2 = HEIGHT - y) |> 
    mutate(s = round((yint[2] - yint[1])/50)*2) |> # 要調節.
    mutate(crop = str_glue("{s}x{s}+{x-(s/2)}+{y2-(s/2)}"))
  
  dout = z |> 
    mutate(img = purrr::pmap(list(crop, id, s), function(CROP,n,s) {
      magick::image_crop(img, CROP) |>
        magick::image_annotate(n, size = 20, font = "Noto Sans",
                               weight = 700, gravity = "southwest", color = "white") |>
        magick::image_annotate("--------------", size = 5, degrees = 0, 
                               location = str_glue("+", 0, "+", s/2-3), strokecolor = "white") |>
        magick::image_annotate("--------------", size = 5, degrees = 90,
                               location = str_glue("+", s/2+2, "+", 0), strokecolor = "white") |>
        magick::image_annotate("--------------", size = 5, degrees = 180,
                               location = str_glue("+", s, "+", s/2+3), strokecolor = "white") |>
        magick::image_annotate("--------------", size = 5, degrees = 270,
                               location = str_glue("+", s/2-2, "+", s), strokecolor = "white") |>
        magick::image_border(color = "white", geometry = "2x2")
    }))
  
  
  # Extract the images from the tibble and restructure it so that
  # we can use it in magick.
  imgs = dout |> pull(img)
  imgs = do.call(c, unlist(imgs)) # Very important part.
  i1 = magick::image_append(imgs[1:10])
  i2 = magick::image_append(imgs[11:20])
  i3 = magick::image_append(imgs[21:30])
  i4 = magick::image_append(imgs[31:40])
  i5 = magick::image_append(imgs[41:50])
  p0 = magick::image_append(c(i1, i2, i3, i4, i5), stack = T) |> magick::image_resize("3000x")
  p0 = p0 |> magick::image_ggplot() # make it a ggplot to append to the other plot.

  p1 = imgggplot +
    # geom_hline(aes(yintercept = yintercept), data = hdata, color = "white", size = 2) +
    # geom_vline(aes(xintercept = xintercept), data = hdata, color = "white", size = 2) +
    geom_point(aes(x = x, y =y), 
               shape = 22, size = 6, 
               stroke = 1,
               data = z,
               col = "white") +
    ggrepel::geom_label_repel(aes(x = x, y = y, label = id, fill = group),
                              color = "white",
                              data = z,
                              size = HEIGHT/300,
                              box.padding = grid::unit(2, "mm"), 
                              min.segment.length = unit(10, "mm"),
                              # seed = 2020
    ) +
    # annotate("text", x = 50, y = 50,
    #          label = basename(file), color = "white",
    #          vjust = 0, hjust = 0, size = 6) + 
    scale_fill_viridis_d(end = 0.8, direction = -1) +
    guides(fill = "none")

  p01 = p0/p1
  
  if(is.null(new_file)){
    ofile = basename(file) |> str_replace(".[JjPp][PpNn][Gg]", "_pointed.png")
    folder = str_remove(file, pattern = basename(file))
    new_file = str_glue(folder, ofile)
  }
  
  ggsave(filename = new_file, plot = p01, height = 3000, units = "px")
  
}

# 使いかた.
file = "~/Lab_Data/tanimaes/share_files/example_data/onchidium_photo_quadrat/taira_A_q1_ind6_220505.JPG"
new_file = "~/your_folder/taira_A_q1_ind6_220505_pointed.JPG"

point_quadrat(file = file, new_file = new_file, trimed = F)

