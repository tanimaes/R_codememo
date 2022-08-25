# random points for quadrat picture
# Shinichiro Tanimae
# 2021/07/10

library(tidyverse)
library(imager)
library(magick)
library(ggrepel)
library(showtext)

Sys.setlocale("LC_TIME", "en_US.UTF-8") # システムロケールはアメリカ英語に設定
showtext_auto()

# changeRGB. -------------------------------------------------------------------
color_quadrat = function(img){
  
  img = img |> magick2cimg()
  
  R_ch = img |> 
    imager::R() |> 
    as.data.frame() |> 
    tibble() |>
    mutate(std_val = (value - mean(value))/sd(value)*0.12 + 0.4) # set SD & MEAN
  
  G_ch = img |>
    imager::G() |> 
    as.data.frame() |> 
    tibble() |> 
    mutate(std_val = (value - mean(value))/sd(value)*0.12 + 0.4)
  
  B_ch = img |> 
    imager::B() |> 
    as.data.frame() |> 
    tibble() |> 
    mutate(std_val = (value - mean(value))/sd(value)*0.12 + 0.4)
  
  img[,,1,1] = matrix(R_ch |> pull(std_val),
                      nrow = R_ch |> nest_by(x) |> nrow(), 
                      ncol = R_ch |> nest_by(y) |> nrow()) # replace red-channel.
  
  img[,,1,2] = matrix(G_ch |> pull(std_val),
                      nrow = G_ch |> nest_by(x) |> nrow(), 
                      ncol = G_ch |> nest_by(y) |> nrow()) # replace green-channel.
  
  img[,,1,3] = matrix(B_ch |> pull(std_val),
                      nrow = B_ch |> nest_by(x) |> nrow(), 
                      ncol = B_ch |> nest_by(y) |> nrow()) # replace blue-channel.
  img = image_read(img)
  img
  
}

# Quadrat Detection Function. --------------------------------------------------
# Greg Nishihara
# 2021 July 17

# Quadrat Detection Function
# img: magick で読み込んだ画像 must be a magick image
# analysis_W: 処理画像の大きさ (pixel) size of image for processing
# 処理画像の大きさは小さいほど処理がはやいが、1000　の pixel 幅しかテストしていない
# 処理画像の解像度を変えたら、Canny法とHough変換のパラメータの変換も必要でしょう（試していない）
# If the size of the image is changed from the defautl 1000, then you will probably
# need to change the parameters for the Canny filter and the Hough transform.

detect_quadrat = function(img, analysis_W = 1000) {
  ##############################################################################
  # Decrease the size of the image to speed up the process
  # ここで処理用の画像をつくる
  conversion_rate = image_info(img)$width / analysis_W
  imgs = img |> image_resize(str_glue("{analysis_W}x")) 
  H = image_info(imgs)$height
  W = image_info(imgs)$width
  
  ##############################################################################
  # Canny edge detection filter
  # Canny 法によるエッジ検出
  canny_radius = 0
  canny_sigma = 1
  canny_lower = 5
  canny_upper = 10
  
  CANNY = str_glue("{canny_radius}x{canny_sigma}+{canny_lower}%+{canny_upper}%")
  
  img2 = imgs |> 
    image_channel("GRAY") |> 
    image_equalize()  |> 
    image_canny(CANNY) 
  
  ##############################################################################
  # Create a black edge and box to remove as many unneeded edges as possible.
  # Hough 変換 (ハフ変換) に渡すエッジを減らしたいので、画像の縁と中央を黒く塗る
  box = image_blank(0.6*H, 0.6*H, color = "black")
  z = image_blank(W, H, "transparent")
  z = image_border(z,color = "black", geometry = "10x10")
  z = z |> image_resize(str_glue("{W}x"))
  
  img2 = image_composite(img2, z, gravity = "center")
  img2 = image_composite(img2, box, gravity = "center")
  
  # Thin the edges, to reduce the size of unimportant edges.
  img2 = img2 |> 
    image_morphology("Thinning", "LineJunctions", iterations = 15) |> 
    image_morphology("Thinning", "LineEnds", iterations = 10) |> 
    image_despeckle(10)
  
  ##############################################################################
  # Apply a Hough transform to detect straight lines
  # Hough変換で直線を検出する
  hough_width   = 30
  hough_height  = 30
  hough_threshold = 110
  
  HOUGH  = str_glue("{hough_width}x{hough_height}+{hough_threshold}")
  tmp = img2 |> image_hough_txt(geometry = HOUGH, format = "mvg") 
  
  ##############################################################################
  # Extract the results from the Hough transform and remove lines that are clearly
  # not horizontal or vertical.
  # Hough変換で検出線の情報はここで集めました。鉛直と水平の線だけ残すようにしています。
  # 
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
  # Sometimes there are multiple vertical and horizontal lines. Until I get a 
  # better idea, I am just using the mean values.
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

# Example with squares. --------------------------------------------------------

point_quadrat = function(file, trimed = T, changeRGB = F) {
  
  img = image_read(file)
  imgggplot = img |> image_ggplot()
  
  if(trimed == T){
    hdata = image_info(img) |> 
      select(vertical = width, horizontal = height)
    xint = c(0, hdata$vertical)
    yint = c(0, hdata$horizontal)
  }else{
    hdata = img |> detect_quadrat()
    yint = hdata |> drop_na(yintercept) |> pull(yintercept) |> as.integer()
    xint = hdata |> drop_na(xintercept) |> pull(xintercept) |> as.integer()
  }
  
  if(changeRGB == T){img = color_quadrat(img)}
  
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
  HEIGHT = img |> image_info() |> pull(height)
  
  z = z |> 
    mutate(y2 = HEIGHT - y) |> 
    mutate(s = round((yint[2] - yint[1])/50)*2) |> # 要調節.
    mutate(crop = str_glue("{s}x{s}+{x-(s/2)}+{y2-(s/2)}"))
  
  dout = z |> 
    mutate(img = purrr::map2(crop, id, function(CROP,n) {
      image_crop(img, CROP) |>
        image_annotate(n, size = 20, weight = 700,
                       font = "Noto Sans", gravity = "southwest", color = "white") |>
        image_border(color = "white", geometry = "2x2")
    }))
  
  
  # Extract the images from the tibble and restructure it so that
  # we can use it in magick.
  imgs = dout |> pull(img)
  imgs = do.call(c, unlist(imgs)) # Very important part.
  i1 = image_append(imgs[1:10])
  i2 = image_append(imgs[11:20])
  i3 = image_append(imgs[21:30])
  i4 = image_append(imgs[31:40])
  i5 = image_append(imgs[41:50])
  p0 = image_append(c(i1, i2, i3, i4, i5), stack = T) |> image_resize("3000x")
  p0 = p0 |> image_ggplot() # make it a ggplot to append to the other plot.
  
  ################################################################################
  
  p1 = imgggplot +
    # geom_hline(aes(yintercept = yintercept), data = hdata, color = "white", size = 2) +
    # geom_vline(aes(xintercept = xintercept), data = hdata, color = "white", size = 2) +
    geom_point(aes(x = x, y =y), 
               shape = 22, size = 5, stroke = 1,
               data = z, col = "white") +
    geom_label_repel(aes(x = x, y = y, label = id, fill = group),
                     color = "white", data = z, size = HEIGHT/300,
                     box.padding = grid::unit(2, "mm"), 
                     min.segment.length = unit(10, "mm"),
                     # seed = 2020
    ) +
    scale_fill_viridis_d(end = 0.8, direction = -1) +
    guides(fill = "none")
  
  # This part is a bit hacky.
  # Save the plot to a png file, then resize with magick.
  # You do not need to resize if you want to look at the original ggplot.
  # Which is big.
  library(patchwork)
  p01 = p0/p1
  
  ofile = basename(file) |> str_replace(".[JjPp][PpNn][Gg]", "_pointed.png")
  folder = str_remove(file, pattern = basename(file))
  ggsave(str_glue(folder, ofile), p01, height = 3000, units = "px")
}


fpath = dir("~/original/",
            pattern = "[JjPp][PpNn][Gg]",
            full.names = T)

for(i in 1:28){
  point_quadrat(file = fpath[i], 
                trimed = F, 
                changeRGB = F)
}

