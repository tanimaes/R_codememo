library(tidyverse)
library(magick) # 画像編集用のパッケージ.

# image_caption(). -------------------------------------------------------------
# img : 画像を指定. パスとimage_read()で読み込んだ画像、どちらでも指定できます.
# cap_text : 図の下に出るキャプションです。"" で囲んでわたす. キャプションが長くなり,
# 改行が必要なら, 文中に\n を挟む.
# cap_just : キャプションと図の間の間隔.
# chr_size : キャプションの文字サイズ.
# v_space : 完成した図の縦の余白を指定できます.
# h_space : 完成した図の横の余白を指定できます.

image_caption = function(img, cap_text, 
                         cap_just = 0, chr_size = 0, v_space = 0, h_space = 0){
  
  if(mode(img) == "character"){
    img = magick::image_read(img)
  }
  
  w = magick::image_info(img)$width
  h = magick::image_info(img)$height
  border1 = paste0(0, "x", h/2)
  border2 = paste0(h_space, "x", v_space)
  chr = h/50 + chr_size
  loc = paste0("+", 0,"+", h+h/2+cap_just)
  
  img |> 
    magick::image_border(geometry = border1, 
                         color = "white") |> 
    magick::image_annotate(text = cap_text,
                           size = chr,
                           location = loc,
                           font = "Noto Serif CJK jp",
                           weight = 500,
                           color = "black") |> 
    magick::image_trim() |> 
    magick::image_border(geometry = border2, 
                         color = "white")
}
