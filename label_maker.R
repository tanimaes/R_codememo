library(tidyverse)
library(lubridate)
library(readxl)
library(magick)
library(showtext)
library(gt)

Sys.setlocale("LC_TIME", "en_US.UTF-8") 
font_add_google("Noto Sans JP","notosans")
showtext_auto()


df = read_xlsx(path = "~/Lab_Data/tanimaes/share_files/herbarium_label/Algal collections .xlsx") |> 
  rename(date = `Date of collection`) |> 
  mutate(date = as.Date(date)) |> 
  filter(date > ymd("2022-07-01"))

list = read_csv("~/Lab_Data/tanimaes/seaweed_data/info/seaweed_sp_info.csv") |> 
  mutate(Species = str_c(genus, species, sep = " ")) |> 
  mutate(species_j = str_remove(species_j, pattern = "-cf")) |> 
  mutate(species_j = ifelse(str_detect(species_j, "[a-z]"), NA, species_j)) |> 
  distinct(species_j, Species)

df1 = df |> left_join(list, by = "Species") |> arrange(Species)

df1 = df1 |> 
  mutate(habit = case_when(
    str_detect(Locality, "^Arikawa|^Naname") ~ "2-6 m; On natural substrate",
    str_detect(Locality, "^Omura") ~ "0-3m; On natural substrate",
    str_detect(Locality, "ECSER") ~ "0-3m; On concrete structure",
    TRUE ~ "0-5m"
  ))

df1 |> filter(is.na(Family))

################################################################################

len = length(df1$Species)

# for write out.
folder = "~/Lab_Data/tanimaes/share_files/herbarium_label/220703-230127_kamigoto/"

for (i in 1:len) {
  
  df2 = df1 |> slice(i)
  
  label = tibble(name = c("Species name: ", 
                          "Common name: ", 
                          "",
                          "Locality: ", 
                          "Depth and Habitat: ",
                          "Leg: ",
                          "Det: ",
                          "Date: ",
                          "Accession No.: "),
                 value = c(df2$Species, 
                           df2$species_j,
                           "", 
                           df2$Locality,
                           df2$habit,
                           df2$Legit,
                           df2$Determinavit,
                           df2$date |> as.character(),
                           df2$`Accession number`)) 
  
  SP = str_replace(df2$Species, pattern = " ", replacement = "-")
  filename = str_c(folder, "label", i, "_", SP, "_", df2$`Accession number`, ".pdf")
  
  label |> 
    gt() |>
    sub_missing(columns = value,
                missing_text = " ") |> 
    tab_header(title = md("**Marine Algal Collections**"),
               subtitle = "Institute for East China Sea Research - Nagasaki University") |> 
    tab_options(table_body.hlines.width = 0, # tableの中の水平線消す
                stub.border.width = 0, # stub列の中の線を消す
                column_labels.border.top.width = 0, 
                heading.border.bottom.width = 0,
                column_labels.border.bottom.width = 0,
                table_body.border.top.width = 0, #テーブルの下線を黒く
                table_body.border.bottom.width = 0, #テーブルの下線を黒く
                row_group.border.top.width = 0,
                row_group.border.bottom.width = 0,
                table.border.top.width = 0,
                table.border.bottom.width = 0,
                table.width = pct(80),
                data_row.padding = px(4)) |> 
    cols_width("name" ~ px(10),
               "value" ~ px(25)) |> 
    cols_label(name = "", value = "") |> 
    tab_style(style = list(cell_text(color = "black", size = "large")),
              locations = list(cells_title(), cells_body())) |> 
    tab_style(style = list(cell_text(style = "italic")),
              locations = list(cells_body(columns = "value", 
                                          rows = str_detect(name, "^Sp")))) |> 
    gtsave(filename)
  
}

################################################################################

for (n in 1:ceiling(len/10)) {
  
  S = seq(1, len, by = 10)[n]
  
  if(n != ceiling(len/10)){
    E = seq(10, len, by = 10)[n]
  }else{
    E = len
    remainder = length(S:E)
  }  
  
  imgs = tibble(fpath = dir(folder, pattern = "pdf", full = T)) |> 
    slice(S:E) |> 
    mutate(img = map(fpath, \(x){
      image_read_pdf(x, density = 200) |> 
        image_trim() |>
        image_border("90x90", color = "white")
    }))
  
  imgs = imgs |> pull(img)
  imgs = do.call(c, unlist(imgs))
  
  filename = str_c(folder, "labels_out_", n, ".png")
  
  if(n != ceiling(len/10)){
    ap1 = image_append(image = c(imgs[c(1:5)]), stack = T)
    ap2 = image_append(image = c(imgs[c(6:10)]), stack = T)
    image_append(image = c(ap1, ap2), stack = F) |> 
      image_border("320x320", color = "white") |> 
      image_write(filename)
  }else{
    if(remainder <= 5){
      ap1 = image_append(image = c(imgs), stack = T)
      BR = image_blank(width = 1326*2, height = 833*5, color = "white")
      BR |> 
        image_composite(composite_image = ap1, gravity = "northwest") |> 
        image_border("320x320", color = "white") |>
        image_write(filename)
    }else{
      ap1 = image_append(image = c(imgs[c(1:5)]), stack = T)
      ap2 = image_append(image = c(imgs[c(6:remainder)]), stack = T)
      BR = image_blank(width = 1326*2, height = 833*5, color = "white")
      BR |>   
        image_composite(composite_image = ap1, gravity = "northwest") |> 
        image_composite(composite_image = ap2, gravity = "northeast") |> 
        image_border("320x320", color = "white") |> 
        image_write(filename)
    }
  }} 


