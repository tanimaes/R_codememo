# 必要なパッケージ
library(tidyverse)
library(lubridate)
library(magick)
library(showtext)
font_add_google("Noto Sans JP", "notosans")
showtext_auto()


sp = read_rds("~/Lab_Data/tanimaes/seaweed_data/rds_write_out/species_writeout_arikawa_2021.rds") |> 
  # filter(near(existence, 1)) |> 
  distinct(year, month, station, species_j, genus, species, existence) |> 
  mutate(date = ymd(paste(year, month, "1", sep = "-")))

sp_df = sp |> distinct(species_j) |> arrange(species_j) |> pull()



gamplot = function(df, num){
  
  SPECIES = sp_df[num]
  SPECIES_c = str_c("^", SPECIES, "$")
  sp1 = sp |> filter(str_detect(species_j, pattern = SPECIES_c))
  PATH = str_glue("~/Lab_Data/tanimaes/share_files/arikawa_seaweed_species/picture_edited/",
                  SPECIES, "_01_arikawa_NA_PROB-edited.PNG")
  FONT_SIZE = 20
  
  # GG =
    sp1 |> 
    mutate(season = case_when(
      month %in% 3:5 ~ "春",
      month %in% 6:8 ~ "夏",
      month %in% 9:11 ~ "秋",
      TRUE ~ "冬"
    )) |> 
    ggplot() +
    geom_jitter(aes(date, existence, color = season),
                width = 10, height = 0, size = 1) + 
    geom_smooth(aes(date, existence), 
                method = "gam", formula = y ~ s(x), method.args = list(family = "binomial"),
                size = 1, color = "steelblue4", se = F) +
    scale_color_manual(breaks = c("春", "夏", "秋", "冬"),
                       values = c("orchid1", "palegreen3", "darkorange1", "azure4")) +
    # scale_color_viridis_d(end = 0.9) +
    scale_x_date(date_breaks = "1 month",
                 labels = scales::label_date_short()
    ) +
    scale_y_continuous(breaks = c(0, 1)) +
    ggtitle(str_c(SPECIES, "の出現時期", sep = " ")) +
    theme_test() +
    theme(axis.title = element_blank(),
          axis.text = element_text(size = FONT_SIZE),
          axis.ticks.x = element_blank(),
          legend.title = element_blank(),
          legend.text = element_text(size = FONT_SIZE, margin = margin(-10,-10,-10,-10)),
          legend.position = "top",
          legend.justification = c(0.95, 1),
          legend.key = element_rect(fill = "gray90"),
          legend.background = element_rect(fill = "gray90", color = "gray90"),
          plot.title = element_text(size = FONT_SIZE, face = "bold"),
          panel.background = element_rect(fill = "white"),
          panel.border = element_rect(color = "white"),
          plot.background = element_rect(fill = "gray90", color = "gray90"),
          legend.box.margin = margin(-17,0,-10,10), # legend 全体に対して上右下左の余白.
    ) 
  
  # ggsave(plot = GG, filename = PATH, width = 666*2, height = 500, units = "px")
}

# for (i in 1:length(sp_df)) {
#   gamplot(sp, i)
# }
