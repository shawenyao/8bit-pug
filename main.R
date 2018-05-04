library(tidyverse)
library(jpeg)

setwd("C:/Users/Wenyao/Desktop/R/8bit_pug")
source("./functions.R")

color_palette <- c(
  "#927f5e", #fawn
  "#0a0e11", #black
  "#972228", #red
  "#f1f1fb"  #grey
) %>% 
  col2rgb() %>% 
  t() %>% 
  `/`(255)

input <- readJPEG("./images/input.jpg")

image <- tidy_image(input) %>% 
  downscale_image(target_image_size = 50)

colored_images <- lapply(
  1:16,
  get_color_palette_with_noise,
  color_palette = color_palette,
  size = 101,
  sd = 0.05
) %>% 
  lapply(
    match_color,
    image = image
  ) %>% 
  bind_rows(.id = "SEED") %>%
  mutate(SEED = factor(SEED, levels = SEED %>% as.numeric() %>% max() %>% seq()))

plot <- ggplot(colored_images,aes(x = x, y = y, fill = fill)) +
  geom_tile() +
  scale_fill_identity() +
  coord_fixed() +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    strip.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    strip.text = element_blank(),
    panel.background = element_blank()
  ) +
  facet_wrap(~SEED, ncol = 4)

print(plot)

jpeg(filename = "./images/output.jpg", width = 1000, height = 1000, quality = 100)
plot
dev.off()
