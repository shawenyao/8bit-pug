
#' @description 
#' convert the format of an image from 3-dimensional array to tidy data.frame
#' 
#' @param image a 3-dimensional array of pixels [x,y,channel]
#' 
#' @return a tidy image data.frame - x,y,r,g,b
#' 
tidy_image <- function(image){
  list(
    color_matrix = lapply(1:3, function(x) image[, , x]),
    channel = c("r", "g", "b")
  ) %>% 
    pmap(
      function(color_matrix, channel){
        color_matrix %>% 
          as.data.frame() %>% 
          mutate(
            y = row_number(),
            channel = channel
          ) %>% 
          gather(x, value, -y, -channel) %>% 
          mutate(
            x = x %>% gsub("V", "", .) %>% as.numeric()
          )
      }
    ) %>% 
    bind_rows() %>% 
    spread(channel, value)
}


#' @description 
#' downscale an image by grouping pixels and taking the average of their rgb values
#' 
#' @param image a data.frame of tidy image
#' @param target_image_size the desired value of min(height,width)
#' 
#' @return a data.frame of downscaled tidy image
#' 
downscale_image <- function(image, target_image_size){
  image %>% 
    mutate(
      x = (x / max(x) * target_image_size) %>% ceiling(),
      y = (y / max(y) * target_image_size) %>% ceiling()
    ) %>% 
    group_by(x, y) %>% 
    summarise_all(mean) %>% 
    mutate(
      # flip y
      y = max(y) - y + 1
    ) %>% 
    ungroup()
}


#' @description 
#' add normal noises to the color palette
#' 
#' @param seed
#' @param color_palette a data.frame of the original color palette
#' @param size the size of the new color palette
#' @param sd standard deviation of normal noise
#' 
#' @return a data.frame of the new color palette
#' 
get_color_palette_with_noise <- function(seed, color_palette, size, sd){
  set.seed(seed)
  
  color_palette %>% 
    `[`(rep(1:nrow(.), each = size-1)) %>% 
    `+`(
      matrix(
        rnorm(nrow(color_palette) * 3 * 100, mean = 0, sd = sd),
        ncol = 3
      )
    ) %>% 
    # prevent illegal rgb values
    pmax(0) %>% 
    pmin(1) %>% 
    # include the orignal noise-free palette
    rbind(color_palette) %>% 
    as.data.frame() %>% 
    mutate(
      fill = rgb(red, green, blue)
    )
}


#' @description
#' for each pixel in the image
#' assign the best matching color from the palette
#' according to the Euclidean distance in the [r,g,b] space
#' 
#' @param color_palette
#' @param image a data.frame of tidy image
#' 
#' @return a data.frame of tidy image with color defined in color_palette
#' 
match_color <- function(color_palette, image){
  image %>% 
    `[`(rep(1:nrow(.), each = nrow(color_palette)),) %>% 
    cbind(color_palette) %>% 
    mutate(
      distance = (r-red)^2 + (g-green)^2 + (b-blue)^2
    ) %>% 
    group_by(x, y) %>% 
    top_n(1, desc(distance)) %>% 
    select(x, y, fill)
}
