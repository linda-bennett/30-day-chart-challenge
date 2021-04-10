library(tidyverse)
library(magick)
library(scales)
library(imager)
library(waffle)
library(cowplot)
library(showtext)
library(patchwork)

font_add_google("Montserrat", "Montserrat")
font_add_google("Sacramento", "Sacramento")
font_add_google("Catamaran", "Catamaran")
showtext_auto()


image <- image_read("Distributions\\Day 10 - Abstract\\kandinsky.jpg")

# Code from this blog: https://www.r-bloggers.com/2019/01/extracting-colours-from-your-images-with-image-quantization/
get_colorPal <- function(im, n=8, cs="RGB"){
  #print(cs) 
  tmp <-im %>% image_resize("100") %>% 
    image_quantize(max=n, colorspace=cs) %>%  ## reducing colours! different colorspace gives you different result
    magick2cimg() %>%  ## I'm converting, becauase I want to use as.data.frame function in imager package.
    RGBtoHSV() %>% ## i like sorting colour by hue rather than RGB (red green blue)
    as.data.frame(wide="c") %>%  #3 making it wide makes it easier to output hex colour
    mutate(hex=hsv(rescale(c.1, from=c(0,360)),c.2,c.3),
           hue = c.1,
           sat = c.2,
           lightness = c.3
           ) %>%
    count(hex, hue, sat, lightness, sort=T) %>% 
    mutate(colorspace = cs)
  
  return(tmp %>% select(colorspace,hex,hue,sat,lightness,n)) ## I want data frame as a result.
  
}


# Check what image resize and quantize in the function does
# image %>%
#   image_resize("100") %>%
#   image_quantize(max=24)


colors <- get_colorPal(image, n=50)

plot <- colors %>%
  ggplot(aes(fill = hex, values = n/10)) +
  coord_equal() +
  geom_waffle(colour = "white") +
  scale_fill_identity() +
  theme_void()


plot2 <- colors %>%
  arrange(desc(hue)) %>%
  ggplot(aes(fill = hex, values = n/10)) +
  coord_equal() +
  geom_waffle(colour = "white") +
  scale_fill_identity() +
  theme_void() 
plot2# +
#   ggtitle("painting color distribution") +
#   theme(plot.title = element_text(hjust=0.5,
#                                   family="Catamaran"))


plot_image <- ggplot() +
  draw_image("Distributions\\Day 10 - Abstract\\kandinsky.jpg") +
  theme_void()


p <-plot_spacer() / plot_image / plot2 / plot_spacer() +
  plot_annotation(
    title = "The Colours of Kandinsky",
    subtitle = "Distribution of Colours in Yellow-Red-Blue",
    caption = "source: www.wassilykandinsky.net | #30DayChartChallenge",
    theme = theme(plot.title = element_text(size = 40,
                                            hjust=0.5,
                                            vjust = -5,
                                            family="Montserrat"),
                  plot.subtitle = element_text(size = 30,
                                               hjust=0.5,
                                               vjust= -1,
                                               family="Sacramento"),
                  plot.caption = element_text(size = 12,
                                              family="Catamaran"),
                  plot.background = element_rect(fill = NA, colour = 'black', size = 3))
  ) +
  plot_layout(heights=c(0.2, 4, 1, 0.5))


ggsave("day10-abstract.png", p, height=6.5, width=6.5, dpi=150)
