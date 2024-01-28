library(hexSticker)
library(ggplot2)

imgurl <- "logo.png"
sticker(imgurl, package="{hprcc}", p_size=42,
    p_y = 1.05, p_color = "#07E947",
    s_x=1, s_y=1, s_width=0.95, s_height=0.95,
    h_size = 0, white_around_sticker = TRUE, 
    spotlight = TRUE, l_width = 5,
        filename="hprcc_logo.png")

#https://gradientdescending.com/how-to-generate-a-hex-sticker-with-openai-and-cropcircles/
install.packages("openai")
install.packages("cropcircles")
 
library(cropcircles)
library(magick)
library(sysfonts)
library(showtext)
library(ggpath)

 
font_add_google("Barlow", "bar")
showtext_auto()
ft <- "bar"
txt <- "black"
 
pkg_name <- "{hprcc}"
 
img_cropped <- hex_crop(
  images = "logo.png",
  border_colour = "black",
  border_size = 2
)
 
ggplot() +
  geom_from_path(aes(0.5, 0.5, path = img_cropped)) +
  annotate("text", x = 0.6, y = 0.06, label = pkg_name, family = ft, size = 35, colour = "ivory",
           angle = 30, hjust = 0, fontface = "bold") +
  xlim(0, 1) +
  ylim(0, 1) +
  theme_void() +
  coord_fixed()
 
ggsave("hex.png", height = 6, width = 6)


usethis::use_logo("hex.png", retina = TRUE)
