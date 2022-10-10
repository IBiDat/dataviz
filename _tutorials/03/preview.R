library(ggplot2)

p <- ggplot(map_data("world")) +
  aes(long, lat, group=group) +
  geom_path() +
  coord_map(xlim=c(-180, 180)) +
  theme_void()

ggsave("_tutorials/03/preview.png", p, width=7, height=4.5, dpi=150)
