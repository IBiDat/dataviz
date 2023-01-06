library(ggplot2)

p <- ggplot(mpg) +
  aes(displ, hwy, color=factor(cyl)) +
  geom_point() +
  ggforce::geom_mark_ellipse(aes(label=cyl, group=cyl)) +
  theme(legend.position="none")

ggsave("_tutorials/06/preview.png", p, width=7, height=4.5, dpi=150)
