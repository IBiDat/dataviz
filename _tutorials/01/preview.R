library(ggplot2)

p1 <- ggplot(mpg)
p2 <- p1 + aes(displ, hwy)
p3 <- p2 + geom_point()
p4 <- p3 + geom_point(aes(color=class))

p <- patchwork::wrap_plots(p1, p2, p3, p4)
ggsave("_tutorials/01/preview.png", p, width=7, height=4, dpi=150)
