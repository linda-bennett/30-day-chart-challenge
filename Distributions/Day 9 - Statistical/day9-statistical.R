library(ggfortify)

p <- ggdistribution(dchisq, seq(0, 20, 0.1), df = 1, colour = 'blue')
p <- ggdistribution(dchisq, seq(0, 20, 0.1), df = 3, colour = 'green', p = p)
p <- ggdistribution(dchisq, seq(0, 20, 0.1), df = 5, colour = 'red', p = p)
p <- ggdistribution(dchisq, seq(0, 20, 0.1), df = 7, colour = 'purple', p = p)
p <- ggdistribution(dchisq, seq(0, 20, 0.1), df = 9, colour = 'blue')
p <- ggdistribution(dchisq, seq(0, 20, 0.1), df = 11, colour = 'green', p = p)
p <- ggdistribution(dchisq, seq(0, 20, 0.1), df = 13, colour = 'red', p = p)
p <- ggdistribution(dchisq, seq(0, 20, 0.1), df = 15, colour = 'purple', fill='green', alpha=0.3, p = p)
p <- ggdistribution(dchisq, seq(0, 20, 0.1), df = 17, colour = 'blue', fill='blue', alpha=0.3, p = p)
p <- ggdistribution(dchisq, seq(0, 20, 0.1), df = 19, colour = 'green', p = p)
p <- ggdistribution(dchisq, seq(0, 20, 0.1), df = 21, colour = 'red', p = p)
p <- ggdistribution(dchisq, seq(0, 20, 0.1), df = 23, colour = 'purple', p = p)
p <- ggdistribution(dchisq, seq(0, 20, 0.1), df = 25, colour = 'blue', p = p)
p <- ggdistribution(dchisq, seq(0, 20, 0.1), df = 27, colour = 'green', p = p)
p <- ggdistribution(dchisq, seq(0, 20, 0.1), df = 29, colour = 'red', fill='red', alpha=0.3, p = p)
p <- ggdistribution(dchisq, seq(0, 20, 0.1), df = 31, colour = 'purple', p = p)
p <- ggdistribution(dchisq, seq(0, 20, 0.1), df = 33, colour = 'blue', p = p)
p <- ggdistribution(dchisq, seq(0, 20, 0.1), df = 35, colour = 'green', p = p)
p <- ggdistribution(dchisq, seq(0, 20, 0.1), df = 37, colour = 'red', p = p)
p <- ggdistribution(dchisq, seq(0, 20, 0.1), df = 39, colour = 'purple', fill='purple', alpha=0.3, p = p)



plot <- p + coord_polar(theta = "y") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank(),
    plot.background = element_rect(fill="black")
  )


ggsave("day9-statistics.png", plot)
