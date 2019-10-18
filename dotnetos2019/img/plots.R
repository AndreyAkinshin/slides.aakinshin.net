library(ggplot2)
library(gridExtra)

build.plot <- function(a, b, filename) {
  q <- c(a, b)
  lims <- extendrange(q, f = 0.1)
  p1 <- ggplot(data.frame(x = 1:length(q), y = q), aes(x = x, y = y)) +
    geom_point() +
    ggtitle("Timeline") +
    xlab("Iteration") +
    ylab("Duration") +
    theme_gray()
  p2 <- ggplot(data.frame(x = a), aes(x = x)) +
    geom_density(bw = "SJ", alpha = .2, fill = "#FF6666") +
    xlim(lims) +
    ggtitle("Before") +
    xlab("Duration") +
    ylab("Density") +
    theme_gray()
  p3 <- ggplot(data.frame(x = b), aes(x = x)) +
    geom_density(bw = "SJ", alpha = .2, fill = "#FF6666") +
    ggtitle("After") +
    xlim(lims) +
    xlab("Duration") +
    ylab("Density") +
    theme_gray()
  p23 <- grid.arrange(p2, p3, nrow = 2)
  p <- grid.arrange(p1, p23 , ncol = 2)
  #p
  ggsave(filename, p)
}

gen.multimodal <- function(mu1, mu2) sample(
  c(mu1 + rbeta(300, 1, 10) * 80 + rnorm(300, sd = 3),
    mu2 + rbeta(700, 1, 10) * 80 + rnorm(700, sd = 3))
)

set.seed(42)
build.plot(rnorm(1000, 120, 10), rnorm(1000, 150, 10), "changepoint1.png")
build.plot(gen.multimodal(20, 250), gen.multimodal(40, 250), "changepoint2.png")
build.plot(gen.multimodal(20, 250), gen.multimodal(40, 210), "changepoint3.png")
