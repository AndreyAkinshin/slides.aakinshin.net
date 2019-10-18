library(ggplot2)
build.df <- function(label, dt) {
  dens <- density(dt$y, bw = "SJ")
  df <- data.frame(Situation = label, x=dens$x, y=dens$y)
  probs <- seq(0.2, 0.8, by = 0.2)
  quantiles <- quantile(dt$y, prob=probs)
  df$quant <- factor(findInterval(df$x,quantiles))
  return(df)
}

set.seed(42)
df <- rbind(
  build.df("Before", data.frame(x = c(1:30), y = rnorm(30, 20, sd = 3))),
  build.df("After", data.frame(x = c(1:30), y = c(rnorm(15, 17), rnorm(15, 27))))
)

ggplot(df, aes(x, y)) +
  geom_line() +
  geom_ribbon(aes(ymin=0, ymax=y, fill=quant)) +
  facet_grid(rows = vars(Situation)) +
  #scale_x_continuous(breaks = quantiles) +
  scale_fill_brewer(guide = "none", palette = "Spectral")
ggsave("shift-build-raw.png")
