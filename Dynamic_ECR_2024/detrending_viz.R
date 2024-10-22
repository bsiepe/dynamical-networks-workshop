# Visualize removal of linear trend via detrending ------------------------
set.seed(35037)
library(tidyverse)


n <- 100
time <- 1:n
trend <- 0.5 * time   # Linear trend
noise <- rnorm(n, mean = 0, sd = 5)
time_series <- trend + noise
model <- lm(time_series ~ time)
detrended_series <- residuals(model)

# Combine into dataframe
df <- data.frame(time, time_series, detrended_series)

# add google font
sysfonts::font_add_google("News Cycle", "news")
# use showtext
showtext::showtext_auto()

# pivot longer
df_long <- df |> 
  pivot_longer(cols = c(detrended_series, time_series), names_to = "series", values_to = "value")

plot_detrend <- df_long |> 
  # give proper names to the time series
  mutate(series = ifelse(series == "time_series", "Raw Series", "Detrended Series")) |>
  mutate(series = factor(series, levels = c("Raw Series", "Detrended Series"))) |> 
  ggplot(aes(x = time, y = value, color = series)) +
  # draw line through the time series
  geom_smooth(method = "lm", se = FALSE, aes(alpha = .7))+
  geom_line() +
  facet_wrap(~series) +
  labs(x = "Time",
       y = "Value") +
  theme_minimal()+
  ggplot2::theme(
    panel.grid.minor = ggplot2::element_blank(),
    plot.title = ggplot2::element_text(face = "plain", size = 22, hjust = 0.5),
    plot.subtitle = ggplot2::element_text(size = 16, hjust = 0.5),
    axis.text.x = ggplot2::element_text(face = "plain", size = 18),
    axis.title.x = ggplot2::element_text(face = "plain", size = 18),
    axis.text.y = ggplot2::element_text(face = "plain", size = 18),
    axis.title.y = ggplot2::element_text(face = "plain", size = 18),
    axis.line = element_line(colour = "#6d6d6e"),
    strip.text = ggplot2::element_text(face = "plain", size = 20, hjust = 0.5),
    strip.text.x.top = ggplot2::element_text(face = "plain", size = 20, hjust = 0.5),
    strip.background = ggplot2::element_rect(fill = NA, color = NA),
    panel.grid = ggplot2::element_line(colour = "#F3F4F5"),
    legend.title = ggplot2::element_text(face = "plain"),
    legend.position = "none",
    legend.justification = 1,
    panel.spacing.x = ggplot2::unit(1.6, "lines"),
    panel.spacing.y = ggplot2::unit(1.6, "lines"),
    panel.grid.major.x = ggplot2::element_blank(),
    panel.grid.major.y = ggplot2::element_blank()
  )+
  scale_color_manual(values = ggokabeito::palette_okabe_ito(c(5,1)))


ggsave("Dynamic_ECR_2024/figures/detrending_viz.svg", plot_detrend, width = 10, height = 6)
