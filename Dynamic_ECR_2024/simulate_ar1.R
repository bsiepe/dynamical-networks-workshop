# Simulate two time series
# 1 with high lag-1 autocorrelation, 1 with low lag-1 autocorrelation
# plot with ggplot

# Load libraries
library(ggplot2)
library(tidyverse)
library(sysfonts)
library(showtext)
library(here)
library(gganimate)
set.seed(35032)

# add google font
sysfonts::font_add_google("News Cycle", "news")
# use showtext
showtext::showtext_auto()

# simulate AR(1) time series
n <- 60
ar1_high <- arima.sim(n = n, list(ar = 0.9))
ar1_low <- arima.sim(n = n, list(ar = 0.1))

ar_df <- data.frame(
  high = ar1_high,
  low = ar1_low
)

ar_df |> 
  mutate(tp = 1:n) |> 
  rename(
    "High autocorrelation (rho= 0.9)" = "high",
    "Low autocorrelation (rho = 0.1)" = "low"
  ) |> 
  pivot_longer(cols = -tp, names_to = "series") |> 
  ggplot(aes(x = tp, y = value, color = series, group = series)) +
  geom_line() +
  theme_minimal(base_family = "news") +
  labs(title = "AR(1) Time Series",
       x = "Time",
       y = "Value") +
  facet_wrap(~series, scales = "free_y", labels = label_parsed()) +
  ggplot2::theme(
    # remove minor grid
    panel.grid.minor = ggplot2::element_blank(),
    # Title and Axis Texts
    plot.title = ggplot2::element_text(face = "plain",
                                       size = 22,
                                       hjust = 0.5),
    plot.subtitle = ggplot2::element_text(size = 16,
                                          hjust = 0.5),
    axis.text.x = ggplot2::element_text(face = "plain", size = 18),
    axis.title.x = ggplot2::element_text(face = "plain", size = 18),
    axis.text.y = ggplot2::element_text(face = "plain", size = 18),
    axis.title.y = ggplot2::element_text(face = "plain", size = 18),
    axis.line = element_line(colour = "#6d6d6e"),
    
    # Faceting
    strip.text = ggplot2::element_text(face = "plain",
                                       size = 20,
                                       hjust = 0.5),
    strip.text.x.top = ggplot2::element_text(face = "plain", 
                                             size = 20,
                                             hjust = 0.5),
    # strip.text.y = element_blank(),
    strip.background = ggplot2::element_rect(fill = NA, color = NA),
    # Grid
    panel.grid = ggplot2::element_line(colour = "#F3F4F5"),
    # Legend
    legend.title = ggplot2::element_text(face = "plain"),
    legend.position = "none",
    legend.justification = 1,
    # Panel/Facets
    panel.spacing.x = ggplot2::unit(1.6, "lines"),
    panel.spacing.y = ggplot2::unit(1.6, "lines"),
    # Remove vertical grid lines
    panel.grid.major.x = ggplot2::element_blank()
    
  )



animated_plot <- ar_df |> 
  mutate(tp = 1:n) |> 
  rename(
    "High~autocorrelation~(rho==0.9)" = "high",
    "Low~autocorrelation~(rho==0.1)" = "low"
  ) |> 
  pivot_longer(cols = -tp, names_to = "series") |> 
  ggplot(aes(x = tp, y = value, color = series, group = series)) +
  geom_line() +
  theme_minimal(base_family = "news") +
  labs(title = "AR(1) Time Series",
       x = "Time",
       y = "Value") +
  scale_color_manual(values = ggokabeito::palette_okabe_ito(c(5,1)))+  
  facet_wrap(~series, scales = "free_y", labeller = label_parsed) +
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
    panel.grid.major.x = ggplot2::element_blank()
  ) +
  # Smoothly reveal the time series over time in each facet
  transition_reveal(tp) +
  geom_point()

gganimate::anim_save(here("Dynamic_ECR_2024/figures/ar1_time_series.gif"), animated_plot, 
interval = 0.25, nframes = 240, height = 4.5, width = 8, units = "in", res = 150) 
