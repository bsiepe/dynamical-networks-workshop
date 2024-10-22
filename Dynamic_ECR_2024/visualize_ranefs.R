# Visualize Random Effects ------------------------------------------------
# Simulate from a normal distribution, viualize with ggdist
# to illustrate random effects of a multilevel model

# Load libraries
library(ggplot2)
library(ggdist)
library(ggrain)
library(sysfonts)
library(showtext)

# Simulate random effects
set.seed(35032)
n <- 75
random_effects <- rnorm(n, 0.2, .1)

# Plot random effects

# add google font
sysfonts::font_add_google("News Cycle", "news")
# use showtext
showtext::showtext_auto()

# Fill color from okabeito
fill_col <- ggokabeito::palette_okabe_ito(5)

plot_ranef <- ggplot(data.frame(random_effects = random_effects), 
       aes(x = 1, y = random_effects)) +
  ggrain::geom_rain(fill = fill_col,
                    alpha = 1)+
  # ggdist::stat_halfeye(fill = fill_col,
  #                      color = fill_col)+
  # stat_slab(aes(thickness = after_stat(pdf*n)), scale = 0.7)+
  theme_minimal() +
  labs(title = "",
       y = "Edge Estimate",
       x = "")+
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
  coord_flip()+
  scale_x_continuous(limits = c(.95, 1.5), labels = NULL)

ggsave("Dynamic_ECR_2024/figures/plot_ranef.png", plot_ranef, width = 4, height = 2.3, dpi = 300)

