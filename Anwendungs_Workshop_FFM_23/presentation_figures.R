
# Detrending --------------------------------------------------------------
fn_detrend <- function(x,                    # Datensatz
                       vars,                 # Vektor mit Variablennamen
                       time_var = "time",    # Zeitvariable
                       sig_only = FALSE){    # nur signifikante Trends detrenden?
  for (v in 1:length(vars)){
    # Regress on time
    lm_form <- as.formula(paste0(vars[v], "~", time_var))
    lm_res <- summary(lm(lm_form, data = x))
    # detrend with residuals
    # [,4] accesses p-values
    # [2] p-value of beta of tp
    if(sig_only){
      if(lm_res$coefficients[,4][2] < 0.05){
        x[!is.na(x[vars[v]]),vars[v]] <- residuals(lm_res)
      }
    }
    if(isFALSE(sig_only)){
      x[!is.na(x[vars[v]]),vars[v]] <- residuals(lm_res)
    }
    
  }
  return(x)
}

# Simulate

set.seed(123)
n <- 300
time_series <- data.frame(y = ts(rnorm(n, mean = 0, sd = 4) + 0.1 * seq(1, n), start = 1),
                          time = 1:300)

# Plot
pre <- ggplot(data = time_series, 
       aes(x = time, y = y))+
  geom_line()+
  warnd::theme_warnd()+
  labs(x = "Zeit",
       y = "")+
  theme(axis.text = element_text(size = 13),
        axis.title = element_text(size = 15))


time_series_detrend <- fn_detrend(x = time_series,
                                  vars = "y",
                                  time_var = "time")


post <- ggplot(data = time_series_detrend, 
       aes(x = time, y = y))+
  geom_line()+
  warnd::theme_warnd()+
  labs(x = "Zeit",
       y = "")+
  theme(axis.text = element_text(size = 13),
        axis.title = element_text(size = 15))


# Combine plots

pg <- cowplot::plot_grid(pre, NULL, post, 
                   nrow = 1,
                   labels = c("Vor Detrending", "", "Nach Detrending"),
                   rel_widths = c(1, 0.15, 1),
                   label_fontfamily = "news",
                   label_size = 18,
                   scale = 0.95)
ggsave("detrending_grid.svg", pg, device = "svg", 
       path = here("Anwendungs_Workshop/figures/"),
       height = 6, width = 12)



