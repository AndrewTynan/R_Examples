
# https://gist.github.com/andrewheiss/6ae82cbc48102b06227b310e0adbd26e?utm_source=pocket_reader

library(tidyverse)
library(brms)
library(tidybayes)

mpg_model <- brm(
  bf(hwy ~ 0 + drv), 
  data = mpg,
  prior = c(
    set_prior("normal(20, 3)", class = "b", lb = 0, ub = 40),
    set_prior("student_t(3, 0, 10)", class = "sigma")
  ),
  seed = 1234)


# TIRED
ggplot(mpg, aes(y = drv, x = hwy, color = drv)) +
  stat_summary(geom = "pointrange", fun.data = "mean_se", 
               fun.args = list(mult = 1.96), size = 1) +
  scale_color_viridis_d(guide = FALSE) +
  labs(x = "Highway MPG", y = "Drive", title = "TIRED",
       subtitle = "Group-specific averages with 95% confidence intervals") +
  coord_cartesian(xlim = c(17, 30)) +
  theme_bw() +
  theme(plot.title = element_text(face = "bold", size = rel(2), hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))
ggsave("1.png", width = 5, height = 4, type = "cairo", dpi = 300)


# WIRED
mpg_draws <- mpg_model %>% 
  gather_draws(`b_.*`, regex = TRUE) %>% 
  ungroup() %>% 
  mutate(.variable = str_remove(.variable, "b_drv"))

ggplot(mpg_draws, aes(y = .variable, x = .value, fill = .variable)) +
  stat_halfeyeh() +
  scale_fill_viridis_d(guide = FALSE) +
  labs(x = "Highway MPG", y = "Drive", title = "WIRED",
       subtitle = "Group-specific draws from a Bayesian model") +
  coord_cartesian(xlim = c(17, 30)) +
  theme_bw() +
  theme(plot.title = element_text(face = "bold", size = rel(2), hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))
ggsave("2.png", width = 5, height = 4, type = "cairo", dpi = 300)


# INSPIRED
mpg_posterior_preds <- mpg %>% 
  modelr::data_grid(drv) %>% 
  add_predicted_draws(mpg_model, seed = 1234)

ggplot(mpg_posterior_preds, aes(y = drv, x = .prediction, color = drv)) +
  stat_dotsh(quantiles = 100, shape = 19) +
  scale_color_viridis_d(guide = FALSE) +
  labs(x = "Highway MPG", y = "Drive", title = "INSPIRED",
       subtitle = "Group-specific posterior predictions from a Bayesian model") +
  theme_bw() +
  theme(plot.title = element_text(face = "bold", size = rel(2), hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))
ggsave("3.png", width = 5, height = 4, type = "cairo", dpi = 300)