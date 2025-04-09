library(tidyverse)
library(fixest)
library(logger)
set.seed(42)

grid <- "600k_101z_600kmax"
model <- "BWznorm"

if (model == "BW") {
  beta <- 1/(1.1)
  delta <-  0.15
  psi_factor <- 1
  rho <- 0.7
  theta <- 0.7
  z_upper_bar <- 1.5
  sigma_z <- 0.5 
} else {
  beta <- 1/(1.1)
  delta <-  0.15
  psi_factor <- 1
  rho <- 0.7
  theta <- 0.7
  z_upper_bar <- 1.5
  sigma_z <- 0.5 
} 

grid_fname <- sprintf("data/grids/precomp_grid_%s_%s.csv", grid, model)

grid_df <- read_csv(grid_fname, col_types = cols())
mink <- min(grid_df$k)

find_grid_z <- function(zval) {
  grid_df %>% 
    filter (k == mink) %>%
    mutate(delta = abs(z - zval)) %>%
    filter(delta == min(delta)) %>% pull(z)
}

tmax <- 25

mat <- array(NA_real_, dim = c(tmax, 4))
colnames(mat) <- c("z", "k", "I", "V")
rownames(mat) <- paste0("t", sprintf("%02d", 1:tmax))
mat[1,] <- c(1.5, 125, NA_real_, NA_real_) 

# Model a one-time profitability shock for intuition.
for (t in 1:tmax) {
  kprime <- grid_df$kprime[grid_df$z == find_grid_z(mat[t, 'z']) & grid_df$k == mat[t, 'k']]
  mat[t, 'I'] <- kprime - (1 - delta)*mat[t, 'k']
  mat[t, 'V'] <- grid_df$v[grid_df$z == find_grid_z(mat[t, 'z']) & grid_df$k == mat[t, 'k']]
  if (t < tmax) {
    mat[t + 1, 'k'] <- kprime
    if (t == 1) mat[t + 1, 'z'] <- 2.5 
    else mat[t + 1, 'z'] <- (1 - rho) * z_upper_bar + rho*mat[t, 'z'] 
  }
}

df <- as_tibble(mat) %>%
  mutate(
    t = 1:tmax,
    pi = z*k^theta,
    psi = psi_factor * (I^2/(2*k)),
    cfo = pi - psi,
    cfi = -I,
    fcf = pi - psi - I,
    ear_cap = pi - psi - delta*k,
    ear_exp = pi - psi - I,
    ear_cap_100 = ear_cap/sum((row_number() == 1)*ear_cap, na.rm = T), 
    ear_exp_100 = ear_exp/sum((row_number() == 1)*ear_exp, na.rm = T),
    V_100 = V/sum((row_number() == 1)*V, na.rm = T)
  )

df_long <- df %>% 
  pivot_longer(!t, names_to = "var", values_to = "value")

ggplot(
  df_long %>% filter(var %in% c("ear_exp", "ear_cap")), 
  aes(x = t, y = value, group = var, color = var)
) +
  geom_point() + 
  geom_line() +
  theme_classic()

ggplot(
  df_long %>% filter(var %in% c("pi", "psi")), 
  aes(x = t, y = value, group = var, color = var)
) +
  geom_point() + 
  geom_line() +
  theme_classic()

ggplot(
  df_long %>% filter(var %in% c("cfo", "cfi", "fcf")), 
  aes(x = t, y = value, group = var, color = var)
) +
  geom_point() + 
  geom_line() +
  theme_classic()

ggplot(
  df_long %>% filter(var %in% c("ear_cap_100", "ear_exp_100", "V_100")), 
  aes(x = t, y = value, group = var, color = var)
) +
  geom_point() + 
  geom_line() +
  scale_y_continuous(labels = scales::percent) +
  scale_color_discrete(
    labels = c("Earnings (recognized)", "Earnings (expensed)", "Value")
  ) +
  labs(color = "", x = "Period", y = "") +  
  theme_classic() +
  theme(legend.position = "bottom")
