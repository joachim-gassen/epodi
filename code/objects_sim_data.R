suppressWarnings(suppressPackageStartupMessages({
  library(tidyverse)
  library(fixest)
  library(logger)
}))

set.seed(42)

PANELS_FNAME <- "data/generated/panels_600k_100z_600kmax_BW_4000firms_25years.rds"
if (!file.exists(PANELS_FNAME)) {
  stop("Please run create_sim_panels.R to generate the panels data.")
}
panels_sim <- readRDS(PANELS_FNAME) %>%
  group_by(gamma, firm) %>%
  mutate(
    lag_accear = lag(accear)
  )

grid <- "600k_101z_600kmax"
model <- "BWznorm"
beta <- 1/(1.1)
delta <-  0.15
psi_factor <- 1
rho <- 0.7
theta <- 0.7
z_upper_bar <- 1.5
sigma_z <- 0.5 

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

fig_sim_shock_rec_exp <- ggplot(
  df_long %>% filter(var %in% c("ear_exp", "ear_cap")), 
  aes(x = t, y = value, group = var, color = var)
) +
  geom_point() + 
  geom_line() +
  theme_classic()

fig_sim_shock_pi_psi <- ggplot(
  df_long %>% filter(var %in% c("pi", "psi")), 
  aes(x = t, y = value, group = var, color = var)
) +
  geom_point() + 
  geom_line() +
  theme_classic()

fig_sim_shock_cfs <- ggplot(
  df_long %>% filter(var %in% c("cfo", "cfi", "fcf")), 
  aes(x = t, y = value, group = var, color = var)
) +
  geom_point() + 
  geom_line() +
  scale_color_discrete(
    labels = c(
      "Cash flow from investing", "Cash flow from operations", "Free cash flow"
    )
  ) +
  labs(color = "", x = "Period", y = "") +  
  theme_classic()

fig_sim_shock_ear_price <- ggplot(
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
  theme_classic() 

reg_results <- function(g) {
  df <- panels_sim %>% filter(gamma == g) %>% group_by(firm) %>%
    mutate(
      lag_accear = lag(accear),
      price_bw = V + (1 - delta)*k + I,
      price = V
    )
  n_firms = n_distinct(df$firm)
  n_years = n_distinct(df$year)
  mod_pers <- feols(accear ~ lag_accear | firm, data = df)
  mod_pe <- feols(price ~ accear | firm, data = df)
  bind_cols(
    tibble(
      n_firms = n_firms, n_years = n_years, gamma = g
    ),
    bind_rows(
      tibble(
        model = "persistence",
        coef = mod_pers$coeftable[1, 1],
        se = mod_pers$coeftable[1, 2],
        n = n_firms*(n_years - 1),
        clusters = n_firms,
        r2 = r2(mod_pers)['ar2']
      ),
      tibble(
        model = "price_earnings",
        coef = mod_pe$coeftable[1, 1],
        se = mod_pe$coeftable[1, 2],
        n = n_firms*n_years,
        clusters = n_firms,
        r2 = r2(mod_pe)['ar2']
      )
    )
  )  
}

table_sim_regs <- bind_rows(lapply(seq(0, 1, by = 0.1), reg_results))

fig_sim_regcoef_persistence <- ggplot(
  table_sim_regs %>% filter(model == "persistence"),
  aes(x = gamma, y = coef)
) + geom_line() + geom_point() + 
  labs(
    x = "γ (0 = all recognized, 1 = all expensed)", y = ""
  ) +
  theme_classic()

fig_sim_regcoef_pemultiple <- ggplot(
  table_sim_regs %>% filter(model == "price_earnings"), 
  aes(x = gamma, y = coef)
) +
  geom_line() + geom_point() + 
  labs(
    x = "γ (0 = all recognized, 1 = all expensed)", y = "",
  ) +
  theme_classic()

fig_sim_scatter_persistence <- function(g) {
  ggplot(
    panels_sim %>% filter(near(gamma, g)), 
    aes(x = lag_accear, y = accear)
  ) + 
    geom_point(alpha = 0.25) + 
    labs(x = "Earnings (t -1)", y = "Earnings (t)") +
    theme_classic() 
}

fig_sim_scatter_pemultiple <- function(g) {
  ggplot(panels_sim %>% filter(near(gamma, g)), aes(x = accear, y = V)) + 
    geom_point(alpha = 0.25) + 
    labs(x = "Earnings (t)", y = "Value (t)") +
    theme_classic() 
}