suppressWarnings(suppressPackageStartupMessages({
  library(tidyverse)
  library(fixest)
}))

PANELS_FNAME <- "data/generated/panels_600k_100z_600kmax_BW_4000firms_25years.rds"

if (!file.exists(PANELS_FNAME)) {
  stop("Please run create_sim_panels.R to generate the panels data.")
}
panels <- readRDS(PANELS_FNAME)

reg_results <- function(g) {
  df <- panels %>% filter(gamma == g) %>% group_by(firm) %>%
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

r <- bind_rows(lapply(seq(0, 1, by = 0.1), reg_results))

ggplot(
  rr %>% filter(model == "persistence"),
  aes(x = gamma, y = coef)
) + geom_line() + geom_point() + 
  labs(
    x = "γ", y = "", title = "Coefficent of Persistence Regression",
    subtitle = "Compare to Table 1, Panel A of BW(2025)"
  ) +
  theme_classic()

ggplot(rr %>% filter(model == "price_earnings"), aes(x = gamma, y = coef)) +
  geom_line() + geom_point() + 
  labs(
    x = "γ", y = "", title = "Coefficent of Price-Earnings Regression",
    subtitle = "Compare to Table 1, Panel B of BW(2025)"
  ) +
  theme_classic()

if (FALSE) {
  df <- panels %>% filter(gamma == 0.5) %>%
    group_by(firm) %>%
    mutate(
      lag_accear = lag(accear),
      total_assets = (1 - delta)*k + I
    )
  
  ggplot(df, aes(x = lag_accear, y = accear)) + 
    geom_point(alpha = 0.25) + 
    theme_classic()
  
  ggplot(df, aes(x = lag_accear/total_assets, y = accear/total_assets)) + 
    geom_point(alpha = 0.25) + 
    theme_classic()
  
  ggplot(df, aes(x = accear, y = V)) + 
    geom_point(alpha = 0.25) + 
    theme_classic()

  ggplot(df, aes(x = accear/total_assets, y = V/total_assets)) + 
    geom_point(alpha = 0.25) + 
    theme_classic()
}
