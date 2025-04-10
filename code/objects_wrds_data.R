suppressPackageStartupMessages({
  library(tidyverse)
})

panel_wrds <- readRDS("data/generated/panel_wrds.rds") 
panel_wrds_winsorized <- ExPanDaR::treat_outliers(panel_wrds)

fig_wrds_scatter_persistence <- function(df) {
  ggplot(
    df, aes(x = lag_earnings, y = earnings)
  ) + 
    geom_point(alpha = 0.25) + 
    labs(x = "Earnings (t -1)", y = "Earnings (t)") +
    theme_classic()
}

fig_wrds_scatter_pemultiples <- function(df) {
  ggplot(df, aes(x = earnings_per_share, y = price_per_share)) + 
    geom_point(alpha = 0.25) + 
    labs(x = "EPS (t)", y = "Share Price (t)") +
    theme_classic()
}

