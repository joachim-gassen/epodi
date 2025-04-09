suppressPackageStartupMessages({
  library(tidyverse)
  library(fixest)
})

smp <- readRDS("data/generated/panel_wrds.rds") 

ggplot(smp, aes(x = lag_earnings, y = earnings)) + 
  geom_point(alpha = 0.25) + 
  scale_x_log10() + scale_y_log10() +
  theme_classic()

ggplot(smp, aes(x = lag_earnings_ta, y = earnings_ta)) + 
  geom_point(alpha = 0.25) + 
  scale_x_log10() + scale_y_log10() +
  theme_classic()

ggplot(smp, aes(x = earnings, y = price)) + 
  geom_point(alpha = 0.25) + 
  scale_x_log10() + scale_y_log10() +
  theme_classic()

ggplot(smp, aes(x = earnings_ta, y = price_ta)) + 
  geom_point(alpha = 0.25) + 
  scale_x_log10() + scale_y_log10() +
  theme_classic()


feols(earnings ~ lag_earnings | gvkey + fyear, data = smp)
feols(earnings_ta ~ lag_earnings_ta | gvkey + fyear, data = smp)
feols(price ~ earnings | gvkey + fyear, data = smp)
feols(price_ta ~ earnings_ta | gvkey + fyear, data = smp)
