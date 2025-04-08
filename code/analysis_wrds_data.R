suppressPackageStartupMessages({
  library(tidyverse)
  library(arrow)
  library(fixest)
})

ccm <- read_parquet("data/wrds/crsp/ccmxpf_linktable.parquet")
crsp <- read_parquet("data/wrds/crsp/msf.parquet")
comp <- read_parquet("data/wrds/comp/company.parquet")
funda <- read_parquet("data/wrds/comp/funda.parquet") %>%
  filter(
    indfmt == "INDL", consol == "C", curcd == "USD", pddur == 12,
    datafmt == 'STD', popsrc == 'D', fic == "USA", at > 0
  ) %>% 
  inner_join(
    comp %>% select(gvkey, sic) %>% 
      filter(
        substr(sic, 1, 1) != 6, as.numeric(sic) > 4949 | as.numeric(sic) < 4900
      ),
    by = "gvkey"
  )

dups <- funda %>%
  group_by(gvkey, fyear) %>%
  mutate(nobs = n()) %>%
  filter(nobs > 1)

if (nrow(dups) > 0) stop (
  "Compustat has duplicate firm-year observations. Needs fix."
)

smp <- funda %>%
  mutate(
    earnings = ib - dvp - (1 - ifelse(fyear >= 2018, 0.21, 0.35))*spi,
    price = mkvalt,
    total_assets = at
  ) %>%
  group_by(gvkey, sic, conm) %>%
  arrange(gvkey, fyear) %>%
  mutate(lag_earnings = lag(earnings)) %>%
  filter(!is.na(earnings), !is.na(price), !is.na(lag_earnings)) %>%
  filter(fyear >= 1975 & fyear <= 2020) %>%
  select(
    gvkey, sic, conm, fyear, total_assets, price, earnings, lag_earnings
  ) %>%
  filter(n() >= 10) %>%
  ungroup()
  
ggplot(smp, aes(x = earnings, y = price)) + 
  geom_point(alpha = 0.25) + 
  scale_x_log10() + scale_y_log10() +
  theme_classic()

ggplot(smp, aes(x = earnings/total_assets, y = price/total_assets)) + 
  geom_point(alpha = 0.25) + 
  scale_x_log10() + scale_y_log10() +
  theme_classic()
