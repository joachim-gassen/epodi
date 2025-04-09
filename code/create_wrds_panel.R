suppressWarnings(suppressPackageStartupMessages({
  library(tidyverse)
  library(arrow)
  library(fixest)
  library(logger)
}))

ccm <- read_parquet("data/wrds/crsp/ccmxpf_linktable.parquet")
crsp <- read_parquet("data/wrds/crsp/msf.parquet")
comp <- read_parquet("data/wrds/comp/company.parquet")

ccmm <- crsp %>% left_join(
  ccm, by = c("permno" = "lpermno", "permco" = "lpermco"),
  relationship = "many-to-many"
) %>% filter(
  !is.na(gvkey), linkprim %in% c("P", "C"),
  date >= linkdt, (date <= linkenddt) | is.na(linkenddt)
) %>% 
  mutate(
    price = (abs(prc) * shrout)/1000
  ) %>%
  select(
    gvkey, permno, permco, date, usedflag, linkprim, linktype, price, shrout
  )

dups <- ccmm %>%
  group_by(gvkey, date) %>%
  mutate(nobs = n()) %>%
  filter(nobs > 1) %>%
  arrange(gvkey, date)

if (nrow(dups) > 0) stop (
  "CRSP/Copustat merged has duplicate firm-date observations. Needs fix."
)

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
    fye = datadate,
    date = ceiling_date(fye %m+% months(3), "month") - days(1),
    earnings = ib - dvp - (1 - ifelse(fyear >= 2018, 0.21, 0.35))*spi,
    price_cstat = mkvalt,
    total_assets = at
  ) %>%
  group_by(gvkey, sic, conm) %>%
  arrange(gvkey, fyear) %>%
  left_join(
    ccmm %>% select(gvkey, date, price, shrout), by = c("gvkey", "date")
  ) %>%
  mutate(
    lag_earnings = lag(earnings),
    lag_total_assets = lag(total_assets),
    earnings_ta = earnings/lag_total_assets,
    lag_earnings_ta = lag_earnings/lag_total_assets,
    price_ta = price/lag_total_assets,
    earnings_per_share = earnings/(shrout/1000),
    price_per_share = price/(shrout/1000)
  ) %>%
  filter(
    !is.na(earnings), !is.na(lag_earnings), !is.na(price),
    !is.na(earnings_ta), !is.na(lag_earnings_ta), !is.na(price_ta)
  ) %>%
  filter(fyear >= 1975 & fyear <= 2020) %>%
  select(
    gvkey, sic, conm, fyear, fye, date, 
    total_assets, earnings, lag_earnings,
    price, price_cstat, 
    earnings_per_share, price_per_share,
    earnings_ta, lag_earnings_ta, price_ta
  ) %>%
  filter(n() >= 10) %>%
  ungroup()

log_info(
  "Sample contains {format(nrow(smp), big.mark = ',')} observations, ",
  "correlation between Compustat and CRSP market values is ",
  "{sprintf('%.3f', cor(smp$price, smp$price_cstat, use = 'complete.obs'))}."
)

saveRDS(smp, "data/generated/panel_wrds.rds")
