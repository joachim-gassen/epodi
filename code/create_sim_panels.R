library(tidyverse)
library(fixest)
library(logger)
set.seed(42)

grid <- "600k_100z_600kmax"
model <- "BW"

grid_fname <- sprintf("data/grids/precomp_grid_%s_%s.csv", grid, model)

grid_df <- read_csv(grid_fname, col_types = cols())

mink <- min(grid_df$k)

N_FIRMS <- 4000
N_YEARS <- 25

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

create_panel <- function(gamma = 0, n_firms = N_FIRMS, n_years = N_YEARS, type = "random") {
  log_info("Creating panel with gamma: {gamma}")
  panel <- expand_grid(
    firm = 1:n_firms,
    year = 1:n_years,
    z = NA,
    k = NA,
    I = NA,
    ear = NA,
    accear = NA,
    dep = NA,
    ocf = NA,
    icf = NA,
    V = NA
  )
  
  find_grid_z <- function(zval) {
    grid_df %>% 
      filter (k == mink) %>%
      mutate(delta = abs(z - zval)) %>%
      filter(delta == min(delta)) %>% pull(z)
  }
  
  for (i in 1:n_firms) {
    for (y in 1:n_years) {
      pos <- (i-1)*n_years + y
      if (y == 1) {
        panel$z[pos] <- find_grid_z(rnorm(1, z_upper_bar, sigma_z)) 
        if (type == "random") panel$k[pos] <- grid_df$k[sample.int(100, 1)*100]
        else panel$k[pos] <- mink
      } else {
        panel$z[pos] <-find_grid_z((1 - rho)* z_upper_bar + rho*panel$z[pos - 1] + 
                                     rnorm(1, 0, sigma_z))
      }
      kprime <- grid_df %>% 
        filter(k == panel$k[pos], z == panel$z[pos]) %>%
        pull(kprime)
      if (y != n_years) panel$k[pos + 1] <- kprime 
      panel$dep[pos] <- delta*panel$k[pos]
      panel$I[pos] <- kprime + panel$dep[pos] - panel$k[pos]
      panel$ear[pos] <- panel$z[pos]*panel$k[pos]^theta - 
        psi_factor*(panel$I[pos]^2/(2*panel$k[pos])) - panel$dep[pos]
      panel$accear[pos] <- panel$z[pos]*panel$k[pos]^theta - 
        psi_factor*(panel$I[pos]^2/(2*panel$k[pos])) - 
        (panel$I[pos] + (1 - gamma)*(panel$dep[pos] - panel$I[pos]))
      panel$ocf[pos] <- panel$z[pos]*panel$k[pos]^theta - 
        psi_factor*(panel$I[pos]^2/(2*panel$k[pos]))
      panel$icf[pos] <- -panel$I[pos]
      panel$V[pos] <- grid_df %>% 
        filter(k == panel$k[pos], z == panel$z[pos]) %>%
        pull(v)
    }
  }
  panel
}

panels_fname <- sprintf(
  "data/generated/panels_%s_%s_%dfirms_%dyears.rds",
  grid, model, N_FIRMS, N_YEARS
)

log_info("Creating panels data. This will take a while")
panels <- bind_rows(
  lapply(
    seq(0, 1, by = 0.1),
    function(x) {
      bind_cols(tibble(gamma = x), create_panel(x))
    }
  )
)

log_info("Saving panels data to '{panels_fname}'.")
saveRDS(
  panels, panels_fname
)
