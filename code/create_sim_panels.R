suppressWarnings(suppressPackageStartupMessages({
  library(tidyverse)
  library(fixest)
  library(logger)
}))

set.seed(42)

grid <- "600k_100z_600kmax"
model <- "BW"

grid_fname <- sprintf("data/grids/grid_%s_%s.csv", grid, model)

grid_df <- read_csv(grid_fname, col_types = cols())

mink <- min(grid_df$k)
zs <- unique(grid_df$z)
ks <- unique(grid_df$k)

N_FIRMS <- 4000
N_YEARS <- 25

if (str_detect(model, fixed("BW"))) {
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

create_base_panel <- function(n_firms, n_years, type) {
  df <- expand_grid(
    firm = 1:n_firms,
    year = 1:n_years
  )
  
  find_grid_z <- function(zval) {
    delta <- abs(zs - zval)
    zs[delta == min(delta)]
  }
  
  find_kprime_v <- function(z, k) {
    unlist(unname(grid_df[near(grid_df$z, z) & near(grid_df$k, k),3:4]))
  }
  
  a <- array(NA_real_, dim = c(n_years, 4, n_firms))
  for (y in 1:n_years) {
    if (y == 1) {
      a[y, 1, ]  <- unlist(lapply(
        rnorm(n_firms, z_upper_bar, sigma_z), find_grid_z
      )) 
      if (type == "random") a[y, 2, ]  <-  sample(ks, n_firms, replace = TRUE)
      else a[y, 2, ] <- mink
    } else {
      a[y, 1, ] <- unlist(lapply(
        (1- rho) * z_upper_bar + rho*a[y - 1, 1, ] + 
          rnorm(n_firms, 0, sigma_z), 
        find_grid_z 
      ))
    }
    a[y, 3:4, ] <- apply(a[y, 1:2, ], 2, function(x) find_kprime_v(x[1], x[2]))
    if (y < n_years) a[y + 1, 2, ] <- a[y, 3, ]
  }
  
  colnames(a) <- c("z", "k", "kprime", "V")
  df2 <- bind_rows(lapply(1:n_firms, function(x) as_tibble(a[,,x])))
  bind_cols(df, df2)  
}

create_panel <- function(
    gamma = 0, n_firms = N_FIRMS, n_years = N_YEARS, type = "random", base_panel = NULL
) {
  log_info("Creating panel with gamma: {gamma}")
  if (is.null(base_panel)) {
    base_panel <- create_base_panel(n_firms, n_years, type)
  }
   base_panel %>%
    mutate(
      dep = delta*k,
      I =  kprime + dep - k,
      ear = z*k^theta - psi_factor*(I^2/(2*k)) - dep,
      accear = z*k^theta - psi_factor*(I^2/(2*k)) - (I + (1 - gamma)*(dep - I)),
      ocf = z*k^theta - psi_factor*(I^2/(2*k)),
      icf = -I
    )
}

panels_fname <- sprintf(
  "data/generated/panels_%s_%s_%dfirms_%dyears.rds",
  grid, model, N_FIRMS, N_YEARS
)

log_info("Creating simulated panels. This will take a while")
log_info("Creating base panel")
base_panel <- create_base_panel(N_FIRMS, N_YEARS, "growth")
panels <- bind_rows(
  lapply(
    seq(0, 1, by = 0.1),
    function(x) {
      bind_cols(
        tibble(gamma = x), 
        create_panel(x, type = "growth", base_panel = base_panel)
      )
    }
  )
)

log_info("Saving simulated panels to '{panels_fname}'.")
#saveRDS(panels, panels_fname)
