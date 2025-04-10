# ------------------------------------------------------------------------------
# Downloads WRDS data to local parquet files using a duckdb/postgres workflow
# See LICENSE file for licensing ;-) 
# ------------------------------------------------------------------------------

# Good starting points to learn more about this workflow are
# - The support pages of WRDS (they also contain the data documentation)
# - The wonderful textbook by Ian Gow (https://iangow.github.io/far_book/),
#   in particular App. D and E

suppressWarnings(suppressPackageStartupMessages({
  library(duckdb)
  library(logger)
}))

# By default the code only downloads missing data. Set the below
# to TRUE if you want to re-download everything.
FORCE_REDOWNLOAD <- FALSE

if (file.exists("config.env")) readRenviron("config.env") else {
  stop(paste(
    "Please copy '_config.env' to 'config.env' and edit it to", 
    "contain your WRDS access data prior to running this code"
  ))
}

link_wrds_to_duckdb <- function(con) {
  rv <- dbExecute(
    con, sprintf(paste(
      "INSTALL postgres;",
      "LOAD postgres;",
      "SET pg_connection_limit=4;",
      "ATTACH '",
      "dbname=wrds host=wrds-pgdata.wharton.upenn.edu port=9737",
      "user=%s password=%s' AS wrds (TYPE postgres, READ_ONLY)"
    ), Sys.getenv("WRDS_USER"), Sys.getenv("WRDS_PWD"))
  )
}

list_wrds_libs_and_tables <- function(con) {
  dbGetQuery(
    con, "SHOW ALL TABLES"
  )
}

download_wrds_table <- function(con, lib, table, path = "data/wrds") {
  time_in <- Sys.time()
  lib_path <- file.path(path, lib)
  parquet_fpath <- file.path(path, lib, paste0(table, ".parquet"))
  if (file.exists(parquet_fpath) & ! FORCE_REDOWNLOAD) {
    log_info(
      "Parquet file '{parquet_fpath}' exists. ",
      "Skipping download but updating its mtime."
    )
    Sys.setFileTime(parquet_fpath, Sys.time())
    return(invisible())
  }
  log_info(
    "Download WRDS data to parquet file '{parquet_fpath}'..."
  )
  if(!dir.exists(lib_path)) {
    dir.create(lib_path)
  }
  rv <- dbExecute(
    con, sprintf(
      "COPY wrds.%s.%s TO '%s'", lib, table, parquet_fpath
    )
  )
  time_spent <- round(Sys.time() - time_in)
  log_info(
    'Copied {lib}.{table} to parquet ({format(rv, big.mark = ",")} rows, ', 
    "time spent: {as_hms(time_spent)})"
  )
}

log_info("Start WRDS Download")

db <- dbConnect(
  duckdb::duckdb(), "data/wrds/wrds_local.duckdb", 
)

link_wrds_to_duckdb(db)
log_info("Linked WRDS to local Duck DB instance...")

# rv <- list_wrds_libs_and_tables(db)

download_wrds_table(db, "comp", "company") 
download_wrds_table(db, "comp", "funda") 
download_wrds_table(db,"crsp", "msf")
download_wrds_table(db, "crsp", "ccmxpf_linktable")

dbDisconnect(db, shutdown = TRUE)
log_info("Done downloading WRDS data")
