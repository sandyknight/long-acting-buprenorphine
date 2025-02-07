# unzip(zipfile = "data/RAND-LAB-dataset.zip", exdir = "data")

library(data.table)
library(lubridate)

main_dt <- data.table::fread("data/K3anon_FullDataset_for_VfM.csv")

sir_dt <- data.table::fread("data/SIR_table_for_VfM_linked.csv")
invisible(
lapply(list.files("R", pattern = "\\.[Rr]$", full.names = TRUE), source)
)


main_dt <-
  prepare_main_data(main_data = main_dt)


main_dt <- main_dt[drug_heroin == 1, ]

sir_dt <-
  prepare_sir_data(sir_data = sir_dt)


counts_dt <-
  calculate_counts(sir_data = sir_dt,
                   main_data = main_dt,
                   end_date = "2024-10-01")

rates_dt <-
  calculate_rates(counts_dt = counts_dt)

rates_dt[outcome %in% c("Died", "Successful completion")]
