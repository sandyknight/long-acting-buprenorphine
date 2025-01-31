# unzip(zipfile = "data/RAND-LAB-dataset.zip", exdir = "data")

main_dt <- data.table::fread("data/K3anon_FullDataset_for_VfM.csv")

sir_dt <- data.table::fread("data/SIR_table_for_VfM_linked.csv")

# Inner-join on ID-JY
dt <-
  data.table::merge.data.table(main_dt, sir_dt, by = c("client_random_id", "n_jy"))

# Since the SIR table doesn't specify a drug group
# check that ID-JY pairs not in the opiate group have been included
assertthat::assert_that(unique(dt[["drug_grp"]]) == "Opiate")

# We cannot count by the `submoddt` month since we would be
# measuring the number of SIRs per month, not the number of clients.
# Assumption that
estimate_counts_per_year <-
  function(dt) {

    # Remove post-discharge SIRs since we want people in treatment

    dt <- dt[post_discharge == "N", ]

    # Select columns for identifier pair, date of SIR and LAB status
    # indicated at SIR

    dt <- dt[, .(client_random_id, n_jy, submoddt, phbudi_any)]


    # Add a year column
    dt[, year := lubridate::year(submoddt)]

    # To count clients by year we can:
    # 1. Add an Other OST column, since we'd otherwise lost non-LAB
    # clients in aggregation.
    dt[, other_ost := data.table::fifelse(phbudi_any == 1L, 0L, 1L)]
    # 2. Aggregate by year AND unique client ID
    dt <-
      dt[, lapply(.SD, sum),                     # sum aggregation
         by = .(client_random_id, year),         # by unique ID and year
         .SDcols = c("phbudi_any", "other_ost")] # across the OST binary cols

    # 3. Here we have to make a choice to count any client who was indicated as
    # receiving LAB at one or more SIR in a calendar year as a LAB client for
    # that year, otherwise they are counted as other OST.
    # By this choice we can convert the OST cols back from count to binary:
    dt <-
      dt[, lapply(.SD, function(x) data.table::fifelse(x > 0, 1, 0)),
        by = year,
        .SDcols = c("phbudi_any", "other_ost")
      ]

    # Add a column with implied toal OST clients
    dt[, total_ost := phbudi_any + other_ost]

    # Sum by year
    dt <-
      dt[, lapply(.SD, sum), by = year]

  }


library(ggplot2)

afcharts::use_afcharts()

sir_dt |>
  ggplot(aes(x = year)) +
  geom_col(aes(y = phbudi_any))


sir_dt |>
  ggplot(aes(x = year)) +
  geom_col(aes(y = phbudi_any/total_ost))

sir_dt |>
  tidyr::pivot_longer(cols = c(phbudi_any, other_ost)) |>
  ggplot(aes(x = year, y = value, group = name)) +
  geom_col(aes(fill = name))
