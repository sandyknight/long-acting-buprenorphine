library(data.table)
library(lubridate)
library(janitor)

# Read and merge datasets
main_dt <- fread("data/K3anon_FullDataset_for_VfM.csv")
sir_dt  <- fread("data/SIR_table_for_VfM_linked.csv")
dt      <- merge(main_dt, sir_dt, by = c("client_random_id", "n_jy"))


# ----------------Validation ---------------
# Check that the drug group is "Opiate"
stopifnot(unique(dt$drug_grp) == "Opiate")
# ------------------------------------------

# Convert date columns and create a year column
date_cols <- c("submoddt", "disd", "triaged")

dt[, (date_cols) := lapply(.SD, as_date), .SDcols = date_cols]

dt[, year := year(submoddt)]

dt <- dt[year > 2019, ]

# Recode disrsn to have fewer categories
dt[, disrsn := fifelse(is.na(disd), "Retained",
                       fifelse(disrsn %chin% c("Exit reason inconsistent",
                                               "Incomplete/other",
                                               "Moved away, referred on or transferred not in custody"),
                               "Other", disrsn))]

# Create yearly date boundaries:
yrs <- sort(unique(dt$year))

# Instead of a loop, subtract a sequence of years from the maximum date:
date_vec  <- max(dt$submoddt) - years(seq_along(yrs))

btwn_dates <- data.table(
  index     = seq_along(date_vec),
  startdate = date_vec,
  enddate   = date_vec + years(1)
)

btwn_dates[, year := lubridate::year(enddate)]

btwn_dates

aggregate_annual_data <-
  function(dt, i, dates = btwn_dates) {

    date_range <- unlist(btwn_dates[index == i, .(startdate, enddate)])

    # Aggregation
    dt_agg <- dt[
      submoddt %between% date_range,
      .(submoddt = max(submoddt),
        phbudi_any = fifelse(sum(phbudi_any) >= 1, "depot_bupe", "no_depot_bupe")),
      by = .(client_random_id, n_jy, disrsn)
    ][
      , .N, by = .(disrsn,
                   phbudi_any = factor(phbudi_any,
                                       levels = c("depot_bupe",
                                                  "no_depot_bupe")))
    ]

    # Pivot
    dt_cast <-
      data.table::dcast.data.table(dt_agg,
                                   disrsn ~ phbudi_any,
                                   value.var = "N",
                                   fill = 0)

    dt_final <-
      dt_cast[
        , `:=`(
          depot_total    = sum(depot_bupe),
          no_depot_total = sum(no_depot_bupe),
          depot_rate     = depot_bupe / sum(depot_bupe),
          no_depot_rate  = no_depot_bupe / sum(no_depot_bupe)
        )
      ][
      , .(disrsn, depot_bupe, no_depot_bupe, depot_rate, no_depot_rate)
    ]


    dt_final[, year := unlist(btwn_dates[index == i, .(year)])]


    return(dt_final)
  }



dt_list <-
  mapply(btwn_dates[["index"]],
         MoreArgs = list(dt = dt),
         FUN = aggregate_annual_data,
         SIMPLIFY = FALSE)

dt_res <-
   data.table::rbindlist(dt_list)


dt_res
