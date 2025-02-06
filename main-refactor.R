library(data.table)
library(lubridate)
library(janitor)

# Read and merge datasets
main_dt <- fread("data/K3anon_FullDataset_for_VfM.csv")
sir_dt <- fread("data/SIR_table_for_VfM_linked.csv")
dt <- merge(main_dt, sir_dt, by = c("client_random_id", "n_jy"))


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
dt[, disrsn := fifelse(
  is.na(disd), "Retained",
  fifelse(
    disrsn %chin% c(
      "Exit reason inconsistent",
      "Incomplete/other",
      "Moved away, referred on or transferred not in custody"
    ),
    "Other", disrsn
  )
)]

# Create yearly date boundaries:
yrs <- sort(unique(dt$year))

# Instead of a loop, subtract a sequence of years from the maximum date:
date_vec <- max(dt$submoddt) - years(seq_along(yrs))

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
      .(
        submoddt = max(submoddt),
        phbudi_any = fifelse(sum(phbudi_any) >= 1, "depot_bupe", "no_depot_bupe")
      ),
      by = .(client_random_id, n_jy, disrsn)
    ][
      , .N,
      by = .(disrsn,
        phbudi_any = factor(phbudi_any,
          levels = c(
            "depot_bupe",
            "no_depot_bupe"
          )
        )
      )
    ]

    # Pivot
    dt_cast <-
      data.table::dcast.data.table(dt_agg,
        disrsn ~ phbudi_any,
        value.var = "N",
        fill = 0
      )

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
    SIMPLIFY = FALSE
  )

dt_res <-
  data.table::rbindlist(dt_list)


dt_rr <-
  dt_res[, .(year, disrsn, depot_bupe, no_depot_bupe)]


dt_rr[, disrsn := data.table::fifelse(
  disrsn == "Died",
  "Died",
  "Did not die"
)]


dt_rr <-
  dt_rr[, lapply(.SD, sum), by = .(year, disrsn)]



# +-----------------+------------------+------------+
# | Group           | Treatment (T)    | Control (C)|
# +-----------------+------------------+------------+
# | Events (E)      | IE               | CE         |
# | Non-events (N)  | IN               | CN         |
# +-----------------+------------------+------------+


calculate_rr_point_estimate <-
  function(dt, yr, invert = "false") {
    switch(invert,
      "false" = {
        te <- dt[year == yr, ][disrsn == "Died", .(depot_bupe)]
        tn <- dt[year == yr, ][disrsn == "Did not die", .(depot_bupe)]
        ce <- dt[year == yr, ][disrsn == "Died", .(no_depot_bupe)]
        cn <- dt[year == yr, ][disrsn == "Did not die", .(no_depot_bupe)]
      },
      "true" = {
        te <- dt[year == yr, ][disrsn == "Died", .(no_depot_bupe)]
        tn <- dt[year == yr, ][disrsn == "Did not die", .(no_depot_bupe)]
        ce <- dt[year == yr, ][disrsn == "Died", .(depot_bupe)]
        cn <- dt[year == yr, ][disrsn == "Did not die", .(depot_bupe)]
      }
    )

    rr <- (te * (ce + cn)) / (ce * (te + tn))

    rr <- as.numeric(rr)

    return(rr)
  }


rr_depot <-
  vapply(
    X = yrs,
    FUN.VALUE = numeric(1),
    FUN = function(x) {
      calculate_rr_point_estimate(
        dt = dt_rr,
        yr = x,
        invert = "false"
      )
    }
  )


rr_no_depot <-
  vapply(
    X = yrs,
    FUN.VALUE = numeric(1),
    FUN = function(x) {
      calculate_rr_point_estimate(
        dt = dt_rr,
        yr = x,
        invert = "true"
      )
    }
  )

rr_res_dt <-
  data.frame(
    "year" = yrs,
    "mortality_RR_depot" = rr_depot,
    "mortality_RR_no_depot" = rr_no_depot
  )
rr_res_dt

# CIs
calculate_rr_CIs <-
  function(dt, yr, invert = "false", confidence_level = 0.95) {
    z_alpha <- qnorm((1 - ((1 - confidence_level) / 2)))

    switch(invert,
      "false" = {
        te <- dt[year == yr, ][disrsn == "Died", .(depot_bupe)]
        tn <- dt[year == yr, ][disrsn == "Did not die", .(depot_bupe)]
        ce <- dt[year == yr, ][disrsn == "Died", .(no_depot_bupe)]
        cn <- dt[year == yr, ][disrsn == "Did not die", .(no_depot_bupe)]
      },
      "true" = {
        te <- dt[year == yr, ][disrsn == "Died", .(no_depot_bupe)]
        tn <- dt[year == yr, ][disrsn == "Did not die", .(no_depot_bupe)]
        ce <- dt[year == yr, ][disrsn == "Died", .(depot_bupe)]
        cn <- dt[year == yr, ][disrsn == "Did not die", .(depot_bupe)]
      }
    )

    rr <- (te * (ce + cn)) / (ce * (te + tn))

    se_log_rr <- sqrt((tn / (te * (te + tn))) + (cn / (ce * (te + cn))))

    CI_log_rr_upper <-
      log(rr) + se_log_rr * z_alpha

    CI_log_rr_lower <-
      log(rr) - se_log_rr * z_alpha

    res <-
      c(
        rr,
        exp(CI_log_rr_upper),
        exp(CI_log_rr_lower)
      )

    names(res) <- c("rr", "upper", "lower")

    res <-
      data.frame(
        "year" = yr,
        "rr" = res$rr,
        "lower" = res$lower,
        "upper" = res$upper
      )

    return(res)
  }

depot_rr <-
  lapply(
    X = yrs,
    FUN = function(x) {
      calculate_rr_CIs(
        dt = dt_rr,
        yr = x,
        invert = "false"
      )
    }
  )

depot_rr <- data.table::rbindlist(depot_rr)

no_depot_rr <-
  lapply(
    X = yrs,
    FUN = function(x) {
      calculate_rr_CIs(
        dt = dt_rr,
        yr = x,
        invert = "true"
      )
    }
  )

no_depot_rr <- data.table::rbindlist(no_depot_rr)


library(ggplot2)



dt_res

chkfile <-
  readxl::read_xlsx("Copy of Depot buprenorphine SR output v5 jk working_.xlsx",
    sheet = "Table 1",
    range = "A5:V16"
  )

chkfile <- data.table::as.data.table(chkfile)

chkfile <- chkfile[!grepl("journey", `LAB status`), .(`Year`, `LAB status`, InTx, Exits_Died, Exits_SC)]

chkfile[, `:=`(mortality_rate = Exits_Died / InTx, sc_rate = Exits_SC / InTx)]

chkfile[, InTx := NULL]

chkfile <- data.table::melt.data.table(chkfile, measure.vars = data.table::patterns("Exits|rate", cols = names(chkfile)))



data.table::dcast.data.table(chkfile, Year + variable ~ `LAB status`)

chkfile






dtres_long <-
  data.table::melt(dt_res[, .(year, disrsn, depot_bupe, no_depot_bupe)],
    measure.vars = c("depot_bupe", "no_depot_bupe")
  )

dtres_long_rate <-
  data.table::melt(dt_res[, .(year, disrsn, depot_rate, no_depot_rate)],
    measure.vars = c("depot_rate", "no_depot_rate")
  )

library(flextable)

data.table::setorder(dt_res, -year, -no_depot_bupe)

dt_res[, .(
  year = forcats::as_factor(year),
  disrsn,
  depot_bupe,
  depot_rate = scales::percent(depot_rate, accuracy = 0.1),
  no_depot_bupe,
  no_depot_rate = scales::percent(no_depot_rate, accuracy = 0.1)
)] |>
  flextable() |>
  flextable::merge_v(j = 1) |>
  flextable::theme_box()




afcharts::use_afcharts()

dtres_long_rate[disrsn == "Died"] |>
  ggplot(aes(x = year, y = value)) +
  geom_col(aes(fill = variable), position = "dodge")
