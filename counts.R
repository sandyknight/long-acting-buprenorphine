library(data.table)
library(lubridate)
library(janitor)
# Read and merge datasets
main_dt <- fread("data/K3anon_FullDataset_for_VfM.csv")
sir_dt <- fread("data/SIR_table_for_VfM_linked.csv")

prepare_sir_data <-
  function(sir_data, start_year = 2019) {

    sir_data <-
      sir_data[-which(duplicated(sir_data))]

    sir_data[, submoddt := lubridate::as_date(submoddt)]

    sir_data <- sir_data[lubridate::year(submoddt) > 2019, ]

    return(sir_data)
  }

# ----------------Validation ---------------
stopifnot(sum(duplicated(sir_dt)) == 0)
# ------------------------------------------

prepare_main_data  <-
  function(main_data) {
    # Filter to opiate clients
    main_dt <- main_dt[drug_grp == "Opiate", ]

    # Convert date columns to compatible data class
    main_dt[, `:=`(
      triaged = lubridate::as_date(triaged),
      disd = lubridate::as_date(disd)
    )]

    # Recode disrsn to have fewer categories
    main_dt[, disrsn := fifelse(
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
 }

calculate_counts <-
  function(i,
           sir_data = sir_dt,
           main_data = main_dt) {

    yrs <-
      unique(lubridate::year(unique(sir_data[["submoddt"]])))

    yrs <- rev(sort(yrs))

    yrs <- yrs[yrs != min(yrs)]

    start_dates <- max(sir_data$submoddt) - years(seq_along(yrs))

    end_dates <- start_dates + lubridate::years(1)

    dt_inter <-
      sir_data[submoddt %between% c(start_dates[i], end_dates[i])]

    dt_inter <-
      dt_inter[, .(
        submoddt = max(submoddt),
        phbudi_any = sum(phbudi_any)
      ),
      by = .(client_random_id, n_jy)
      ]

    dt_inter[, phbudi_any := data.table::fifelse(phbudi_any > 0, 1, 0)]

    dt <-
      data.table::merge.data.table(
        main_data[, .(
          client_random_id,
          n_jy,
          disd,
          disrsn
        )],
        dt_inter
      )

    dt <- dt[, .N, by = .(phbudi_any, disrsn)]

    dt[, year := yrs[i]]

    dt <-
      data.table::dcast.data.table(dt, year + disrsn ~ phbudi_any, value.var = "N")

    data.table::setnames(dt, c("year", "outcome", "no_depot_bupe", "depot_bupe"))

    return(dt)
  }


dt_counts <-
  data.table::rbindlist(lapply(X = seq_along(yrs), FUN = calculate_counts))

calculate_rates <-
  function(counts_dt) {

    all_outcomes_totals <-
      counts_dt[, lapply(.SD, sum),
                by = year,
                .SDcols = c("no_depot_bupe", "depot_bupe")]

    counts_dt <-
      data.table::merge.data.table(counts_dt,
        all_outcomes_totals,
        by = "year",
        suffixes = c("", "_total")
      )

    counts_dt[, `:=`(
      depot_rate = depot_bupe / depot_bupe_total,
      no_depot_rate = no_depot_bupe / no_depot_bupe_total
    )]

    dt_res <-
      dt[, .(year, outcome, depot_bupe, depot_rate, no_depot_bupe, no_depot_rate)]

    dt_res[outcome == "Died", lapply(.SD, scales::percent),
      by = .(year, outcome),
      .SDcols = c("depot_rate", "no_depot_rate")
    ]

}

plot_rates <-
  function(rates_dt) {

    dt_rates_long <-
      data.table::melt.data.table(
        rates_dt[
          outcome == "Died",
          .(
            year,
            outcome,
            depot_rate,
            no_depot_rate
          )
        ],
        id.vars = c("year", "outcome"),
        measure.vars = c("depot_rate", "no_depot_rate")
      )

    afcharts::use_afcharts()

    dt_rates_long |>
      ggplot(aes(x = year, y = value)) +
      geom_col(aes(fill = variable), position = "dodge") +
      scale_y_continuous(labels = scales::percent)
  }
