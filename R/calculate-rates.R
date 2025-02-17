calculate_rates <-
  function(counts_data) {
    all_outcomes_totals <-
      counts_data[, lapply(.SD, sum), by = year, .SDcols = c(
        "no_depot_bupe",
        "depot_bupe"
      )]

    counts_data <-
      data.table::merge.data.table(counts_data,
        all_outcomes_totals,
        by = "year",
        suffixes = c("", "_total")
      )

    counts_data[, `:=`(
      depot_rate = depot_bupe / depot_bupe_total,
      no_depot_rate = no_depot_bupe / no_depot_bupe_total
    )]

    dt_res <- counts_data[, .(
      year,
      outcome,
      depot_bupe,
      depot_rate,
      no_depot_bupe,
      no_depot_rate
    )]

    dt_res[outcome == "Died",
      lapply(
        .SD,
        scales::percent
      ),
      by = .(year, outcome), .SDcols = c(
        "depot_rate",
        "no_depot_rate"
      )
    ]

    data.table::setnames(dt_res, new = function(x) gsub("bupe", "count", x))

    return(dt_res)
  }
