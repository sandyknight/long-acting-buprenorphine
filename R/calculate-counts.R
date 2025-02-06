
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
