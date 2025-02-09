calculate_count <-
  function(i, sir_data, main_data, start_dates, end_dates, yrs) {

    dt_inter <- sir_data[submoddt %between% c(start_dates[i], end_dates[i])]

    dt_inter <- dt_inter[, .(submoddt = max(submoddt),
                             phbudi_any = sum(phbudi_any)),
                         by = .(client_random_id,
                         n_jy)]

    dt_inter <- unique(dt_inter)

    dt_inter[, phbudi_any := data.table::fifelse(phbudi_any > 0, 1, 0)]

    dt_outcomes <- main_data[, .(client_random_id,
                                               n_jy,
                                               disd,
                                               disrsn)]

    dt_outcomes [, disd := data.table::fifelse(is.na(disd), end_dates[i], disd)]

    dt_outcomes <-
      dt_outcomes[disd %between% c(start_dates[i], end_dates[i])]

    dt <-
      data.table::merge.data.table(dt_outcomes[,
                                             .(client_random_id,
                                               n_jy,
                                               disd,
                                               disrsn)], dt_inter)

    dt <- dt[, .N, by = .(phbudi_any, disrsn)]

    dt[, year := yrs[i]]

    dt <- data.table::dcast.data.table(dt, year + disrsn ~ phbudi_any, value.var = "N")

    data.table::setnames(dt, c("year", "outcome", "no_depot_bupe", "depot_bupe"))

    return(dt)
}

calculate_counts <-
  function(sir_data, main_data, end_date) {
    end_date <- lubridate::as_date(end_date)

    yrs <- unique(lubridate::year(sir_data[["submoddt"]]))
    yrs <- sort(yrs)
    yrs <- yrs[yrs != min(yrs)]

    # Create the start and end date vectors for each period
    start_dates <- end_date - lubridate::years(seq_along(yrs))
    end_dates <- start_dates + lubridate::years(1)

    dt_counts <- data.table::rbindlist(lapply(seq_along(yrs), function(i) {
        calculate_count(i, sir_data, main_data, start_dates, end_dates, yrs)
    }))

    return(dt_counts)
}
