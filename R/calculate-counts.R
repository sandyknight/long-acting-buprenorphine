#' Calculate count of outcomes for treatment and non-treatment groups
#'
#' @import data.table
#' @param i Int which index of `start dates` and `end dates` to use
#' @param sir_data the SIR dataset as prepared by prepare_sir_data()
#' @param main_data the main datset as prepared by prepare_main_data()
#' @param start_dates vector of dates to be considered the start of 12-month period
#' @param end_dates vector of dates to be considered the end of 12-month period
#' @param yrs vector of years as integers
#' @details This function is iterated over yrs by calculate_count()
calculate_count <-
  function(i, sir_data, main_data, start_dates, end_dates, yrs) {
    # Filter SIR data such that submoddt is within the 12-month period
    dt_inter <- sir_data[submoddt %between% c(start_dates[i], end_dates[i])]

    # Summarise SIR data to show the latest review as the date and
    # the "max" of intervention binary variable i.e. if yes then max == 1
    dt_inter <-
      dt_inter[, .(submoddt = max(submoddt),
                   phbudi_any = max(phbudi_any)),
               by = .(client_random_id, n_jy)]

    # Filter to unique observations within period
    dt_inter <- unique(dt_inter)

    # Ensure intervention remains a binary variable
    dt_inter[, phbudi_any := data.table::fifelse(phbudi_any > 0, 1, 0)]

    dt_outcomes <-
      main_data[, .(client_random_id,
                    n_jy,
                    disd,
                    disrsn)]

    dt_outcomes[, disd := data.table::fifelse(is.na(disd), end_dates[i], disd)]

    dt_outcomes <-
      dt_outcomes[disd %between% c(start_dates[i], end_dates[i])]

    dt <-
      data.table::merge.data.table(dt_outcomes[
        ,
        .(
          client_random_id,
          n_jy,
          disd,
          disrsn
        )
      ], dt_inter)

    dt <- dt[, .N, by = .(phbudi_any, disrsn)]

    dt[, year := yrs[i]]

    dt <- data.table::dcast.data.table(dt, year + disrsn ~ phbudi_any, value.var = "N")

    data.table::setnames(dt, c("year", "outcome", "no_depot_bupe", "depot_bupe"))

    return(dt)
  }


#' Calculate count of outcomes for treatment and non-treatment groups
#'
#' @import data.table
#' @param sir_data the SIR dataset as prepared by prepare_sir_data()
#' @param main_data the main datset as prepared by prepare_main_data()
#' @param end_date a date determining the cut-off point
#' @details Takes the two datasets and a cut-off date and calculates the annual
#' count by outcome and intervention status. The intervention status is a binary
#' measure of whether the client has receieved long-acting buprenorphine in the
#' in the last 12 months. The "annual" counts of outcomes period is determined
#' by the cut-off date. For example "2024-03-31" returns counts for the fiscal
#' years from 2023-24 back to 2020-21, but excludes the data from March '24 to
#' to October '24.
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
