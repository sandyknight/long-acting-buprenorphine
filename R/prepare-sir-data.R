prepare_sir_data <- function(sir_data, start_year = 2019) {
    sir_data <- sir_data[-which(duplicated(sir_data))]

    sir_data[, submoddt := lubridate::as_date(submoddt)]

    sir_data <- sir_data[lubridate::year(submoddt) > 2019, ]

    return(sir_data)
}
