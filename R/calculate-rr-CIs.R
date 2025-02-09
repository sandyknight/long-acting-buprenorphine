#' Calculate relative risk and confidence intervals
#' @param dt A data table in the exact format:
#' year, outcomes, control, intervention
#' @param yr integer providing the year of interest
#' @param event a string appearing in the outcomes column
#' @param invert if TRUE, calculates relative risk of event from being in
#' control group
#' Confidence level for CIs
calculate_rr_CIs <-
  function(dt, yr, event = "Died", invert = FALSE, confidence_level = 0.95) {

    # Rename columns

    data.table::setnames(dt,
                         new = c("year",
                                 "outcome",
                                 "control",
                                 "intervention"))

    # Convert figures to type `double` to prevent integer overflow

    dt[, `:=`(control = as.double(control),
              intervention = as.double(intervention))]

    # Count events and non-events

    nonevents <-
      dt[year == yr,
              ][outcome != event
              ][, .(control = sum(control), intervention = sum(intervention))]


    events <- dt[year == yr,
                      ][outcome == event
                      ][, .(control = sum(control), intervention = sum(intervention))]


    # Define parameter for RR calculation

    ce <- events[["control"]]
    te <- events[["intervention"]]
    tn <- nonevents[["intervention"]]
    cn <- nonevents[["control"]]

    # Calculate relative risk and standard error
    # conditional on `invert` parameter

    if (isTRUE(invert)) {
      # Relative risk of event for control group (i.e., inverted)
      rr <- (ce * (te + tn)) / (te * (ce + cn))

      se_log_rr <- sqrt((tn / (te * (te + tn))) + (cn / (ce * (ce + cn))))
    } else {
      # Relative risk of event for intervention group (default)
      rr <- (te * (ce + cn)) / (ce * (te + tn))

      se_log_rr <- sqrt((tn / (te * (te + tn))) + (cn / (ce * (ce + cn))))
    }

    # Calculate z parameter according to `confidence_level` parameter
    z_alpha <-
      qnorm((1 - ((1 - confidence_level)/2)))

    # Calculate CIs
    ci_log_rr_upper <- log(rr) + se_log_rr * z_alpha

    ci_log_rr_lower <- log(rr) - se_log_rr * z_alpha

    # Clean-up and return results as a data frame
    res <- c(rr,
             exp(ci_log_rr_upper),
             exp(ci_log_rr_lower))

    names(res) <- c("rr", "upper", "lower")

    res <- data.frame(year = yr,
                      rr = res[["rr"]],
                      lower = res[["lower"]],
                      upper = res[["upper"]])

    return(res)
}
