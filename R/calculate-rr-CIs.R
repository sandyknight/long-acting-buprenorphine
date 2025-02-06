
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
