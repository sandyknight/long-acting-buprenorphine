library(data.table)
library(flextable)
library(ggplot2)
library(lubridate)

main_dt <- data.table::fread("data/K3anon_FullDataset_for_VfM.csv")

sir_dt <- data.table::fread("data/SIR_table_for_VfM_linked.csv")

invisible(lapply(list.files("R", pattern = "\\.[Rr]$", full.names = TRUE), source))

main_dt <-
  prepare_main_data(main_data = main_dt)

sir_dt <-
  prepare_sir_data(sir_data = sir_dt)


counts_dt <-
  calculate_counts(
    sir_data = sir_dt,
    main_data = main_dt,
    end_date = "2024-10-01"
  )

rates_dt <-
  calculate_rates(counts_data = counts_dt)


mx_rr_t1 <-
  data.table::rbindlist(mapply(
    FUN = calculate_rr_CIs,
    yr = unique(counts_dt[["year"]]),
    MoreArgs = list(dt = counts_dt),
    SIMPLIFY = FALSE
  ))

mx_rr_t1[, outcome := "Mortality"][, intervention := "LAB"]

mx_rr_t0 <-
  data.table::rbindlist(mapply(
    FUN = calculate_rr_CIs,
    yr = unique(counts_dt[["year"]]),
    MoreArgs = list(dt = counts_dt, invert = TRUE),
    SIMPLIFY = FALSE
  ))

mx_rr_t0[, outcome := "Mortality"][, intervention := "Other OST"]

sc_rr_t1 <-
  data.table::rbindlist(mapply(
    FUN = calculate_rr_CIs,
    yr = unique(counts_dt[["year"]]),
    MoreArgs = list(dt = counts_dt),
    SIMPLIFY = FALSE
  ))

sc_rr_t1[, outcome := "Successful completion"][, intervention := "LAB"]

sc_rr_t0 <-
  data.table::rbindlist(mapply(
    FUN = calculate_rr_CIs,
    yr = unique(counts_dt[["year"]]),
    MoreArgs = list(dt = counts_dt, invert = TRUE),
    SIMPLIFY = FALSE
  ))

sc_rr_t0[, outcome := "Successful completion"][, intervention := "Other OST"]

relative_risks <- data.table::rbindlist(l = list(mx_rr_t1, mx_rr_t0, sc_rr_t1, sc_rr_t0))

data.table::fwrite(relative_risks, "lab-relative-risks.csv")

plot_rr <-
  function(dt, n) {
    ggplot(dt, aes(x = year)) +
      geom_point(aes(y = rr), pch = 18, size = 5) +
      geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.05) +
      geom_hline(yintercept = 1, linetype = 2) +
      scale_x_continuous(breaks = seq(2021, 2021 + n, 1)) +
      coord_flip(ylim = c(0, 2))
  }
