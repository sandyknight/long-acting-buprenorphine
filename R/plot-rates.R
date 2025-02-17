plot_rates <- function(rates_dt, event) {
  dt_rates_long <-
    data.table::melt.data.table(
      rates_dt[
        outcome == event,
        .(
          year,
          outcome,
          depot_rate,
          no_depot_rate
        )
      ],
      id.vars = c(
        "year",
        "outcome"
      ),
      measure.vars = c(
        "depot_rate",
        "no_depot_rate"
      )
    )

  dt_rates_long[, variable := data.table::fifelse(
    variable == "depot_rate",
    "Long-acting buprenorphine",
    "Other OST"
  )]

  afcharts::use_afcharts()

  dt_rates_long |>
    ggplot(aes(x = year, y = value)) +
    geom_col(aes(fill = variable), position = "dodge") +
    scale_y_continuous(labels = scales::percent) +
    labs(x = NULL, y = NULL, fill = NULL) +
    theme(
      legend.position = "bottom",
      legend.justification = "left"
    )
}
