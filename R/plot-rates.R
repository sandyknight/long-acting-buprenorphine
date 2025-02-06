
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
