
plot_rr <-
  function(dt, n) {
    ggplot(dt, aes(x = year)) +
      geom_point(aes(y = rr), pch = 18, size = 5) +
      geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.05) +
      geom_hline(yintercept = 1, linetype = 2) +
      scale_x_continuous(breaks = seq(2021, 2021 + n, 1)) +
      coord_flip(ylim = c(0, max(dt$upper, 2))) +
      labs(x = NULL, y = NULL)
  }
