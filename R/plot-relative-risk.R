#' Plot Risk Ratios with Confidence Intervals
#' 
#' Creates a  plot showing risk ratios as points with error bars, 
#' reference line at RR=1.
#'
#' @param dt A data frame containing columns:
#'   \itemize{
#'     \item year - The year of measurement
#'     \item rr - Risk ratio values
#'     \item lower - Lower confidence interval bounds
#'     \item upper - Upper confidence interval bounds
#'   }
#' @param n Integer indicating the number of years to show after 2021 in the x-axis scale
#'
#' @return A ggplot2 object showing:
#'   \itemize{
#'     \item Points representing risk ratios
#'     \item Error bars showing confidence intervals
#'     \item Dashed reference line at RR=1
#'   }
#'   The plot uses flipped coordinates with years on the vertical axis and
#'   risk ratios on the horizontal axis.
#'
#' @import ggplot2
#'
#' @examples
#' data <- data.frame(
#'   year = 2021:2023,
#'   rr = c(1.2, 1.5, 0.8),
#'   lower = c(0.9, 1.2, 0.6),
#'   upper = c(1.5, 1.8, 1.1)
#' )
#' plot_rr(data, n = 2)
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
