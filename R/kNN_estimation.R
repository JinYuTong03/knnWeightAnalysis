#' Plot kNN Estimates
#'
#' Plots the kNN estimates for given values of k and compares them with the true function.
#'
#' @param X Numeric vector of predictor values.
#' @param Y Numeric vector of response values.
#' @param X_grid Numeric vector of points for plotting the estimates.
#' @param k_values Numeric vector of k values to use for kNN estimation.
#' @param g_star Function representing the true function to compare against.
#' @return A ggplot object showing the kNN estimates and the true function.
#' @examples
#' # Define the true function
#' g_star <- function(x) { 10 * (x - 0.5)^2 + 2 }
#'
#' # Generate data
#' set.seed(123)
#' X <- seq(0, 1, length.out = 100)
#' Y <- g_star(X) + rnorm(100, 0, 0.05)
#' X_grid <- seq(0, 1, length.out = 1000)
#'
#' # Plot kNN estimates
#' plot_knn_estimates(X, Y, X_grid, c(1, 3, 15, 100), g_star)
#' @import ggplot2
#' @importFrom FNN knn.reg
#' @importFrom reshape2 melt
#' @export
plot_knn_estimates <- function(X, Y, X_grid, k_values, g_star) {
  g_hat <- function(k) {
    knn_fit <- FNN::knn.reg(train = matrix(X, ncol = 1), test = matrix(X_grid, ncol = 1), y = Y, k = k)
    return(knn_fit$pred)
  }

  predictions <- data.frame(X_grid = X_grid)
  for (k in k_values) {
    predictions[[paste0("k_", k)]] <- g_hat(k)
  }

  predictions_long <- reshape2::melt(predictions, id.vars = "X_grid", variable.name = "k", value.name = "g_hat")
  plot_data <- data.frame(X = X, Y = Y)

  ggplot2::ggplot() +
    ggplot2::geom_point(data = plot_data, ggplot2::aes(x = X, y = Y), color = 'black') +
    ggplot2::stat_function(fun = g_star, color = "brown", size = 1, linetype = "solid") +
    ggplot2::geom_line(data = predictions_long, ggplot2::aes(x = X_grid, y = g_hat, color = k), size = 0.5) +
    ggplot2::scale_color_manual(values = c("k_1" = "red", "k_3" = "blue", "k_15" = "green", "k_100" = "purple")) +
    ggplot2::labs(title = "kNN Estimation with Different k Values", x = "x", y = "Y") +
    ggplot2::theme_minimal() +
    ggplot2::guides(color = ggplot2::guide_legend(title = "kNN estimates"))
}

