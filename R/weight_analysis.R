#' Analyze Weights for Different Regression Approaches
#'
#' This function analyzes the weights of different regression approaches at specified indices.
#'
#' @param X A numeric vector of predictor values.
#' @param Y A numeric vector of response values.
#' @param point_indices A numeric vector indicating which indices of X to analyze.
#' @param max_degree An integer specifying the maximum degree of the polynomial.
#' @return A list containing the weights data and plots for each method.
#' @examples
#' X <- seq(0, 1, length.out = 100)
#' Y <- 10 * (X - 0.5)^2 + rnorm(100)
#' analyze_weights(X, Y, c(10, 100), 10)
#' @importFrom ggplot2 ggplot aes geom_line labs theme_minimal scale_color_manual guides guide_legend
#' @importFrom reshape2 melt
#' @importFrom splines ns bs
#' @importFrom stats poly lm
#' @importFrom magrittr %>%
#' @export
analyze_weights <- function(X, Y, point_indices, max_degree) {
  # Ensure that ggplot2 and reshape2 functions are properly referenced
  ggplot <- ggplot2::ggplot
  aes <- ggplot2::aes
  geom_line <- ggplot2::geom_line
  labs <- ggplot2::labs
  theme_minimal <- ggplot2::theme_minimal
  scale_color_manual <- ggplot2::scale_color_manual
  guides <- ggplot2::guides
  guide_legend <- ggplot2::guide_legend
  melt <- reshape2::melt

  trig_hat_matrix <- function(X, M) {
    X_trig <- trig_basis(X, M)
    H <- X_trig %*% solve(t(X_trig) %*% X_trig) %*% t(X_trig)
    return(H)
  }

  polynomial_hat_matrix <- function(X, M) {
    X_poly <- poly(X, M, raw = TRUE)
    H <- X_poly %*% solve(t(X_poly) %*% X_poly) %*% t(X_poly)
    return(H)
  }

  natural_splines_hat_matrix <- function(X, df) {
    X_ns <- ns(X, df = df)
    H <- X_ns %*% solve(t(X_ns) %*% X_ns) %*% t(X_ns)
    return(H)
  }

  bs_splines_hat_matrix <- function(X, df) {
    X_bs <- bs(X, df = df)
    H <- X_bs %*% solve(t(X_bs) %*% X_bs) %*% t(X_bs)
    return(H)
  }

  H_trig <- trig_hat_matrix(X, max_degree)
  H_poly <- polynomial_hat_matrix(X, max_degree)
  H_ns <- natural_splines_hat_matrix(X, max_degree)
  H_bs <- bs_splines_hat_matrix(X, max_degree)

  weights_df <- data.frame(
    Xi = X,
    Poly_X10 = H_poly[point_indices[1], ],
    Poly_X100 = H_poly[point_indices[2], ],
    NS_X10 = H_ns[point_indices[1], ],
    NS_X100 = H_ns[point_indices[2], ],
    BS_X10 = H_bs[point_indices[1], ],
    BS_X100 = H_bs[point_indices[2], ],
    Trig_X10 = H_trig[point_indices[1], ],
    Trig_X100 = H_trig[point_indices[2], ]
  )

  weights_long <- melt(weights_df, id.vars = "Xi")

  p_trig <- ggplot(weights_long %>% dplyr::filter(variable %in% c("Trig_X10", "Trig_X100")),
                   aes(x = Xi, y = value, color = variable)) +
    geom_line(size = 1) +
    labs(title = "Weights for Trigonometric Basis Regression", x = "X_i", y = "Weights") +
    theme_minimal() +
    scale_color_manual(values = c("red", "blue"), labels = c("Trig_X10", "Trig_X100")) +
    guides(color = guide_legend(title = "Weights"))

  p_poly <- ggplot(weights_long %>% dplyr::filter(variable %in% c("Poly_X10", "Poly_X100")),
                   aes(x = Xi, y = value, color = variable)) +
    geom_line(size = 1) +
    labs(title = "Weights for Polynomial Regression", x = "X_i", y = "Weights") +
    theme_minimal() +
    scale_color_manual(values = c("green", "purple"), labels = c("Poly_X10", "Poly_X100")) +
    guides(color = guide_legend(title = "Weights"))

  p_ns <- ggplot(weights_long %>% dplyr::filter(variable %in% c("NS_X10", "NS_X100")),
                 aes(x = Xi, y = value, color = variable)) +
    geom_line(size = 1) +
    labs(title = "Weights for Natural Splines Regression", x = "X_i", y = "Weights") +
    theme_minimal() +
    scale_color_manual(values = c("orange", "brown"), labels = c("NS_X10", "NS_X100")) +
    guides(color = guide_legend(title = "Weights"))

  p_bs <- ggplot(weights_long %>% dplyr::filter(variable %in% c("BS_X10", "BS_X100")),
                 aes(x = Xi, y = value, color = variable)) +
    geom_line(size = 1) +
    labs(title = "Weights for B-Splines Regression", x = "X_i", y = "Weights") +
    theme_minimal() +
    scale_color_manual(values = c("cyan", "magenta"), labels = c("BS_X10", "BS_X100")) +
    guides(color = guide_legend(title = "Weights"))

  return(list(weights_data = weights_long, plots = list(trig = p_trig, poly = p_poly, ns = p_ns, bs = p_bs)))
}
