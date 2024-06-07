#' Trigonometric Basis Functions
#'
#' Creates a trigonometric basis for the input vector `x` up to the specified order `M`.
#'
#' @param x A numeric vector.
#' @param M An integer specifying the maximum order of the trigonometric basis functions.
#' @return A matrix where each column represents a trigonometric basis function.
#' @examples
#' x <- seq(0, 1, length.out = 100)
#' M <- 5
#' basis <- trig_basis(x, M)
#' @export
trig_basis <- function(x, M) {
  basis <- matrix(1, nrow = length(x), ncol = 1)
  for (k in 1:M) {
    cos_term <- sqrt(2) * cos(2 * pi * k * x)
    sin_term <- sqrt(2) * sin(2 * pi * k * x)
    basis <- cbind(basis, cos_term, sin_term)
  }
  colnames(basis) <- c("Intercept", as.vector(sapply(1:M, function(k) c(paste0("cos_", k), paste0("sin_", k)))))
  return(basis)
}

#' Orthogonal Projection using Trigonometric Basis
#'
#' Fits a linear model using trigonometric basis functions up to the specified order `M`.
#'
#' @param X A numeric vector of predictor values.
#' @param Y A numeric vector of response values.
#' @param M An integer specifying the maximum order of the trigonometric basis functions.
#' @return A linear model object.
#' @examples
#' X <- seq(0, 1, length.out = 100)
#' Y <- 10 * (X - 0.5)^2 + rnorm(100)
#' M <- 5
#' fit <- orthogonal_projection(X, Y, M)
#' summary(fit)
#' @export
orthogonal_projection <- function(X, Y, M) {
  X_ext = trig_basis(X, M)
  fit <- lm(Y ~ X_ext - 1)
  return(fit)
}

#' Cross-Validation for Trigonometric Basis Functions
#'
#' Performs leave-one-out cross-validation to select the optimal truncation parameter `M` for trigonometric basis functions.
#'
#' @param X A numeric vector of predictor values.
#' @param Y A numeric vector of response values.
#' @param M An integer specifying the maximum order of the trigonometric basis functions.
#' @return The mean squared error (MSE) of the cross-validated model.
#' @examples
#' X <- seq(0, 1, length.out = 100)
#' Y <- 10 * (X - 0.5)^2 + rnorm(100)
#' M <- 5
#' mse <- cross_validation(X, Y, M)
#' print(mse)
#' @export
cross_validation <- function(X, Y, M) {
  y_pred = sapply(1:length(X), function(i) {
    fit_mins <- orthogonal_projection(X[-i], Y[-i], M)
    X_ext_c = trig_basis(X[i], M)
    Y_predi <- fit_mins$coefficients %*% t(X_ext_c)
    Y_predi
  })
  mse <- mean((Y - y_pred)^2)
  return(mse)
}
