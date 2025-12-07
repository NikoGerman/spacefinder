#' Synthetic Hyperparameter Benchmark Data
#'
#' @description
#' Simulated hyperparameter tuning results for demonstrating spacefinder
#' functionality. Contains performance metrics (AUC) for various hyperparameter
#' configurations across multiple tasks and optimizers.
#'
#' @format A \code{data.table} with 1000 rows and 5 columns:
#' \describe{
#'   \item{task}{Character. Task identifier (task1 through task5)}
#'   \item{learning_rate}{Numeric. Learning rate in range \eqn{[0.0001, 0.1]}}
#'   \item{max_depth}{Integer. Maximum tree depth in range \eqn{[3, 15]}}
#'   \item{optimizer}{Character. Optimizer type: "SGD", "Adam", or "RMSprop"}
#'   \item{auc}{Numeric. Area Under ROC Curve in range \eqn{[0.5, 0.98]}. Higher is better.
#'     Most values between 0.6-0.85, with top performers rarely exceeding 0.90}
#' }
#'
#' @details
#' The data simulates realistic hyperparameter tuning scenarios where:
#' \itemize{
#'   \item Performance peaks around learning_rate \eqn{0.003} and max_depth \eqn{8}
#'   \item Adam optimizer provides slight performance boost over SGD and RMSprop
#'   \item Each task has slight performance variations
#'   \item Most configurations are mediocre, with excellent performance being rare
#' }
#'
#' Generated using a mixture of normal distributions with realistic noise and
#' soft capping to ensure values above 0.9 are extremely rare.
#'
#' @source Synthetically generated for package examples
#'
#' @examples
#' data(benchmark_data)
#' head(benchmark_data)
#'
#' # Summary statistics
#' summary(benchmark_data$auc)
#'
#' # Create a task
#' task <- TaskSubspace$new(
#'   data = benchmark_data,
#'   target_measure = "auc",
#'   hps = c("learning_rate", "max_depth")
#' )
"benchmark_data"
