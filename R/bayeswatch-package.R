#' bayeswatch: MCMC Animation Viewer for Stan Models
#'
#' Wraps `brms::brm()` and `rethinking::ulam()` / `map2stan()` to open
#' Chi Feng's interactive MCMC demo in the RStudio Viewer pane while
#' your model samples in the background.
#'
#' ## Main functions
#'
#' - [brm_bayeswatch()]       — drop-in for `brms::brm()`
#' - [ulam_bayeswatch()]      — drop-in for `rethinking::ulam()`
#' - [map2stan_bayeswatch()]  — drop-in for `rethinking::map2stan()`
#' - [close_bayeswatch()]          — manually stop the viewer server
#'
#' @keywords internal
"_PACKAGE"

#' @importFrom httpuv startServer
#' @importFrom rstudioapi isAvailable viewer
#' @importFrom jsonlite toJSON
#' @importFrom tools file_ext
NULL
