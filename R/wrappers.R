# ── Core machinery ────────────────────────────────────────────────────────────

#' @keywords internal
run_with_viz <- function(fn, fn_args, .port = find_free_port()) {

  start_viewer(.port)
  message("[bayeswatch] Sampling in background \u2014 watch the Viewer pane!")

  bg <- callr::r_bg(
    func      = fn,
    args      = fn_args,
    package   = FALSE,
    libpath   = .libPaths(),
    supervise = TRUE,
    stdout    = NULL,
    stderr    = NULL
  )

  tryCatch({
    while (bg$is_alive()) {
      httpuv::service(timeoutMs = 100)
    }
  }, interrupt = function(e) {
    message("\n[bayeswatch] Interrupted \u2014 killing background sampler.")
    bg$kill()
  })

  for (i in seq_len(15)) httpuv::service(timeoutMs = 100)
  mark_done()
  for (i in seq_len(15)) httpuv::service(timeoutMs = 100)

  tryCatch(
    bg$get_result(),
    error = function(e) stop("[bayeswatch] Sampling failed:\n", conditionMessage(e))
  )
}


# ── Public wrappers ────────────────────────────────────────────────────────────

#' Fit a brms model with an HMC animation in the RStudio Viewer
#'
#' Drop-in for [brms::brm()]. Opens Chi Feng's MCMC animation in the Viewer
#' pane while sampling runs in a background process.
#'
#' @param ... Passed to [brms::brm()].
#' @param .port Port for the local viewer server (auto-detected).
#' @return A \code{brmsfit} object.
#' @export
brm_with_viz <- function(..., .port = find_free_port()) {
  if (!requireNamespace("brms",  quietly = TRUE))
    stop("Package 'brms' is required.")
  if (!requireNamespace("callr", quietly = TRUE))
    stop("Package 'callr' is required.")

  run_with_viz(
    fn      = function(args) do.call(brms::brm, args),
    fn_args = list(args = list(...)),
    .port   = .port
  )
}


#' Fit a rethinking model with an HMC animation in the RStudio Viewer
#'
#' Drop-in for [rethinking::ulam()] or [rethinking::map2stan()]. Opens Chi
#' Feng's MCMC animation in the Viewer pane while sampling runs in background.
#'
#' ## The CSV-not-found problem
#'
#' When \code{cmdstan=TRUE} (the default since rethinking >= 2.21), ulam uses
#' CmdStanR and stores the \code{CmdStanMCMC} R6 object as an R *attribute*
#' on the S4 fit: \code{attr(fit, "cstanfit")}.  It writes CSV output into the
#' *child* process's \code{tempdir()}, which is deleted when the child exits,
#' so \code{read_cmdstan_csv()} fails in the parent with "File does not exist".
#'
#' ## The fix
#'
#' Before returning, the child calls
#' \code{attr(fit, "cstanfit")$save_output_files(output_dir)} to copy every
#' CSV to a persistent directory created by the parent, and updates the path
#' references inside the \code{CmdStanMCMC} object so the parent session can
#' call \code{precis()}, \code{plot()}, etc. without errors.
#'
#' For robustness we also check \code{attr(fit, "stanfit")} (legacy RStan path)
#' and fall back to scanning all attributes for any R6 object that has a
#' \code{save_output_files} method.
#'
#' @param flist Model formula list.
#' @param ... Passed to [rethinking::ulam()] / [rethinking::map2stan()].
#' @param .fn \code{"ulam"} (default) or \code{"map2stan"}.
#' @param .port Port for the local viewer server (auto-detected).
#' @return The fitted model object.
#' @export
ulam_with_viz <- function(flist, ..., .fn = c("ulam", "map2stan"),
                          .port = find_free_port()) {
  if (!requireNamespace("rethinking", quietly = TRUE))
    stop("Package 'rethinking' is required.")
  if (!requireNamespace("callr", quietly = TRUE))
    stop("Package 'callr' is required.")

  fn_name <- match.arg(.fn)

  # Persistent directory visible to both parent and child (not a tempdir()).
  output_dir <- file.path(
    tools::R_user_dir("bayeswatch", which = "cache"),
    paste0("ulam_", format(Sys.time(), "%Y%m%d%H%M%S"), "_", Sys.getpid())
  )
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  run_with_viz(
    fn = function(fn_name, flist, args, output_dir) {

      rethinking_fn <- getExportedValue("rethinking", fn_name)
      fit <- do.call(rethinking_fn, c(list(flist), args))

      # The CmdStanMCMC R6 object is stored as attr(fit, "cstanfit") in
      # current rethinking (cmdstan=TRUE path).  The legacy RStan path uses
      # attr(fit, "stanfit"), which does not need CSV relocation.
      # We try the known attribute names first, then scan all attributes as a
      # fallback so this keeps working if the internals change again.
      cmdstan_fit <- attr(fit, "cstanfit")

      if (is.null(cmdstan_fit)) {
        # Fallback: scan all attributes for an R6 object with the method
        for (aname in names(attributes(fit))) {
          obj <- attr(fit, aname)
          if (is.environment(obj) && is.function(obj[["save_output_files"]])) {
            cmdstan_fit <- obj
            break
          }
        }
      }

      if (!is.null(cmdstan_fit)) {
        tryCatch(
          cmdstan_fit$save_output_files(dir = output_dir, overwrite = TRUE),
          error = function(e) {
            warning("[bayeswatch] Could not relocate CmdStan CSV files: ",
                    conditionMessage(e))
          }
        )
      }

      fit
    },
    fn_args = list(
      fn_name    = fn_name,
      flist      = flist,
      args       = list(...),
      output_dir = output_dir
    ),
    .port = .port
  )
}


#' @inheritParams ulam_with_viz
#' @export
map2stan_with_viz <- function(flist, ..., .port = find_free_port()) {
  ulam_with_viz(flist, ..., .fn = "map2stan", .port = .port)
}


#' Stop the HMC viewer server
#' @export
close_viz <- function() stop_viewer()
