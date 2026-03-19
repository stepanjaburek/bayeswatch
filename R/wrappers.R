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
#' @param ... Passed to [brms::brm()].
#' @param .port Port for the local viewer server (auto-detected).
#' @return A \code{brmsfit} object.
#' @export
brm_with_viz <- function(..., .port = find_free_port()) {
  if (!requireNamespace("brms",  quietly = TRUE)) stop("Package 'brms' is required.")
  if (!requireNamespace("callr", quietly = TRUE)) stop("Package 'callr' is required.")
  run_with_viz(
    fn      = function(args) do.call(brms::brm, args),
    fn_args = list(args = list(...)),
    .port   = .port
  )
}


#' Fit a rethinking model with an HMC animation in the RStudio Viewer
#'
#' @param flist Model formula list.
#' @param ... Passed to [rethinking::ulam()] / [rethinking::map2stan()].
#' @param .fn \code{"ulam"} (default) or \code{"map2stan"}.
#' @param .port Port for the local viewer server (auto-detected).
#' @return The fitted model object.
#' @export
ulam_with_viz <- function(flist, ..., .fn = c("ulam", "map2stan"),
                          .port = find_free_port()) {
  if (!requireNamespace("rethinking", quietly = TRUE)) stop("Package 'rethinking' is required.")
  if (!requireNamespace("callr",      quietly = TRUE)) stop("Package 'callr' is required.")
  if (!requireNamespace("cmdstanr",   quietly = TRUE)) stop("Package 'cmdstanr' is required.")

  fn_name <- match.arg(.fn)

  # Use a plain tempfile() path — guaranteed to be a real absolute path on
  # this machine, shared between parent and child (same filesystem).
  # tools::R_user_dir can theoretically differ if XDG env vars change between
  # the parent session and the callr child.  tempfile() is safer.
  output_dir <- tempfile(pattern = "bayeswatch_ulam_")
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  # ── Child: sample, save CSVs to output_dir, return S4 shell (no R6) ────────
  child_fn <- function(fn_name, flist, args, output_dir) {

    rethinking_fn <- getExportedValue("rethinking", fn_name)
    fit <- do.call(rethinking_fn, c(list(flist), args))

    # Locate the CmdStanMCMC R6 object.
    # rethinking (cmdstan=TRUE path) stores it as attr(fit, "cstanfit").
    cmdstan_fit <- attr(fit, "cstanfit")
    attr_used   <- "cstanfit"

    if (is.null(cmdstan_fit)) {
      # Fallback: scan all attributes
      for (aname in names(attributes(fit))) {
        obj <- attr(fit, aname)
        if (is.environment(obj) && is.function(obj[["save_output_files"]])) {
          cmdstan_fit <- obj
          attr_used   <- aname
          break
        }
      }
    }

    if (is.null(cmdstan_fit)) {
      return(fit)   # no CmdStanMCMC found — return as-is
    }

    # Move CSVs from child tempdir() to the shared output_dir.
    # Raises an error (propagated to parent via callr) if this fails.
    cmdstan_fit$save_output_files(dir = output_dir)

    # Strip the R6 before the callr serialisation round-trip: R6 environments
    # lose all methods through saveRDS/readRDS.  Parent rebuilds from CSVs.
    attr(fit, attr_used) <- NULL

    fit
  }

  # ── Run child ───────────────────────────────────────────────────────────────
  fit <- run_with_viz(
    fn      = child_fn,
    fn_args = list(fn_name = fn_name, flist = flist,
                   args = list(...), output_dir = output_dir),
    .port   = .port
  )

  # ── Parent: rebuild CmdStanMCMC from the saved CSVs ─────────────────────────
  csv_files <- sort(list.files(output_dir, pattern = "\\.csv$", full.names = TRUE))

  if (length(csv_files) == 0L) {
    warning("[bayeswatch] No CSV files found in ", output_dir,
            "\nThe fit object will not support precis(), plot(), etc.")
    return(fit)
  }

  tryCatch({
    attr(fit, "cstanfit") <- cmdstanr::as_cmdstan_fit(csv_files)
  }, error = function(e) {
    warning("[bayeswatch] Could not reconstruct CmdStanMCMC:\n",
            conditionMessage(e), "\nCSVs are at: ", output_dir)
  })

  fit
}


#' @inheritParams ulam_with_viz
#' @export
map2stan_with_viz <- function(flist, ..., .port = find_free_port()) {
  ulam_with_viz(flist, ..., .fn = "map2stan", .port = .port)
}


#' Stop the HMC viewer server
#' @export
close_viz <- function() stop_viewer()
