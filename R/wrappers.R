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
#' ## Why this is tricky
#'
#' Two compounding problems:
#'
#' 1. **Wrong temp directory.** CmdStanR writes CSV files into the child
#'    process's \code{tempdir()}, which is deleted when the child exits.
#'
#' 2. **R6 objects don't survive serialisation.** \code{callr} transfers the
#'    return value via \code{saveRDS}/\code{readRDS}.  The \code{CmdStanMCMC}
#'    R6 object stored as \code{attr(fit, "cstanfit")} loses all its methods
#'    in transit, so even if we relocated the CSV files the parent's fit object
#'    can't use them.
#'
#' ## The fix (two steps, both in the child)
#'
#' 1. Call \code{attr(fit, "cstanfit")$save_output_files(output_dir)} to copy
#'    the CSVs to a persistent directory shared with the parent.
#' 2. Strip the broken R6 object from the fit before returning, so the parent
#'    receives the S4 shell without \code{cstanfit}.
#'
#' ## Reconstruction in the parent
#'
#' After \code{bg$get_result()} returns the S4 shell, the parent calls
#' \code{cmdstanr::as_cmdstan_fit()} on the saved CSV files to rebuild a
#' fresh, fully-functional \code{CmdStanMCMC} object and attaches it back as
#' \code{attr(fit, "cstanfit")}.
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
  if (!requireNamespace("callr",      quietly = TRUE))
    stop("Package 'callr' is required.")
  if (!requireNamespace("cmdstanr",   quietly = TRUE))
    stop("Package 'cmdstanr' is required.")

  fn_name <- match.arg(.fn)

  # Persistent directory visible to both parent and child.
  output_dir <- file.path(
    tools::R_user_dir("bayeswatch", which = "cache"),
    paste0("ulam_", format(Sys.time(), "%Y%m%d%H%M%S"), "_", Sys.getpid())
  )
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  # ── Child function ──────────────────────────────────────────────────────────
  # Returns the ulam S4 object *without* cstanfit attached.
  # Separately saves the CSV files to output_dir so the parent can reconstruct.
  child_fn <- function(fn_name, flist, args, output_dir) {
    rethinking_fn <- getExportedValue("rethinking", fn_name)
    fit <- do.call(rethinking_fn, c(list(flist), args))

    # Locate the CmdStanMCMC R6 object.
    # rethinking stores it as attr(fit, "cstanfit") on the cmdstan path.
    cmdstan_fit <- attr(fit, "cstanfit")

    # Fallback: scan all attributes in case the name ever changes.
    if (is.null(cmdstan_fit)) {
      for (aname in names(attributes(fit))) {
        obj <- attr(fit, aname)
        if (is.environment(obj) && is.function(obj[["save_output_files"]])) {
          cmdstan_fit <- obj
          break
        }
      }
    }

    if (!is.null(cmdstan_fit)) {
      # Step 1: copy CSVs to the shared persistent directory.
      tryCatch(
        cmdstan_fit$save_output_files(dir = output_dir, overwrite = TRUE),
        error = function(e) {
          warning("[bayeswatch] Could not relocate CmdStan CSV files: ",
                  conditionMessage(e))
        }
      )
    }

    # Step 2: strip the R6 object before returning — it won't survive
    # callr's saveRDS/readRDS serialisation anyway, and sending a broken
    # environment across the wire wastes time and causes confusion.
    attr(fit, "cstanfit") <- NULL

    fit
  }

  # ── Run child, collect S4 shell ─────────────────────────────────────────────
  fit <- run_with_viz(
    fn      = child_fn,
    fn_args = list(
      fn_name    = fn_name,
      flist      = flist,
      args       = list(...),
      output_dir = output_dir
    ),
    .port = .port
  )

  # ── Reconstruct CmdStanMCMC in the parent ───────────────────────────────────
  csv_files <- sort(list.files(output_dir, pattern = "\\.csv$",
                               full.names = TRUE))
  if (length(csv_files) > 0) {
    tryCatch({
      fresh_fit <- cmdstanr::as_cmdstan_fit(csv_files)
      attr(fit, "cstanfit") <- fresh_fit
    }, error = function(e) {
      warning("[bayeswatch] Could not reconstruct CmdStanMCMC from saved ",
              "CSV files:\n", conditionMessage(e),
              "\nCSVs are in: ", output_dir)
    })
  } else {
    warning("[bayeswatch] No CSV files found in ", output_dir,
            " — fit object may be incomplete.")
  }

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
