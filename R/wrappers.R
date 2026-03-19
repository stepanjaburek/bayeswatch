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
#' ## Why the plain background-process approach breaks
#'
#' \code{ulam()} uses CmdStanR internally. CmdStanR writes CSV output files to
#' a path inside the *child* process's \code{tempdir()}.  When the child exits
#' that directory is cleaned up, so the parent's \code{read_cmdstan_csv()} call
#' fails with "File does not exist".
#'
#' ## The fix
#'
#' Before returning, the child calls \code{fit@@stanfit$save_output_files(dir)}
#' where \code{dir} is a persistent cache directory created by the parent
#' *before* launching the child.  This copies every CSV to the shared directory
#' and — crucially — updates the internal path references inside the
#' \code{CmdStanMCMC} object so the parent's \code{read_cmdstan_csv()} finds
#' the files correctly.  No changes to \code{ulam()}'s own argument list are
#' needed.
#'
#' @param flist Model formula list.
#' @param ... Passed to [rethinking::ulam()] / [rethinking::map2stan()].
#' @param .fn \code{"ulam"} (default) or \code{"map2stan"}.
#' @param .port Port for the local viewer server (auto-detected).
#' @return The fitted model object (a \code{ulam} / \code{map2stan} fit).
#' @export
ulam_with_viz <- function(flist, ..., .fn = c("ulam", "map2stan"),
                          .port = find_free_port()) {
  if (!requireNamespace("rethinking", quietly = TRUE))
    stop("Package 'rethinking' is required.")
  if (!requireNamespace("callr", quietly = TRUE))
    stop("Package 'callr' is required.")

  fn_name <- match.arg(.fn)

  # Create a persistent output directory visible to both the parent and child
  # process (a real filesystem path, not a session-scoped tempdir()).
  output_dir <- file.path(
    tools::R_user_dir("bayeswatch", which = "cache"),
    paste0("ulam_", format(Sys.time(), "%Y%m%d%H%M%S"), "_", Sys.getpid())
  )
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  run_with_viz(
    fn = function(fn_name, flist, args, output_dir) {
      rethinking_fn <- getExportedValue("rethinking", fn_name)
      fit <- do.call(rethinking_fn, c(list(flist), args))

      # Move CmdStan CSV files to the shared persistent directory and update
      # the path references inside the fit object before it is serialised and
      # sent back to the parent.  Without this the parent cannot call
      # read_cmdstan_csv() because the child's tempdir() is deleted on exit.
      #
      # ulam stores the raw CmdStanMCMC object in fit@stanfit.
      if (methods::is(fit, "ulam") && !is.null(fit@stanfit)) {
        tryCatch(
          fit@stanfit$save_output_files(dir = output_dir, overwrite = TRUE),
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
