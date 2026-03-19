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
    stdout    = NULL,   # let child stdout go directly to console
    stderr    = NULL    # let child stderr go directly to console
  )

  # Service httpuv in 100ms slices so the Viewer pane can load the page and
  # poll /status while the background sampler is running.  bg$wait() with
  # timeoutMs=0 returns before libuv dispatches HTTP callbacks, leaving the
  # viewer blank; using service(100) fixes that.
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
#' The root cause of CSV-not-found errors is that \code{ulam()} uses CmdStanR
#' internally, which writes its CSV files to a path derived from
#' \code{tempdir()} in the *child* process.  That temp directory is different
#' from the parent's \code{tempdir()}, so \code{read_cmdstan_csv()} cannot
#' find the files afterwards.  We work around this by:
#'
#' \enumerate{
#'   \item Creating a **persistent** output directory in the parent process
#'         (inside \code{tools::R_user_dir("bayeswatch", "cache")}).
#'   \item Passing it to \code{ulam()} via \code{cmdstan_args$output_dir}.
#'   \item After the child returns, calling \code{cmdstanr::read_cmdstan_csv()}
#'         on the files that now live in the shared directory so the returned
#'         model object is fully usable in the parent.
#' \end{enumerate}
#'
#' @param flist Model formula list.
#' @param ... Passed to the rethinking function.
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

  # Create a stable output directory that is visible to both processes.
  # tools::R_user_dir() is available in R >= 4.0 and survives across sessions.
  output_dir <- file.path(
    tools::R_user_dir("bayeswatch", which = "cache"),
    paste0("ulam_", format(Sys.time(), "%Y%m%d%H%M%S"), "_", Sys.getpid())
  )
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  # Merge our output_dir into whatever cmdstan_args the caller supplied.
  dots <- list(...)
  cmdstan_args <- dots[["cmdstan_args"]]
  if (is.null(cmdstan_args)) cmdstan_args <- list()
  cmdstan_args[["output_dir"]] <- output_dir

  # Rebuild the dots list with the updated cmdstan_args.
  dots[["cmdstan_args"]] <- cmdstan_args

  run_with_viz(
    fn = function(fn_name, flist, args) {
      rethinking_fn <- getExportedValue("rethinking", fn_name)
      do.call(rethinking_fn, c(list(flist), args))
    },
    fn_args = list(
      fn_name = fn_name,
      flist   = flist,
      args    = dots
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
