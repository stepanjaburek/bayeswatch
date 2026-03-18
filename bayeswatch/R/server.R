# Internal environment to hold server state
.bayeswatch_env <- new.env(parent = emptyenv())
.bayeswatch_env$server  <- NULL
.bayeswatch_env$port    <- NULL
.bayeswatch_env$done    <- FALSE
.bayeswatch_env$message <- "Sampling\u2026"

#' Start the local httpuv server and open the RStudio Viewer
#'
#' @param port Integer port to listen on (default: random free port)
#' @keywords internal
start_viewer <- function(port = find_free_port()) {

  # Clean up any previous server so we never leak ports
  stop_viewer()

  www_dir <- system.file("www", package = "bayeswatch")

  app <- list(
    call = function(req) {
      path <- req$PATH_INFO

      # Status endpoint polled by the JS page
      if (path == "/status") {
        body <- jsonlite::toJSON(list(
          done    = .bayeswatch_env$done,
          message = .bayeswatch_env$message
        ), auto_unbox = TRUE)
        return(list(
          status  = 200L,
          headers = list("Content-Type" = "application/json"),
          body    = body
        ))
      }

      # Serve index.html (and any other static assets)
      file_path <- if (path == "/" || path == "") {
        file.path(www_dir, "index.html")
      } else {
        file.path(www_dir, sub("^/", "", path))
      }

      if (file.exists(file_path)) {
        content_type <- guess_mime(file_path)
        return(list(
          status  = 200L,
          headers = list("Content-Type" = content_type),
          body    = readBin(file_path, "raw", file.info(file_path)$size)
        ))
      }

      list(status = 404L, headers = list(), body = "Not found")
    }
  )

  srv <- httpuv::startServer("127.0.0.1", port, app)
  .bayeswatch_env$server <- srv
  .bayeswatch_env$port   <- port
  .bayeswatch_env$done   <- FALSE

  url <- paste0("http://127.0.0.1:", port)
  message("[bayeswatch] Viewer running at ", url)

  # Open in RStudio Viewer if available, otherwise browser
  if (rstudioapi::isAvailable()) {
    rstudioapi::viewer(url)
  } else {
    utils::browseURL(url)
  }

  invisible(url)
}

#' Stop the local server
#' @keywords internal
stop_viewer <- function() {
  if (!is.null(.bayeswatch_env$server)) {
    .bayeswatch_env$server$stop()
    .bayeswatch_env$server <- NULL
    message("[bayeswatch] Viewer stopped.")
  }
}

#' Mark sampling as complete so the status badge updates
#' @keywords internal
mark_done <- function() {
  .bayeswatch_env$done    <- TRUE
  .bayeswatch_env$message <- "Done"
}

#' Find a free TCP port using a socket probe
#' @keywords internal
find_free_port <- function(start = 8600L, end = 8700L) {
  for (p in start:end) {
    con <- tryCatch(
      suppressWarnings(socketConnection("127.0.0.1", port = p,
                                        open = "r+b", blocking = FALSE,
                                        timeout = 0.05)),
      error = function(e) NULL
    )
    if (is.null(con)) {
      # Nothing listening on that port — it's free
      return(p)
    }
    close(con)
  }
  stop("[bayeswatch] No free port found in range ", start, "-", end)
}

#' Guess MIME type from file extension
#' @keywords internal
guess_mime <- function(path) {
  ext <- tolower(tools::file_ext(path))
  switch(ext,
    html = "text/html",
    css  = "text/css",
    js   = "application/javascript",
    json = "application/json",
    png  = "image/png",
    jpg  = ,
    jpeg = "image/jpeg",
    svg  = "image/svg+xml",
    "text/plain"
  )
}
