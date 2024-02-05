#' Download data from the JSA
#' @param urls A vector of two (2!) URLs; the second is a fallback for the first
#' @param file Path to the file where the data should be saved
#' @returns Returns the full path to the file where the data is saved.
#'
dl_file <- function(urls,
                    file = tempfile(fileext = ".xlsx")) {
  stopifnot(tools::file_ext(file) == "xlsx")

  stopifnot(length(urls) == 2)

  safely_dl <- purrr::safely(utils::download.file)

  latest_url_result <- suppressWarnings(safely_dl(
    url = urls[1],
    destfile = file,
    mode = "wb",
    quiet = TRUE
  ) )

  if (is.null(latest_url_result$error)) {
    return(file)
  }

  prior_url_result <- safely_dl(
    url = urls[2],
    destfile = file,
    mode = "wb",
    quiet = TRUE
  )

  if (is.null(prior_url_result$error)) {
    return(file)
  } else {
    stop(
      "Could not download REOS file from urls: ",
      paste(urls, collapse = ", or "),
      "."
    )
  }
}
