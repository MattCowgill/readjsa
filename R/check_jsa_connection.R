# These functions adapted from {readabs} by Matt Cowgill et al

#' This function checks to see if the JSA website is available.
#' If available, it invisibly returns `TRUE`. If unavailable, it will
#' stop with an error.
#'
#' Note that it tries first using `httr::HEAD()` via the `url_exists()` function
#' (below). It is given one second to execute `url_exists()`. If this either
#' fails or takes >1 second, an alternative method is tried.
#'
#' We use two methods as (1) some networks do not play well with curl and
#' functions that rely on it, like `HEAD()`, but (2) the second method of
#' checking the website is slower than the first, so we don't want to default
#' to it and slow down all users.
#'
#' @noRd

check_jsa_connection <- function(url = "https://www.jobsandskills.gov.au") {
  jsa_url_works <- url_exists(url)


  if (isFALSE(jsa_url_works)) {
    jsa_url_works_nocurl <- url_exists_nocurl(url)

    if (isFALSE(jsa_url_works_nocurl)) {
      stop(
        "R cannot access the JSA website.",
        " Please check your internet connection and security settings."
      )
    }
  }

  invisible(TRUE)
}

# Function from: https://stackoverflow.com/a/52915256
#' Internal function to check if URL exists.
#' @param url URL for website to check
#' @return Logical. `TRUE` if URL exists and returns HTTP status code in the
#' 200 range; `FALSE` otherwise.
#' @noRd

url_exists <- function(url = "https://www.jobsandskills.gov.au") {
  sHEAD <- purrr::safely(httr::HEAD)
  sGET <- purrr::safely(httr::GET)

  # Try HEAD first since it's lightweight
  res <- sHEAD(url)

  if (is.null(res$result) ||
      ((httr::status_code(res$result) %/% 200) != 1)) {
    res <- sGET(url)

    if (is.null(res$result)) {
      return(FALSE)
    }

    if (((httr::status_code(res$result) %/% 200) != 1)) {
      warning(sprintf("[%s] appears to be online but isn't responding as expected; HTTP status code is not in the 200-299 range", url))
      return(FALSE)
    }

    return(TRUE)
  } else {
    return(TRUE)
  }
}

#' Internal function to check if URL exists. Slower than url_exists. Used
#' for networks that block curl.
#' @param url URL for website to check
#' does not return expected output).
#' @return Logical. `TRUE` if URL exists and returns HTTP status code in the
#' 200 range; `FALSE` otherwise.
#' @noRd
url_exists_nocurl <- function(url = "https://www.jobsandskills.gov.au") {
  con <- url(url)
  out <- suppressWarnings(tryCatch(readLines(con), error = function(e) e))
  jsa_url_works <- all(class(out) != "error")
  close(con)
  return(jsa_url_works)
}
