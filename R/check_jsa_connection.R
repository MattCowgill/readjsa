# These functions adapted from {readabs} by Matt Cowgill et al

#' This function checks to see if the JSA website is available.
#' If available, it invisibly returns `TRUE`. If unavailable, it will
#' stop with an error.
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

#' Internal function to check if URL exists using httr2
#' @param url URL for website to check
#' @return Logical. `TRUE` if URL exists and returns HTTP status code in the
#' 200 range; `FALSE` otherwise.
#' @noRd
url_exists <- function(url = "https://www.jobsandskills.gov.au") {
  safe_request <- function(method) {
    tryCatch({
      resp <- httr2::request(url) |>
        httr2::req_method(method) |>
        httr2::req_timeout(1) |>
        httr2::req_perform()
      resp
    }, error = function(e) NULL)
  }

  # Try HEAD first
  resp <- safe_request("HEAD")

  if (is.null(resp) || ((httr2::resp_status(resp) %/% 100) != 2)) {
    # Fallback to GET
    resp <- safe_request("GET")

    if (is.null(resp)) {
      return(FALSE)
    }

    if ((httr2::resp_status(resp) %/% 100) != 2) {
      warning(sprintf("[%s] appears to be online but isn't responding as expected; HTTP status code is not in the 200 to 299 range", url))
      return(FALSE)
    }
    return(TRUE)
  }

  return(TRUE)
}

#' Internal function to check if URL exists. Slower than url_exists. Used
#' for networks that block curl.
#' @param url URL for website to check
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
