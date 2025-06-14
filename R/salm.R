#' Read data from Jobs & Skills Australia's Small Area Labour Markets (SALM)
#'
#' @param tables A vector of tables to read, or "all" to read all tables.
#' Possible options are `c("lf_persons", "unemp_persons", "unemp_rate", "all")`
#' @param geog_type A character specifying geography data type
#' Possible options are `c("LGA", "SA2")`
#' @param path Path to the directory where the file(s) should be saved
#' @returns A tibble with the SALM data, in 'long'/tidy format
#' @export
#' @examples
#' \dontrun{
#' # Get a single table
#' read_salm(tables = "lf_persons", geog_type = "LGA")
#'
#' # Get multiple tables
#' read_salm(c("lf_persons", "unemp_persons"))
#'
#' # Get all tables
#' read_salm("all")
#' }
read_salm <- function(tables = "all",
                      geog_type = "LGA",
                      path = tempdir()){

  # Check the table type is valid
  rlang::arg_match(tables, values = c("lf_persons", "unemp_persons", "unemp_rate", "all"), multiple = TRUE)
  rlang::arg_match(geog_type, values = c("LGA", "SA2"))

  # Define path to download data to
  temp_file_loc <- file.path(
    path,
    paste0("jsa_salm_", geog_type, ".csv")
  )

  # Download if not downloaded already
  if (file.exists(temp_file_loc) == FALSE){
    urls <- get_salm_urls()
    dl_file(urls[grep(tolower(geog_type), urls)], temp_file_loc)
  }

  # Process and return the data
  process_salm(temp_file_loc, tables)
}

# Identify URLs for the JSA SALM data - SA2 and LGA level data
get_salm_urls <- function() {

  # Check SALM url for links to excel
  req <- httr2::request("https://www.dewr.gov.au/employment-research/small-area-labour-markets") |>
    httr2::req_options(http_version=2) # Set to HTTP 1.1
  resp <- httr2::req_perform(req) # Perform request

  resp_html <- httr2::resp_body_string(resp) # Convert response to string
  rvest_html <- rvest::minimal_html(resp_html) # Convert to rvest-parsable html

  links <- rvest_html |>
    rvest::html_elements("a") |>
    rvest::html_attr("href")

  datafile_page_url_fragments <- links[grep("datafiles", links)]

  datafile_page_urls <- paste0("https://www.dewr.gov.au",
                               datafile_page_url_fragments)

  get_csv_url <- function(page_url) {
    links_on_page <- page_url |>
      httr2::request() |>
      httr2::req_options(http_version=2) |>
      httr2::req_perform() |>
      httr2::resp_body_string() |>
      rvest::minimal_html() |>
      rvest::html_elements("a") |>
      rvest::html_attr("href")

    paste0("https://www.dewr.gov.au",
           links_on_page[grep("csv", links_on_page)])
  }

  urls <- purrr::map_chr(
    datafile_page_urls,
    get_csv_url
  )

  return(urls)
}

# Function to process the downloaded data selected from wide to long
process_salm <- function(file_path, tables = "all"){

  # Convenience function to convert character numbers to numeric
  chr2num <- function(x){
    if (is.numeric(x)) {
      x
    } else {
      as.numeric(gsub(",", "", dplyr::na_if(x, "-")))
    }

  }

  # Read in the wide data
  raw_dat <- readr::read_csv(file_path, skip = 1, show_col_types = FALSE)
   # convert date columns to numeric
  salm_dat <- raw_dat |>
    dplyr::mutate(
      dplyr::across(
        .cols = dplyr::matches(".*\\d+$", perl = TRUE),
        .fn = chr2num)
    ) |>
    janitor::clean_names() # Tidy up colnames

  # Split into list of 3 tibbles by data_item
  # (i.e., lf persons, unemp persons, emp rate %)
  salm_dat_split <- janitor::clean_names(
    split(salm_dat, salm_dat$data_item)
  )

  # Lookup values for convenient typing
  table_lookup <- c(
    lf_persons = "smoothed_labour_force_persons",
    unemp_persons = "smoothed_unemployment_persons",
    unemp_rate = "smoothed_unemployment_rate_percent")

  # Subset to selected tables if "all" is not selected
  if (!("all" %in% tables)){
    selected_tables <- table_lookup[tables]
    salm_dat_split <- salm_dat_split[selected_tables]
  }

  # Convenience function to convert to proper date data
  fix_dates <- function(dates){
    chr_dates <- gsub("_", " ", dates) # Drop the underscore
    chr_dates <- sub("(.)", ("\\U\\1"), tolower(chr_dates), perl = TRUE) # Convert to proper
    chr_dates <- paste("01", chr_dates) # Paste in the day
    as.Date(chr_dates, format = "%d %b %y") # Convert to date
  }

  # Tidy each dataset required
  salm_dat_tidy <- purrr::map(
    salm_dat_split,
    \(.x){
      df <- tidyr::pivot_longer(
        data = .x,
        cols = dplyr::matches(".*\\d+$", perl = TRUE),
        names_to = "quarter",
        values_to = "value"
      )
      df$quarter <- fix_dates(df$quarter)
      return(df)
    }
  )

  # Return a tibble
  salm_dat_comb <- salm_dat_tidy |>
    dplyr::bind_rows()
  return(salm_dat_comb)
}
