#' Read data from Jobs & Skills Australia's Internet Vacancy Index (IVI)
#'
#' @param tables A vector of tables to read, or "all" to read all tables.
#' Possible options are `c("all", skill", "4dig", "2dig_states", "2dig_regions")`
#' @param path Path to the directory where the file(s) should be saved
#' @returns A tibble with the REOS data, in 'long'/tidy format
#' @export
#' @examples
#'
#' # Get a single table
#' read_ivi("4dig")
#'
#' # Get multiple tables
#' read_ivi(c("4dig", "2dig_states"))
#'
#' # Get all tables
#' read_ivi("all")
#'
read_ivi <- function(tables,
                     path = tempdir()) {
  stopifnot(tables %in% c("all", "skill", "4dig", "2dig_states", "2dig_regions"))

  tables_to_read <- if (tables[1] == "all") {
    c("skill", "4dig", "2dig_states", "2dig_regions")
  } else {
    tables
  }

  purrr::map_dfr(tables_to_read,
    dl_and_read_ivi_table,
    path = path,
    .progress = "Reading IVI"
  )
}

dl_and_read_ivi_table <- function(table,
                                  path) {
  urls <- possible_ivi_urls(table)

  ivi_file <- dl_file(
    urls,
    file.path(
      path,
      paste0("jsa_ivi_", table, ".xlsx")
    )
  )

  read_individual_ivi_table(
    ivi_file,
    table
  )
}

#' @autoglobal
read_individual_ivi_table <- function(file,
                                      table) {
  stopifnot(file.exists(file))
  stopifnot(table %in% c("skill", "4dig", "2dig_states", "2dig_regions"))

  sheet <- switch(table,
    "skill" = c("Trend", "Seasonally Adjusted"),
    "4dig" = "4 digit 3 month average",
    "2dig_states" = c("Trend", "Seasonally Adjusted"),
    "2dig_regions" = "Averaged"
  )

  raw_sheet <- purrr::map_dfr(
    sheet,
    \(x) readxl::read_excel(file,
      sheet = x
    ) |>
      dplyr::mutate(series_type = x) |>
      dplyr::mutate(series_type = dplyr::if_else(series_type %in% c("Trend", "Seasonally Adjusted"),
        series_type,
        "3mma"
      ))
  )

  metadata_cols <- c(
    "anzsco_code",
    "anzsco_title",
    "state",
    "level",
    "title",
    "skill_level",
    "anzsco_code",
    "region",
    "table",
    "series_type"
  )

  tidy_sheet <- raw_sheet |>
    dplyr::mutate(table = table) |>
    dplyr::rename_with(tolower) |>
    tidyr::pivot_longer(
      !dplyr::any_of(metadata_cols),
      names_to = "date"
    ) |>
    dplyr::mutate(
      date = janitor::excel_numeric_to_date(as.numeric(date)),
      value = suppressWarnings(readr::parse_number(as.character(value)))
    ) |>
    dplyr::rename(dplyr::any_of(c("title" = "anzsco_title")))

  tidy_sheet
}

# Form a vector of 2 possible URLs for the latest JSA IVI URLs for a given table
possible_ivi_urls <- function(table) {
  this_month <- as.Date(format(Sys.Date(), "%Y-%m-01"))

  prev_month <- subtract_month(this_month)

  this_month_short <- format(this_month, "%Y-%m")
  prev_month_short <- format(prev_month, "%Y-%m")

  month_to_url_text <- function(date) {
    paste(format(date, "%B"),
      format(date, "%Y"),
      sep = "%20"
    )
  }

  prev_month_long <- month_to_url_text(prev_month)
  two_months_ago_long <- month_to_url_text(subtract_month(prev_month))

  base_url <- "https://www.jobsandskills.gov.au/sites/default/files/"

  table_specific_url_fragment <- switch(table,
    "skill" = "%20Skill%20Level%2C%20States%20and%20Territories%20-%20",
    "4dig" = "4%20Occupations%2C%20States%20and%20Territories%20-%20",
    "2dig_states" = "2%20Occupations%2C%20States%20and%20Territories%20-%20",
    "2dig_regions" = "2%20Occupations%2C%20IVI%20Regions%20-%20"
  )

  urls <- paste0(
    base_url,
    c(
      this_month_short,
      prev_month_short
    ),
    "/Internet%20Vacancies%2C%20ANZSCO",
    table_specific_url_fragment,
    c(
      prev_month_long,
      two_months_ago_long
    ),
    ".xlsx"
  )

  urls
}
