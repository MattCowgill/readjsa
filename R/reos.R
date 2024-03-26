#' Read data from Jobs & Skills Australia's
#' Recruitment Experiences and Outlook Survey (REOS)
#'
#' @param tables A vector of table numbers to read, or "all" to read all tables
#' @param file Path to the file where the data should be saved
#' @returns A tibble with the REOS data, in 'long'/tidy format
#' @export
#' @examples
#' \dontrun{
#' # Get a single table
#'
#' read_reos(tables = "1.1")
#'
#' # Get multiple tables
#'
#' read_reos(tables = c("1.1", "1.2"))
#'
#' # Get all tables
#'
#' read_reos(tables = "all")
#' }
read_reos <- function(tables = "all",
                      file = tempfile(fileext = ".xlsx")) {
  reos_path <- dl_file(
    urls = possible_reos_urls(),
    file
  )

  if (tables[1] == "all") {
    all_tables <- readxl::excel_sheets(file)
    non_info_tables <- all_tables[!grepl("Information", all_tables)]
    tables <- readr::parse_number(non_info_tables)
  }

  purrr::map_dfr(as.character(tables),
    read_individual_reos_table,
    file = reos_path
  )
}

#' @autoglobal
read_individual_reos_table <- function(file,
                                       table) {

  metadata <- get_reos_metadata(file) |>
    dplyr::mutate(table = as.character(table))

  table_sheets <- readxl::excel_sheets(file)

  table_sheet_name <- table_sheets[grepl(table, table_sheets)]

  topic <- substr(table_sheet_name, 5, nchar(table_sheet_name))

  raw_table <- readxl::read_excel(file,
    sheet = table_sheet_name,
    col_names = FALSE,
    .name_repair = "minimal"
  )

  first_row <- which(
    tolower(raw_table[[1]]) %in% c("month", "quarter")
  )[1]

  colnames(raw_table) <- raw_table[first_row, ]
  colnames(raw_table)[1] <- "date"
  raw_table <- raw_table[(first_row + 1):nrow(raw_table), ]

  raw_table |>
    dplyr::mutate(
      dplyr::across(
        dplyr::everything(),
        \(x) suppressWarnings(
          as.numeric(x)
        )
      ),
      date = janitor::excel_numeric_to_date(date)
    ) |>
    tidyr::pivot_longer(!date,
      names_to = "disaggregation"
    ) |>
    dplyr::mutate(table = as.character(table),
                  topic = topic) |>
    dplyr::left_join(metadata,
      by = "table"
    ) |>
    dplyr::select(
      date, table, topic, series,
      frequency, disaggregation,
      value
    )
}





#' Given a date, identify two URLs for the JSA REOS data - corresponding to
#' the previous two months.
#' @returns A vector of URLs, length 2
possible_reos_urls <- function() {
  this_month <- as.Date(format(Sys.Date(), "%Y-%m-01"))

  prev_month <- subtract_month(this_month)

  this_month_short <- format(this_month, "%Y-%m")
  prev_month_short <- format(prev_month, "%Y-%m")

  prev_month_long <- tolower(format(prev_month, "%B_%Y"))
  two_months_ago_long <- tolower(format(subtract_month(prev_month), "%B_%Y"))

  base_url <- "https://www.jobsandskills.gov.au/sites/default/files/"

  urls <- paste0(
    base_url,
    c(
      this_month_short,
      prev_month_short
    ),
    "/recruitment_insights_report_-_",
    c(
      prev_month_long,
      two_months_ago_long
    ),
    "_data_file.xlsx"
  )

  urls
}

subtract_month <- function(date) {
  this_month_num <- as.numeric(format(date, "%m"))
  this_year_num <- as.numeric(format(date, "%Y"))

  prev_month_num <- ifelse(this_month_num == 1,
    12,
    this_month_num - 1
  )

  prev_year_num <- ifelse(this_month_num == 1,
    this_year_num - 1,
    this_year_num
  )

  prev_month <- as.Date(paste(prev_year_num, prev_month_num, "01", sep = "-"))

  prev_month
}

get_reos_metadata <- function(reos_file = dl_file(possible_reos_urls())) {
  reos_file |>
    readxl::read_excel(range = "Information!B15:E27") |>
    dplyr::rename_with(tolower) |>
    dplyr::mutate(table = readr::parse_number(table)) |>
    dplyr::select(series, frequency, table)
}
