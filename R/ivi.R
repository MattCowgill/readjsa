

possible_ivi_urls <- function(table) {
  this_month <- as.Date(format(Sys.Date(), "%Y-%m-01"))

  prev_month <- subtract_month(this_month)

  this_month_short <- format(this_month, "%Y-%m")
  prev_month_short <- format(prev_month, "%Y-%m")

  month_to_url_text <- function(date) {
    paste(format(date, "%B"),
          format(date, "%Y"),
          sep = "%20")
  }

  prev_month_long <- month_to_url_text(prev_month)
  two_months_ago_long <- month_to_url_text(subtract_month(prev_month))

  base_url <- "https://www.jobsandskills.gov.au/sites/default/files/"

  table_specific_url_fragment <- switch(table,
                                        "skill" = "%20Skill%20Level%2C%20States%20and%20Territories%20-%20",
                                        "4dig" = "4%20Occupations%2C%20States%20and%20Territories%20-%20",
                                        "2dig_states" = "2%20Occupations%2C%20States%20and%20Territories%20-%20",
                                        "2dig_regions" = "2%20Occupations%2C%20IVI%20Regions%20-%20")

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
