

dl_reos <- function(file = tempfile(fileext = ".xlsx"),
                    urls = possible_reos_urls()) {
  
  stopifnot(length(urls) == 2)
  
  safely_dl <- purrr::safely(download.file)
  
  latest_url_result <- safely_dl(url = urls[1],
                                 destfile = file,
                                 mode = "wb",
                                 quiet = TRUE)
  
  if (!is.null(latest_url_result$error)) {
    return(file)
  }
  
  prior_url_result <- safely_dl(url = urls[2],
                                destfile = file,
                                mode = "wb",
                                quiet = TRUE)
  
  if (!is.null(latest_url_result$error)) {
    return(file)
  } else {
    stop("Could not download REOS file from urls: ", 
         paste(urls, collapse = ", or "), 
         ".")
  }
  
}

#' Given a date, identify two URLs for the JSA REOS data - corresponding to 
#' the previous two months.
#' @value A vector of URLs, length 2
possible_reos_urls <- function(today = Sys.Date()) {
  "https://www.jobsandskills.gov.au/sites/default/files/2024-01/recruitment_insights_report_-_december_2023_data_file.xlsx"
  
  this_month <- as.Date(format(today, "%Y-%m-01"))
  
  subtract_month <- function(date) {
    this_month_num <- as.numeric(format(date, "%m"))
    this_year_num <- as.numeric(format(date, "%Y"))
    
    prev_month_num <- ifelse(this_month_num == 1, 
                             12, 
                             this_month_num - 1)
    
    prev_year_num <- ifelse(this_month_num == 1, 
                            this_year_num - 1, 
                            this_year_num)
    
    prev_month <- as.Date(paste(prev_year_num, prev_month_num, "01", sep = "-"))

    prev_month
  }
  
  this_month_short <- format(this_month, "%Y-%m")
  prev_month_short <- format(subtract_month(this_month), "%Y-%m")
  
  prev_month_long <- tolower(format(prev_month, "%B_%Y"))
  two_months_ago_long <- tolower(format(subtract_month(prev_month), "%B_%Y"))
  
  base_url <- "https://www.jobsandskills.gov.au/sites/default/files/"
  
  urls <- paste0(base_url,
         c(this_month_short,
           prev_month_short),
         "/recruitment_insights_report_-_",
         c(prev_month_long,
           two_months_ago_long),
         "_data_file.xlsx")
  
  urls
}
