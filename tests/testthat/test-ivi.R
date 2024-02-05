test_that("possible_ivi_urls() works", {
  urls <- possible_ivi_urls("4dig")

  expect_type(urls, "character")
  expect_true(all(tools::file_ext(urls) == "xlsx"))
  expect_length(urls, 2)
})

test_that("dl_files() works", {
  testthat::skip_if_offline()
  check_jsa_connection()

  ivi_file <- dl_file(possible_ivi_urls("4dig"))
  expect_true(file.exists(ivi_file))
})

test_that("read_ivi() works", {
  testthat::skip_if_offline()
  check_jsa_connection()

  test_ivi_file <- function(df) {
    expect_s3_class(df$date, "Date")
    expect_type(df$value, "double")
    expect_gt(nrow(df), 0)
  }

  # Test individual tables
  purrr::walk(
    c("skill", "4dig", "2dig_states", "2dig_regions"),
    \(x) read_ivi(x) |>
      test_ivi_file()
  )

  # Test tables = "all"
  test_ivi_file(read_ivi("all"))
})
