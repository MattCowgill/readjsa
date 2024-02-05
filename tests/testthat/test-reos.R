test_that("possible_reos_urls() works", {
  urls <- possible_reos_urls()

  expect_type(urls, "character")
  expect_true(all(tools::file_ext(urls) == "xlsx"))
  expect_length(urls, 2)
})

test_that("dl_files() works", {
  testthat::skip_if_offline()
  reos_file <- tempfile(fileext = ".xlsx")
  expect_false(file.exists(reos_file))
  reos_file <- dl_file(possible_reos_urls())
  expect_true(file.exists(reos_file))
})

test_that("read_reos() works", {
  testthat::skip_if_offline()
  test_reos_file <- function(df) {
    expect_length(df, 7)
    expect_s3_class(df$date, "Date")
    expect_type(df$value, "double")
    expect_gt(nrow(df), 0)
  }

  test_reos_file(read_reos("1.1"))
  test_reos_file(read_reos(c("1.1", "1.2")))
  test_reos_file(read_reos())

  expect_identical(
    read_reos(),
    read_reos("all")
  )
})
