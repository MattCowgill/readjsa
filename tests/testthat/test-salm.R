test_that("get_salm_urls () works", {
  urls <- get_salm_urls()

  expect_type(urls, "character")
  expect_true(all(tools::file_ext(urls) == "csv"))
  expect_length(urls, 2)
})

test_that("dl_salm() works", {
  skip_if_offline()
  skip_on_ci()
  check_jsa_connection()

  urls <- get_salm_urls()
  for (geog_type in c("LGA", "SA2")){
    salm_file_path <- tempfile(fileext = ".csv")
    expect_false(file.exists(salm_file_path))
    salm_file <- dl_salm(urls[geog_type], path = salm_file_path)
    expect_true(file.exists(salm_file_path))
  }
})

# TODO: Add more tests for read_salm - check reos for reference
