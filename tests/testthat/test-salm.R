test_that("get_salm_urls () works", {
  urls <- get_salm_urls()

  expect_type(urls, "character")
  expect_length(urls, 2)
})


test_that("read_salm() works", {
  skip_if_offline()
  skip_on_ci()
  check_jsa_connection()

  test_salm_file <- function(df) {
    expect_s3_class(df$quarter, "Date")
    expect_type(df$value, "double")
    expect_gt(nrow(df), 0)
  }

  test_salm_file(read_salm("lf_persons"))

})
