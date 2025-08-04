test_that("hid() works", {
  ex <- example_hid()
  path <- test_path("fixtures/A02_RD14-0003-15d2U60-0.25GF-Q4.5_01.15sec.hid")

  expect_no_error(hid(path, keep_data = "both"))

  test_hid <- hid(path, keep_data = "both")

  # Ignore the file path
  test_hid$file <- NULL
  ex$file <- NULL

  expect_identical(test_hid, ex)
})

test_that("new_hid() works", {
  ex <- example_hid()
  path <- test_path("fixtures/A02_RD14-0003-15d2U60-0.25GF-Q4.5_01.15sec.hid")

  test_hid <- new_hid(path, keep_data = "both")

  # Ignore the file path
  test_hid$file <- NULL
  ex$file <- NULL

  expect_identical(test_hid, ex)

  expect_error(new_hid(test_path("fixtures/example_hid.RDS")))
})
