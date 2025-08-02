test_that("hid() works", {
  ex <- example_hid()
  path <- test_path("fixtures/A02_RD14-0003-15d2U60-0.25GF-Q4.5_01.15sec.hid")

  expect_no_error(hid(path))

  test_hid <- hid(path)

  test_hid$file <- NULL
  ex$file <- NULL
  expect_identical(test_hid, ex)
})

test_that("new_hid() works", {
  ex <- example_hid()
  path <- test_path("fixtures/A02_RD14-0003-15d2U60-0.25GF-Q4.5_01.15sec.hid")

  test_hid <- new_hid(path)
  test_hid$file <- NULL
  ex$file <- NULL
  expect_identical(test_hid, ex)

  expect_error(new_hid(test_path("fixtures/example_hid.RDS")),
               regexp = NULL)
})

test_that("hid_data() works", {
  ex <- example_hid()
  data_names <- names(ex$data[grep("DATA", names(ex$data))])
  expect_equal(hid_data(ex), ex$data)
  expect_setequal(names(hid_data(ex, "DATA")), data_names)
  expect_error(hid_data(list(data = 1)))
  expect_error(hid_data(ex, c("1", "2")))

  ex$data <- NULL
  expect_warning(hid_data(ex))
})

test_that("hid_peaks() works", {
  ex <- example_hid()
  expect_equal(hid_peaks(ex), ex$peaks)
})
