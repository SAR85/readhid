test_that("parse_dat() works for element type 1", {
  ex <- example_hid()

  # Element type 1
  expect_equal(
    parse_dat(ex$directory$AAct.1$raw_data),
    ex$directory$AAct.1$parsed_data
  )
})
