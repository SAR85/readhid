test_that("seek_raw() works", {
  dat <- as.raw(c(0x00, 0x00, 0x50, 0x65, 0x61, 0x6B, 0x00, 0x00, 0x00, 0x06))
  ex <- example_hid()

  expect_equal(seek_raw("Peak", 6L, dat), 3L)
  expect_equal(seek_raw("DATA", 1L, ex$raw_data), 416631L)
})

test_that("special_array_length() works", {
  ex <- example_hid()
  file_offset <- seek_raw("Peak", 1L, ex$raw_data)

  expect_equal(
    special_array_length(file_offset, ex$raw_data),
    ex$directory$Peak.1$num_elements
  )
})

test_that("special_data_offset() works", {
  ex <- example_hid()

  expect_equal(
    special_data_offset(ex$raw_data, "Peak", 6L),
    ex$directory$Peak.6$data_offset
  )
})

test_that("extract_raw_data() works", {
  ex <- example_hid()

  dir_entry <- ex$directory$AAct.1
  dir_entry2 <- ex$directory$DyeN.1

  expect_equal(
    extract_raw_data(
      ex$raw_data,
      dir_entry$data_offset,
      dir_entry$data_size
    ),
    ex$directory$AAct.1$raw_data
  )

  expect_equal(
    extract_raw_data(
      ex$raw_data,
      dir_entry2$data_offset,
      dir_entry2$data_size
    ),
    ex$directory$DyeN.1$raw_data
  )
})

test_that("parse_data() works for element type 1", {
  ex <- example_hid()

  # Element type 1
  expect_equal(
    parse_data(
      ex$directory$AAct.1$raw_data,
      ex$directory$AAct.1$type,
      ex$directory$AAct.1$num_elements
    ),
    ex$data$AAct.1
  )
})
