test_that("new_abif_head() works", {
  header_raw <- as.raw(c(0x41, 0x42, 0x49, 0x46, 0x01, 0x2c, 0x74, 0x64, 0x69,
                         0x72, 0x00, 0x00, 0x00, 0x01, 0x03, 0xff, 0x00, 0x1c, 0x00, 0x00,
                         0x00, 0xb1, 0x00, 0x00, 0x15, 0x00, 0x00, 0x0c, 0xdf, 0x1b))
  expected <- list(file_format = "ABIF", file_format_version = 300L, name = "tdir",
                   num = 1L, type = 1023L, element_size = 28L, num_elements = 177L,
                   directory_size = 5376L, directory_offset = 843547L)
  class(expected) <- "abif_head"

  expect_equal(new_abif_head(header_raw), expected)
  expect_error(new_abif_head(raw(0x00)))
})
