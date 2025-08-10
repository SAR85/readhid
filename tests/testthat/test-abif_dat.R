test_that("parse_dat() works for element type 1", {
  ex <- abif_dat(as.raw(0x01), type = 1L)

  expect_setequal(
    parse_dat(ex),
    1
  )
})

test_that("parse_dat() works for element type 2", {
  ex <- abif_dat(
    charToRaw("test element 2"),
    2L
  )

  expect_setequal(
    parse_dat(ex),
    "test element 2"
  )
})

test_that("parse_dat() works for element type 3", {
  ex <- abif_dat(
    as.raw(c(0x05, 0x39)),
    3L
  )

  expect_setequal(
    parse_dat(ex),
    1337L
  )
})

test_that("parse_dat() works for element type 4", {
  ex <- abif_dat(
    as.raw(c(
      0xff, 0xfd, 0xff, 0xfd, 0xff, 0xfc, 0xff, 0xff, 0xff,
      0xfc
    )),
    4L
  )

  expect_setequal(
    parse_dat(ex),
    c(-3L, -3L, -4L, -1L, -4L)
  )
})

test_that("parse_dat() works for element type 5", {
  ex <- abif_dat(
    as.raw(c(
      0x00, 0x00, 0x0b, 0x67, 0x00, 0x00, 0x0b, 0x68, 0x00,
      0x00, 0x0b, 0x69, 0x00, 0x00, 0x0b, 0x6a
    )),
    5L
  )

  expect_setequal(
    parse_dat(ex),
    2919:2922
  )
})

test_that("parse_dat() works for element type 7", {
  ex <- abif_dat(
    as.raw(c(0x40, 0x00, 0x00, 0x00)),
    7L
  )

  expect_setequal(
    parse_dat(ex),
    2
  )
})

test_that("parse_dat() works for element type 8", {
  ex <- abif_dat(
    as.raw(c(
      0xbf, 0xf0, 0x63, 0x7d, 0xe9, 0x39, 0xea, 0xdd, 0xbf,
      0xc2, 0x0c, 0x9d, 0x9d, 0x34, 0x58, 0xcd, 0x40, 0x18, 0x01, 0xd5,
      0x3c, 0xdd, 0xd6, 0xe0
    )),
    8L
  )

  expect_setequal(
    parse_dat(ex),
    c(-1.02429, -0.14101, 6.00179)
  )
})

test_that("parse_dat() works for element type 10", {
  ex <- abif_dat(
    as.raw(c(0x07, 0xe0, 0x01, 0x06)),
    10L
  )

  expect_setequal(
    parse_dat(ex),
    as.Date("2016-01-06")
  )
})

test_that("parse_dat() works for element type 11", {
  ex <- abif_dat(
    as.raw(c(0x13, 0x05, 0x04, 0x00)),
    11L
  )

  expect_setequal(
    parse_dat(ex),
    list(19L, 5L, 4L, 0L)
  )
})

test_that("parse_dat() works for element type 13", {
  ex <- abif_dat(as.raw(0x01), type = 13L)

  expect_setequal(
    parse_dat(ex),
    TRUE
  )
})

test_that("parse_dat() works for element type 18", {
  ex <- abif_dat(
    as.raw(c(
      0x08, 0x43, 0x6f, 0x6d, 0x6d, 0x65, 0x6e,
      0x74, 0x3a
    )),
    18L
  )

  expect_setequal(
    parse_dat(ex),
    "Comment:"
  )
})

test_that("parse_dat() works for element type 19", {
  ex <- abif_dat(
    as.raw(c(
      0x32, 0x30, 0x31, 0x36, 0x2d, 0x30, 0x35,
      0x2d, 0x31, 0x33, 0x20, 0x30, 0x38, 0x3a, 0x30, 0x30, 0x3a, 0x30,
      0x30, 0x2e, 0x30, 0x00
    )),
    19L
  )

  expect_setequal(
    parse_dat(ex),
    "2016-05-13 08:00:00.0"
  )
})

test_that("special types are correct", {
  expect_equal(special_types(),
                  c(28L, 30L:34L))
})
