# int32
ex_int32 <- writeBin(c(0L, 1L, 2L, 3L, -200L), raw(), size = 4, endian = "big")
test_that("32-bit integers parse correctly", {
  expect_identical(int32(ex_int32), 0L)
  expect_identical(int32(ex_int32, n = length(ex_int32)), c(0L, 1L, 2L, 3L, -200L))
})

# int16
ex_int16 <- writeBin(c(0L, 1L, 2L, 3L, -200L), raw(), size = 2, endian = "big")
test_that("16-bit integers parse correctly", {
  expect_identical(int16(ex_int16), 0L)
  expect_identical(int16(ex_int16, n = length(ex_int16)), c(0L, 1L, 2L, 3L, -200L))
})

# int8
ex_int8 <- writeBin(c(0L, 1L, 2L, 3L, -2L), raw(), size = 1, endian = "big")
test_that("8-bit integers parse correctly", {
  expect_identical(int8(ex_int8), 0L)
  expect_identical(int8(ex_int8, n = length(ex_int8)), c(0L, 1L, 2L, 3L, -2L))
})

# uint16
ex_uint16 <- writeBin(c(0L, 1L, 2L, 3L), raw(), size = 2, endian = "big")
test_that("16-bit unsigned integers parse correctly", {
  expect_identical(uint16(ex_uint16), 0L)
  expect_identical(uint16(ex_uint16, n = length(ex_uint16)), c(0L, 1L, 2L, 3L))
})

# uint8
ex_uint8 <- writeBin(c(0L, 1L, 2L, 3L), raw(), size = 1, endian = "big")
test_that("8-bit unsigned integers parse correctly", {
  expect_identical(uint8(ex_uint8), 0L)
  expect_identical(uint8(ex_uint8, n = length(ex_uint8)), c(0L, 1L, 2L, 3L))
})

# float32
test_that("32-bit floats parse correctly", {
  ex_float32 <- writeBin(c(0, 1, 2, 3), raw(), size = 4, endian = "big")
  expect_equal(float32(ex_float32), 0)
  expect_equal(float32(ex_float32, n = length(ex_float32)), c(0, 1, 2, 3))
})

# float64

test_that("64-bit floats parse correctly", {
  ex_float64 <- writeBin(c(0, 1, 2, 3), raw(), size = 8, endian = "big")
  expect_equal(float64(ex_float64), 0)
  expect_equal(float64(ex_float64, n = length(ex_float64)), c(0, 1, 2, 3))
})

# rtc
test_that("basic rtc() functionality works", {
  expect_identical(rtc(charToRaw("RTC test")), "RTC test")
})

# hidDate
test_that("basic hidDate functionality works", {
  expect_identical(
    hidDate(as.raw(c(0x07, 0xe0, 0x01, 0x06))),
    as.Date("2016-01-06")
  )
})

# hidTime
test_that("basic hidTime functionality works", {
  expect_identical(
    hidTime(as.raw(c(0x13, 0x05, 0x04, 0x00))),
    list(
      hour = 19L,
      minute = 5L,
      second = 4L,
      hsecond = 0L
    )
  )
})

# char
test_that("basic char functionality works", {
  expect_identical(char(as.raw(c(0x33, 0x35, 0x30, 0x30))), "3500")
})

# pString
test_that("basic pString functionality works", {
  expect_identical(
    pString(as.raw(c(
      0x15, 0x32, 0x30, 0x31, 0x36, 0x2d, 0x30, 0x34, 0x2d,
      0x30, 0x39, 0x20, 0x30, 0x38, 0x3a, 0x30, 0x30, 0x3a, 0x30, 0x30,
      0x2e, 0x30
    ))),
    "2016-04-09 08:00:00.0"
  )
})
