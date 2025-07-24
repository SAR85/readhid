test_that("hid() works", {
  ex <- example_hid()
  path <- test_path("fixtures/A02_RD14-0003-15d2U60-0.25GF-Q4.5_01.15sec.hid")
  expect_equal(hid(path), hid(path))
})
