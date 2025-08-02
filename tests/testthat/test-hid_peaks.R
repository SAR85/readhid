test_that("peaks_to_df works", {
  exhid <- example_hid()
  expect_equal(peaks_to_df(exhid$data),
               exhid$peaks)

  no_peaks <- exhid$data[-grep("Peak", names(exhid$data))]
  expect_equal(peaks_to_df(no_peaks),
               NULL)
})
