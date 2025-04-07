test_that("locate downlod links returns expected output", {

  obj <- locate_download_link(year = 2012,
                            adm_url = "https://pubfs-rma.fpac.usda.gov/pub/References/actuarial_data_master/")

  expect_equal(is.list(obj), TRUE)
  expect_equal(length(obj), 3)
  expect_equal(names(obj), c("data", "layout","update_date"))
  expect_equal(grepl("2012_ADM_YTD.zip", obj$data), TRUE)
  expect_equal(grepl("ADMLayout.zip", obj$layout), TRUE)

})
