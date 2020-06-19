
context("transmute functions")

test_that("it should convert character to date using a handler", {
  h <- to_date("%Y-%m-%d")
  expect_equal(h("2017-01-01"), as.Date("2017-01-01"))
})

test_that("it should convert character to numeric using a handler", {
  h <- to_dbl()
  expect_equal(h("1"), 1)
  expect_is(h("1"), "numeric")
  h <- to_dbl(dec = ",")
  expect_equal(h("1,5"), 1.5)
  h <- to_dbl(dec = ",", thousands = ".")
  expect_equal(h("1.001,5"), 1001.5)
  h <- to_dbl(dec = ",", thousands = ".")
  expect_equal(h("1.001.001,5"), 1001001.5)
  h <- to_dbl(dec = ",", percent = TRUE)
  expect_equal(h("5,5 %"), 0.055)
  h <- to_dbl(percent = TRUE)
  expect_equal(h("5.5 %"), 0.055)
})

test_that("it should convert character to numeric using functions", {
  expect_equal(as_dbl("1"), 1)
  expect_is(as_dbl("1"), "numeric")
  expect_equal(as_dbl("1,5", dec = ","), 1.5)
  expect_equal(as_dbl("1.001,5", dec = ",", thousands = "."), 1001.5)
  expect_equal(as_dbl("1.001.001,5", dec = ",", thousands = "."), 1001001.5)
  expect_equal(as_dbl("5,5 %", dec = ",", percent = TRUE), 0.055)
  expect_equal(as_dbl("5.5 %", percent = TRUE), 0.055)
})
