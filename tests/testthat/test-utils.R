test_that("match_value", {
  object <- "foo"
  expect_error(match_value(object, letters), "object must be one of {a, ",
               fixed = TRUE)
  expect_silent(match_value("a", letters))
})
