test_that("function exists", {
  expect_type(qvb, "closure")
})

test_that("function returns a proper length at age value", {
  expect_equal(
    qvb(age = 1,
        p = list(300, 0.75, 1.2, 2, 0)),
    243.75,
    tolerance = 0.01)
})

test_that("function returns multiple length at age values", {
  expect_equal(
    qvb(age = seq(0, 10, by = 1),
        p = list(300, 0.75, 1.2, 2, 0)) %>%
      round(digits = 1),
    c(0, 243.7, 343.2, 398.7, 432.4, 453.8, 468.0, 477.6, 484.4, 489.2, 492.7)
  )
})

test_that("function stops and give message
          when incorrect variables are given", {
  expect_error(
    qvb("foo",
        p = list(100, 1, 0.5, 2, 0)),
    "Age must be numeric."
  )
  expect_error(
    qvb(2,
        p = list("100", 1, 0.5, 2, 0)),
    "Parameters must be numeric."
  )
  expect_error(
    qvb(2,
        p = list(100, 1, 0.5, 2)),
    "All five parameters needs to have a value."
  )
})

test_that("function warns when q = 1 is used", {
  expect_error(
    qvb(2,
        p = list(100, 1, 1, 2, 0)),
    "Parameter 'q' must not be 1."
  )
})
