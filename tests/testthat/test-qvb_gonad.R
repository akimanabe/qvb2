test_that("function exists", {
  expect_type(qvb_gonad, "closure")
})

test_that("function returns a proper length at age value", {
  expect_equal(
    qvb_gonad(L = 200,
              p = list(300, 0.75, 1.2, 2, 0.005),
              a = 0.0001,
              b = 3),
    5.508,
    tolerance = 0.01)

  expect_equal(
    qvb_gonad(L = 300,
              p = list(500, 2, 0.8, 4, 0.005),
              a = 0.0001,
              b = 3),
    14.15,
    tolerance = 0.01)
})

test_that("function returns multiple length at age values", {
  expect_equal(
    qvb_gonad(L = c(300,400),
              p = list(500, 2, 0.8, 4, 0.005),
              a = 0.0001,
              b = 3),
    c(14.14607, 36.73578),
    tolerance = 0.0001
  )
})

test_that("function stops and give message
          when incorrect variables are given", {
            expect_error(
              qvb_gonad("foo",
                        p = list(100, 1, 0.5, 2, 0.0005),
                        a = 0.0001,
                        b = 3),
              "Length must be numeric."
            )
            expect_error(
              qvb_gonad(2,
                        p = list("100", 1, 0.5, 2, 0.0005),
                        a = 0.0001,
                        b = 3),
              "Parameters must be numeric."
            )
            expect_error(
              qvb_gonad(2,
                        p = list(100, 1, 0.5, 2),
                        a = 0.0001,
                        b = 3),
              "All five parameters needs to have a value."
            )
            expect_error(
              qvb_gonad(2,
                        p = list(100, 1, 0.5, 2, 0),
                        a = 0.0001,
                        b = 3),
              "Conversion factor c msut be greater than 0."
            )
          })

test_that("function warns when q = 1 is used", {
  expect_error(
    qvb_gonad(2,
              p = list(100, 1, 1, 2, 0),
              a = 0.0001,
              b = 3),
    "Parameter 'q' must not be 1."
  )
})
