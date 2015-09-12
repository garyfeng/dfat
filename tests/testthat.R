### Unit testing ###
require(testthat)
require(dfat)

testvec <- list(
  list(from=1, to="here", date="1990-12-12"), 
  list(from=2, to="there", via=list("train", "airplane"), date=123.45)
)

key <- "from"

test_that("`@` and `@@` return the desired properties as a vector", {
  expect_equal(testvec%@%"from", c(1,2))
  expect_equal(testvec%@%"to", c("here", "there"))
  expect_equal(testvec@key, c(1,2))
  expect_equal(testvec@"to", c("here", "there"))
  expect_equal(`@`(testvec, 2), c("here", "there"))
  
  expect_equal(testvec%@@%"from", c(1,2))
  expect_equal(testvec%@@%"to", c("here", "there"))
  expect_equal(`%@@%`(testvec, 2), c("here", "there"))
})

test_that("`@` and `%@@%` return a vector of NA when the property 
          does not exist in the element, without a warning or a message", {
            expect_equal(testvec%@%"not a property", c(NA, NA))
            expect_equal(testvec%@@%"not a property", c(NA, NA))
            })

test_that("`@` and `%@@%` coerce returned vector into the class of first element", {
  expect_equal(testvec@"date", c("1990-12-12", "123.45"))
  expect_equal(testvec%@@%"date", c("1990-12-12", "123.45"))
})

test_that("`@` returns a vector of NA when the property does not exist, whereas
           the more permissive `%@@%` will return a list", {
             expect_equal(testvec%@@%"via", list(NA, list("train", "airplane")))
             expect_equal(testvec%@%"via", c(NA, NA))
           })

test_that("`@` and `%@@%` return a vector of NA when the index is out of bound, and prints a message", {
  expect_equal(`@`(testvec, 10), c(NA, NA))
  expect_equal(`%@%`(testvec, list(1,"that")), c(NA, NA))
  expect_equal(`%@@%`(testvec, 10), c(NA, NA))
  expect_equal(`%@@%`(testvec, list(1,"that")), c(NA, NA))
})

test_that("`@` and `%@@%` throw errors in the following tests", {
  expect_error(`@`(whatever, 10))
  expect_error(`@`(testvec, nonsenseKey))
  expect_error(`%@%`(list(1,"that")))
  expect_error(`%@%`("that", 1))
  expect_error(`%@@%`(c(1,2,3), 1))
})