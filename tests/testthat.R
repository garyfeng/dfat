### Unit testing ###
require(testthat)
# until this gets on CRAN
# require(dfat)
# we use the github site
require(devtools)
devtools::install_github('garyfeng/dfat')
# or source("R/dfat.R")

############
testvec <- list(
  list(from=1, to="here", date="1990-12-12"), 
  list(from=2, to="there", via=list("train", "airplane"), date=123.45)
)
key <- "from"
###########

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

test2<- list(
  list(key=c(1,2,3), key1=c("a", "b"), key2=c("this", "that")),
  list(key=c(5,6,7), key1=c("c", "d"), key2=c("one", "two", "three"))
)

# # This is a desired feature? [update] we return NA to keep things strict
# test_that("`@` and `%@@%` return a matrix when the value is a vector itself", {
#   expect_equal(dim(test2@"key"), c(3,2))
#   expect_equal(class(test2@"key1"), "matrix")
#   expect_equal(class(test2@"key2"), "list")
# })

# UNDESIRED side effects when attr contains more than single atomic value (vectors, lists, etc.)
# In the strict @ case, we simply return NA for them
# Use the permissive %@@% to get the result in a list
test_that("these are undesired side-effects that we need to fix", {
  # when the attribute contains vectors of different lengths across rows, we get a list
  expect_equal(test2@"key", c(NA, NA))
  expect_equal(test2@"key1", c(NA, NA))
  expect_equal(test2@"key2", c(NA, NA))
})


