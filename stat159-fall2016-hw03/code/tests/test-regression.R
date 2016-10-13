# load the source code of the functions to be tested
source("functions/regression_functions.R")

#---------------------------
# multiple regression
reg = lm(mpg ~ disp + hp, data = mtcars)

# summary of 'reg'
regsum = summary(reg)
#---------------------------

# context with one test that groups expectations
context("Test for residual sum of squares value") 

test_that("RSS works as expected", {
  x = reg
  
  expect_equal(residual_sum_squares(x), 4)
  expect_length(residual_sum_squares(x), 1)
  expect_type(residual_sum_squares(x), 'double')
})


test_that("RSS works as expected", {
  y <- c(1, 2, 3, 4, NA)
  
  expect_equal(range_value(y), 3)
  expect_length(range_value(y), 1)
})

test_that("RSS works as expected", {
  z <- c(TRUE, FALSE, TRUE)
  
  expect_equal(range_value(z), 1L)
  expect_length(range_value(z), 1)
  expect_type(range_value(z), 'integer')
})

test_that("RSS works as expected", {
  w <- letters[1:5]
  
  expect_error(range_value(w), 'non-numeric argument to binary operator')
})