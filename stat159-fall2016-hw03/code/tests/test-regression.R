# load the source code of the functions to be tested
source("../functions/regression-functions.R")

#---------------------------
# multiple regression
reg = lm(mpg ~ disp + hp, data = mtcars)

# summary of 'reg'
regsum = summary(reg)
#---------------------------

context("Test for residual sum of squares value") 
test_that("RSS works as expected", {
  x = reg
  expect_equal(residual_sum_squares(x), sum(reg$residuals^2))
  expect_length(residual_sum_squares(x), 1)
  expect_type(residual_sum_squares(x), 'double')
})


context("Test for total sums of squares value") 
test_that("TSS works as expected", {
  x = reg
  expect_equal(total_sum_squares(x), sum((mtcars$mpg - mean(mtcars$mpg))^2))
  expect_length(total_sum_squares(x), 1)
  expect_type(total_sum_squares(x), 'double')
})



context("Test for residual standard errors value") 
test_that("RSE works as expected", {
  x = reg
  expect_equal(residual_std_error(x), regsum$sigma)
  expect_length(residual_std_error(x), 1)
  expect_type(residual_std_error(x), 'double')
})


context("Test for r squared value") 
test_that("r squared works as expected", {
  x = reg
  expect_equal(r_squared(x), regsum$r.squared)
  expect_length(r_squared(x), 1)
  expect_type(r_squared(x), 'double')
})



context("Test for f statistic value") 
test_that("f statistic works as expected", {
  x = reg
  expect_equal(f_statistic(x), regsum$fstatistic[1])
  expect_length(f_statistic(x), 1)
  expect_type(f_statistic(x), 'double')
})







