
residual_sum_squares = function(object){
  # calculates the residual sum of squares by taking an lm object as input and returns a numeric RSS
  # sum of (y_i - y_hat)^2
  return(sum((object$residuals)^2))
}

total_sum_squares = function(object){
  # calculates the total sum of squares by taking an lm object as input and returns a numeric TSS
  y = object$model[ ,1]
  y_bar = mean(y)
  return(sum((y - y_bar)^2))
}

r_squared = function(object){
  # calculates the r sqaured by taking an lm object as input and returns a numeric r squared
  return(1-(residual_sum_squares(object)/total_sum_squares(object)))
}
  
f_statistic = function(object){
  # calculates the F stat by taking an lm object as input and returns a numeric F stat
  return (summary(object)$fstatistic[1])
  
}

residual_std_error = function(object){
  # calculates the residual stadard error by taking an lm object as input and returns a numeric RSE
  n = nrow(object$model)
  p = length(object$coefficients) - 1
  RSS = residual_sum_squares(object)
  return(((1/(n - p - 1))*RSS)^(1/2))
}
