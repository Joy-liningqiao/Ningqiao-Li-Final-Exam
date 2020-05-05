
obtain_residual <- function(dataset, x, y, p) {
# regression first to obtain the residual.
  modelresult <- lm(y ~ x)
  dataset$result_res  <- resid(modelresult)

# generate a new column to obtain the obsolute value of residuals.
  dataset$result_res_abs <- abs(dataset$result_res )

# select the data less than 10% largest value.
  my_selections <- dataset$result_res_abs < quantile(dataset$result_res_abs, 1-p)

# exclude the students with 10% largest residual value, and generate a new dataset.
  new_dataset <- dataset[my_selections,]
  return(new_dataset)
}


