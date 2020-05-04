library(here)

source(here("Functions", "Exclude Data by Residual.R"))

obtain_residual <- function(dataset, x, y, p) {
# regression first to obtain the residual.
  modelresult <- lm(y ~ x)
  dataset$result_res  <- resid(modelresult)

# generate a new column to obtain the obsolute value of residuals.
  dataset$result_res_abs <- abs(dataset$result_res )

# exclude the students with 10% largest residual value, and generate a new dataset.
  new_dataset <- dataset[dataset$result_res_abs < quantile(dataset$result_res_abs, 1-p),]
  return(new_dataset)
}


