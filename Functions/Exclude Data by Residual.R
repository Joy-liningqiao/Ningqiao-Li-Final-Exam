library(here)

source(here("Functions", "Exclude Data by Residual.R"))

obtain_residual <- function(dataset, x, y, p) {
  modelresult <- lm(y ~ x)
  dataset$result_res  <- resid(modelresult)
  dataset$result_res_abs <- abs(dataset$result_res )
  new_dataset <- dataset[dataset$result_res_abs < quantile(dataset$result_res_abs, 1-p),]
  return(new_dataset)
}


