
new_survey_data <- function(survey_data, scale_number, reverse_vec){
  as.matrix(survey_data)
  survey_data[, reverse_vec] <- (scale_number + 1) - survey_data[, reverse_vec]
  sum_score <- rowSums(survey_data)
  survey_data$Sum <- sum_score
  return(survey_data)
}



