
library(tidyverse)

# Model lazy loading ----

predict_lazily <- function(model_type = "log", new_data){
  if(model_type == "log"){
    prediction <- predict(readr::read_rds("models/enhanced_log_workflow.RDS"), new_data, type = 'prob')$.pred_1
  } else if(model_type == "xgboost"){
    prediction <- predict(readr::read_rds("models/enhanced_xgboost_workflow.RDS"), new_data, type = 'prob')$.pred_1
  }
  gc()
  return(prediction)
}

ensembled_predictions <- function(log_pred, xgboost_pred){

  ensemble_method <- tibble(
    log_pred = log_pred
    , xgboost_pred = xgboost_pred
  ) %>%
    mutate(avg_pred = (log_pred * .67 + xgboost_pred * .33)
           , pred = ifelse(avg_pred >= .25, 1, 0))
}

# ensembled_predictions <- function(xgboost_pred){
#   
#   ensemble_method <- tibble(
#     xgboost_pred = xgboost_pred
#   ) %>%
#     mutate(avg_pred = (xgboost_pred * 1)
#            , pred = ifelse(avg_pred >= .25, 1, 0)) 
# }