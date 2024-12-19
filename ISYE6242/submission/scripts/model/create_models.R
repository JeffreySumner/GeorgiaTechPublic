setwd("submission")
library(tidymodels)
library(tidyverse)

data <- readr::read_csv("data/BRFSS_Analysis_Data_2022.csv")

custom_threshold <- .3
# Baseline Model ----

baseline_model_data <- data %>%
  filter(!DIABETE4 %in% c(7,9)) %>%
  mutate(
    Diabetes = case_when(
      DIABETE4 %in% c(1,2,4) ~ 1
      , TRUE ~ 0
    ) %>% as_factor()
  ) %>%
  select(-c(1:9)) %>%
  filter(complete.cases(.)) %>%
  mutate(key = row_number()) 



set.seed(1234)
training_data <- slice_sample(baseline_model_data, prop = .8) 
testing_data <- baseline_model_data %>% anti_join(training_data)

glm_model <- glm(Diabetes ~ ., data = training_data %>% select(-key), family = "binomial")

temp <- predict(glm_model, testing_data, type = "response")

baseline_recipe <- recipe(Diabetes ~ ., data = training_data %>% select(-key, -DIABETE4))

baseline_log_reg <- logistic_reg() %>%
  set_engine("glm")

baseline_log_workflow <- workflow() %>%
  add_model(baseline_log_reg) %>%
  add_recipe(baseline_recipe) %>%
  fit(training_data)

baseline_log_pred <- predict(baseline_log_workflow, testing_data)

baseline_log_pred_temp <- testing_data %>% mutate(pred = baseline_log_pred$.pred_class) %>%
  select(Diabetes, pred) %>%
  filter(!is.na(pred))

caret::confusionMatrix(
  reference = baseline_log_pred_temp$Diabetes  %>% 
    as_factor() %>% 
    factor(levels = rev(levels(.)))
  , data = baseline_log_pred_temp$pred  %>% 
    as_factor() %>%
    factor(levels = rev(levels(.)))) %>%
  draw_confusion_matrix()

## Baseline XGBoost ----

baseline_xgboost_reg <- boost_tree() %>%
  set_engine("xgboost") %>%
  set_mode("classification")

baseline_xgboost_workflow <- workflow() %>%
  add_model(baseline_xgboost_reg) %>%
  add_recipe(baseline_recipe) %>%
  fit(training_data)

baseline_xgboost_pred <- predict(baseline_xgboost_workflow, testing_data)

baseline_xgboost_pred_temp <- testing_data %>% mutate(pred = baseline_xgboost_pred$.pred_class) %>%
  select(Diabetes, pred) %>%
  filter(!is.na(pred))

caret::confusionMatrix(
  reference = baseline_xgboost_pred_temp$Diabetes  %>% 
    as_factor() %>% 
    factor(levels = rev(levels(.)))
  , data = baseline_xgboost_pred_temp$pred  %>% 
    as_factor() %>%
    factor(levels = rev(levels(.)))) %>%
  draw_confusion_matrix()

### XGBoost Importance Plot ----
library(vip)
baseline_xgboost_importance <- vip::vip(baseline_xgboost_workflow, num_features = 20)
baseline_xgboost_importance +
  ggtitle("Variable Importance Plot from XGBOOST Baseline Model") + 
  theme_classic()

# Enhanced framework Recipe 1 ----

enhanced_model_data <- data %>%
  filter(!DIABETE4 %in% c(7,9)) %>%
  mutate(
    Diabetes = case_when(
      DIABETE4 %in% c(1,2,4) ~ 1
      , TRUE ~ 0
    ) %>% as_factor()
    , BMI = `_BMI5`/100
  ) %>%
  mutate(key = row_number()) %>%
  select(key
         , Diabetes
         , `_STATE`
         , `_RFHLTH` # GENHLTH group
         , `_AGE80` # Age groups from 18-24 through 80 or older
         , `_BMI5CAT`
         , DRNKANY6 # Any alcohol last 30 days - just to indicate if drinks or not
         , CHECKUP1
         , CHCKDNY2 # Kidney disease check; https://www.kidney.org/news/newsroom/fsindex#:~:text=People%20with%20kidney%20disease%20are,to%20or%20worsen%20the%20other.
         , EMPLOY1
         , CVDINFR4
         , SEXVAR
         , `_INCOMG1`
         , CVDCRHD4 # Angina or coronary heart disease
         , `_EDUCAG`
         , EXERANY2
         , PERSDOC3
         , CVDSTRK3 # Ever diagnosed with a stroke; https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5298897/#:~:text=Diabetes%20is%20a%20well%2Destablished,stroke%20with%20uncontrolled%20glucose%20levels.
         , `_CHLDCNT`
         , COVIDPOS
         , HAVARTH4
         
         # , MARITAL
         # , PHYSHLTH
         # , VETERAN3
         # , MEDCOST1
         # , GENHLTH # Propose we remove this
         # , ALCDAY4 # Propose we remove this
         ) %>%
  filter(complete.cases(.)) %>%
  mutate(`_AGE80` = case_when(
    `_AGE80` >= 18 & `_AGE80` <= 24 ~ "18 - 24",
    `_AGE80` >= 25 & `_AGE80` <= 29 ~ "25 - 29",
    `_AGE80` >= 30 & `_AGE80` <= 34 ~ "30 - 34",
    `_AGE80` >= 35 & `_AGE80` <= 39 ~ "35 - 39",
    `_AGE80` >= 40 & `_AGE80` <= 44 ~ "40 - 44",
    `_AGE80` >= 45 & `_AGE80` <= 49 ~ "45 - 49",
    `_AGE80` >= 50 & `_AGE80` <= 54 ~ "50 - 54",
    `_AGE80` >= 55 & `_AGE80` <= 59 ~ "55 - 59",
    `_AGE80` >= 60 & `_AGE80` <= 64 ~ "60 - 64",
    `_AGE80` >= 65 & `_AGE80` <= 69 ~ "65 - 69",
    `_AGE80` >= 70 & `_AGE80` <= 74 ~ "70 - 74",
    `_AGE80` >= 75 & `_AGE80` <= 79 ~ "75 - 79",
    `_AGE80` >= 80 & `_AGE80` <= 99 ~ "80 or older",
    TRUE ~ NA_character_  # for ages outside the specified range or NA values
  ))

enhanced_model_data[,3:ncol(enhanced_model_data)] <- lapply(enhanced_model_data[,3:ncol(enhanced_model_data)], as_factor)
saveRDS(enhanced_model_data, "models/model_data/enhanced_model_data.RDS")
set.seed(1234)
training_data <- slice_sample(enhanced_model_data, prop = .80) 
testing_data <- enhanced_model_data %>% anti_join(training_data)

saveRDS(training_data, "models/model_data/enhanced_model_training_data.RDS")
saveRDS(testing_data, "models/model_data/enhanced_model_testing_data.RDS")

enhanced_recipe <- recipe(Diabetes ~ ., data = training_data %>% select(-key)) %>%
  # step_impute_mean(all_numeric(), -all_outcomes()) %>%
  step_naomit(all_predictors(), -all_outcomes()) %>%
  step_dummy(all_factor_predictors(), -all_outcomes())
saveRDS(enhanced_recipe, "models/enhanced_recipe.RDS")
## Logistic Regression Baseline Recipe ----
log_reg <- logistic_reg(penalty = double(1), mixture = double(1)) %>%
  set_engine("glmnet")

log_workflow <- workflow() %>%
  add_model(log_reg) %>%
  add_recipe(enhanced_recipe) %>%
  fit(training_data)

saveRDS(log_workflow, "models/enhanced_log_workflow.RDS")

log_pred <- predict(log_workflow, testing_data)

log_pred_temp <- testing_data %>% mutate(pred = log_pred$.pred_class) %>%
  select(Diabetes, pred) %>%
  filter(!is.na(pred))

caret::confusionMatrix(
  reference = log_pred_temp$Diabetes  %>% 
    as_factor() %>% 
    factor(levels = rev(levels(.)))
  , data = log_pred_temp$pred  %>% 
    as_factor() %>%
    factor(levels = rev(levels(.)))) %>%
  draw_confusion_matrix()

## Decision Tree Baseline Recipe ----
# 
# ranger_reg <- rand_forest(mtry = 1, trees = 100) %>%
#   set_engine("ranger") %>%
#   set_mode("classification")
# 
# ranger_workflow <- workflow() %>%
#   add_model(ranger_reg) %>%
#   add_recipe(enhanced_recipe) %>%
#   fit(training_data)
# 
# temp <- predict(ranger_workflow, testing_data)
# 
# enhanced_prob_temp <- testing_data %>% mutate(pred = temp$.pred_class) %>%
#   select(Diabetes, pred) %>%
#   filter(!is.na(pred))
# 
# caret::confusionMatrix(
#   reference = enhanced_prob_temp$Diabetes  %>% 
#     as_factor() %>% 
#     factor(levels = rev(levels(.)))
#   , data = enhanced_prob_temp$pred  %>% 
#     as_factor() %>%
#     factor(levels = rev(levels(.)))) %>%
#   draw_confusion_matrix()


### Bagged Tree ----
# library(baguette)
# bagged_reg <- bag_tree() %>%
#   set_engine("rpart") %>%
#   set_mode("classification")
# 
# bagged_workflow <- workflow() %>%
#   add_model(bagged_reg) %>%
#   add_recipe(enhanced_recipe) %>%
#   fit(training_data)
# 
# bagged_pred <- predict(bagged_workflow, testing_data)
# 
# bagged_pred_temp <- testing_data %>% mutate(pred = bagged_pred$.pred_class) %>%
#   select(Diabetes, pred) %>%
#   filter(!is.na(pred))
# 
# caret::confusionMatrix(
#   reference = bagged_pred_temp$Diabetes  %>% 
#     as_factor() %>% 
#     factor(levels = rev(levels(.)))
#   , data = bagged_pred_temp$pred  %>% 
#     as_factor() %>%
#     factor(levels = rev(levels(.)))) %>%
#   draw_confusion_matrix()

#### Bagged Tree Importance Plot ----
# importance <- bagged_workflow$fit$fit$fit$imp
# importance_df <- data.frame(Variable = names(importance), Importance = importance)
# ggplot(importance_df, aes(x = reorder(Variable, Importance), y = Importance)) +
#   geom_bar(stat = "identity") +
#   coord_flip() +
#   labs(title = "Variable Importance", x = "Variable", y = "Importance")

### XGBoost ----

xgboost_reg <- boost_tree() %>%
  set_engine("xgboost") %>%
  set_mode("classification")

xgboost_workflow <- workflow() %>%
  add_model(xgboost_reg) %>%
  add_recipe(enhanced_recipe) %>%
  fit(training_data)

saveRDS(xgboost_workflow, "models/enhanced_xgboost_workflow.RDS")

xgboost_pred <- predict(xgboost_workflow, testing_data)

xgboost_pred_temp <- testing_data %>% mutate(pred = xgboost_pred$.pred_class) %>%
  select(Diabetes, pred) %>%
  filter(!is.na(pred))


caret::confusionMatrix(
  reference = xgboost_pred_temp$Diabetes  %>% 
    as_factor() %>% 
    factor(levels = rev(levels(.)))
  , data = xgboost_pred_temp$pred  %>% 
    as_factor() %>%
    factor(levels = rev(levels(.)))) %>%
  draw_confusion_matrix()


#### Ensemble Prediction ----

ensemble_method <- tibble(
  Diabetes = testing_data$Diabetes
  , log_model = predict(log_workflow, testing_data, type = 'prob')$.pred_1
  , tree_model = predict(xgboost_workflow, testing_data, type = 'prob')$.pred_1
) %>%
  mutate(avg_pred = (log_model * .67 + tree_model * .33)
         , pred = ifelse(avg_pred >= .25, 1, 0)) 

caret::confusionMatrix(
  reference = ensemble_method$Diabetes  %>% 
    as_factor() %>% 
    factor(levels = rev(levels(.)))
  , data = ensemble_method$pred  %>% 
    as_factor() %>%
    factor(levels = rev(levels(.)))) %>%
  draw_confusion_matrix()


