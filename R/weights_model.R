set.seed(2020)

create_train_test <-
  function(data, variables_to_drop, prop = 0.85) {
    matches_df <-
      matches[!names(matches) %in% variables_to_drop] %>%
      mutate(result = ifelse(result == 1, "V", "NV"))
    matches_df$result <- factor(matches_df$result)

    split <- sample(seq_len(nrow(matches_df)),
      size = floor(prop * nrow(matches_df))
    )
    train <- matches_df[split, ]
    test <- matches_df[-split, ]

    returned_list <- list(
      train = train,
      test = test
    )
    returned_list
  }


df <- create_train_test(
  data = matches,
  variables_to_drop = c(
    "match_id",
    "home_score",
    "away_score",
    "team_type",
    "team_id",
    "goal_assist",
    "season",
    "match_date",
    "foul_6_seconds"
  ),
  prop = 0.8
)



preprocess_data <-
  function(train, test) {
    # create data recipe
    # center and scale data
    data_recipe <- recipe(result ~ ., data = train) %>%
      # step_corr(all_predictors(), -all_outcomes(), threshold = .6) %>% 
      step_center(all_predictors(), -all_outcomes()) %>%
      step_scale(all_predictors(), -all_outcomes()) %>%
      prep(data = train)

    train_b <- bake(data_recipe, new_data = train)
    test_b <- bake(data_recipe, new_data = test)

    returned_list <- list(
      train = train_b,
      test = test_b
    )

    returned_list
  }

df_preprocessed <- preprocess_data(df$train, df$test)



run_models <-
  function(train, test) {
    # Specify train control-CV
    cv_ctrl <- trainControl(
      method = "cv",
      number = 10,
      classProbs = TRUE,
      summaryFunction = twoClassSummary
    )

    # MODEL 1: SVM Radial
    svm_tune_radial <- train(
      form = result ~ .,
      data = train,
      method = "svmRadial",
      # metric = "ROC",
      tuneGrid = expand.grid(
        sigma = c(.01, .015, 0.2),
        C = c(0.75, 0.9, 1, 1.1, 1.25)
      ),
      trControl = cv_ctrl
    )

    # MODEL 2: SVM Linear
    svm_tune_linear <- train(
      form = result ~ .,
      data = train,
      method = "svmLinear",
      # metric = "ROC",
      tuneGrid = expand.grid(C = c(0.75, 0.9, 1, 1.1, 1.25)),
      trControl = cv_ctrl
    )

    # MODEL 3: Logistic regression
    logis <- train(
      form = result ~ .,
      data = train,
      method = "glm",
      family = "binomial",
      # metric = "ROC",
      trControl = cv_ctrl
    )

    models <- list(
      svm_radial = svm_tune_radial,
      svm_linear = svm_tune_linear,
      logistic = logis
    )

    models
  }


all_models <- run_models(train = df_preprocessed$train, test = df_preprocessed$test)



pick_best_model <-
  function(models, test) {
    auc_models <- map(
      all_models,
      function(x) {
        pred <- predict(x, test)
        ModelMetrics::auc(actual = test$result, predicted = pred)
      }
    )

    cat(
      "Best model:", names(which.max(auc_models)), "\n",
      "AUC:", auc_models[[names(which.max(auc_models))]], "\n"
    )
    
    return(all_models[[names(which.max(auc_models))]])
  }

selected_model <- pick_best_model(models = all_models, test = df_preprocessed$test)




evaluation_models_plot <- 
  function(models) {
    r_values <- resamples(models)
    bwp <- bwplot(r_values,
                  scales = list(cex = 1.8),
                  main = list('Models evaluation', cex = 2),
                  par.strip.text = list(cex = 2))
    png(filename = here('analyses', 'plots', 'models_evaluation.png'),
        width = 1500, 
        height = 750)
    print(bwp)
    invisible(dev.off())
  }

evaluation_models_plot(all_models)



grab_coefficients <-
  function(model) {
    # if selected model is logistic
    if (model$method == "glm") {
      return(tidy(model$finalModel)[-1, c("term", "estimate")])
    }

    # if not, selected model is SVM
    coefs <- model$finalModel@coef[[1]]
    mat <- model$finalModel@xmatrix[[1]]

    coefs <-
      as.data.frame(t(coefs %*% mat)) %>%
      rownames_to_column() %>%
      rename(
        term = rowname,
        estimate = V1
      )

    coefs
  }


coefs <- grab_coefficients(model = selected_model)



save_model <- function(model, coefs) { 
    save(model, coefs, file = here('model', 'model.Rda'))
}


save_model(model = selected_model, coefs = coefs)





