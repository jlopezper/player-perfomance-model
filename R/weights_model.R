set.seed(1005)
matches_df <-
  matches %>% 
  select(-c(match_id, home_score, away_score, team_type, team_id, goal_assist, season, match_date, foul_6_seconds)) %>% 
  mutate(result = ifelse(result == 1, 'V', 'NV'))


split <- sample(seq_len(nrow(matches_df)),
                size = floor(0.85 * nrow(matches_df)))
train <- matches_df[split, ]
test <- matches_df[-split, ]



data_recipe<- recipe(result ~ ., data = train) %>%
  step_center(all_predictors(), -all_outcomes()) %>%
  step_scale(all_predictors(), -all_outcomes()) %>%
  prep(data = train)



train_b <- bake(data_recipe, new_data = train)
test_b <- bake(data_recipe, new_data = test)
glimpse(train_b)



cv.ctrl <- trainControl(method = "cv", number = 10, classProbs = TRUE)


logis <- train(form=result ~ ., data=train_b, method="glm", family="binomial",trControl=cv.ctrl)
predlog<-predict(logis,test_b)
confusionMatrix(predlog, test_b$result, positive = "V")

svm <- train(form=result ~ ., data=train_b, method="svmLinear", trControl=cv.ctrl)
predsvm<-predict(svm,test_b)
confusionMatrix(predsvm, test_b$result, positive = "V")

coefs <- svm$finalModel@coef[[1]]
mat <- svm$finalModel@xmatrix[[1]]

tibble::rownames_to_column(as.data.frame(t(coefs %*% mat)), "variables") %>% 
  rename(coef = V1) %>% 
  as_tibble()

auc(response = as.numeric(test_b$result), predictor = as.numeric(predlog))
auc(response = as.numeric(test_b$result), predictor = as.numeric(predsvm))


# calibration plot
pred_results <- data.frame(logistica = predict(logis, test_b, type = "prob")[,"NV"], 
                           svm = predict(svm, test_b, type = "prob")[,"NV"],
                           result = test_b$result)


lift_obj <- lift(result ~ logistica + svm, data = pred_results)
plot(lift_obj, values = 20, auto.key = list(columns = 3,
                                            lines = TRUE,
                                            points = FALSE))


# extract coefs
coefs <- tidy(logis$finalModel)




#################

cv.ctrl <- trainControl(method = "cv", 
                        number = 10, 
                        classProbs = TRUE,
                        summaryFunction = twoClassSummary)

# Use the expand.grid to specify the search space	
grid <- expand.grid(sigma = c(.01, .015, 0.2),
                    C = c(0.75, 0.9, 1, 1.1, 1.25))


svm.tune <- train(form = result ~ .,
                  data = train_b,
                  method = "svmRadial",
                  metric="ROC",
                  tuneGrid = grid,
                  trControl= cv.ctrl)


grid <- expand.grid(C = c(0.75, 0.9, 1, 1.1, 1.25))

svm.tune2 <- train(form = result ~ .,
                  data = train_b,
                  method = "svmLinear",
                  metric="ROC",
                  tuneGrid = grid,
                  trControl= cv.ctrl)


logis1 <- train(form=result ~ .,
               data=train_b, 
               method="glm", 
               family="binomial",
               trControl=cv.ctrl)

svm.tune
svm.tune2
logis1

predsvm <- predict(svm.tune, test_b)
confusionMatrix(predsvm, test_b$result)
auc(response = as.numeric(test_b$result), predictor = as.numeric(predsvm))



rValues <- resamples(list(svm=svm.tune, svm2 = svm.tune2, logis = logis1))
rValues$values

summary(rValues)

trellis.par.set(caretTheme())
bwplot(rValues)
dotplot(rValues, metric = "ROC")

difValues <- diff(rValues)
dotplot(difValues)


coefs <- svm.tune$finalModel@coef[[1]]
mat <- svm.tune$finalModel@xmatrix[[1]]

coefs <-
  as.data.frame(t(coefs %*% mat)) %>% 
  rownames_to_column() %>% 
  rename(term = rowname,
         estimate = V1) 
