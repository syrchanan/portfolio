# Cleaning and Loading ####

library(tidyverse)
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(Cairo)
library(CORElearn)
library(magrittr)
library(forcats)
library(dplyr)
library(stringr)
library(e1071)
library(mlr)
library(caret)
library(naivebayes)
library(janitor)
library(neuralnet)
library(xgboost)
library(nnet)
library(scales)
library(knn)
library(DiagrammeR)
library(gt)
library(webshot)
#webshot::install_phantomjs()

setwd("C:/Users/cdawg/git_repos/ist407/project")

insurance <- read_csv("insurance.csv")

# Web Scraping ####

#took table data and pasted into csv
state_ranks <- clean_names(read_csv("stateinfo.csv"))


northeast <- tibble("states" = c('maine', 'massachusetts', 'rhode island', 'connecticut',
                                'new hampshire', 'vermont', 'new york', 'pennsylvania',
                                'new jersey', 'delaware', 'maryland', 'ohio', 'indiana',
                                'michigan', 'illinois', 'iowa', 'minnesota', 'wisconsin'),
                    "region" = rep("northeast", 18))

southwest <- tibble("states" = c('california', 'nevada', 'new mexico', 'arizona', 'colorado',
                                 'utah', 'oklahoma', 'texas', 'hawaii'),
                    "region" = rep("southwest", 9))

northwest <- tibble("states" = c('washington', 'oregon', 'montana', 'idaho', 'wyoming',
                                 'north dakota', 'south dakota', 'alaska', 'nebraska', 'kansas'),
                    "region" = rep("northwest", 10))

southeast <- tibble("states" = c('west virginia', 'virginia', 'kentucky', 'tennessee', 'missouri',
                                 'arkansas', 'louisiana', 'georgia', 'alabama', 'florida', "mississippi",
                                 'north carolina', 'south carolina'),
                    "region" = rep("southeast", 13))

northeast %>% 
  bind_rows(northwest,
            southeast,
            southwest) -> state_region

state_ranks %>% 
  mutate(state = tolower(state)) %>% 
  full_join(state_region, by = c("state" = "states")) %>% 
  group_by(region) %>% 
  summarise(mental_health_avg = round(mean(mental_health)),
            low_infant_mortality_rate_avg = round(mean(low_infant_mortality_rate)),
            low_mortality_rate_avg = round(mean(low_mortality_rate)),
            low_obesity_rate_avg = round(mean(low_obesity_rate)),
            low_smoking_rate_avg = round(mean(low_smoking_rate)),
            low_suicide_rate_avg = round(mean(low_suicide_rate))) -> regional_ranks

regional_ranks %>% 
  mutate(mental_health_rank = rank(mental_health_avg),
         low_infant_mortality_rate_rank = rank(low_infant_mortality_rate_avg),
         low_mortality_rate_rank = rank(low_mortality_rate_avg),
         low_obesity_rate_rank = rank(low_obesity_rate_avg),
         low_smoking_rate_rank = rank(low_smoking_rate_avg),
         low_suicide_rate_rank = rank(low_suicide_rate_avg)) %>% 
  select(1, 8:13) -> regional_sorted

insurance %>% 
  inner_join(regional_sorted, by = c("region" = "region")) -> full_insurance
  

# Joining and Discretization ####
full_insurance %>% 
  select(-region) %>%
  mutate(bmi = cut(full_insurance$bmi, breaks = c(0, 18.5, 24.9, 29.9, Inf), 
                   labels = c("Underweight", "Normal Weight", "Overweight", "Obese")),
         age = cut(full_insurance$age, breaks = c(10, 20, 30, 40, 50, 60, Inf), right = FALSE,
                   labels = c("Teen", "Twenties", "Thirties", "Fourties", "Fifties", "Sixties")),
         children = cut(full_insurance$children, breaks = c(-Inf, 0, 2, Inf), 
                        labels = c("Zero", "One or Two", "Three Plus")),
         charges = cut(full_insurance$charges, breaks = 3,
                       labels = c("Low", "Medium", "High")),
         mental_health_rank = factor(full_insurance$mental_health_rank, 
                                     levels = c(1, 2, 3, 4),
                                     labels = c("first", "second", "third", "fourth")),
         low_infant_mortality_rate_rank = factor(full_insurance$low_infant_mortality_rate_rank, 
                                     levels = c(1, 2, 3, 4),
                                     labels = c("first", "second", "third", "fourth")),
         low_mortality_rate_rank = factor(full_insurance$low_mortality_rate_rank, 
                                     levels = c(1, 2, 3, 4),
                                     labels = c("first", "second", "third", "fourth")),
         low_obesity_rate_rank = factor(full_insurance$low_obesity_rate_rank, 
                                     levels = c(1, 2, 3, 4),
                                     labels = c("first", "second", "third", "fourth")),
         low_smoking_rate_rank = factor(full_insurance$low_smoking_rate_rank, 
                                     levels = c(1, 2, 3, 4),
                                     labels = c("first", "second", "third", "fourth")),
         low_suicide_rate_rank = factor(full_insurance$low_suicide_rate_rank, 
                                     levels = c(1, 2, 3, 4),
                                     labels = c("first", "second", "third", "fourth")),
         sex = factor(sex),
         smoker = factor(smoker)) -> clean_insurance

summary(clean_insurance)

# Setting seed for sampling of testing ####
set.seed(44)

# Decision Tree ####

Method.CORElearn <- CORElearn::attrEval(clean_insurance$charges ~ ., data=clean_insurance,  estimator = "InfGain")
Method.CORElearn2 <- CORElearn::attrEval(clean_insurance$charges ~ ., data=clean_insurance,  estimator = "Gini")
Method.CORElearn3 <- CORElearn::attrEval(clean_insurance$charges ~ ., data=clean_insurance,  estimator = "GainRatio")

Method.CORElearn
Method.CORElearn2
Method.CORElearn3

fit <- rpart(clean_insurance$charges ~ ., data = clean_insurance, method="class")
summary(fit)
pred_fit <- predict(fit,clean_insurance, type="class")
accuracy_dt <- sum(pred_fit == clean_insurance$charges)/length(clean_insurance$charges)
cm_accuracy_dt <- confusionMatrix(data = pred_fit, reference = clean_insurance$charges)
cm_accuracy_dt

printcp(fit)

fit2 <- rpart(clean_insurance$charges ~ ., data = clean_insurance, method="class",
              control=rpart.control(minsplit=2, cp=0.01))
summary(fit2)
pred_fit2 <- predict(fit2,clean_insurance, type="class")
accuracy_dt_2 <- sum(pred_fit2 == clean_insurance$charges)/length(clean_insurance$charges)
cm_accuracy_dt_2 <- confusionMatrix(data = pred_fit2, reference = clean_insurance$charges)
cm_accuracy_dt_2

# SVM ####

# polynomial
svm_poly <- svm(clean_insurance$charges~., data=clean_insurance, 
                     kernel="polynomial", cost=100, 
                     scale=FALSE)
poly_predict_charges <- predict(svm_poly, clean_insurance, type="class")
table(poly_predict_charges,clean_insurance$charges)
accuracy_svm_poly <- (sum((poly_predict_charges==clean_insurance$charges))/nrow(clean_insurance))
cm_accuracy_svm_poly <- confusionMatrix(data = poly_predict_charges, reference = clean_insurance$charges)
cm_accuracy_svm_poly



# linear

svm_lin <- svm(clean_insurance$charges~., data=clean_insurance, 
                kernel="linear", cost=100, 
                scale=FALSE)
lin_predict_charges <- predict(svm_lin, clean_insurance, type="class")
table(lin_predict_charges,clean_insurance$charges)
accuracy_svm_lin <- (sum((lin_predict_charges==clean_insurance$charges))/nrow(clean_insurance))
cm_accuracy_svm_lin <- confusionMatrix(data = lin_predict_charges, reference = clean_insurance$charges)
cm_accuracy_svm_lin

# radial

svm_rad <- svm(clean_insurance$charges~., data=clean_insurance, 
                kernel="rad", cost=100, 
                scale=FALSE)
rad_predict_charges <- predict(svm_rad, clean_insurance, type="class")
table(rad_predict_charges,clean_insurance$charges)
accuracy_svm_rad <- (sum((rad_predict_charges==clean_insurance$charges))/nrow(clean_insurance))
cm_accuracy_svm_rad <- confusionMatrix(data = rad_predict_charges, reference = clean_insurance$charges)
cm_accuracy_svm_rad

# Naive Bayes ####

nb_classifier <- naiveBayes(clean_insurance$charges ~.,data=clean_insurance)
nb_predict_charges <-predict(nb_classifier, clean_insurance[-6], type = "class")
table(nb_predict_charges,clean_insurance$charges)
accuracy_nb <- (sum((nb_predict_charges==clean_insurance$charges))/nrow(clean_insurance))
cm_accuracy_nb <- confusionMatrix(data = nb_predict_charges, reference = clean_insurance$charges)
cm_accuracy_nb

ggplot(clean_insurance) +
  aes(charges, nb_predict_charges, color = charges) + 
  geom_jitter(width=0.2, height = 0.1, size = 2) +
  theme_minimal() +
  labs(color = "Charges") +
  xlab("Actual Charges") +
  ylab("Predicted Charges") + 
  scale_color_brewer(palette = "Set1", direction = -1)

ggsave("nb_classification.png", device = "png")


# ANN with neuralnet ####
#install.packages("neuralnet")
#need numeric values, going to re-clean the data for it and leave as numeric (except for classifier of course)

# full_insurance %>%
#   select(1:7) %>%
#   # select(-region) %>%
#   mutate(sex = case_when(
#     sex=="male" ~ 1,
#     sex=="female" ~ 0
#   )) %>%
#   mutate(region = case_when(
#     region=="northwest" ~ 1,
#     region=="northeast" ~ 2,
#     region=="southwest" ~ 3,
#     region=="southeast" ~ 4
#   )) %>%
#   mutate(age = scale(age),
#          bmi = scale(bmi),
#          children = scale(children)) %>%
#   mutate(smoker = case_when(
#     smoker=="yes" ~ 1,
#     smoker=="no" ~ 0
#   )) %>%
#   mutate(charges = cut(full_insurance$charges, breaks = 3,
#                        labels = c("Low", "Medium", "High"))) -> nn_data
# 
# nn_ca <- function() {
#   prediction <-data.frame(neuralnet::compute(nn,
#                                              data.frame(nn_data[,-7]))$net.result)
#   labels <- c("Low", "Medium", "High")
#   prediction_label <- data.frame(max.col(prediction)) %>%
#     mutate(prediction=labels[max.col.prediction.]) %>%
#     select(2) %>%
#     unlist()
# 
#   tibble(prediction_label) %>%
#     mutate(prediction_label = factor(prediction_label)) -> pred_table
# 
#   confusionMatrix(data = pred_table$prediction_label, reference = nn_data$charges)
# }
# 
# nn=neuralnet(charges ~ age+sex+bmi+children+smoker+region,
#              data=nn_data,
#              hidden= c(4),
#              linear.output = F,
#              stepmax = 1e7,
#              algorithm = "backprop",
#              learningrate = 0.03,
#              lifesign = "full",
#              threshold = 0.01)
# 
# plot(nn)
# 
# pred_nn <- predict(nn, nn_data)
# pred_nn <- as.data.frame(pred_nn)
# colnames(pred_nn) <- levels(nn_data$charges)
# 
# pred_nn$prediction = apply(pred_nn,1,function(x) colnames(pred_nn)[which.max(x)])
# 
# confusionMatrix(data = factor(pred_nn$prediction), reference = nn_data$charges)
# 
# accuracy_nn <- (sum((pred_nn$prediction==nn_data$charges))/nrow(pred_nn))

# ANN with nnet ####
full_insurance %>%
  select(1:7) %>%
  # select(-region) %>%
  mutate(sex = case_when(
    sex=="male" ~ 1,
    sex=="female" ~ 0
  )) %>%
  mutate(region = case_when(
    region=="northwest" ~ 1,
    region=="northeast" ~ 2,
    region=="southwest" ~ 3,
    region=="southeast" ~ 4
  )) %>%
  mutate(age = scale(age),
         bmi = scale(bmi),
         children = scale(children)) %>%
  mutate(smoker = case_when(
    smoker=="yes" ~ 1,
    smoker=="no" ~ 0
  )) %>%
  mutate(charges = cut(full_insurance$charges, breaks = 3,
                       labels = c("Low", "Medium", "High"))) -> nnet_data


sample_id <- sample(nrow(nnet_data),floor(0.75*nrow(nnet_data)))
nn <- nnet(charges ~ ., data = nnet_data, subset = sample_id,
           size = 4, decay = 1.0e-7, maxit = 10000)



table(nnet_data$charges[-sample_id], predict(nn, nnet_data[-sample_id, ],type="class"))
predictions <- factor(predict(nn, nnet_data[-sample_id, ], type = 'class'))
conf <- confusionMatrix(data = predictions, reference = nnet_data$charges[-sample_id])
accuracy_nnet <- conf[3]$overall[1]

# XGBoost ####

full_insurance %>%
  select(1:7) %>%
  # select(-region) %>%
  mutate(sex = case_when(
    sex=="male" ~ 1,
    sex=="female" ~ 0
  )) %>%
  mutate(region = case_when(
    region=="northwest" ~ 1,
    region=="northeast" ~ 2,
    region=="southwest" ~ 3,
    region=="southeast" ~ 4
  )) %>%
  mutate(age = scale(age),
         bmi = scale(bmi),
         children = scale(children)) %>%
  mutate(smoker = case_when(
    smoker=="yes" ~ 1,
    smoker=="no" ~ 0
  )) %>%
  mutate(charges = cut(full_insurance$charges, breaks = 3,
                       labels = c("Low", "Medium", "High"))) -> xgb_data

charges = xgb_data$charges
label = as.integer(xgb_data$charges)-1
xgb_data$charges = NULL

n = nrow(xgb_data)
train.index = sample(n,floor(0.75*n))
train.data = as.matrix(xgb_data[train.index,])
train.label = label[train.index]
test.data = as.matrix(xgb_data[-train.index,])
test.label = label[-train.index]

xgb.train = xgb.DMatrix(data=train.data,label=train.label)
xgb.test = xgb.DMatrix(data=test.data,label=test.label)

num_class = length(levels(charges))
params = list(
  booster="gbtree",
  eta=0.001,
  max_depth=5,
  gamma=3,
  subsample=0.75,
  colsample_bytree=1,
  objective="multi:softprob",
  eval_metric="mlogloss",
  num_class=num_class
)

xgb.fit=xgb.train(
  params=params,
  data=xgb.train,
  nrounds=10000,
  nthreads=3,
  early_stopping_rounds=100,
  watchlist=list(val1=xgb.train,val2=xgb.test),
  verbose=1,
  print_every_n=1000
)

#xgb.fit

xgb.pred = predict(xgb.fit,test.data,reshape=T)
xgb.pred = as.data.frame(xgb.pred)
colnames(xgb.pred) = levels(charges)

xgb.pred$prediction = apply(xgb.pred,1,function(x) colnames(xgb.pred)[which.max(x)])
xgb.pred$label = levels(charges)[test.label+1]

accuracy_xgb <- (sum((xgb.pred$prediction==xgb.pred$label))/nrow(xgb.pred))

# K-NN ####

#k_guess <- round(sqrt(nrow(clean_insurance)))
k_guess <- 3

knn_fit <- class::knn(train=xgb_data, test=xgb_data, 
                      cl=clean_insurance$charges, k = k_guess, prob=F)

accuracy_knn <- (sum((knn_fit==clean_insurance$charges))/nrow(clean_insurance))

# Accuracy Compiling ####

class_accuracy <- tibble("Model" = c('Decision Tree', 'Decision Tree (Pruned)', 'Naive Bayes',
                                         'SVM (Linear Kernel)', 'SVM (Polynomial Kernel)', 'SVM (Radial Kernel)',
                                         'eXtreme Gradient Boosting', "K-Nearest Neighbors", "Artificial Neural Net"),
                         "Accuracy" = c(accuracy_dt, accuracy_dt_2, accuracy_nb, accuracy_svm_lin,
                                              accuracy_svm_poly, accuracy_svm_rad, accuracy_xgb, 
                                              accuracy_knn, accuracy_nnet))

class_accuracy %>% 
  arrange(desc(Accuracy)) -> class_accuracy
  
write.csv(class_accuracy, file = "classification_accuracies.csv")

class_accuracy %>% 
  gt() %>% 
  tab_header(
    title = "Classification Accuracy by Model"
  ) %>% 
  fmt_percent(
    columns = Accuracy,
    
  ) %>% 
  tab_style(
    style = list(cell_fill(color = "#D0CFCF")),
    locations = cells_body(rows = c(2,4,6,8))
  ) %>% 
  gtsave("accuracy_table.png")

