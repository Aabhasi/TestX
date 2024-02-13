# Load train and test dataset
dat_train <- read.csv("HousingBoston_train.csv", stringsAsFactors = TRUE)
dat_test <- read.csv("HousingBoston_test.csv", stringsAsFactors = TRUE)
# change factors to binary with values of 0 and 1
dat_train$mvalue <- 1*(dat_train$mvalue == "below")
dat_test$mvalue <- 1*(dat_test$mvalue == "below")
# Plot Average number of rooms per dwelling
# To ensure correct color coding we revert to factors
plot(dat_train$rm, col = as.factor(dat_train$mvalue),
     ylab = "Average number of rooms per dwelling", xlab = "Observations")
legend('bottomright', legend = unique(as.factor(dat_train$mvalue)),
       pch = 1, col = unique(as.factor(dat_train$mvalue)))

## Question 1: Using number of rooms (rm) as a predictor for the home price (mvalue)
q1 <- 2

## Question 2: Running logistic regression model on "mvalue" against "rm" in training dataset
log.model_train <- glm(formula = mvalue ~ rm, family = binomial, data = dat_train)
summary(log.model_train)
# Assessing the coefficient
hat.beta_train <- coef(log.model_train)
q2 <- as.numeric(hat.beta_train[2])

## Question 3: Compute the estimated probability of a house with 5.5 rooms to be below median price
q3 <- as.numeric(predict(log.model_train, newdata = data.frame(rm = 5.5), type = "response")[1])
# Compute the estimated probability of below median for all the data in test set
probs_test <- predict(log.model_train, newdata = dat_test, type="response")
# Predict class 1 (i.e. mvalue=below) if estimated probability > 0.2
class.pred_test <- 1*(probs_test > 0.2)
# Create truth table: Rows represents actual class, Column represents predicted
truth.table_test <- table(dat_test$mvalue, class.pred_test)
# Total number of observations in truth.table
N <- sum(truth.table_test)

## Question 4: Number of correct predictions at a threshold of 0.2 on the test set
q4 <- (truth.table_test[1,1]+ truth.table_test[2,2])

## Question 5: Misclassification error at a threshold of 0.2 on the test set
miscl_error_rm <- (truth.table_test[1,2]+ truth.table_test[2,1])/N
q5 <- miscl_error_rm

## Question 6: Threshold should we use to predict the most BELOW median house prices
# Calculate predicted classes for different thresholds
threshold_0_5 <- 1*(probs_test > 0.5)
threshold_0_8 <- 1*(probs_test > 0.8)
# Calculate misclassification errors for different thresholds
misclass_error_0_5 <- mean(threshold_0_5 != dat_test$mvalue)
misclass_error_0_8 <- mean(threshold_0_8 != dat_test$mvalue)
# Compare the misclassification errors
misclassification_errors <- data.frame(
  Threshold = c(0.2, 0.5, 0.8),
  Error = c(miscl_error_rm, misclass_error_0_5, misclass_error_0_8)
)
# Determine which threshold has the smallest misclassification error
misclassification_errors$Threshold[which.min(misclassification_errors$Error)]
q6 <- 3

## Question 7: Logistic regressing age and rooms on the target mvalue on the training data set
cor(dat_train$age, dat_train$rm)
log.model_train2 <- glm(formula = mvalue ~ rm + age, family = binomial, data = dat_train)
summary(log.model_train2)

# Create a new data frame with the average values
average_data <- data.frame(rm = mean(dat_train$rm), age = mean(dat_train$age))
predict(log.model_train2, newdata = average_data, type = "link")
q7 <- c(2,5)

# Compute the estimated probability of below median for all the data in test set
probs_test2 <- predict(log.model_train2, newdata = dat_test, type="response")
# Predict class 1 (i.e. mvalue=below) if estimated probability > 0.2
class.pred_test2 <- 1*(probs_test2 > 0.2)
# Create truth table: Rows represents actual class, Column represents predicted
truth.table_test2 <- table(dat_test$mvalue, class.pred_test2)
# Total number of observations in truth.table
N_2 <- sum(truth.table_test2)

## Question 8: Number of correct predictions at a threshold of 0.2 on the test set
q8 <- (truth.table_test2[1,1]+ truth.table_test2[2,2])

## Question 9: Plotting the decision boundary of the new model
# Scatterplot of rm against age using default to colour data
plot(dat_train$rm, dat_train$age, col = as.factor(dat_train$mvalue))
# Get estimated coefficients
hb <- coef(log.model_train2)
# Plotting threshold = 0.5
abline(a=-hb[1]/hb[3], b=-hb[2]/hb[3], col="blue")
# Plotting threshold = 0.2
abline(a = (-hb[1] - log(0.2/0.8)) / hb[3], b = (-hb[2] / hb[3]), col = "green")
# For threshold = 0.8
abline(a = (-hb[1] - log(0.8/0.2)) / hb[3], b = (-hb[2] / hb[3]), col = "red")
q9 <- c(1,4)

## Question 10:
# Logistic regression all available predictors against mvalue on training data set
log.model_train3 <- glm(formula = mvalue ~ ., family = binomial, data = dat_train)
summary(log.model_train3)
# Compute the estimated probability of below median for all the data in test set
probs_test3 <- predict(log.model_train3, newdata = dat_test, type="response")
# Predict class 1 (i.e. mvalue=below) if estimated probability > 0.2
class.pred_test3 <- 1*(probs_test3 > 0.2)
# Create truth table: Rows represents actual class, Column represents predicted
truth.table_test3 <- table(dat_test$mvalue, class.pred_test3)
# Total number of observations in truth.table
N_3 <- sum(truth.table_test3)
# Misclassification error at a threshold of 0.2 on the test set on full model
miscl_error_full <- (truth.table_test3[1,2]+ truth.table_test3[2,1])/N_3
q10 <- miscl_error_full - miscl_error_rm

