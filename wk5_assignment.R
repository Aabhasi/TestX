# This is test example
# Aabhasi Chachire

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

#Question 1: What does the plot tell you about the potential use of the number of rooms (rm) as a predictor for the home price (mvalue)?
q1 <- 2

#Question 2: What coefficient do you get for βˆ1?
# Fit linear regression model
log.model_train <- glm( mvalue ~ rm, family = binomial, data = dat_train)
summary(log.model_train)
# Assessing the coefficient
hat.beta_train <- coef(log.model_train)
q2 <- as.numeric(hat.beta_train[2])

#Question 3: What is the probability for a house with 5.5 rooms to be below the median house price?
q3 <- as.numeric(predict(log.model_train, newdata = data.frame(rm = 5.5), type = "response")[1])
  
#Question 4: How many predicted house prices are correctly classified as BELOW median using a threshold of 0.2 on the test set using rooms?
probs <- predict(log.model_train, newdata = dat_test, type="response")
class.pred_test <- 1*(probs > 0.2)
truth.table_test <- table(dat_test$mvalue, class.pred_test)
# Total number of observations in truth.table
N <- sum(truth.table_test)
q4 <- (truth.table_test[1,1]+ truth.table_test[2,2])

#Question 5: What is the corresponding misclassification error at a threshold of 0.2 on the test set?
# Total number of observations in truth.table
N <- sum(truth.table_test)
# Misclassification error 
misclassification_error_1 <- (truth.table_test[1,2]+ truth.table_test[2,1])/N
q5<- misclassification_error_1

#Question 6: What threshold should we use to predict the most BELOW median house prices (class = 1) correctly from your rm model?
# First plot the observations (as points)
plot(dat_train$rm, dat_train$mvalue)
x <- seq(from = min(dat_train$rm), to=max(dat_train$rm))
hat.beta <- coef(log.model_train)
hat.beta
#Estimate probabilities from Log. Regr. formula
lines(x, (1 + exp(-hat.beta[1] - hat.beta[2]*x))^(-1), col="blue")
abline(h=0.5, col="green")
abline(v=-hat.beta[1]/hat.beta[2], col="red")

q6<- 3

#Question 7: What can you interpret the model summary (coefficients) and their significance?
# Estimate logistic regression with 2 predictors
cor(dat_train$age, dat_train$rm)
log.model2 <- glm(mvalue ~ rm + age, data=dat_train, family = binomial)
summary(log.model2)
q7<- c(2,5)

#Question 8: How many predicted observations of house prices are correctly classified as BELOW median using a threshold of 0.2 on the test set using age and rooms?
average_data <- data.frame(rm = mean(dat_train$rm), age = mean(dat_train$age))
predict(log.model2, newdata = average_data, type = "link")
probs2 <- predict(log.model2, newdata = dat_test, type="response")
# Predict class 1 (i.e. mvalue=below) if estimated probability > 0.2
class.pred2 <- 1*(probs2 > 0.2)
truth.table2 <- table(dat_test$mvalue, class.pred2)
N_2 <- sum(truth.table2)
q8 <- (truth.table2[1,1]+ truth.table2[2,2])

#Question 9: What does changing the decision boundary impact?
# Scatterplot of income against balance using default to colour data
plot(dat_train$rm, dat_train$age, col = as.factor(dat_train$mvalue))
# Get estimated coefficients
hb <- coef(log.model2)
# Decision boundary for threshold = 0.2 
abline(a=(-hb[1]-log(8))/hb[3], b=-hb[2]/hb[3], col="blue")
# Decision boundary for threshold = 0.5 
abline(a=-hb[1]/hb[3], b=-hb[2]/hb[3], col="green")
# Decision boundary for threshold = 0.8 
abline(a=(-hb[1]-log(2))/hb[3], b=-hb[2]/hb[3], col="red")
q9 <- c(1,4)

#Question 10: What is the misclassification error improvement on the test set from your full model (all variables) over the one containing only the room variable at a threshold of 0.2 (i.e., MisclErrorfull − MisclErrorrm)?
log.model3 <- glm(formula = mvalue ~ ., family = binomial, data = dat_train)
summary(log.model3)
probs3 <- predict(log.model3, newdata = dat_test, type="response")
class.pred3 <- 1*(probs3 > 0.2)
truth.table3 <- table(dat_test$mvalue, class.pred3)
truth.table3

# Total number of observations in truth.table
N3 <- sum(truth.table3)
# Misclassification error 
misclassification_error_2 <- (truth.table3[1,2]+ truth.table3[2,1])/N3
q10<- misclassification_error_2 - misclassification_error_1