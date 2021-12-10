#----------- about project ---------------------
  
# using data from a bank on customer details, we will build a model**
#**using logistic regression that can be used to flag down suspected defaulters
  
#------------ data cleaning, preparation $ exploration ----------------

head(data)

library(tidyverse)

data <- select(data, c("age","job", "marital", "education","default",
                       "balance","housing"))

head(data)

#convert all categorical variables to factors

data <- data %>% 
  mutate(
    job=as.factor(job),
    marital=as.factor(marital),
    education=as.factor(education),
    default=as.factor(default),
    housing=as.factor(housing)
  )

head(data)

#----------------------- model ------------------------------

#training and testing sets

set.seed(123)

split <- sample(2, nrow(data), replace = T, prob = c(0.7,0.3))

train <- data[split==1,]

test <- data[split==2,]

# training model using training set
model <- glm(default ~., data = train, family = "binomial")

summary(model)

#------------------ model evaluation----------------------------

# predict using test data

pred <- predict(model,test, type = "response")

pred[1:10]

#allocation of dummy variables

contrasts(default)

# Changing probabilities
predicted <- ifelse(pred >0.3, 1, 0)

# Evaluating model accuracy
# using confusion matrix
confusionmatrix <- table(test$default, predicted)

sensitivity <- 13/(12+13)

specificity <- 13164/(13164+247)

#ROC curve

ROCPred <- prediction(predicted, test$default)

ROCPer <- performance(ROCPred, measure = "tpr",
                      x.measure = "fpr")
plot(ROCPer)

# the model has high specificity and low sensitivity
# we would like to increase the sensitivity.
# this will increase the accuracy of identifying cases which may default


#-----------------------imbalanced class problem---------------------------

class <- table(data$default)

class

# we have very few cases of default
# our model which be dominated by the "no" class

# we will perform oversampling for the rare class

library(ROSE)

over <- ovun.sample(default ~., data = train, method = "over", N=80000)$data

table(over$default)

# new model

model <- glm(default ~., data = over, family = "binomial")

summary(model)


#predicting test data

pred <- predict(model,test, type = "response")

predicted <- ifelse(pred >0.5, 1, 0)

confusionmatrix <- table(test$default, predicted)

confusionmatrix
