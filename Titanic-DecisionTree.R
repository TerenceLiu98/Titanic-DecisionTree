#Predicting Titanic survival rate using decision tree
library(mice)
library(caTools)
library(rpart)

#Load the training data
setwd('C:\\Venkat\\Github\\Titanic-DecisionTree')
titanic = read.csv('train.csv')
names(titanic)

#Select the variables of interest
titanic = titanic[,c(2,3,5,6,7,8,10)]
names(titanic)

#Determing the modeling type of selected features
str(titanic)

#Change the modeling type of Survived and Pclass
titanic$Survived = factor(titanic$Survived)
titanic$Pclass = factor(titanic$Pclass, 
                        levels = c(3,2,1),
                        labels = c(3,2,1),
                        ordered = TRUE)
str(titanic)

#Look for missing values and impute missing values
md.pattern(titanic)

#Impute missing Age feature using mice package
titanic_impute = mice(data = titanic,
                      m = 1, 
                      maxit = 3)
titanic_imputed = complete(titanic_impute,1)

#Scale the continuous features
titanic_imputed[,c(4,5,6,7)] = scale(titanic_imputed[,c(4,5,6,7)])

#Split the data into training and test set
set.seed(5)
split = sample.split(Y = titanic_imputed$Survived,
                     SplitRatio = 0.8)
training_set = subset(titanic_imputed,
                      split == TRUE)
test_set = subset(titanic_imputed,
                  split == FALSE)

#Build the decision tree classifier using the training set
titanic_decisiontree = rpart(formula = Survived ~ .,
                             data = titanic_imputed)

#Predict the survival in test set using the decision tree classifier
y_pred = predict(titanic_decisiontree,
                 newdata = test_set[,-1],
                 type = 'class')

#Visualizing the decision tree
plot(titanic_decisiontree)
text(titanic_decisiontree)

#Build the confusion matrix
cm = table(test_set[,1], y_pred)
cm

#Calculate the misclassification rate
misclassification_rate = (cm[2,1] + cm[1,2])/ (cm[1,1] + cm[1,2] + cm[2,1] + cm[2,2])
misclassification_rate

