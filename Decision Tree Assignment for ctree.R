
library(readr)
library(readxl)
data("iris")
View(iris)
head(iris)
install.packages("party")
library(party)
str(iris)
summary(iris)
?ctree
#Predicting iris data by sepal length(iris)
tree <- ctree(Species~Sepal.Length, data = iris)
plot(tree)

#Predicting iris data by sepal width
tree1 <- ctree(Species~Sepal.Width, data = iris)
plot(tree1)

#Predicting iris data by petal length
tree2 <- ctree(Species~Petal.Length, data = iris)
plot(tree2)

#Predicting iris data by sepal length
tree3 <- ctree(Species~Petal.Width, data = iris)
plot(tree3)

#Decision Tree with 2 variables
#Predicting iris data by sepal dimensions
tree4 <- ctree(Species~Sepal.Length + Sepal.Width, data = iris)
plot(tree4)

#Predicting iris data by Petal dimensions
tree5 <- ctree(Species~Petal.Length + Petal.Width, data = iris)
plot(tree5)

#Predicting decision tree with Sepal and Petal dimensions
#Predicting iris data by sepal dimensions
tree6 <- ctree(Species~Sepal.Length + Sepal.Width +Petal.Length + Petal.Width, data = iris)
plot(tree6)

#Splitting the data into Train and test dataset
set.seed(1234) #to get reproducible results
ind <- sample(2,nrow(iris), replace = TRUE, prob=c(0.7,0.3))
trainData <- iris[ind==1,]
testData <- iris[ind==2,]

#Building the model
myFormula <- Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width
iris_ctree <- ctree(myFormula, data=trainData)

#predict train data
train_predict <- predict(iris_ctree,trainData,type="response")

#confusion matrix
table(train_predict,trainData$Species)

mean(train_predict != trainData$Species) * 100 #3.571

#Validating model on the test data
test_predict <- predict(iris_ctree, newdata= testData,type="response")

#confusion matrix
table(test_predict, testData$Species)
mean(test_predict != testData$Species) * 100 #5.263
print(iris_ctree)

#Decision tree plotting 
plot(iris_ctree)
plot(iris_ctree, type="simple")



