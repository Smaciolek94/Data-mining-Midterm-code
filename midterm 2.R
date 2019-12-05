install.packages("glmnet")
install.packages("penalizedSVM")
install.packages("e1071")
install.packages("randomForest")
install.packages("dplyr")
library("glmnet")
library("penalizedSVM")
library("e1071")
library("randomForest")
library("dplyr")

#the data set location will have to be updated
data <- load("E:\\588\\Midterm\\geno.R")
x0 <- t(x0)
x0 <- as.data.frame(x0)

#seperating data into testing and training sets
set.seed(145003903) 
rando <- runif(72)
x0 <- cbind(x0,rando)
x0 <- x0[order(rando),]
y <- cbind(y, rando)
y <- y[order(rando),]
ytrain <- y[1:57,]
ytrain <- ytrain[,-2]
ytrain <- factor(ytrain)
ytest <- y[58:72,]
ytest <- ytest[,-2]
ytest <- factor(ytest)

train <- x0[1:57,]
train <- train[,-36255]
test <- x0[58:72,]
test <- test[,-36255]

#SVM:
svm <- svm(train, ytrain)
svmpredicted <- predict(svm, newdata = test)
table(svmpredicted)

#random forest:
rF <- randomForest(train, ytrain, test, ytest, ntree = 500,)
table(rF$test$predicted)

#naive Bayes
nB <- naiveBayes(train, ytrain)
nbpredicted <- predict(nB,newdata = test)
table(nbpredicted)

#GLM net:
glm <- glmnet(as.matrix(train), ytrain, family = "multinomial")
glmpredicted <- predict(glm,newx = as.matrix(test),s=min(glm$lambda),type = "class")
table(glmpredicted)

#penalized SVM:
ytest <- recode(ytest, "0" = "-1", "2" = "1")
ytrain <- recode(ytrain, "0" = "-1", "2" = "1")
psvm <- svmfs(as.matrix(train), y=ytrain, fs.method = "scad+L2",maxevals = 5)
psvmpredicted <- predict.penSVM(psvm, newdata = test)
psvmround <- rep(0,15)
for (i in 1:15){
  if (psvmpredicted$fitted[i] < 0) {psvmround[i] = -1}
  else if (psvmpredicted$fitted[i] > 0) {psvmround[i] = 1}
}
table(psvmround)

