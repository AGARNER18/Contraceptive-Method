# Amber Garner
# 6/13/2016
# random forest on contraceptive method data

# install needed package
install.packages("randomForest")
library(randomForest)

# load data 
cont <- read.csv("cmc.csv", header = T, sep = ",")

# change to factor type
cont$WifeEducation <- factor(cont$WifeEducation)
cont$HusbandEducation <- factor(cont$HusbandEducation)
cont$WifeReligion <- factor(cont$WifeReligion)
cont$WifeWorking <- factor(cont$WifeWorking)
cont$HusbandOccupation <- factor(cont$HusbandOccupation)
cont$LivingStandardIndex <- factor(cont$LivingStandardIndex)
cont$MediaExposure <- factor(cont$MediaExposure)
cont$ContraceptiveMethod <- factor(cont$ContraceptiveMethod)

# divide into training (60%) and test (40%) sets
set.seed(1234)
ind <- sample(2, nrow(cont), replace = TRUE, prob = c(0.6, 0.4))
train.data <- cont[ind == 1, ]
test.data <- cont[ind == 2, ]
table(train.data$ContraceptiveMethod)/nrow(train.data)
table(test.data$ContraceptiveMethod)/nrow(test.data)

str(cont)


model2 <- randomForest(ContraceptiveMethod~., train.data, ntree=500, importance=T)
plot(model2)

# build confusion matrix on training data
table(predict(model2), train.data$ContraceptiveMethod)

# Calcualte classification accuracy
train_accuracy <-(268+60+163)/(nrow(train.data))
train_accuracy 

# Calculate classification error rate
train_error <- (47+101+36+46+88+76)/(nrow(train.data))
train_error

# table of probabilities
prop.table(table(predict(model2), train.data$ContraceptiveMethod))

# confusion matrix on test data
testPred <- predict(model2, newdata = test.data)
table (testPred, test.data$ContraceptiveMethod)

# Calcualte classification accuracy
test_accuracy <-(160+39+107)/(nrow(test.data))
test_accuracy 

# Calculate classification error rate
test_error <- (52+72+16+22+61+59)/(nrow(test.data))
test_error

