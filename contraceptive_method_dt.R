# Amber Garner
# Nov. 5, 2016
# Decision Tree on contraceptive method

#*********Setup*************************************** 

install.packages("party")
library(party)

# Load credit approval dataset
cont <- read.csv("cmc.csv", header = T, sep = ",")

#**********DATA PREPROCESSING****

# structure and summary of data
summary(cont)
str(cont)

# change to factor type
cont$WifeEducation <- factor(cont$WifeEducation)
cont$HusbandEducation <- factor(cont$HusbandEducation)
cont$WifeReligion <- factor(cont$WifeReligion)
cont$WifeWorking <- factor(cont$WifeWorking)
cont$HusbandOccupation <- factor(cont$HusbandOccupation)
cont$LivingStandardIndex <- factor(cont$LivingStandardIndex)
cont$MediaExposure <- factor(cont$MediaExposure)
cont$ContraceptiveMethod <- factor(cont$ContraceptiveMethod)

# Verify changes
summary(cont)

# Combine  
cont <- bind_rows(one, two, three)

# divide into training (90%) and test (10%) sets
set.seed(1234)
ind <- sample(2, nrow(cont), replace = TRUE, prob = c(0.7, 0.3))
train.data <- cont[ind == 1, ]
test.data <- cont[ind == 2, ]

# build decision tree with ctree
myFormula <- ContraceptiveMethod~.
model <- ctree(myFormula, data=train.data)

# print credit decision tree model
print(model)

# view nodes starting at the 2nd node
nodes(model, 2)

# plot decision tree model
plot(model)

# plot simple decision tree model
plot(model, type="simple")


#********ACCURACY****************

# build confusion matrix on training data
table(predict(model), train.data$ContraceptiveMethod)

# Calcualte classification accuracy
train_accuracy <-(304+53+248)/(nrow(train.data))
train_accuracy 

# Calculate classification error rate
train_error <- (49+90+20+22+128+130)/(nrow(train.data))
train_error

# table of probabilities
prop.table(table(predict(model), train.data$ContraceptiveMethod))

# confusion matrix on test data
testPred <- predict(model, newdata = test.data)
table (testPred, test.data$ContraceptiveMethod)

# Calcualte classification accuracy
test_accuracy <-(111+17+100)/(nrow(test.data))
test_accuracy 

# Calculate classification error rate
test_error <- (22+44+7+7+59+62)/(nrow(test.data))
test_error

#*******THE END**************