# Machine Learning Exam 2 Solutions

# Problem 4b - Random Forest Model to Predict Credit Score
library(randomForest)

# Set seed for reproducibility and create training set
set.seed(2)
train = sample(1:nrow(CreditScores), 25e3)
CreditScores.train = CreditScores[train,]

# Build random forest model
rf.CreditScores = randomForest(Credit_Score ~ ., data = CreditScores.train)
rf.CreditScores

# Predict on test set and calculate test error
CreditScores.test = CreditScores[-train,]
pred = predict(rf.CreditScores, CreditScores.test[,1:20])
actual = CreditScores.test[,21]
test.error = sum(pred != actual) / 50780
test.error


# Problem 4c - k-Nearest Neighbors (k=5 and k=201)
library(class)

# Prepare data: select and scale numerical predictors
Credit_Scorev2 = CreditScores[,c(3,4,5,6,7,8,9,10,11,13,14,15,17,18,20)]
Credit_Scorev2[, 1:15] = scale(Credit_Scorev2[,1:15])
Credit_Scorev2$Credit_Score = CreditScores$Credit_Score

# Split training and testing data
train.CSx = Credit_Scorev2[train, -16]
test.CSx = Credit_Scorev2[-train, -16]
train.CSy = Credit_Scorev2[train, 16]

test.CSy = Credit_Scorev2[-train, 16]

# 5-Nearest Neighbors
set.seed(2)
knn5 = knn(train.CSx, test.CSx, train.CSy, k = 5)
table(knn5, CreditScores.test$Credit_Score)
sum(knn5 != test.CSy) / 50780

# 201-Nearest Neighbors
knn201 = knn(train.CSx, test.CSx, train.CSy, k = 201)
table(knn201, CreditScores.test$Credit_Score)
sum(knn201 != test.CSy) / 50780


# Problem 4di - Big Decision Tree
library(rpart)

# Create a big tree using rpart
set.seed(2)
tree.CreditScores.big = rpart(Credit_Score ~ ., data = CreditScores, cp = 0.001)

# Predict and calculate test error for big tree
pred = predict(tree.CreditScores.big, CreditScores.test[,1:20], type = "class")
actual = CreditScores.test[,21]
sum(pred != actual) / 50780


# Problem 4dii - Find Best cp Value for Pruning
# Plot complexity parameter (cp) table to decide pruning
plotcp(tree.CreditScores.big)
# Best cp value for pruning seems around 0.0013


# Problem 4e - Linear Regression Model to Predict Monthly_Balance

# Build model using training data
lrm = lm(Monthly_Balance ~ Monthly_Inhand_Salary + Num_Credit_Card + Interest_Rate, data = CreditScores.train)

# Predict Monthly_Balance using test data and compute RMSE
lrm = lm(CreditScores.test$Monthly_Balance ~ CreditScores.test$Monthly_Inhand_Salary + CreditScores.test$Num_Credit_Card + CreditScores.test$Interest_Rate)
x.test = CreditScores.test[,20]
x.test = as.data.frame(x.test)
colnames(x.test) = "Monthly_Balance"
summary(x.test)

yhat.testErr = predict(lrm, x.test)
sqrt(mean((yhat.testErr - CreditScores.test$Monthly_Balance)^2))

# Find most important predictor based on correlation
cor(CreditScores.test$Monthly_Balance, CreditScores.test[, c(4, 6, 7)])


# Problem 4fi - Clean Strange Payment Behaviour Values

# Remove rows with strange value in Payment_Behaviour
CreditScoresv3 = subset(CreditScores, CreditScores$Payment_Behaviour != "!@9#%8")
CreditScoresv3 = droplevels(CreditScoresv3)


# Problem 4fii - Find Percent of Good Credit Ratings by Payment Behaviour Category

table(CreditScoresv3$Payment_Behaviour)

# High_spent_Large_value_payments
large_good = nrow(CreditScoresv3[CreditScoresv3$Payment_Behaviour == "High_spent_Large_value_payments" & CreditScoresv3$Credit_Score == "Good", ]) 
large_good

# High_spent_Medium_value_payments
medium_good = nrow(CreditScoresv3[CreditScoresv3$Payment_Behaviour == "High_spent_Medium_value_payments" & CreditScoresv3$Credit_Score == "Good", ])
medium_good

# High_spent_Small_value_payments
small_good = nrow(CreditScoresv3[CreditScoresv3$Payment_Behaviour == "High_spent_Small_value_payments" & CreditScoresv3$Credit_Score == "Good", ])
small_good


# Problem 4g - Unsupervised Learning (Hierarchical Clustering)

# Remove categorical predictors and target variable to prepare data
CreditScores_USL = CreditScores[,c(3,4,5,6,7,8,9,10,11,13,14,15,17,18,20)]

# Hierarchical clustering on two halves of the data
set.seed(2)
hc = hclust(dist(CreditScores_USL[1:40000,]), method = "complete")
hc1 = hclust(dist(CreditScores_USL[40001:75780,]), method = "complete")

# Cut into 3 clusters and analyze Good credit rating percentages
clusters = cutree(hc, k = 3)
clusters1 = cutree(hc1, k = 3)

good_credit = CreditScores$Credit_Score == "Good"

# Percent Good in each cluster (first half)
good1 = sum(good_credit[which(clusters == 1)]) / length(which(clusters == 1))
good1
good2 = sum(good_credit[which(clusters == 2)]) / length(which(clusters == 2))
good2
good3 = sum(good_credit[which(clusters == 3)]) / length(which(clusters == 3))
good3

# Percent Good in each cluster (second half)
good1 = sum(good_credit[which(clusters1 == 1)]) / length(which(clusters1 == 1))
good1
good2 = sum(good_credit[which(clusters1 == 2)]) / length(which(clusters1 == 2))
good2
good3 = sum(good_credit[which(clusters1 == 3)]) / length(which(clusters1 == 3))
good3


# Extra Credit Section (left empty for experiments)
CreditScores_EC = CreditScores[,c()]
