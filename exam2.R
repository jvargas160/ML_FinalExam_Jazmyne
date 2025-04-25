#4b
library(randomForest)
set.seed(2);  train = sample(1:nrow(CreditScores), 25e3) 
CreditScores.train = CreditScores[train,]
rf.CreditScores = randomForest(Credit_Score~., data = CreditScores.train)
rf.CreditScores
CreditScores.test = CreditScores[-train,]
pred = predict(rf.CreditScores, CreditScores.test[,1:20])
actual = CreditScores.test[,21]
test.error = sum(pred != actual)/50780
test.error

#4c
library(class)
Credit_Scorev2 = CreditScores[,c(3,4,5,6,7,8,9,10,11,13,14,15,17,18,20)]
Credit_Scorev2[, 1:15] = scale(Credit_Scorev2[,1:15])
Credit_Scorev2$Credit_Score = CreditScores$Credit_Score
train.CSx = Credit_Scorev2[train, -16]
test.CSx = Credit_Scorev2[-train, -16]
train.CSy = Credit_Scorev2[train, 16]
set.seed(2)
knn5 = knn(train.CSx, test.CSx, train.CSy, k = 5)
test.CSy = Credit_Scorev2[-train, 16]
table(knn5, CreditScores.test$Credit_Score)
sum(knn5 != test.CSy)/50780
knn201 = Credit_Scorev2 = knn(train.CSx, test.CSx, train.CSy, k = 201)
table(knn201, CreditScores.test$Credit_Score)
sum(knn201 != test.CSy)/50780

#4di
library(rpart)
set.seed(2)
tree.CreditScores.big = rpart(Credit_Score~., data= CreditScores, cp = 0.001)
pred= predict(tree.CreditScores.big, CreditScores.test[,1:20], type = "class")
actual = CreditScores.test[,21]
sum(pred != actual)/ 50780

#4dii
plotcp(tree.CreditScores.big)
#Seems like the best cp value if w wanted to prune this tree would be 0.0013.

#4e
lrm = lm(Monthly_Balance ~ Monthly_Inhand_Salary+Num_Credit_Card+Interest_Rate, data=CreditScores.train)

#4ei
lrm = lm(CreditScores.test$Monthly_Balance ~CreditScores.test$Monthly_Inhand_Salary+CreditScores.test$Num_Credit_Card+CreditScores.test$Interest_Rate)
x.test = CreditScores.test[,20]
x.test =as.data.frame(x.test)
colnames(x.test) = "Monthly_Balance"
summary(x.test)
yhat.testErr = predict(lrm, x.test)
sqrt(mean((yhat.testErr - CreditScores.test$Monthly_Balance)^2))

#4eii
cor(CreditScores.test$Monthly_Balance, CreditScores.test[, c(4, 6, 7)])

#4fi
CreditScoresv3 = subset(CreditScores, CreditScores$Payment_Behaviour !="!@9#%8")
CreditScoresv3 = droplevels(CreditScoresv3)

#4fii
table(CreditScoresv3$Payment_Behaviour)

large_good = nrow(CreditScoresv3[CreditScoresv3$Payment_Behaviour == "High_spent_Large_value_payments" & CreditScoresv3$Credit_Score == "Good", ]) 
large_good

medium_good = nrow(CreditScoresv3[CreditScoresv3$Payment_Behaviour == "High_spent_Medium_value_payments" & CreditScoresv3$Credit_Score == "Good", ])
medium_good

small_good = nrow(CreditScoresv3[CreditScoresv3$Payment_Behaviour == "High_spent_Small_value_payments" & CreditScoresv3$Credit_Score == "Good", ])
small_good


#4g
CreditScores_USL = CreditScores[,c(3,4,5,6,7,8,9,10,11,13,14,15,17,18,20)]
set.seed(2)
hc = hclust(dist(CreditScores_USL[1:40000,]), method = "complete")
hc1 = hclust(dist(CreditScores_USL[40001:75780,]), method = "complete")
#plot(hc, hang = -1, main = "Dendrogram for CreditScores_USL")
#clusters=cutree(hc, 3)
#plot(CreditScores, col=clusters, pch =1, lwd=2,cex=2)

clusters <- cutree(hc, k = 3)
clusters1 <- cutree(hc1, k = 3)
good_credit <- CreditScores$Credit_Score == "Good"



good1= (sum(good_credit[which(clusters == 1)]))/(length(which(clusters == 1)))
good1
good2= (sum(good_credit[which(clusters == 2)]))/(length(which(clusters == 2)))
good2
good3= (sum(good_credit[which(clusters == 3)]))/(length(which(clusters == 3)))
good3

good1= (sum(good_credit[which(clusters1 == 1)]))/(length(which(clusters1 == 1)))
good1
good2= (sum(good_credit[which(clusters1 == 2)]))/(length(which(clusters1 == 2)))
good2
good3= (sum(good_credit[which(clusters1 == 3)]))/(length(which(clusters1 == 3)))
good3


#Extra credit
CreditScores_EC = CreditScores[,c()]

