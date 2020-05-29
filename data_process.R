# process the smart campus project data.

library(VIM) # plot the missing value
library(missForest) # miss forest to process missing value
library("lattice") # Better Graphical function
library("ggplot2") # Data visualization
library("plotly") # Interactive data visualizations
library("psych") # For correlation visualizations
library("rattle") # Graphing decision trees
library("caret") # Machine learning
library("party") # Decision Tree
library(pROC) # draw ROC curve
library(randomForest)
library("class")
library("e1071")


# clear all the global environment to continue process
rm(list = ls())

# set the documentation to this folder and load .Rda file
### ******  must be in Rstudio  ****** ###
getwd()
script <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(script)
load(file = "preprocessed_data/DT_UQ.Rda")

head(DT.UQ)
summary(DT.UQ)




### Have to process the missing value, firstly find them out, give prediction later

complete.cases(DT.UQ)
# find how many sets are intact and list them
nrow(DT.UQ[complete.cases(DT.UQ),])
# find the columns with missing values, True if missing, False if intact
is.na(DT.UQ)

# use library VIM to show the missing value
aggr(DT.UQ,prop=FALSE,number=TRUE)
aggr(DT.UQ,prop=TRUE,number=TRUE)

# show columns missing value
ms.columns<-which(is.na(c(DT.UQ$clientType,DT.UQ$building)))
ms.columns


sub.building<-which(is.na(DT.UQ$building))
sub.building
DT.UQ[-sub.building,3] # show the columns missing value in building

sub.client<-which(is.na(DT.UQ$clientType))
sub.client    # show the columns missing value in client
# get all columns missing value
ms.columns <- c(ms.columns, sub.building, sub.client)
ms.columns <- ms.columns[!duplicated(ms.columns)]

DT.UQ[-ms.columns,2:4]   # show all columns no missing value
summary(DT.UQ$building)

###  here, as.numeric can assign building as the serial number of building set  ###
###  It is important for applying kNN

as.numeric(DT.UQ$building[1])
as.numeric(DT.UQ[2,3])



# use classification to fill up missing value
DT.UQ.ms <- DT.UQ[ms.columns,]
DT.UQ.cp <- DT.UQ[-ms.columns,]
DT.UQ.cp[1:200,]

save(DT.UQ.ms,file = "./preprocessed_data/DT_UQ_ms.Rda")
save(DT.UQ.cp,file = "./preprocessed_data/DT_UQ_cp.Rda")


kNN.UQ<-DT.UQ
kNN.UQ[,2] <- as.numeric(kNN.UQ[,2])
kNN.UQ[,3] <- as.numeric(kNN.UQ[,3])
kNN.UQ[,4] <- as.numeric(kNN.UQ[,4])
kNN.UQ.ms <- kNN.UQ[ms.columns,]
kNN.UQ.cp <- kNN.UQ[-ms.columns,]
kNN.UQ.cp[1:200,2:4]



# see the breif correlation of UQ data
pairs(DT.UQ.cp[1:20,], pch = 1, lower.panel = NULL)
pairs.panels(DT.UQ.cp[1:50,], scale = TRUE, bg = c("red","green","blue")[DT.UQ$clientType], pch = 21, main = "Correlation Matrix of UQ Data")
ggplot(data = DT.UQ, mapping = aes(x = DT.UQ$clientType, y = DT.UQ$service, fill = DT.UQ$clientType)) + geom_boxplot()
plot_ly(data = DT.UQ.cp[1:20,], x = ~DT.UQ$lon, y = ~DT.UQ$lat, z = ~DT.UQ$building, color = ~DT.UQ$clientType, type = "scatter3d")


#################################################################################
#################################################################################
# 1.     use kNN to handle the building for its robust to noise data (k)        #
#        can use lon and lat to verify the result                               #
# 2. use ctree/Naive bayes (attributes independence) to label the client type - #
#    and service.                                                               #
#################################################################################
#################################################################################


####################################  kNN  ######################################

# Divide data into training and test sets.
set.seed(45)
indice <- sample(2, nrow(kNN.UQ.cp),replace = TRUE, prob = c(0.7, 0.3))
train_set <- kNN.UQ.cp[indice == 1,]
test_set <- kNN.UQ.cp[indice == 2,]
dim(train_set)
dim(test_set)
dim(train_set$clientType)

# If prob is true, the proportion of the votes for the winning class are returned as attribute prob.
?knn

############################ use column 2:4 to predict the client type, good try
### k=1
prediction_knn  <- knn(train_set[1:1500,2:4], test_set[1:1500,2:4], train_set[1:1500,2], k=1, prob=TRUE)
# length(prediction_knn)
# prediction_knn
summary(test_set$clientType[1:1500])
table(prediction_knn, test_set[1:1500,2])
table(prediction_knn, test_set[1:1500,2]) %>% prop.table() %>% round(2)

# Then calculate the accuracy
cm = as.matrix(table(Actual = test_set$clientType[1:1500], Predicted = prediction_knn))
cm   # confusion matrix
# accuracy
sum(diag(cm))/length(test_set$clientType[1:1500])
# precision=TP/(TP+FP)
# recall=TP/(TP+FN)

### k=2
prediction_knn  <- knn(train_set[1:1500,2:4], test_set[1:1500,2:4], train_set[1:1500,2], k=2, prob=TRUE)
summary(test_set$clientType[1:1500])
table(prediction_knn, test_set[1:1500,2])
table(prediction_knn, test_set[1:1500,2]) %>% prop.table() %>% round(2)
# Then calculate the accuracy
cm = as.matrix(table(Actual = test_set$clientType[1:1500], Predicted = prediction_knn))
cm   # confusion matrix
# accuracy
sum(diag(cm))/length(test_set$clientType[1:1500])

### k=3
prediction_knn  <- knn(train_set[1:1500,2:4], test_set[1:1500,2:4], train_set[1:1500,2], k=3, prob=TRUE)
summary(test_set$clientType[1:1500])
table(prediction_knn, test_set[1:1500,2])
table(prediction_knn, test_set[1:1500,2]) %>% prop.table() %>% round(2)
# Then calculate the accuracy
cm = as.matrix(table(Actual = test_set$clientType[1:1500], Predicted = prediction_knn))
cm   # confusion matrix
# accuracy
sum(diag(cm))/length(test_set$clientType[1:1500])

####################################  ctree  ######################################
ctree.formula <- clientType ~ .
ctree.UQ <- ctree(ctree.formula, data = train_set[1:20000,2:4])
plot(ctree.UQ)

### end of ctree

# use regression model to handel data
reg.client<-DT.UQ[-ms.columns,]
reg.client
fit<-lm(lon+lat~building,data = reg.client)
fit
DT.UQ[ms.columns,c("clientType","building")]<-round(predict(fit,DT.UQ[ms.columns,]))
DT.UQ[ms.columns,"building"]<-round(predict(fit,DT.UQ[ms.columns,]))

DT.UQ.cp[,2:4]

# use the random forest to process missing value
UQ_randomforest <- randomForest(ctree.formula,
                                data = train_set[1:200,2:4],
                                ntree =500,
                                mtry=3,
                                importance=TRUE ,
                                proximity=TRUE)
UQ_randomforest$importance
varImpPlot(UQ_randomforest, main = "variable importance")
# Predict the test set
pre_ran <- predict(UQ_randomforest,newdata=test_set)
# Integrate the true and predicted values together
obs_p_ran = data.frame(prob=pre_ran,obs=test_set$clientType)
# make confusion matrix
table(test_set$clientType,pre_ran,dnn=c("true value","predicted value"))
# draw ROC curve
ran_roc <- roc(test_set$clientType,as.numeric(pre_ran))
plot(ran_roc, print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2),grid.col=c("green", "red"), max.auc.polygon=TRUE,auc.polygon.col="skyblue", print.thres=TRUE,main='Random forest ROC curve,mtry=3,ntree=500')



################################################################################
############################ clustering ########################################
################################################################################

############################## Kmeans ##########################################
# Rescale the values into the range of (0,1)
UQ_clus <- apply(kNN.UQ.cp[,c(2:4,6:7)], MARGIN = 2, FUN = function(X) (X - min(X))/diff(range(X)))
UQ_clus <- as.data.frame(UQ_clus)
UQ_clus

# k=2 # clustering the lon and lat
UQ.clus.2 <- kmeans(UQ_clus[1:500,],2,iter.max = 9, nstart = 10)
typeof(UQ.clus.2)
table(UQ.clus.2$cluster,kNN.UQ.cp$service[1:500])
kmeans.2.cluster <- as.factor(UQ.clus.2$cluster)
ggplot(kNN.UQ.cp[1:500,c(2:4,6:7)],aes(lon,lat,color=kmeans.2.cluster))+geom_point()

# k=3 # clustering the clientType and service
UQ.clus.3 <- kmeans(kNN.UQ.cp[1:5000,2:4],3,iter.max = 9, nstart = 10)
typeof(UQ.clus.3)
table(UQ.clus.3$cluster,kNN.UQ.cp$clientType[1:5000])
kmeans.3.cluster <- as.factor(UQ.clus.3$cluster)
ggplot(kNN.UQ.cp[1:5000,2:4],aes(clientType,service,color=kmeans.3.cluster))+geom_point()

# k=4 # clustering the lon and lat
UQ.clus.4 <- kmeans(UQ_clus[1:50000,],4,iter.max = 9, nstart = 10)
typeof(UQ.clus.4)
table(UQ.clus.4$cluster,kNN.UQ.cp$service[1:50000])
kmeans.4.cluster <- as.factor(UQ.clus.4$cluster)
ggplot(kNN.UQ.cp[1:50000,c(2:4,6:7)],aes(lon,lat,color=kmeans.4.cluster))+geom_point()

# k=5 # clustering the clientType and service
UQ.clus.5 <- kmeans(kNN.UQ.cp[1:5000,2:4],5,iter.max = 9, nstart = 10)
typeof(UQ.clus.5)
table(UQ.clus.5$cluster,kNN.UQ.cp$clientType[1:5000])
kmeans.5.cluster <- as.factor(UQ.clus.5$cluster)
ggplot(kNN.UQ.cp[1:5000,2:4],aes(clientType,service,color=kmeans.5.cluster))+geom_point()

# k=6 # clustering the lon and lat
UQ.clus.6 <- kmeans(UQ_clus[1:50000,],6,iter.max = 9, nstart = 10)
typeof(UQ.clus.6)
table(UQ.clus.6$cluster,kNN.UQ.cp$service[1:50000])
kmeans.6.cluster <- as.factor(UQ.clus.6$cluster)
ggplot(kNN.UQ.cp[1:50000,c(2:4,6:7)],aes(lon,lat,color=kmeans.6.cluster))+geom_point()

######################## end of Kmeans ###########################


########################## agglomerative clustering ###############################
idx <- sample(1:5000,40)
idx <- sample(1:100)
distance_matrix <- dist(as.matrix(UQ_clus[idx,-8]), method = "euclidean")
hc <- hclust(distance_matrix, method = "average")
plot(hc, hang = -1, labels = DT.UQ.cp$clientType[idx])
nclust <- 3
rect.hclust(hc,  k = nclust)
groups <- cutree(hc, k = nclust)
rect.hclust(hc,  k = 4)
rect.hclust(hc,  k = 5)
rect.hclust(hc,  k = 6)
rect.hclust(hc,  k = 7)
rect.hclust(hc,  k = 10)


