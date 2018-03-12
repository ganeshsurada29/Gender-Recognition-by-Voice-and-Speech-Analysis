library(caret)
library(lattice)
library(ggplot2)

vc<-read.csv("voice (3).csv", stringsAsFactors = F)
for(i in 1:nrow(vc))
{
  if((vc$label[i])=="male"){
    vc$label[i] <- 1 
  }
  else{
    vc$label[i] <- 0
  }
  
}

vc$label <- as.numeric(vc$label)

head(vc$label)
tail(vc$label)

set.seed(55)
vctrain <- sample(row.names(vc), 0.5*dim(vc)[1])
vcvalid <- setdiff(row.names(vc), vctrain)

vctrain.norm <- vctrain
vcvalid.norm <- vcvalid
vc.norm <- vc

norm <- preProcess(vctrain[, -21], method=c("center", "scale"))
vctrain.norm[, -21] <- predict(norm, vctrain[, -21])
vcvalid.norm[, -21] <- predict(norm, vcvalid[, -21])
vc.norm[, -21] <- predict(norm, vc[, -21])



########################################
##### applying KNN algorithm  ##########
########################################

library(FNN)
nn <- knn(train = vctrain.norm[, -21], test = vcvalid.norm[,-21], vctrain.norm[, 21] , k = 1)
nn
row.names(vctrain)[attr(nn, "nn.index")]

library(caret)

accuracy <- data.frame(k = seq(1, 100, 1), accuracy = rep(0, 100))

for(i in 1:100) {
  knn.pred <- knn(vctrain.norm[, -21], vcvalid.norm[, -21],
                  cl = vctrain.norm[, 21], k = i)
  accuracy[i, 2] <- confusionMatrix(knn.pred, vcvalid.norm[, 21])$overall[1]
}
accuracy

nnfinal <- knn(train = vctrain.norm[, -21], test = vcvalid.norm[,-21], vctrain.norm[, 21], k = 8)
nnfinal

confusionMatrix(nnfinal, vcvalid.norm[, 21])$overall[1]



########################################################
##### applying logistic regression algorithm  ##########
########################################################


logmodel <- glm(label~., data = vctrain, family = binomial)
class(predicted)
predicted <- predict(logmodel, vcvalid, type = "response")
predicted <- as.data.frame(predicted)
dim(predicted)
predicted$result <- 0
colnames(predicted)
predicted$predicted[1]
for(i in 1:nrow(predicted)){
  if(predicted$predicted[i]>0.5){
    predicted$result[i] <- 1
  }
}

z = 0
for(i in 1:nrow(predicted)){
  if(predicted$result[i]==vcvalid$label[i]){
    z = z+1
  }
}
z/nrow(predicted)



###################################################
##### applying Decision trees algorithm  ##########
###################################################

prunedmodel <- rpart(label~ ., data = vctrain,
                     control = rpart.control(minbucket = 1, minsplit = 10, maxdepth = 30, cp = 0.001), xval = 5)
pruned.ct <- prune(prunedmodel, cp = prunedmodel$cptable[which.min(prunedmodel$cptable[,"xerror"]),"CP"])
length(pruned.ct$frame$var[pruned.ct$frame$var == "<leaf>"])
prp(pruned.ct, type = 1, extra = 1, split.font = 1, varlen = -10) 
printcp(pruned.ct)

classified2 <- predict(prunedmodel, vcvalid)

class(classified2)

classified2 <- as.data.frame(classified2)
classified2$result <- 0 

colnames(classified2) <- c("male","result")

for(i in 1:nrow(classified2)){
  if(classified2$male[i]>0.5){
    classified2$result[i] <- 1
  }
}

m = 0

for(i in 1:nrow(classified2)){
  if(classified2$result[i]==vcvalid$label[i]){
    m = m+1
  }
}
m/nrow(classified2)

###################################################
##### applying Random forests algorithm  ##########
###################################################

rf.model=randomForest(label~.,data=vc,subset=vctraindata)
oob.err=double(13)
test.err=double(13)

for(mtry in 1:20)
{
  rf.finalmodel=randomForest(label~.,data=vc,subset=vctraindata, mtry=mtry, ntree=400)
  oob.err[mtry]=rf.finalmodel$mse[400]
  pred=predict(rf.finalmodel,vcvalid)
  test.err[mtry]=with(vcvalid,mean(label-pred)^2)
  cat(mtry, "")
}

matplot(1:mtry, cbind(test.err,oob.err),pch=19, col=c("red","blue"),type="b",ylab="Mean Squared Error")
legend("topright", legend=c("OOB","Test"), pch=19, col=c("red","blue"))


modelrf=randomForest(label~.,data=vc,subset=vctraindata, mtry=mtry, ntree=400)
predd <- predict(modelrf,vcvalid)


confusionMatrix(ifelse(predd>0.5,1,0),vcvalid$label)

