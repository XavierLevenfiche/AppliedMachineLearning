library(rpart)
library(nnet)
library(ISLR)
library(alr4)
library(MASS)
library(glmnet)
library(randomForest)
library(caret)
library(ggplot2)

setwd({"Your Working Directory (with data in it)"}) ##Insert path here
data<-read.csv({"Your CSV File.csv"},header=TRUE) ##Read in your CSV file
EngageData<-na.omit(data) ##Create working data set, ommitting all NAs
EngageData<-droplevels(EngageData) ##Drop unused levels from the data
data<-NULL ## wipe the original read in

attach(EngageData)
head(EngageData)
dim(EngageData)

EngageData$Engagement.Score<-EngageData$index
summary(EngageData$Engagement.Score)
hist(EngageData$Engagement.Score)
EngageData<-EngageData[!(EngageData$Engagement.Score>150),]
hist(EngageData$Engagement.Score)
summary(EngageData$Engagement.Score)

attach(EngageData)

## Visualize VOI against Index
plot(Engagement.Score~medium,data = EngageData)
summary(medium)

plot(Engagement.Score~channel,data = EngageData)
summary(channel)

plot(Engagement.Score~Word.Interaction,data = EngageData)
summary(Word.Interaction)

plot(Engagement.Score~Word.CentricWord.Centricity,data = EngageData)
summary(Word.CentricWord.Centricity)

## Remove single level and non imp vars
EngageData<-EngageData[,-1] ## Remove Client
EngageData<-EngageData[,-1] ## Remove brand_product
EngageData<-EngageData[,-3] ## Remove SES
EngageData<-EngageData[,-3] ## Remove Benchmark
EngageData<-EngageData[,-3] ## Remove Index 
EngageData<-EngageData[,-21] ## every row item has connect_share = 1
EngageData<-EngageData[,-22] ## every row item has click_on_rollover... = 1
EngageData<-EngageData[,-17] ## every row item has cry_emotional = 0
EngageData<-EngageData[,-17] ## every row item has laugh_comical = 0
EngageData<-EngageData[,-18] ## every row item has competitor_consumer = 0

#As.Factor Binary Vars
EngageData$character._keywords<-as.factor(EngageData$character._keywords)
EngageData$cta_purchase<-as.factor(EngageData$cta_purchase)
EngageData$cta._education<-as.factor(EngageData$cta._education)
EngageData$cta._.coupon.Promo<-as.factor(EngageData$cta._.coupon.Promo)
EngageData$text._clarity._body<-as.factor(EngageData$text._clarity._body)
EngageData$text_.clarity_head<-as.factor(EngageData$text_.clarity_head)
EngageData$product_.present<-as.factor(EngageData$product_.present)
EngageData$consumer_.present<-as.factor(EngageData$consumer_.present)
EngageData$reward_present<-as.factor(EngageData$reward_present)
EngageData$challenge_dare<-as.factor(EngageData$challenge_dare)
EngageData$inspire_try<-as.factor(EngageData$inspire_try)
EngageData$competitor_brand<-as.factor(EngageData$competitor_brand)
EngageData$closable<-as.factor(EngageData$closable)

attach(EngageData)

mod1<-glm(Engagement.Score~.,EngageData,family=gaussian())
summary(mod1)
mod1$coefficients
mod1AIC<-stepAIC(mod1)
mod2<-glm(Engagement.Score~channel+medium+Sensory.Involvement+Word.CentricWord.Centricity+character_.count+Push.or.Pull,EngageData,family="gaussian")
summary(mod2)
summary(Word.CentricWord.Centricity)
mod2$coefficients

hist(EngageData$Engagement.Score)
## Setting up Model Selection
set.seed(0392)
Samp=sample(1:length(EngageData$Engagement.Score),length(EngageData$Engagement.Score),replace=FALSE)
EngageData$SampID=rep(0,length(EngageData$Engagement.Score))
s<-round(length(EngageData$Engagement.Score)/5)
EngageData$SampID[Samp[1:s]]=1
EngageData$SampID[Samp[(s+1):(s+s)]]=2
EngageData$SampID[Samp[(s*2+1):(s*3)]]=3
EngageData$SampID[Samp[(s*3)+1:(s*4)]]=4
EngageData$SampID[Samp[(s*4)+1:length(EngageData$Engagement.Score)]]=5

ErrOLS=0
ErrGLM=0
ErrRidge=0
ErrLasso=0
ErrEN=0
ErrTree=0
ErrTreev2=0
ErrTreev3=0
ErrRF=0
ErrRFv2=0
ErrRFv3=0

ErrEN6=0
ErrEN7=0
ErrEN75=0
ErrEN8=0

for(i in 1:5){
  Set=which(EngageData$SampID==i)
  
  OLSTr<-lm(Engagement.Score~.,data=EngageData[-Set,])
  OLSTe<-predict(OLSTr,newdata=EngageData[Set,])
  ErrOLS<-sum((EngageData[Set,]$Engagement.Score-OLSTe)^2)+ErrOLS
  
  GLMTr<-glm(Engagement.Score~.,family = gaussian(),data=EngageData[-Set,])
  GLMTe<-predict(GLMTr,newdata=EngageData[Set,])
  ErrGLM<-sum((EngageData[Set,]$Engagement.Score-GLMTe)^2)+ErrGLM
  
}

summary(OLSTr)
sqrt(ErrOLS/dim(EngageData)[1])
sqrt(ErrGLM/dim(EngageData)[1])
plot(OLSTr)

#Tree and Random Forest Model
for(i in 1:5){
  Set=which(EngageData$SampID==i)

  TreeTr<-rpart(Engagement.Score~.,data=EngageData[-Set,],control=rpart.control(minsplit = 20))
  TreeTe<-predict(TreeTr,newdata=EngageData[Set,])
  ErrTree<-sum((EngageData[Set,]$Engagement.Score-TreeTe)^2)+ErrTree

  TreeTrv2<-rpart(Engagement.Score~channel+medium+Sensory.Involvement+Word.CentricWord.Centricity+character_.count+Push.or.Pull,data=EngageData[-Set,],control=rpart.control(minsplit = 20))
  TreeTev2<-predict(TreeTrv2,newdata=EngageData[Set,])
  ErrTreev2<-sum((EngageData[Set,]$Engagement.Score-TreeTev2)^2)+ErrTreev2
  
  TreeTrv3<-rpart(Engagement.Score~cta_purchase+cta._education+cta._.coupon.Promo+text_.clarity_head+text._clarity._body+product_.present+consumer_.present+reward_present+challenge_dare+inspire_try+competitor_brand+closable,data=EngageData[-Set,],control=rpart.control(minsplit = 20))
  TreeTev3<-predict(TreeTrv3,newdata=EngageData[Set,])
  ErrTreev3<-sum((EngageData[Set,]$Engagement.Score-TreeTev3)^2)+ErrTreev3

  RFTr<-randomForest(Engagement.Score~. -SampID,data=EngageData[-Set,],ntree=100)
  RFTe<-predict(RFTr,newdata=EngageData[Set,])
  ErrRF<-sum((EngageData[Set,]$Engagement.Score-RFTe)^2)+ErrRF

   RFTrv2<-randomForest(Engagement.Score~channel+medium+Sensory.Involvement+Word.CentricWord.Centricity+character_.count+Push.or.Pull,data=EngageData[-Set,],ntree=100)
  RFTev2<-predict(RFTrv2,newdata=EngageData[Set,])
  ErrRFv2<-sum((EngageData[Set,]$Engagement.Score-RFTev2)^2)+ErrRFv2
  
   RFTrv3<-randomForest(Engagement.Score~cta_purchase+cta._education+cta._.coupon.Promo+text_.clarity_head+text._clarity._body+product_.present+consumer_.present+reward_present+challenge_dare+inspire_try+competitor_brand+closable,data=EngageData[-Set,],ntree=100)
  RFTev3<-predict(RFTrv3,newdata=EngageData[Set,])
  ErrRFv3<-sum((EngageData[Set,]$Engagement.Score-RFTev3)^2)+ErrRFv3
}

## plotting tree
RFTr
plot(RFTr,log="y")
varImpPlot(RFTr,sort=TRUE,10)
importance(RFTr)

RFTrv2
plot(RFTrv2,log="y")
varImpPlot(RFTrv2)

TreeTr
plot(TreeTr,margin=0.2)
text(TreeTr, use.n = TRUE, all=TRUE, cex=0.6)


TreeTrv2
plot(TreeTrv2,margin=0.2)
text(TreeTrv2, use.n = TRUE, all=TRUE, cex=0.5)

plot(RFTrv3,log="y")
varImpPlot(RFTrv3)

plot(TreeTrv3,margin=0.2)
text(TreeTrv3, use.n = TRUE, all=TRUE, cex=0.5)


## RF Error
sqrt(ErrRF/dim(EngageData)[1])
plot(RFTr$predicted)
plot(RFTr$mse)
(sqrt(ErrRF/dim(EngageData)[1]))/(summary(EngageData$Engagement.Score)[4])
(sqrt(ErrRF/dim(EngageData)[1]))/(summary(EngageData$Engagement.Score)[3])
RFTr$importance
RFTr$bestvar

## Previous code, Brand1 SES Error= 424 =657% error - All data
## Previous code, Brand1 SES Error=  =32% error - 80 lim
## Previous code, Brand1 SES Error=  =18% error - 60 lim

## New Code, New data setup.

## Brand1, Ses Error= 8.6 = 23% error <-same prior dataset but modified
## Brand1&Brand2, Index Error= 19.4 = 15% error
## Brand1, Index Error= 14.5 = 12% error

## Brand2VASTAsBrand1&Brand2, Index Error= 14 = 12.6% error @ 150 lim

## Brand2VASTAsBrand1&Brand2, SES Error= 12.8 = 21.9% error @ 10-85 lim

## Brand1&Brand2VAST, Index Error= 26 = 20% error @ 200 lim

## Brand1&Brand2VAST, Index Error= 14 = 12% error @ 150 lim

## Brand1&Brand2VAST, Ses Error= 10.77 = 20.8% error @ 85 lim?

## Brand1&Brand2VAST, Ses Error= 10.71 = 21% error @ 10-85 lim?

## Brand1, Ses Error= 8.6 = 23% error?

## Brand1&Brand2, SES Error= 9.3 = 24% error?

## Brand1AsBrand1&Brand2, SES Error= 3.6 = 11% error?????

## Brand1AsBrand1&Brand2, SES Error= 6.3 = 17% error?????


# Lasso, Ridge, EN, and differnt EN variants

for(i in 1:5){
  Set=which(EngageData$SampID==i)
  Y=EngageData$Engagement.Score[-Set]
  YPr=EngageData[Set,]$Engagement.Score
  X=model.matrix(Engagement.Score~., EngageData[-Set,])
  XPr=model.matrix(Engagement.Score~.,EngageData[Set,])
  
  LMod.bin=glmnet(X,Y,alpha=1,family="gaussian")
  LMod.cv.bin=cv.glmnet(X,Y,alpha=1,family="gaussian")
  LTe<-predict(LMod.bin,newx=XPr,s=LMod.cv.bin$lambda.min)
  ErrLasso=ErrLasso+sum((YPr-LTe)^2)
  
  RMod.bin=glmnet(X,Y,alpha=0,family="gaussian")
  RMod.cv.bin=cv.glmnet(X,Y,alpha=0,family="gaussian")
  RTr<-predict(RMod.bin,newx=XPr,s=RMod.cv.bin$lambda.min)
  ErrRidge=ErrRidge+sum((YPr-RTr)^2)
  
  ENMod.bin=glmnet(X,Y,alpha=0.5,family="gaussian")
  ENMod.cv.bin=cv.glmnet(X,Y,alpha=0.5,family="gaussian")
  ENTr<-predict(ENMod.bin,newx=XPr,s=ENMod.cv.bin$lambda.min)
  ErrEN=ErrEN+sum((YPr-ENTr)^2)
}


