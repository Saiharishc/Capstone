library("dplyr")


setwd("C:\\Users\\Sai\\Downloads\\Jigsaw\\Capstone")
getwd()
dat<-read.csv("sampletelecomfinal.csv",stringsAsFactors = FALSE)
names(dat)
str(dat)
colSums(is.na(dat))

names(dat)
colSums(is.na(dat))
length(which(dat$car_buy %in% "New"))
length(which(dat$car_buy %in% "UNKNOWN"))
#Dropping NA columns
indx<-which(names(dat) %in% c("mailordr","occu1","numbcars","retdays","wrkwoman","solflag","proptype","mailresp","cartype","car_buy","children","div_type"))
dat2 <- dat[,-indx]
names(dat2)


dat <- dat2


#attach(dat)
#detach(dat)
#DataType1<-list()
DataType<-list()
NoOfRecords<-list()
UniqueRecords<-list()
DataAvailable<-list()
AvailablePercent<-list()
Missing<-list()
MissingPercent<-list()
Minimum<-list()
Maximum<-list()
Mean<-list()
`5th Percentile` <-list()
`10th Percentile`<-list()
`25th Percentile`<-list()
`50th Percentile`<-list()
`75th Percentile`<-list()
`90th Percentile`<-list()
`95th Percentile`<-list()
str(dat)
j<-1
#class(dat[,"mou_Mean"])
for (i in names(dat)){
  #DataType1[j]<-class(dat[,i])
  DataType[j]<-class(dat[[i]])    # dat$i, "dat$i" dat[[i]] dat[,i]
  NoOfRecords[j]<-length(dat[[i]])
  UniqueRecords[j]<-length(unique(dat[[i]]))
  DataAvailable[j]<-(length(dat[[i]])-sum(is.na(dat[[i]])))
  AvailablePercent[j]<-(as.numeric(DataAvailable[j])/as.numeric(NoOfRecords[j]))
  Missing[j]<-sum(is.na(dat[[i]]))
  MissingPercent[j]<-(as.numeric(Missing[j])/as.numeric(NoOfRecords[j]))
  if (class(dat[[i]])=="character"){
    Minimum[j]<-0
    Maximum[j]<-0
    Mean[j]<-0
    
    `5th Percentile`[j]<-0
    `10th Percentile`[j]<-0
    `25th Percentile`[j]<-0
    `50th Percentile`[j]<-0
    `75th Percentile`[j]<-0
    `90th Percentile`[j]<-0
    `95th Percentile`[j]<-0
    
    
  } else {
    Minimum[j]<-min(dat[[i]], na.rm = TRUE)
    Maximum[j]<-max(dat[[i]],na.rm = TRUE)
    Mean[j]<- mean(dat[[i]],na.rm = TRUE)
    
    `5th Percentile`[j]<-quantile(dat[[i]],c(0.05),na.rm = TRUE)
    `10th Percentile`[j]<- quantile(dat[[i]],c(0.1),na.rm = TRUE)
    `25th Percentile`[j]<- quantile(dat[[i]],c(0.25),na.rm = TRUE)
    `50th Percentile`[j]<-quantile(dat[[i]],c(0.5),na.rm = TRUE)
    `75th Percentile`[j] <- quantile(dat[[i]],c(0.75),na.rm = TRUE)
    `90th Percentile`[j]<-quantile(dat[[i]],c(0.90),na.rm = TRUE)
    `95th Percentile`[j]<-quantile(dat[[i]],c(0.95),na.rm = TRUE)
  }
  
  j<-j+1
}



DataType<-unlist(DataType)
NoOfRecords<-unlist(NoOfRecords)
UniqueRecords<- unlist(UniqueRecords)
DataAvailable<-unlist(DataAvailable)
AvailablePercent<-unlist(AvailablePercent)
Missing<-unlist(Missing)
MissingPercent<-unlist(MissingPercent)
Minimum<-unlist(Minimum)
Maximum<-unlist(Maximum)
Mean<-unlist(Mean)
`5th Percentile` <-unlist(`5th Percentile`)
`10th Percentile`<-unlist(`10th Percentile`)
`25th Percentile`<-unlist(`25th Percentile`)
`50th Percentile`<-unlist(`50th Percentile`)
`75th Percentile`<-unlist(`75th Percentile`)
`90th Percentile`<-unlist(`90th Percentile`)
`95th Percentile`<-unlist(`95th Percentile`)

cb <-cbind(DataType, NoOfRecords,UniqueRecords,DataAvailable,AvailablePercent,Missing,MissingPercent,Minimum,Maximum,Mean,`5th Percentile`,`10th Percentile`,`25th Percentile`,`50th Percentile`,`75th Percentile`,`90th Percentile`,`95th Percentile`)
#class(as.data.frame(cbind(DataType, NoOfRecords)))

QualityReport <- as.data.frame(cb)
rownames(QualityReport)<-colnames(dat)
head(QualityReport)

write.csv(QualityReport,"C:\\Users\\Sai\\Downloads\\Jigsaw\\Capstone\\Sample Quality Report1.csv")



sort(table(dat$marital),decreasing=TRUE)



?sort


# Continuous Variable Profiling

str(dat2)
names(dat)

#Adults,car_buy,cartype,children,churn,creditcd,crtcount,div_type,dualband,educ1,hhstatin,infobase,(kid0_2,kid3_5,kid6_10,kid11_15,kid16_17),last_swap,lor,mailflag,mailordr,mailresp,new_cell,numbcars,occu1,ownrent,pcowner,phones,pre_hnd_price,proptype,ref_qty,rv,solflag,tot_acpt,tot_ret,wrkwoman
ind <- which(names(dat2) %in% c("actvsubs","age1","age2","area","asl_flag","crclscod","csa","Customer_ID","dwllsize","dwlltype","ethnic","forgntvl","hnd_price","hnd_webcap","income","marital","models","mtrcycle","prizm_social_one","refurb_new","truck","uniqsubs","churn"))

dat2_cat1 <- dat2[,ind]
names(dat2_cat1)
summary(dat2_cat1)
str(dat2_cat1)

dat2_cat1%>%count(churn,levels=area)%>%filter(churn==1)->datC1
datC1$N<-unclass(dat2_cat1%>%filter(area%in%datC1$levels)%>%count(area))[[2]]
datC1$ChurnPerc<-datC1$n/datC1$N 
datC1$Var.Name<-rep("area",nrow(datC1))
datC1


#models,actvsub,uniqsubs,forgntvl,mtrcycle,truck,churn  --- yet to convert to categorical variables
index<-which(names(dat2) %in% c("crclscod","asl_flag","area","refurb_new","marital","ethnic","csa","models","actvsubs","uniqsubs","forgntvl","mtrcycle","truck","churn"))
dat2_cont <- dat2[,-index]
names(dat2_cont)

dat2_cat <- dat2[,index]
names(dat2_cat)

#dropping Customer_ID column
index1<- which(names(dat2_cont) %in% "Customer_ID")
dat2_cont <- dat2_cont[,-index1]
length(names(dat2_cont))
str(dat2_cont)


indexContReq <- which(names(dat2) %in% c("mou_Mean","totmrc_Mean","mou_Range","drop_blk_Mean","drop_vce_Range","owylis_vce_Range","mou_opkv_Range","months","eqpdays","totcalls","iwylis_vce_Mean","rev_Mean","comp_vce_Mean","avgmou","avg6mou","avg6qty","totrev","avgrev"
))
length(indexContReq)

dat2ContReq <- dat[,indexContReq]
names(dat2ContReq)

indexCatReq <- which(names(dat2) %in% c("churn","income","crclscod","prizm_social_one","area","marital","ethnic","age1","age2","models","hnd_price","actvsubs","uniqsubs","dwllsize","csa"))
length(indexCatReq)
dat2CatReq <- dat[,indexCatReq]
names(dat2CatReq)

unique(dat2CatReq$income)

for(i in names(dat2CatReq))
{
  dat2CatReqF[[i]] <- as.factor(dat2CatReq[[i]])
}
str(dat2CatReqF)

dat2ReqFinal <- cbind(dat2ContReq,dat2CatReqF)
names(dat2ReqFinal)

#mou_mean -> 26 rows removed
index1<-which(is.na(dat2ReqFinal$mou_Mean))
dat2ReqFinal<-dat2ReqFinal[-index1,]

index2<-which(is.na(dat2ReqFinal$marital))
dat2ReqFinal<-dat2ReqFinal[-index2,]

index3<-which(is.na(dat2ReqFinal$avg6mou))
dat2ReqFinal<-dat2ReqFinal[-index3,]

index4 <- which(is.na(dat2ReqFinal$area))
dat2ReqFinal <- dat2ReqFinal[-index4,]

dat2ReqFinal <- dat2ReqFinal[,-c(19,21,28,31)]

str(dat2ReqFinal)
colSums(is.na(dat2ReqFinal))


#Reducing number of levels
head(dat2ReqFinal)
#Use decision tree to prepare
library(rpart)
#classification: method will be class
#Regression: method will be anova
mod<-rpart(target~text,data=data,method="class")
unique(mod$where)
data$Nodes<-mod$where
head(data)

data$Nodes=ifelse(data$Nodes==2,"Group1","Group2")

------------------------
  
  predtrain<-reduced$fitted.values
head(predtrain)

#install.packages("irr")
library(irr)
kappa2(data.frame(train$churn,predtrain))
confusionMatrix(as.factor(predtrain),as.factor(train$churn),positive = "1")

pred<-predict(reduced,newdata=test,type="response")
head(pred)
str(pred)
unique(pred)

table(dat2_req_final$churn)/nrow(dat2_req_final)

pred<-ifelse(pred>0.238,1,0)
str(pred)


library(irr)
library(gains)
library(caret)
kappa2(data.frame(test$churn,pred))

confusionMatrix(as.factor(pred),as.factor(test$churn),positive = "1")

library(ROCR)
head(predicted)
predicted <- reduced$fitted.values
str(predicted) 
pred1<-prediction(predicted,train$churn)
auc<-performance(pred1,"auc")
auc

#precision
513/(513+1625)
#recall
513/(513+1253)


#for test auc
pred2<-prediction(pred,test$churn.1)
auc<-performance(pred2,"auc")
auc



#https://rpubs.com/kaz_yos/rocLogistic

#install.packages("epicalc")
#install.packages("pROC")
library(pROC)
#library(epicalc)
logistic.display(fitLogistic)
resLroc <- lroc(fitLogistic)
resLroc$auc

library(pROC)
roc(train$churn.1,reduced$fitted.values,plot=TRUE)

roc(train$churn.1,reduced$fitted.values,plot=TRUE,legacy.axes=TRUE,percent = TRUE)


sigmodel<-glm(formula = churn.1 ~ mou_Mean + totmrc_Mean + mou_Range + 
                drop_vce_Range + owylis_vce_Range + mou_opkv_Range + months + 
                totcalls + eqpdays + rev_Mean + avg6mou + avg6qty + completed_perc + 
                asl_flag.Y + prizm_social_one.S + prizm_social_one.U + area.CALIFORNIA.NORTH.AREA + 
                area.CHICAGO.AREA + area.DALLAS.AREA + area.DC.MARYLAND.VIRGINIA.AREA + 
                area.MIDWEST.AREA + area.NEW.YORK.CITY.AREA + area.NORTH.FLORIDA.AREA + 
                area.NORTHWEST.ROCKY.MOUNTAIN.AREA + area.PHILADELPHIA.AREA + 
                area.SOUTH.FLORIDA.AREA + area.TENNESSEE.AREA + marital.M + 
                marital.S + models.2 + models.3 + models.4 + uniqsubs.2 + 
                uniqsubs.3 + uniqsubs.4 + uniqsubs.5 + uniqsubs.7 + uniqsubs.12 + 
                crclscod_Level.2 + crclscod_Level.3 + ethnic_Level.2 + ethnic_Level.3 + 
                hnd_price_Level.2 + hnd_price_Level.3 + actvsubs.1, family = binomial, 
              data = train)

summary(sigmodel)

predtrain<-sigmodel$fitted.values
###this is replacement of table()/nrow() code
tapply(predtrain, train$churn.1, mean)
table(train$churn.1, predtrain > 0.3) #for confusion matrix of train results

pred<-predict(sigmodel,newdata=test,type="response")
table(test$churn.1,pred>0.3) #for confusion matrix of test result


#precision:
279/(279+593)

1388/(1388+3083)

library(pROC)
roc(train$churn.1,sigmodel$fitted.values,plot=TRUE)

summary(test$totrev)


names(dat2_req_final_dummy)
hist(log(dat2_req_final_dummy$mou_Mean))
hist(log(dat2_req_final_dummy$mou_Range))
hist(log(dat2_req_final_dummy$drop_blk_Mean))
hist(log(dat2_req_final_dummy$drop_vce_Range))
hist(log(dat2_req_final_dummy$owylis_vce_Range))
hist(log(dat2_req_final_dummy$mou_opkv_Range))
hist(log(dat2_req_final_dummy$totcalls))
hist(log(dat2_req_final_dummy$rev_Mean))
hist(log(dat2_req_final_dummy$avg6mou))
hist(log(dat2_req_final_dummy$avg6qty))
hist(log(dat2_req_final_dummy$totrev))

---------------
  '''
#str(predtrain)
#str(train$churn.1)

table(dat2_req_final$churn)/nrow(dat2_req_final)

pred<-ifelse(predtrain>0.3,1,0)
str(pred)

kappa2(data.frame(test$churn,predtrain))
confusionMatrix(as.factor(predtrain),as.factor(test$churn.1),positive = "1")


pred<-predict(sigmodel,newdata=test,type="response")
pred<-ifelse(pred>0.238,1,0)


library(pROC)
roc(train$churn.1,sigmodel$fitted.values,plot=TRUE)'''






'''
#variation in modeling
sigmodel<-glm(formula = churn.1 ~ mou_Mean + totmrc_Mean + mou_Range + 
drop_blk_Mean + months + eqpdays + rev_Mean + comp_vce_Mean + avgmou + asl_flag.Y + 
models.2 + models.3 + models.4 + actvsubs.3 + 
uniqsubs.2 + uniqsubs.3 + uniqsubs.4 + uniqsubs.5 + crclscod_Level.2 + crclscod_Level.3 + ethnic_Level.2 + 
ethnic_Level.3 + hnd_price_Level.2 + hnd_price_Level.3, family = binomial, 
data = train)

summary(sigmodel)

predtrain<-sigmodel$fitted.values
###this is replacement of table()/nrow() code
tapply(predtrain, train$churn.1, mean)
table(train$churn.1, predtrain > 0.3) #for confusion matrix of train results

pred<-predict(sigmodel,newdata=test,type="response")
table(test$churn.1,pred>0.3) #for confusion matrix of test resulta

#precision:
279/(279+593)

1388/(1388+3083)

library(pROC)
roc(train$churn.1,sigmodel$fitted.values,plot=TRUE)'''

sigmodel<-glm(formula = churn.1 ~ mou_Mean + totmrc_Mean + mou_Range + 
                drop_blk_Mean + months + eqpdays + rev_Mean + comp_vce_Mean + avgmou + asl_flag.Y + prizm_social_one.S + 
                prizm_social_one.T + prizm_social_one.U + area.CALIFORNIA.NORTH.AREA + 
                area.MIDWEST.AREA + area.NEW.YORK.CITY.AREA + area.NORTHWEST.ROCKY.MOUNTAIN.AREA + 
                area.PHILADELPHIA.AREA + area.TENNESSEE.AREA + marital.S + 
                models.2 + models.3 + models.4 + actvsubs.3 + 
                uniqsubs.2 + uniqsubs.3 + uniqsubs.4 + uniqsubs.5 + crclscod_Level.2 + crclscod_Level.3 + ethnic_Level.2 + 
                ethnic_Level.3 + hnd_price_Level.2 + hnd_price_Level.3, family = binomial, 
              data = train)

summary(sigmodel)

predtrain<-sigmodel$fitted.values
###this is replacement of table()/nrow() code
tapply(predtrain, train$churn.1, mean)
table(train$churn.1, predtrain > 0.3) #for confusion matrix of train results

pred<-predict(sigmodel,newdata=test,type="response")
table(test$churn.1,pred>0.3) #for confusion matrix of test result

predtest<-ifelse(pred>0.3,1,0)

