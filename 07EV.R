library(spdep)

gal1211<-read.gal("E:/program/neon_test/neon_test/GEDI_abby_wref_forest_sample.gal")
class(gal1211)

cw<-nb2listw(gal1211)
cmat<-listw2mat(cw)
n<-length(gal1211)
Pmat<-diag(n)-matrix(1,n,n)/n
PCP<-Pmat%*%cmat%*%Pmat
PCPeig<-eigen(PCP,symmetric=T)
eigmax<-max(PCPeig$values)
eigmin<-min(PCPeig$values)
which.max(PCPeig$values)
which.min(PCPeig$values)

EV<-as.data.frame(PCPeig$vectors[,PCPeig$values/PCPeig$values[1]>0.25])
cat("Numver of eigenvectos selected",dim(EV)[2])
colnames(EV)<-paste("EV",1:NCOL(EV),sep="")
colnames(EV)

data <- read.csv('./neon_gedi_s2l8to_pred.csv',header=T)
colnames(data)
EV_data <- data.frame(data,EV)
colnames(EV_data)
write.csv(EV_data,file='./neon_gedi_s2l8to_pred_EV.csv',row.names = F)

rf_pred_EV_data = data.frame(data[c('rf.pred')],data[c(sel.lm.rf.y)],EV)
write.csv(rf_pred_EV_data,file='./data_lmEV8000_rfy.csv',row.names = F)
data <- read.csv('./data_lmEV8000_rfy.csv',header=T)
data_EV800 <- data[,1:892]
set.seed(1)
ind <- sample(2, nrow(data), replace=T,prob = c(0.7,0.3))
data.lmEV_rfy.train <- data[ind==1,]
data.lmEV_rfy.test <- data[ind==2,]
write.csv(data.lmEV_rfy.train,file = './data_lm_allEV_rfy_train.csv',row.names=F)
write.csv(data.lmEV_rfy.test,file = './data_lm_allEV_rfy_test.csv',row.names=F)

ind <- sample(2, nrow(data_EV800), replace=T,prob = c(0.7,0.3))
data.lmEV_rfy.train <- data_EV800[ind==1,]
data.lmEV_rfy.test <- data_EV800[ind==2,]
write.csv(data_EV800,file = './data_lmEV800_rfy.csv',row.names=F)
write.csv(data.lmEV_rfy.train,file = './data_lmEV800_rfy_train.csv',row.names=F)
write.csv(data.lmEV_rfy.test,file = './data_lmEV800_rfy_test.csv',row.names=F)

###EV stepwise
library(MASS)
###sel metrics
#xs <- rf_pred_EV_data
fit<-lm(rf.pred~.,data = rf_pred_EV_data)
step.both <- stepAIC(fit,direction = "both",trace=F)
summary(step.both)
save(step.both,file="./step.both.allrh.Rdata")
sel.lm <- names(step.both$coefficients)
sel.lm
sel.lm.allrh <- sel.lm[2:length(sel.lm)] 
sel.lm.allrh
save(sel.lm.allrh,file="./sel.lm.allrh.Rdata")
load('./sel.lm.allrh.Rdata')


rf_pred_EV_data <- data.frame(data[c('rf.pred')],data[c(sel.lm.rf.y)],EV[,1:200])
rf_pred_EV_data <- data.frame(data[c('rf.pred')],data[c(sel.lm.rf.y)],EV[,1:50])
colnames(rf_pred_EV_data)
set.seed(1)
ind <- sample(2, nrow(rf_pred_EV_data), replace=T,prob = c(0.7,0.3))
data.lmEV_rfy.train <- rf_pred_EV_data[ind==1,]
data.lmEV_rfy.test <- rf_pred_EV_data[ind==2,]
write.csv(rf_pred_EV_data,file = './data_lmEV200_rfy.csv',row.names=F)
write.csv(data.lmEV_rfy.train,file = './data_lmEV200_rfy_train.csv',row.names=F)
write.csv(data.lmEV_rfy.test,file = './data_lmEV200_rfy_test.csv',row.names=F)


library(foreign)
library(stringr)
train_data<- read.csv('data_lmEV_rfy_train_11modelpre.csv',header = T)
test_data<- read.csv('data_lmEV_rfy_test_11modelpre.csv',header = T)
colnames(train_data)
colnames(test_data)

rmse <- function(x,y){
  sqrt(mean((x-y)^2))
}

result<- data.frame(model=character(),
                    train.rmse=numeric(),
                    train.r=numeric(),
                    test.rmse=numeric(),
                    test.r=numeric())

for(i in 3:length(colnames(train_data))){
  train.r <- cor(train_data[['rf.pred']],train_data[,i])
  test.r <- cor(test_data[['rf.pred']],test_data[,i])
  train.rmse <- rmse(train_data[['rf.pred']],train_data[,i])
  test.rmse <- rmse(test_data[['rf.pred']],test_data[,i])
  model.name <- str_sub(colnames(train_data)[i],start=11L)
  result[nrow(result)+1,] <- c(model.name,train.rmse,train.r,test.rmse,test.r)
}

write.csv(result,file='./data_lmEV_rfy_train_11modelpre_result.csv',row.names = F)





train_data<- read.csv('data_lmEV200_rfy_train_11modelpre.csv',header = T)
test_data<- read.csv('data_lmEV200_rfy_test_11modelpre.csv',header = T)
colnames(train_data)
colnames(test_data)

rmse <- function(x,y){
  sqrt(mean((x-y)^2))
}

result<- data.frame(model=character(),
                    train.rmse=numeric(),
                    train.r=numeric(),
                    test.rmse=numeric(),
                    test.r=numeric())

for(i in 3:length(colnames(train_data))){
  train.r <- cor(train_data[['rf.pred']],train_data[,i])
  test.r <- cor(test_data[['rf.pred']],test_data[,i])
  train.rmse <- rmse(train_data[['rf.pred']],train_data[,i])
  test.rmse <- rmse(test_data[['rf.pred']],test_data[,i])
  model.name <- str_sub(colnames(train_data)[i],start=11L)
  result[nrow(result)+1,] <- c(model.name,train.rmse,train.r,test.rmse,test.r)
}

write.csv(result,file='./data_lmEV200_rfy_train_11modelpre_result.csv',row.names = F)




train_data<- read.csv('data_lmEV800_rfy_train_11modelpre.csv',header = T)
test_data<- read.csv('data_lmEV800_rfy_test_11modelpre.csv',header = T)
colnames(train_data)
colnames(test_data)

rmse <- function(x,y){
  sqrt(mean((x-y)^2))
}

result<- data.frame(model=character(),
                    train.rmse=numeric(),
                    train.r=numeric(),
                    test.rmse=numeric(),
                    test.r=numeric())

for(i in 3:length(colnames(train_data))){
  train.r <- cor(train_data[['rf.pred']],train_data[,i])
  test.r <- cor(test_data[['rf.pred']],test_data[,i])
  train.rmse <- rmse(train_data[['rf.pred']],train_data[,i])
  test.rmse <- rmse(test_data[['rf.pred']],test_data[,i])
  model.name <- str_sub(colnames(train_data)[i],start=11L)
  result[nrow(result)+1,] <- c(model.name,train.rmse,train.r,test.rmse,test.r)
}

write.csv(result,file='./data_lmEV800_rfy_train_11modelpre_result.csv',row.names = F)













train_data<- read.csv('data_lmEV200_rfy_train_11modelpre.csv',header = T)
test_data<- read.csv('data_lmEV200_rfy_test_11modelpre.csv',header = T)
colnames(train_data)
colnames(test_data)

rmse <- function(x,y){
  sqrt(mean((x-y)^2))
}

result<- data.frame(model=character(),
                    train.rmse=numeric(),
                    train.r=numeric(),
                    test.rmse=numeric(),
                    test.r=numeric())

for(i in 3:length(colnames(train_data))){
  train.r <- cor(train_data[['rf.pred']],train_data[,i])
  test.r <- cor(test_data[['rf.pred']],test_data[,i])
  train.rmse <- rmse(train_data[['rf.pred']],train_data[,i])
  test.rmse <- rmse(test_data[['rf.pred']],test_data[,i])
  model.name <- str_sub(colnames(train_data)[i],start=11L)
  result[nrow(result)+1,] <- c(model.name,train.rmse,train.r,test.rmse,test.r)
}

write.csv(result,file='./data_lmEV200_rfy_train_11modelpre_result.csv',row.names = F)




train_data<- read.csv('data_lm_allEV_rfy_train_11modelpre.csv',header = T)
test_data<- read.csv('data_lm_allEV_rfy_test_11modelpre.csv',header = T)
colnames(train_data)
colnames(test_data)

rmse <- function(x,y){
  sqrt(mean((x-y)^2))
}

result<- data.frame(model=character(),
                    train.rmse=numeric(),
                    train.r=numeric(),
                    test.rmse=numeric(),
                    test.r=numeric())

for(i in 3:length(colnames(train_data))){
  train.r <- cor(train_data[['rf.pred']],train_data[,i])
  test.r <- cor(test_data[['rf.pred']],test_data[,i])
  train.rmse <- rmse(train_data[['rf.pred']],train_data[,i])
  test.rmse <- rmse(test_data[['rf.pred']],test_data[,i])
  model.name <- str_sub(colnames(train_data)[i],start=11L)
  result[nrow(result)+1,] <- c(model.name,train.rmse,train.r,test.rmse,test.r)
}

write.csv(result,file='./data_lm_allEV_rfy_11modelpre_result.csv',row.names = F)


