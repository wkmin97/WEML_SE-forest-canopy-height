gc()
rm(list=ls())

library(MASS)
library(VSURF)
library(randomForest)
data<- read.csv('sample_data_allrh.csv',header = T)
data <- data[,-302]
colnames(data)[302]<- 'gfc'

load("E:/program/neon_test/neon_test/model.rf.allrh.Rdata")
load("E:/program/neon_test/neon_test/model.lm.allrh.Rdata")
load("E:/program/neon_test/neon_test/sel.lm.allrh.Rdata")
load("E:/program/neon_test/neon_test/sel.rf.allrh.Rdata")

lm.pred <- predict(model.lm,data[sel.lm.allrh])
data$lm.pred <- lm.pred
rf.pred <- predict(model.rf,data[sel.rf.allrh])
data$rf.pred <- rf.pred
write.table(data,file='sample_data_neon_pre.txt',row.names = F)
write.csv(data,file='sample_data_neon_pre',row.names = F)


data <- read.table('./abby_GEDI_s2l8to_pred.txt',header = T,sep=' ')
y <- data[['rh95']]
xs <- data[c(108:304)]
xs <- xs[,-195]
#data= cbind(y,xs)
#colnames(data)[1]<-'rh95'
#xs <- data[c(2:195)]
###rh95 sel vars####
xxs <- cbind(y,xs)
fit<-lm(y~.,data = xxs)
step.both <- stepAIC(fit,direction = "both",trace=F)
summary(step.both)
save(step.both,file="./step.both.rh95.Rdata")
sel.lm <- names(step.both$coefficients)
sel.lm
sel.lm.rh95 <- sel.lm[2:length(sel.lm)] 
sel.lm.rh95
save(sel.lm.rh95,file="./sel.lm.rh95.Rdata")
model.lm<-lm(y~.,data = xxs)
save(model.lm,file="./model.lm.rh95.step.Rdata")

rmse <- function(x,y){
  sqrt(mean((x-y)^2))
}
tenfold <- function(sel.lm){
  df <- data.frame(rmse.train=numeric(), 
                   rmse.test=numeric(), 
                   r.train=numeric(), 
                   r.test=numeric()) 
  ind <- sample(10, nrow(data), replace=T)
  for (i in 1:10){
    xs <- data[c("rh95",sel.lm)]
    data.train <- xs[ind!=i,]
    data.test<- xs[ind==i,]
    model.lm<-lm(rh95~.,data = data.train)
    pred.train <- predict(model.lm,data.train)
    pred.test <- predict(model.lm,data.test)
    r.train <- cor(data.train$rh95,pred.train)
    r.test <- cor(data.test$rh95,pred.test)
    rmse.train <- rmse(data.train$rh95,pred.train)
    rmse.test <- rmse(data.test$rh95,pred.test)
    df[nrow(df)+1,] <- c(rmse.train,rmse.test,r.train,r.test)
  }
  c(mean(df$rmse.train),mean(df$rmse.test),mean(df$r.train),mean(df$r.test))
}
result.lm.rh95 <- tenfold(sel.lm.rh95)
save(result.lm.rh95,file="./result.lm.rh95.Rdata")
result.lm.rh95

vsurf <- VSURF(xs,y,parallel=TRUE)
summary(vsurf)
save(vsurf,file="./vsurf.rh95.Rdata")
load("./vsurf.rh95.Rdata")
sel.rf.rh95 <- names(xs[vsurf$varselect.pred])
save(sel.rf.rh95,file="./sel.rf.rh95.Rdata")
sel.rf.rh95

rf_tenfold <- function(ntree,mtry,sel,target){
  df <- data.frame(ntree=numeric(), 
                   mtry=numeric(), 
                   rmse.train=numeric(), 
                   rmse.test=numeric(), 
                   r.train=numeric(), 
                   r.test=numeric()) 
  ind <- sample(10, nrow(xxs), replace=T)
  for (i in 1:10){
    data.train <- xxs[ind!=i,]
    data.test<- xxs[ind==i,]
    rf.model = randomForest(x=data.train[sel], y=data.train[[target]], 
                            ntree=ntree, mtry=mtry,importance=TRUE )
    pred.train <- predict(rf.model,data.train[sel])
    pred.test <- predict(rf.model,data.test[sel])
    rmse.train <- rmse(pred.train,data.train[[target]])
    r.train <- cor(pred.train,data.train[[target]])
    rmse.test <- rmse(pred.test,data.test[[target]])
    r.test <- cor(pred.test,data.test[[target]])
    df[nrow(df)+1,] <- c(ntree,mtry,rmse.train,rmse.test,r.train,r.test)
  }
  result.tenfold <- data.frame(ntree=numeric(), 
                               mtry=numeric(), 
                               rmse.train=numeric(), 
                               rmse.test=numeric(), 
                               r.train=numeric(), 
                               r.test=numeric()) 
  result.tenfold[1,] <- c(ntree,mtry,mean(df$rmse.train),mean(df$rmse.test),
                          mean(df$r.train),mean(df$r.test))
  return(result.tenfold)
}

####### function to apply 10-fold #####
tune_tenfold <- function(sel.rf,strmodel){
  ntrees = seq(250,1000,250)
  mtrys= 1:(length(sel.rf)-1)
  tune.rf <- data.frame(ntree=numeric(),
                        mtry=numeric(), 
                        rmse.train=numeric(), 
                        rmse.test=numeric(), 
                        r.train=numeric(), 
                        r.test=numeric()) 
  for (i in ntrees) {
    for (j in mtrys) {
      tune.rf[nrow(tune.rf)+1,] <- rf_tenfold(i,j,sel=sel.rf,target="y")
    }
  }
  # check best parameters 
  #tune.rf
  #min_rmse=min(tune.rf$rmse.test);#min_rmse
  #idx=which(tune.rf$rmse.test==min_rmse);#idx 
  max_r <- max(tune.rf$r.test)
  idx=which(tune.rf$r.test==max_r)
  model.rf = randomForest(x=data[sel.rf], y=data[["y"]], ntree=tune.rf$ntree[idx], mtry=tune.rf$mtry[idx],importance=TRUE )
  save(tune.rf, file=paste0("./tune.rf.",strmodel,".Rdata"))
  save(model.rf,file=paste0("./model.rf.",strmodel,".Rdata"))
  return(tune.rf[idx,])
}
tune.rf.rh95.best <- tune_tenfold(sel.rf.rh95, 'rh95')
tune.rf.rh95.best


########lm pred rh#########
lm.y <- data[['lm.pred']]
xxs= cbind(lm.y,xs)
fit<-lm(lm.y~.,data = xxs)
step.both <- stepAIC(fit,direction = "both",trace=F)
summary(step.both)
save(step.both,file="./step.both.lm.y.Rdata")
sel.lm <- names(step.both$coefficients)
sel.lm
sel.lm.lm.y <- sel.lm[2:length(sel.lm)] 
sel.lm.lm.y
save(sel.lm.lm.y,file="./sel.lm.lm.y.Rdata")
model.lm<-lm(lm.y~.,data = xxs)
save(model.lm,file="./model.lm.lm.y.step.Rdata")

rmse <- function(x,y){
  sqrt(mean((x-y)^2))
}
tenfold <- function(sel.lm){
  df <- data.frame(rmse.train=numeric(), 
                   rmse.test=numeric(), 
                   r.train=numeric(), 
                   r.test=numeric()) 
  ind <- sample(10, nrow(data), replace=T)
  for (i in 1:10){
    xs <- xxs[c("lm.y",sel.lm.lm.y)]
    data.train <- xs[ind!=i,]
    data.test<- xs[ind==i,]
    model.lm<-lm(lm.y~.,data = data.train)
    pred.train <- predict(model.lm,data.train)
    pred.test <- predict(model.lm,data.test)
    r.train <- cor(data.train$lm.y,pred.train)
    r.test <- cor(data.test$lm.y,pred.test)
    rmse.train <- rmse(data.train$lm.y,pred.train)
    rmse.test <- rmse(data.test$lm.y,pred.test)
    df[nrow(df)+1,] <- c(rmse.train,rmse.test,r.train,r.test)
  }
  c(mean(df$rmse.train),mean(df$rmse.test),mean(df$r.train),mean(df$r.test))
}
result.lm.lm.y <- tenfold(sel.lm.lm.y)
save(result.lm.lm.y,file="./result.lm.lm.y.Rdata")
result.lm.lm.y

y=data[['lm.pred']]
vsurf <- VSURF(xs,y,parallel=TRUE)
summary(vsurf)
save(vsurf,file="./vsurf.lm.y.Rdata")
load("./vsurf.lm.y.Rdata")
sel.rf.lm.y<- names(xs[vsurf$varselect.pred])
save(sel.rf.lm.y,file="./sel.rf.lm.y.Rdata")
sel.rf.lm.y

rf_tenfold <- function(ntree,mtry,sel,target){
  df <- data.frame(ntree=numeric(), 
                   mtry=numeric(), 
                   rmse.train=numeric(), 
                   rmse.test=numeric(), 
                   r.train=numeric(), 
                   r.test=numeric()) 
  ind <- sample(10, nrow(xxs), replace=T)
  for (i in 1:10){
    data.train <- xxs[ind!=i,]
    data.test<- xxs[ind==i,]
    rf.model = randomForest(x=data.train[sel], y=data.train[[target]], 
                            ntree=ntree, mtry=mtry,importance=TRUE )
    pred.train <- predict(rf.model,data.train[sel])
    pred.test <- predict(rf.model,data.test[sel])
    rmse.train <- rmse(pred.train,data.train[[target]])
    r.train <- cor(pred.train,data.train[[target]])
    rmse.test <- rmse(pred.test,data.test[[target]])
    r.test <- cor(pred.test,data.test[[target]])
    df[nrow(df)+1,] <- c(ntree,mtry,rmse.train,rmse.test,r.train,r.test)
  }
  result.tenfold <- data.frame(ntree=numeric(), 
                               mtry=numeric(), 
                               rmse.train=numeric(), 
                               rmse.test=numeric(), 
                               r.train=numeric(), 
                               r.test=numeric()) 
  result.tenfold[1,] <- c(ntree,mtry,mean(df$rmse.train),mean(df$rmse.test),
                          mean(df$r.train),mean(df$r.test))
  return(result.tenfold)
}

####### function to apply 10-fold #####
tune_tenfold <- function(sel.rf,strmodel){
  ntrees = seq(250,1000,250)
  mtrys= 1:(length(sel.rf)-1)
  tune.rf <- data.frame(ntree=numeric(),
                        mtry=numeric(), 
                        rmse.train=numeric(), 
                        rmse.test=numeric(), 
                        r.train=numeric(), 
                        r.test=numeric()) 
  for (i in ntrees) {
    for (j in mtrys) {
      tune.rf[nrow(tune.rf)+1,] <- rf_tenfold(i,j,sel=sel.rf,target="lm.y")
    }
  }
  # check best parameters 
  #tune.rf
  #min_rmse=min(tune.rf$rmse.test);#min_rmse
  #idx=which(tune.rf$rmse.test==min_rmse);#idx 
  max_r <- max(tune.rf$r.test)
  idx=which(tune.rf$r.test==max_r)
  model.rf = randomForest(x=data[sel.rf], y=data[["lm.y"]], ntree=tune.rf$ntree[idx], mtry=tune.rf$mtry[idx],importance=TRUE )
  save(tune.rf, file=paste0("./tune.rf.",strmodel,".Rdata"))
  save(model.rf,file=paste0("./model.rf.",strmodel,".Rdata"))
  return(tune.rf[idx,])
}
tune.rf.lm.y.best <- tune_tenfold(sel.rf.lm.y, 'lm.y')
tune.rf.lm.y.best




########rf pred  rh#######

rf.y <- data[['rf.pred']]
xxs= cbind(rf.y,xs)
fit<-lm(rf.y~.,data = xxs)
step.both <- stepAIC(fit,direction = "both",trace=F)
summary(step.both)
save(step.both,file="./step.both.rf.y.Rdata")
sel.lm <- names(step.both$coefficients)
sel.lm
sel.lm.rf.y <- sel.lm[2:length(sel.lm)] 
sel.lm.rf.y
save(sel.lm.rf.y,file="./sel.lm.rf.y.Rdata")
model.lm<-lm(rf.y~.,data = xxs)
save(model.lm,file="./model.lm.rf.y.step.Rdata")

rmse <- function(x,y){
  sqrt(mean((x-y)^2))
}
tenfold <- function(sel.lm){
  df <- data.frame(rmse.train=numeric(), 
                   rmse.test=numeric(), 
                   r.train=numeric(), 
                   r.test=numeric()) 
  ind <- sample(10, nrow(data), replace=T)
  for (i in 1:10){
    xs <- xxs[c("rf.y",sel.lm.rf.y)]
    data.train <- xs[ind!=i,]
    data.test<- xs[ind==i,]
    model.lm<-lm(rf.y~.,data = data.train)
    pred.train <- predict(model.lm,data.train)
    pred.test <- predict(model.lm,data.test)
    r.train <- cor(data.train$rf.y,pred.train)
    r.test <- cor(data.test$rf.y,pred.test)
    rmse.train <- rmse(data.train$rf.y,pred.train)
    rmse.test <- rmse(data.test$rf.y,pred.test)
    df[nrow(df)+1,] <- c(rmse.train,rmse.test,r.train,r.test)
  }
  c(mean(df$rmse.train),mean(df$rmse.test),mean(df$r.train),mean(df$r.test))
}
result.lm.rf.y <- tenfold(sel.lm.rf.y)
save(result.lm.rf.y,file="./result.lm.rf.y.Rdata")
result.lm.rf.y

vsurf <- VSURF(xxs,y,parallel=TRUE)
summary(vsurf)
save(vsurf,file="./vsurf.rf.y.Rdata")
load("./vsurf.rf.y.Rdata")
sel.rf.rf.y <- names(xxs[vsurf$varselect.pred])
save(sel.rf.rf.y,file="./sel.rf.rf.y.Rdata")
sel.rf.rf.y

rf_tenfold <- function(ntree,mtry,sel,target){
  df <- data.frame(ntree=numeric(), 
                   mtry=numeric(), 
                   rmse.train=numeric(), 
                   rmse.test=numeric(), 
                   r.train=numeric(), 
                   r.test=numeric()) 
  ind <- sample(10, nrow(data), replace=T)
  for (i in 1:10){
    data.train <- data[ind!=i,]
    data.test<- data[ind==i,]
    rf.model = randomForest(x=data.train[sel], y=data.train[[target]], 
                            ntree=ntree, mtry=mtry,importance=TRUE )
    pred.train <- predict(rf.model,data.train[sel])
    pred.test <- predict(rf.model,data.test[sel])
    rmse.train <- rmse(pred.train,data.train[[target]])
    r.train <- cor(pred.train,data.train[[target]])
    rmse.test <- rmse(pred.test,data.test[[target]])
    r.test <- cor(pred.test,data.test[[target]])
    df[nrow(df)+1,] <- c(ntree,mtry,rmse.train,rmse.test,r.train,r.test)
  }
  result.tenfold <- data.frame(ntree=numeric(), 
                               mtry=numeric(), 
                               rmse.train=numeric(), 
                               rmse.test=numeric(), 
                               r.train=numeric(), 
                               r.test=numeric()) 
  result.tenfold[1,] <- c(ntree,mtry,mean(df$rmse.train),mean(df$rmse.test),
                          mean(df$r.train),mean(df$r.test))
  return(result.tenfold)
}

####### function to apply 10-fold #####
tune_tenfold <- function(sel.rf,strmodel){
  ntrees = seq(250,1000,250)
  mtrys= 1:(length(sel.rf)-1)
  tune.rf <- data.frame(ntree=numeric(),
                        mtry=numeric(), 
                        rmse.train=numeric(), 
                        rmse.test=numeric(), 
                        r.train=numeric(), 
                        r.test=numeric()) 
  for (i in ntrees) {
    for (j in mtrys) {
      tune.rf[nrow(tune.rf)+1,] <- rf_tenfold(i,j,sel=sel.rf,target="rf.y")
    }
  }
  # check best parameters 
  #tune.rf
  #min_rmse=min(tune.rf$rmse.test);#min_rmse
  #idx=which(tune.rf$rmse.test==min_rmse);#idx 
  max_r <- max(tune.rf$r.test)
  idx=which(tune.rf$r.test==max_r)
  model.rf = randomForest(x=data[sel.rf], y=data[["neon_ht"]], ntree=tune.rf$ntree[idx], mtry=tune.rf$mtry[idx],importance=TRUE )
  save(tune.rf, file=paste0("./tune.rf.",strmodel,".Rdata"))
  save(model.rf,file=paste0("./model.rf.",strmodel,".Rdata"))
  return(tune.rf[idx,])
}
tune.rf.rf.y.best <- tune_tenfold(sel.rf.rf.y, 'rf.y')
tune.rf.rf.y.best
