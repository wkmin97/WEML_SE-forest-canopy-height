library(foreign)
library(stringr)
train_data<- read.csv('data_rf_lmpred_train_11modelpre.csv',header = T)
test_data<- read.csv('data_rf_lmpred_test_11modelpre.csv',header = T)
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
  train.r <- cor(data_neon_train,train_data[,i])
  test.r <- cor(data_neon_test,test_data[,i])
  train.rmse <- rmse(data_neon_train,train_data[,i])
  test.rmse <- rmse(data_neon_test,test_data[,i])
  model.name <- str_sub(colnames(train_data)[i],start=11L)
  result[nrow(result)+1,] <- c(model.name,train.rmse,train.r,test.rmse,test.r)
}

write.csv(result,file='./rf_lmpred_11model_result_neonht.csv',row.names = F)


train_data<- read.csv('data_rf_rfpred_train_11modelpre.csv',header = T)
test_data<- read.csv('data_rf_rfpred_test_11modelpre.csv',header = T)
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
  train.r <- cor(data_neon_train,train_data[,i])
  test.r <- cor(data_neon_test,test_data[,i])
  train.rmse <- rmse(data_neon_train,train_data[,i])
  test.rmse <- rmse(data_neon_test,test_data[,i])
  model.name <- str_sub(colnames(train_data)[i],start=11L)
  result[nrow(result)+1,] <- c(model.name,train.rmse,train.r,test.rmse,test.r)
}

write.csv(result,file='./rf_rfpred_11model_result_neonht.csv',row.names = F)





#####通过neon_ht验证
neon_ht <- data[['neon_ht']]
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
    
    data.train <- xs[ind!=i,]
    data.test<- xs[ind==i,]
    neon_ht.train <- neon_ht[ind!=i]
    neon_ht.test <- neon_ht[ind==i]
    model.lm<-lm(rf.pred~.,data = data.train)
    pred.train <- predict(model.lm,data.train)
    pred.test <- predict(model.lm,data.test)
    r.train <- cor(neon_ht.train,pred.train)
    r.test <- cor(neon_ht.test,pred.test)
    rmse.train <- rmse(neon_ht.train,pred.train)
    rmse.test <- rmse(neon_ht.test,pred.test)
    df[nrow(df)+1,] <- c(rmse.train,rmse.test,r.train,r.test)
  }
  c(mean(df$rmse.train),mean(df$rmse.test),mean(df$r.train),mean(df$r.test))
}

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
    neon_ht.train <- neon_ht[ind!=i]
    neon_ht.test <- neon_ht[ind==i]
    rf.model = randomForest(x=data.train[sel], y=data.train[[target]], 
                            ntree=ntree, mtry=mtry,importance=TRUE )
    pred.train <- predict(rf.model,data.train[sel])
    pred.test <- predict(rf.model,data.test[sel])
    rmse.train <- rmse(pred.train,neon_ht.train)
    r.train <- cor(pred.train,neon_ht.train)
    rmse.test <- rmse(pred.test,neon_ht.test)
    r.test <- cor(pred.test,neon_ht.test)
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
      tune.rf[nrow(tune.rf)+1,] <- rf_tenfold(i,j,sel=sel.rf,target="rf.pred")
    }
  }
  # check best parameters 
  #tune.rf
  #min_rmse=min(tune.rf$rmse.test);#min_rmse
  #idx=which(tune.rf$rmse.test==min_rmse);#idx 
  max_r <- max(tune.rf$r.test)
  idx=which(tune.rf$r.test==max_r)
  model.rf = randomForest(x=data[sel.rf], y=data[["rf.pred"]], ntree=tune.rf$ntree[idx], mtry=tune.rf$mtry[idx],importance=TRUE )
  save(tune.rf, file=paste0("./tune.rf.",strmodel,".Rdata"))
  save(model.rf,file=paste0("./model.rf.",strmodel,".Rdata"))
  return(tune.rf[idx,])
}



#y=data[['lm.pred']]
xs <- data[c("lm.pred",sel.lm.abby.lm.y)]
result.lm.abby.lm.y <- tenfold(sel.lm.abby.lm.y)
#xxs <- data[c(sel.lm.abby.lm.y)]
tune.rf.lm.abby.y.best <- tune_tenfold(sel.rf.abby.lm.y, 'lm.pred')


xs <- data[c("rf.pred",sel.lm.abby.rf.y)]
result.lm.abby.rf.y <- tenfold(sel.lm.abby.lrf.y)
xxs <- data[c(sel.lm.abby.lm.y)]
tune.rf.rf.abby.y.best <- tune_tenfold(sel.rf.abby.rf.y, 'rf.pred')
tune.rf.rf.abby.y.best <- tune_tenfold(sel.rf.abby.rf.y, 'rf.y')



seed(111)
data_lm_lmpred <- data[c('lm.pred',sel.lm.abby.lm.y)]
data_lm_rfpred <- data[c('rf.pred',sel.lm.abby.rf.y)]
data_rf_lmpred <- data[c('lm.pred',sel.rf.abby.lm.y)]
ind <- sample(2, nrow(data), replace=T,prob = c(0.7,0.3))
data_lm_lmpred_train <- data_lm_lmpred[ind==1,]
data_lm_lmpred_test <- data_lm_lmpred[ind==2,]
data_lm_rfpred_train <- data_lm_rfpred[ind==1,]
data_lm_rfpred_test <- data_lm_rfpred[ind==2,]
data_rf_lmpred_train <- data_rf_lmpred[ind==1,]
data_rf_lmpred_test <- data_rf_lmpred[ind==2,]
write.csv(data_lm_lmpred_train,file='./data_lm_lmpred_train_8000.csv',row.names = F)
write.csv(data_lm_lmpred_test,file='./data_lm_lmpred_test_8000.csv',row.names = F)
write.csv(data_lm_rfpred_train,file='./data_lm_rfpred_train_8000.csv',row.names = F)
write.csv(data_lm_rfpred_test,file='./data_lm_rfpred_test_8000.csv',row.names = F)
write.csv(data_rf_lmpred_train,file='./data_rf_lmpred_train_8000.csv',row.names = F)
write.csv(data_rf_lmpred_test,file='./data_rf_lmpred_test_8000.csv',row.names = F)