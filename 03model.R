rm(list = ls());gc()
library(foreign)
library(stringr)
library(base)

path_neon = 'F:/NEON/ABBY/NEON_struct-ecosystem'
neon_ht = read.dbf(file=paste(path_neon,'ABBY_GEDI.dbf',sep='/'))
neon_dem = read.dbf(file=paste(path_neon,'ABBY_dem.dbf',sep='/'))
neon_slope = read.dbf(file=paste(path_neon,'ABBY_slope.dbf',sep='/'))
neon_aspect = read.dbf(file=paste(path_neon,'ABBY_aspect.dbf',sep='/'))
neon_gfc = read.dbf(file=paste(path_neon,'ABBY_GFC19.dbf',sep='/'))
neon_nlcd = read.dbf(file=paste(path_neon,'ABBY_NLCD.dbf',sep='/'))
colnames(neon_ht)[115]<-'neon_ht'
colnames(neon_aspect)[115]<-'aspect'
colnames(neon_dem)[115]<-'dem'
colnames(neon_slope)[115]<-'slope'
colnames(neon_gfc)[115]<-'gfc'
colnames(neon_nlcd)[115]<-'nlcd'
neon_data <- cbind(neon_ht,neon_aspect$aspect,neon_dem$dem,neon_gfc$gfc,
                   neon_nlcd$nlcd,neon_slope$slope)
neon_data[c('layer','path')]<-NULL
colnames(neon_data)[114:118]<-c('aspect','dem','gfc','nlcd','slope')
neon_data$sht_nmb = format(neon_data$sht_nmb,digit=12) 
neon_data[neon_data==-9999]<-NA
neon_data <- neon_data[neon_data$neon_ht !=0,]
neon <- na.omit(neon_data)
write.csv(neon,file = './neon_data_abby_v1.csv',row.names = F)

data <- read.csv(file='E:/program/neon_test/neon_test/neon_data_abby_v1.csv',header = T)
#####建立RH校正模型
data <- neon
colnames(data)

####### lm ####
library(MASS)
###sel metrics
xs <- data[c(12:118)]
fit<-lm(neon_ht~.,data = xs)
step.both <- stepAIC(fit,direction = "both",trace=F)
summary(step.both)
save(step.both,file="./step.both.allrh.Rdata")
sel.lm <- names(step.both$coefficients)
sel.lm
sel.lm.allrh <- sel.lm[2:length(sel.lm)] 
sel.lm.allrh
save(sel.lm.allrh,file="./sel.lm.allrh.Rdata")
sel.lm.allrh.rh95 <- c(sel.lm.allrh,'rh95')
save(sel.lm.allrh.rh95,file="./sel.lm.allrh.rh95.Rdata")
xs <- data[c('neon_ht',sel.lm.allrh)]
model.lm<-lm(neon_ht~.,data = xs)
save(model.lm,file="./model.lm.allrh.step.Rdata")

model.lm<-lm(neon_ht~.,data = xs)
save(model.lm,file="./model.lm.allrh.Rdata")

all.metrics <- colnames(neon_data)[12:118]
all.metrics <- all.metrics[-102]
save(all.metrics,file="./all.metrics.Rdata")

sel.arti <- c('rh25','rh50','rh75','rh95','dem','slope','aspect','nlcd','gfc')
save(sel.arti,file="./sel.arti.Rdata")

sel.lm.allrh.cor <- c(sel.lm.allrh,'lt_lwst','ln_lwst')
save(sel.lm.allrh.cor,file="./sel.lm.allrh.cor.Rdata")
####tenfold
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
    xs <- data[c("neon_ht",sel.lm)]
    data.train <- xs[ind!=i,]
    data.test<- xs[ind==i,]
    model.lm<-lm(neon_ht~.,data = data.train)
    pred.train <- predict(model.lm,data.train)
    pred.test <- predict(model.lm,data.test)
    r.train <- cor(data.train$neon_ht,pred.train)
    r.test <- cor(data.test$neon_ht,pred.test)
    rmse.train <- rmse(data.train$neon_ht,pred.train)
    rmse.test <- rmse(data.test$neon_ht,pred.test)
    df[nrow(df)+1,] <- c(rmse.train,rmse.test,r.train,r.test)
  }
  c(mean(df$rmse.train),mean(df$rmse.test),mean(df$r.train),mean(df$r.test))
}
result.lm.allrh <- tenfold(sel.lm.allrh)
save(result.lm.allrh,file="./result.lm.allrh.Rdata")
result.lm.allrh

result.lm.allrh.rh95 <- tenfold(sel.lm.allrh.rh95)
save(result.lm.allrh.rh95,file="./result.lm.allrh.rh95.Rdata")
result.lm.allrh.rh95

result.lm.all.metrics <- tenfold(all.metrics)
save(result.lm.all.metrics,file="./result.lm.all.metrics.Rdata")
result.lm.all.metrics

result.lm.arti <- tenfold(sel.arti)
save(result.lm.arti,file="./result.lm.arti.Rdata")
result.lm.arti

result.lm.allrh.cor <- tenfold(sel.lm.allrh.cor)
save(result.lm.allrh.cor,file="./result.lm.allrh.cor.Rdata")
result.lm.allrh.cor

####### vsurf ####
library(VSURF)
library(randomForest)
set.seed(1234)
y <- data[['neon_ht']]
xs <- data[c(all.metrics)]
vsurf <- VSURF(xs,y,parallel=TRUE)
summary(vsurf)
save(vsurf,file="./vsurf.allrh.Rdata")
load("./vsurf.allrh.Rdata")
sel.rf.allrh <- names(xs[vsurf$varselect.pred])
save(sel.rf.allrh,file="./sel.rf.allrh.Rdata")
sel.rf.allrh

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
      tune.rf[nrow(tune.rf)+1,] <- rf_tenfold(i,j,sel=sel.rf,target="neon_ht")
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

tune.rf.allrh.best <- tune_tenfold(sel.rf.allrh, 'allrh')
tune.rf.allrh.best

sel.rf.allrh.rh95 <- c(sel.rf.allrh,'rh95')
tune.rf.allrh.rh95.best <- tune_tenfold(sel.rf.allrh.rh95, 'allrh.rh95')
tune.rf.allrh.rh95.best

tune.rf.sel.arti.best <- tune_tenfold(sel.arti, 'arti')
tune.rf.sel.arti.best








id <- rownames(data)
data <- cbind(id,data)

ind <- sample(10, nrow(data), replace=T)
for (i in 1:10){
  data.train <- data[ind!=i,]
  train.data.name <- paste('train_data',i,'.csv',sep='')
  write.csv(data.train,file=train.data.name,row.names = F)
  data.test <- data[ind==i,]
  test.data.name <- paste('test_data',i,'.csv',sep='')
  write.csv(data.test,file=test.data.name,row.names = F)
}


rmse <- function(x,y){
  sqrt(mean((x-y)^2))
}
r2 <- function(x,y,z){
  sum((x-z)^2)/sum((y-z)^2)
}
mre <- function(x,y,z){
  (mean(abs(x-y)))/z
}


rf_tenfold_02 <- function(ntree,mtry,sel,target){
  df <- data.frame(RMSE.train=numeric(), 
                   R2.train=numeric(),
                   r.train=numeric(),
                   MRE.train=numeric(),
                   RMSE.test=numeric(), 
                   R2.test=numeric(),
                   r.test=numeric(),
                   MRE.test=numeric())
  tenfold_train_data <- data.frame(id=numeric(),
                                   neon_ht=numeric(),
                                   rf.pred=numeric())
  tenfold_test_data <- data.frame(id=numeric(),
                                  neon_ht=numeric(),
                                  rf.pred=numeric())
  for (i in 1:10){
    train.data.name <- paste('train_data',i,'.csv',sep='')
    data.train <- read.csv(train.data.name,header = T)
    test.data.name <- paste('test_data',i,'.csv',sep='')
    data.test <- read.csv(test.data.name,header = T)
    rf.model = randomForest(x=data.train[sel], y=data.train[[target]], 
                            ntree=ntree, mtry=mtry,importance=TRUE )
    pred.train <- predict(rf.model,data.train[sel])
    pred.test <- predict(rf.model,data.test[sel])
    RMSE.train <- rmse(pred.train,data.train[[target]])
    R2.train <- r2(pred.train,data.train[[target]],mean(data.train[[target]]))
    r.train <- cor(pred.train,data.train[[target]])
    MRE.train <- mre(pred.train,data.train[[target]],mean(data.train[[target]]))
    RMSE.test <- rmse(pred.test,data.test[[target]])
    R2.test <- r2(pred.test,data.test[[target]],mean(data.test[[target]]))
    r.test <- cor(pred.test,data.test[[target]])
    MRE.test <- mre(pred.test,data.test[[target]],mean(data.test[[target]]))
    df[nrow(df)+1,] <- c(RMSE.train,R2.train,r.train,MRE.train,
                         RMSE.test,R2.test,r.test,MRE.test)
    pred_data.train <- cbind.data.frame(data.train$id,data.train$neon_ht,pred.train)
    colnames(pred_data.train)<-c('id','neon_ht','rf.pred')
    tenfold_train_data<-rbind(tenfold_train_data,pred_data.train)
    pred_data.test <- cbind.data.frame(data.test$id,data.test$neon_ht,pred.test)
    colnames(pred_data.test)<-c('id','neon_ht','rf.pred')
    tenfold_test_data<-rbind(tenfold_test_data,pred_data.test)
  }
  write.csv(tenfold_train_data,file=paste('tenfold_rf_train_data.csv',sep=''),row.names = F)
  write.csv(tenfold_test_data,file=paste('tenfold_rf_test_data.csv',sep=''),row.names = F)
  return(df)
}

reult_rh95 <- rf_tenfold_02(500,19,sel.rf.allrh,'neon_ht')
output_name <- 'rf.abby.calib_10fold.csv'
write.csv(reult_rh95,file=output_name,row.names = F)

library(tidyverse)
library(RColorBrewer)
library(ggthemes)
tenfold_train_data <- read.csv(file='./tenfold_rf_train_data.csv',header = T)
p_data <- data.frame(as.double(tenfold_train_data$rf.pred), as.double(tenfold_train_data$neon_ht))
colnames(p_data)<- c('x','y')
p_RMSE=rmse(p_data$x,p_data$y)
p_nS=dim(p_data)[1]
p_r <- cor(p_data$x, p_data$y)
x_max=50; y_max=50
y_step=y_max/10
palette<-brewer.pal(11,"Spectral")
plot2 <- ggplot(data = p_data,aes(x = x,y = y)) +
  stat_bin_2d(binwidth = c(.3,.3))+
  scale_fill_gradientn(colours = rev(palette),limits=c(0,50), breaks=c(0,10,20,30,40,50),
                       labels=c("0","10",'20','30','40','>50'))+
  #绘制拟合线并设置为红色
  geom_smooth(method = 'lm',se = F,color='red',size=1)+
  #geom_point(alpha=0.5,size=2,show.legend=NA)+
  #绘制对角线
  geom_abline(slope = 1,intercept = 0,color='black',linetype = "dashed",size=1) +
  scale_x_continuous(limits = c(0,50),expand = c(0,0))+
  scale_y_continuous(limits = c(0,50),expand = c(0,0))+
  guides(fill = guide_colorbar(title = "Counts", title.position = "top",title.hjust = .5,ticks = T))+
  theme_bw() + geom_blank()+
  labs(x ='RF_pred ht (m)',y="NEON ht (m)",
       title = "NEON_ABBY ht & RF_pred ht 10fold_train")+
  #subtitle = "scatter R-ggplot2 Exercise",
  #caption = 'Visualization by DataCharm')+
  #theme_classic()+
  theme(text = element_text(family = "Times_New_Roman",face='bold'),
        axis.text = element_text(family = 'Times_New_Roman',size = 12,face = 'bold'),
        #修改刻度线内
        axis.ticks.length=unit(-0.22, "cm"), 
        #加宽图边框
        #plot.background = element.rect(size=.8),
        #去除图例标题
        legend.title = element_blank(),
        #设置刻度label的边距
        legend.position = c(.9,.2),
        axis.text.x = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm")), 
        axis.text.y = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm")))+
  geom_text(x=0.5,y=y_max-y_step*0.3,  label=paste('N = ',p_nS,sep=''),hjust=0,size=5) + # N
  #geom_text(x=0,y=y_max-y_step*2,label=paste('R2 = ',sprintf('%.2f',p_r2),sep=''),hjust=0,size=5) + # R2
  geom_text(x=0.5,y=y_max-y_step*0.9,label=paste('RMSE = ',sprintf('%.2f',p_RMSE),' (m)',sep=''),hjust=0,size=5) + # RMSE
  geom_text(x=0.5,y=y_max-y_step*1.5,label=paste('r = ',sprintf('%.2f',p_r),sep=''),hjust=0,size=5)

plot2


tenfold_test_data <- read.csv(file='./tenfold_rf_test_data.csv',header = T)
p_data <- data.frame(as.double(tenfold_test_data$rf.pred), as.double(tenfold_test_data$neon_ht))
colnames(p_data)<- c('x','y')
p_RMSE=rmse(p_data$x,p_data$y)
p_nS=dim(p_data)[1]
p_r <- cor(p_data$x, p_data$y)
x_max=50; y_max=50
y_step=y_max/10
palette<-brewer.pal(11,"Spectral")
plot2 <- ggplot(data = p_data,aes(x = x,y = y)) +
  stat_bin_2d(binwidth = c(.3,.3))+
  scale_fill_gradientn(colours = rev(palette),limits=c(0,10), breaks=c(0,2,4,6,8,10),
                       labels=c("0","2",'4','6','8','>10'))+
  #绘制拟合线并设置为红色
  geom_smooth(method = 'lm',se = F,color='red',size=1)+
  #geom_point(alpha=0.5,size=2,show.legend=NA)+
  #绘制对角线
  geom_abline(slope = 1,intercept = 0,color='black',linetype = "dashed",size=1) +
  scale_x_continuous(limits = c(0,50),expand = c(0,0))+
  scale_y_continuous(limits = c(0,50),expand = c(0,0))+
  guides(fill = guide_colorbar(title = "Counts", title.position = "top",title.hjust = .5,ticks = T))+
  theme_bw() + geom_blank()+
  labs(x ='RF_pred ht (m)',y="NEON ht (m)",
       title = "NEON_ABBY ht & RF_pred ht 10fold_test")+
  #subtitle = "scatter R-ggplot2 Exercise",
  #caption = 'Visualization by DataCharm')+
  #theme_classic()+
  theme(text = element_text(family = "Times_New_Roman",face='bold'),
        axis.text = element_text(family = 'Times_New_Roman',size = 12,face = 'bold'),
        #修改刻度线内
        axis.ticks.length=unit(-0.22, "cm"), 
        #加宽图边框
        #plot.background = element.rect(size=.8),
        #去除图例标题
        legend.title = element_blank(),
        #设置刻度label的边距
        legend.position = c(.9,.2),
        axis.text.x = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm")), 
        axis.text.y = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm")))+
  geom_text(x=0.5,y=y_max-y_step*0.3,  label=paste('N = ',p_nS,sep=''),hjust=0,size=5) + # N
  #geom_text(x=0,y=y_max-y_step*2,label=paste('R2 = ',sprintf('%.2f',p_r2),sep=''),hjust=0,size=5) + # R2
  geom_text(x=0.5,y=y_max-y_step*0.9,label=paste('RMSE = ',sprintf('%.2f',p_RMSE),' (m)',sep=''),hjust=0,size=5) + # RMSE
  geom_text(x=0.5,y=y_max-y_step*1.5,label=paste('r = ',sprintf('%.2f',p_r),sep=''),hjust=0,size=5)

plot2



#######lm 10fold
lm_tenfold_02 <- function(sel,target){
  df <- data.frame(RMSE.train=numeric(), 
                   R2.train=numeric(),
                   r.train=numeric(),
                   MRE.train=numeric(),
                   RMSE.test=numeric(), 
                   R2.test=numeric(),
                   r.test=numeric(),
                   MRE.test=numeric())
  tenfold_train_data <- data.frame(neon_ht=numeric(),
                                   lm.pred=numeric())
  tenfold_test_data <- data.frame(neon_ht=numeric(),
                                  lm.pred=numeric())
  for (i in 1:10){
    train.data.name <- paste('train_data',i,'.csv',sep='')
    data.train <- read.csv(train.data.name,header = T)
    data.train <- data.train[c("neon_ht",sel.lm.allrh)]
    test.data.name <- paste('test_data',i,'.csv',sep='')
    data.test <- read.csv(test.data.name,header = T)
    data.test <- data.test[c("neon_ht",sel.lm.allrh)]
    model.lm<-lm(neon_ht~.,data = data.train)
    pred.train <- predict(model.lm,data.train[sel])
    pred.test <- predict(model.lm,data.test[sel])
    RMSE.train <- rmse(pred.train,data.train[[target]])
    R2.train <- r2(pred.train,data.train[[target]],mean(data.train[[target]]))
    r.train <- cor(pred.train,data.train[[target]])
    MRE.train <- mre(pred.train,data.train[[target]],mean(data.train[[target]]))
    RMSE.test <- rmse(pred.test,data.test[[target]])
    R2.test <- r2(pred.test,data.test[[target]],mean(data.test[[target]]))
    r.test <- cor(pred.test,data.test[[target]])
    MRE.test <- mre(pred.test,data.test[[target]],mean(data.test[[target]]))
    df[nrow(df)+1,] <- c(RMSE.train,R2.train,r.train,MRE.train,
                         RMSE.test,R2.test,r.test,MRE.test)
    pred_data.train <- cbind.data.frame(data.train$neon_ht,pred.train)
    colnames(pred_data.train)<-c('neon_ht','lm.pred')
    tenfold_train_data<-rbind(tenfold_train_data,pred_data.train)
    pred_data.test <- cbind.data.frame(data.test$neon_ht,pred.test)
    colnames(pred_data.test)<-c('neon_ht','lm.pred')
    tenfold_test_data<-rbind(tenfold_test_data,pred_data.test)
  }
  write.csv(tenfold_train_data,file=paste('tenfold_lm_train_data.csv',sep=''),row.names = F)
  write.csv(tenfold_test_data,file=paste('tenfold_lm_test_data.csv',sep=''),row.names = F)
  return(df)
}

result_rh95 <- lm_tenfold_02(sel.lm.allrh,'neon_ht')
output_name <- 'rf.ABBY.calib_10fold.csv'
write.csv(result_rh95,file=output_name,row.names = F)

tenfold_train_data <- read.csv(file='./tenfold_lm_train_data.csv',header = T)
p_data <- data.frame(as.double(tenfold_train_data$lm.pred), as.double(tenfold_train_data$neon_ht))
colnames(p_data)<- c('x','y')
p_RMSE=rmse(p_data$x,p_data$y)
p_nS=dim(p_data)[1]
p_r <- cor(p_data$x, p_data$y)
x_max=50; y_max=50
y_step=y_max/10
palette<-brewer.pal(11,"Spectral")
plot2 <- ggplot(data = p_data,aes(x = x,y = y)) +
  stat_bin_2d(binwidth = c(.3,.3))+
  scale_fill_gradientn(colours = rev(palette),limits=c(0,50), breaks=c(0,10,20,30,40,50),
                       labels=c("0","10",'20','30','40','>50'))+
  #绘制拟合线并设置为红色
  geom_smooth(method = 'lm',se = F,color='red',size=1)+
  #geom_point(alpha=0.5,size=2,show.legend=NA)+
  #绘制对角线
  geom_abline(slope = 1,intercept = 0,color='black',linetype = "dashed",size=1) +
  scale_x_continuous(limits = c(0,50),expand = c(0,0))+
  scale_y_continuous(limits = c(0,50),expand = c(0,0))+
  guides(fill = guide_colorbar(title = "Counts", title.position = "top",title.hjust = .5,ticks = T))+
  theme_bw() + geom_blank()+
  labs(x ='MLR_pred ht (m)',y="NEON ht (m)",
       title = "NEON_ABBY ht & MLR_pred ht 10fold_train")+
  #subtitle = "scatter R-ggplot2 Exercise",
  #caption = 'Visualization by DataCharm')+
  #theme_classic()+
  theme(text = element_text(family = "Times_New_Roman",face='bold'),
        axis.text = element_text(family = 'Times_New_Roman',size = 12,face = 'bold'),
        #修改刻度线内
        axis.ticks.length=unit(-0.22, "cm"), 
        #加宽图边框
        #plot.background = element.rect(size=.8),
        #去除图例标题
        legend.title = element_blank(),
        #设置刻度label的边距
        legend.position = c(.9,.2),
        axis.text.x = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm")), 
        axis.text.y = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm")))+
  geom_text(x=0.5,y=y_max-y_step*0.3,  label=paste('N = ',p_nS,sep=''),hjust=0,size=5) + # N
  #geom_text(x=0,y=y_max-y_step*2,label=paste('R2 = ',sprintf('%.2f',p_r2),sep=''),hjust=0,size=5) + # R2
  geom_text(x=0.5,y=y_max-y_step*0.9,label=paste('RMSE = ',sprintf('%.2f',p_RMSE),' (m)',sep=''),hjust=0,size=5) + # RMSE
  geom_text(x=0.5,y=y_max-y_step*1.5,label=paste('r = ',sprintf('%.2f',p_r),sep=''),hjust=0,size=5)

plot2


tenfold_test_data <- read.csv(file='./tenfold_lm_test_data.csv',header = T)
p_data <- data.frame(as.double(tenfold_test_data$lm.pred), as.double(tenfold_test_data$neon_ht))
colnames(p_data)<- c('x','y')
p_RMSE=rmse(p_data$x,p_data$y)
p_nS=dim(p_data)[1]
p_r <- cor(p_data$x, p_data$y)
x_max=50; y_max=50
y_step=y_max/10
palette<-brewer.pal(11,"Spectral")
plot2 <- ggplot(data = p_data,aes(x = x,y = y)) +
  stat_bin_2d(binwidth = c(.3,.3))+
  scale_fill_gradientn(colours = rev(palette),limits=c(0,10), breaks=c(0,2,4,6,8,10),
                       labels=c("0","2",'4','6','8','>10'))+
  #绘制拟合线并设置为红色
  geom_smooth(method = 'lm',se = F,color='red',size=1)+
  #geom_point(alpha=0.5,size=2,show.legend=NA)+
  #绘制对角线
  geom_abline(slope = 1,intercept = 0,color='black',linetype = "dashed",size=1) +
  scale_x_continuous(limits = c(0,50),expand = c(0,0))+
  scale_y_continuous(limits = c(0,50),expand = c(0,0))+
  guides(fill = guide_colorbar(title = "Counts", title.position = "top",title.hjust = .5,ticks = T))+
  theme_bw() + geom_blank()+
  labs(x ='MLR_pred ht (m)',y="NEON ht (m)",
       title = "NEON_ABBY ht & MLR_pred ht 10fold_test")+
  #subtitle = "scatter R-ggplot2 Exercise",
  #caption = 'Visualization by DataCharm')+
  #theme_classic()+
  theme(text = element_text(family = "Times_New_Roman",face='bold'),
        axis.text = element_text(family = 'Times_New_Roman',size = 12,face = 'bold'),
        #修改刻度线内
        axis.ticks.length=unit(-0.22, "cm"), 
        #加宽图边框
        #plot.background = element.rect(size=.8),
        #去除图例标题
        legend.title = element_blank(),
        #设置刻度label的边距
        legend.position = c(.9,.2),
        axis.text.x = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm")), 
        axis.text.y = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm")))+
  geom_text(x=0.5,y=y_max-y_step*0.3,  label=paste('N = ',p_nS,sep=''),hjust=0,size=5) + # N
  #geom_text(x=0,y=y_max-y_step*2,label=paste('R2 = ',sprintf('%.2f',p_r2),sep=''),hjust=0,size=5) + # R2
  geom_text(x=0.5,y=y_max-y_step*0.9,label=paste('RMSE = ',sprintf('%.2f',p_RMSE),' (m)',sep=''),hjust=0,size=5) + # RMSE
  geom_text(x=0.5,y=y_max-y_step*1.5,label=paste('r = ',sprintf('%.2f',p_r),sep=''),hjust=0,size=5)

plot2
