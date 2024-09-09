rm(list=ls()); gc();
library(foreign)
library(stringr)
library(base)

rename_dbf_mean <-function(dfname,dname){
  d <- read.dbf(dfname)
  names(d)[115] <- dname
  return(d)
}
###
#path_tiles="E:/project/wuhan/extvalue_landcover/forest/metricsC"
path_tiles="F:/project/wuhan/extvalue_forest/metricsC_v003/tiles"
list_tiles=c(dir(path_tiles))
list_tiles

######test one
path_dbf=paste(path_tiles,list_tiles[1],sep='/')
list_dbf=c(dir(path_dbf,pattern = ".dbf"))
list_dbf
setwd(path_dbf)
ip_name <- str_sub(list_dbf[1],start=1L,end=-5L)
indbf <- rename_dbf_mean(list_dbf[1],ip_name)
mergedata <- data.frame(indbf$beam,indbf$sht_nmb,indbf$snstvty,indbf$slr_lvt,
                        indbf$lt_lwst,indbf$ln_lwst,indbf[12:113])

for (i in 7:length(list_tiles)){
  path_dbf=paste(path_tiles,list_tiles[i],sep='/')
  list_dbf=c(dir(path_dbf,pattern = ".dbf"))
  setwd(path_dbf)
  ip_name <- str_sub(list_dbf[1],start=1L,end=-5L)
  indbf <- rename_dbf_mean(list_dbf[1],ip_name)
  mergedata <- data.frame(indbf$beam,indbf$sht_nmb,indbf$snstvty,indbf$slr_lvt,
                          indbf$lt_lwst,indbf$ln_lwst,indbf[12:113])
  for (j in 2:length(list_dbf)){
    ip_name <- str_sub(list_dbf[j],start=1L,end=-5L)
    indbf <- rename_dbf_mean(list_dbf[j],ip_name)
    
    mergedata <- data.frame(mergedata,indbf[113])
    print(paste0(' done:  #', j,': dbf ',ip_name))
  }
  csvname <- paste(list_tiles[i],".csv",sep='')
  txtname <- paste(list_tiles[i],".txt",sep='')
  mergedata$indbf.sht_nmb <- format(mergedata$indbf.sht_nmb,digit=17)
  write.csv(mergedata,file=csvname,row.names = F)
  write.table(mergedata,file=txtname,row.names = F)
  print(paste0(' done:  #', i,': tile ',list_tiles[i]))
}

path_csv="F:/project/wuhan/extvalue_forest/metricsC_v003/csv_allrh"
setwd(path_csv)
list_csv <- c(dir(path_csv,pattern = '.csv'))
merge_csv <- data.frame()
###test one
csv_data <- read.csv(list_csv[1],head=T)
merge_csv <- rbind(merge_csv,csv_data)

ip_name <- str_sub(list_dbf[1],start=1L,end=-5L)
aspect_dbf <- rename_dbf_mean(list_dbf[1],ip_name)
ip_name <- str_sub(list_dbf[23],start=1L,end=-5L)
dem_dbf <- rename_dbf_mean(list_dbf[23],ip_name)
ip_name <- str_sub(list_dbf[140],start=1L,end=-5L)
slope_dbf <- rename_dbf_mean(list_dbf[140],ip_name)
csv_data <- cbind(csv_data[,1:107],aspect_dbf$aspect,csv_data[,108:128],
                  dem_dbf$dem,csv_data[,129:244],slope_dbf$slope,
                  csv_data[,245:ncol(csv_data)])
colnames(csv_data)[108]<- 'aspect'
colnames(csv_data)[130]<- 'dem'
colnames(csv_data)[247]<- 'slope'
csvname <- paste(list_tiles[i],".csv",sep='')
txtname <- paste(list_tiles[i],".txt",sep='')
write.csv(csv_data,file=csvname,row.names = F)
write.table(csv_data,file=txtname,row.names = F)

###all
for (icsv in 2:length(list_csv)){
  csv_data <- read.csv(list_csv[icsv],head=T)
  merge_csv <- rbind(merge_csv,csv_data)
  print (list_csv[icsv])
}

setwd("E:/program/GEDI_v002_sample")
names(merge_csv)[1] <- 'beam'
names(merge_csv)[2] <- 'sht_nmb'
names(merge_csv)[3] <- 'snstvty'
names(merge_csv)[4] <- 'slr_lvt'
names(merge_csv)[5] <- 'lt_lwst'
names(merge_csv)[6] <- 'ln_lwst'
write.table(merge_csv,file='data_v003_C_allrh.txt',row.names = F)
write.csv(merge_csv,file='data_v003_C_allrh.csv',row.names = F)

path_tiles="F:/project/wuhan/GFC"
list_dbf=c(dir(path_tiles,pattern = '.dbf'))
list_dbf
dbf <- rename_dbf_mean(paste(path_tiles,list_dbf[1],sep='/'),'gfc')
merge_gfc <- data.frame(dbf$sht_nmb,dbf$gfc)
for(i in 2:13){
  dbf <- rename_dbf_mean(paste(path_tiles,list_dbf[i],sep='/'),'gfc')
  gfc <- data.frame(dbf$sht_nmb,dbf$gfc)
  merge_gfc <- rbind(merge_gfc,gfc)
  print (list_dbf[i])
}

names(merge_gfc)[1] <- 'sht_nmb'
merge_gfc$sht_nmb <- format(merge_gfc$sht_nmb,digit=17)
mergedata<-merge(merge_csv,merge_gfc,by="sht_nmb",all.x =T)

write.table(mergedata,file='data_C_allrh.txt',row.names = F)
write.csv(mergedata,file='data_C_allrh.csv',row.names = F)

#######sample_data

library(dplyr)
library(sampling)
data <- na.omit(mergedata)
set.seed(1234)
n <- nrow(data)
a <- sample(n,5000,replace=T)
sample_data <- data[a,]
setwd('E:/program/neon_test/neon_test')
write.table(sample_data,file='sample_data_allrh.txt',row.names = F)
write.csv(sample_data,file='sample_data_allrh.csv',row.names = F)




#####NEON GEDI pre
rm(list=ls()); gc();
library(foreign)
library(stringr)
library(base)

rename_dbf_mean <-function(dfname,dname){
  d <- read.dbf(dfname)
  names(d)[113] <- dname
  return(d)
}
###
#path_tiles="E:/project/wuhan/extvalue_landcover/forest/metricsC"
path_tiles="F:/NEON/ABBY_WREF/metrics"
list_tiles=c(dir(path_tiles))
list_tiles

######test one
path_dbf=paste(path_tiles,list_tiles[1],sep='/')
list_dbf=c(dir(path_dbf,pattern = ".dbf"))
list_dbf
setwd(path_dbf)
ip_name <- str_sub(list_dbf[1],start=1L,end=-5L)
indbf <- rename_dbf_mean(list_dbf[1],ip_name)
mergedata <- data.frame(indbf$beam,indbf$sht_nmb,indbf$snstvty,indbf$slr_lvt,
                        indbf$lt_lwst,indbf$ln_lwst,indbf[12:113])

for (i in 7:length(list_tiles)){
  path_dbf=paste(path_tiles,list_tiles[i],sep='/')
  list_dbf=c(dir(path_dbf,pattern = ".dbf"))
  setwd(path_dbf)
  ip_name <- str_sub(list_dbf[1],start=1L,end=-5L)
  indbf <- rename_dbf_mean(list_dbf[1],ip_name)
  mergedata <- data.frame(indbf$beam,indbf$sht_nmb,indbf$snstvty,indbf$slr_lvt,
                          indbf$lt_lwst,indbf$ln_lwst,indbf[12:113])
  for (j in 2:length(list_dbf)){
    ip_name <- str_sub(list_dbf[j],start=1L,end=-5L)
    indbf <- rename_dbf_mean(list_dbf[j],ip_name)
    
    mergedata <- data.frame(mergedata,indbf[113])
    print(paste0(' done:  #', j,': dbf ',ip_name))
  }
  csvname <- paste(list_tiles[i],".csv",sep='')
  txtname <- paste(list_tiles[i],".txt",sep='')
  mergedata$indbf.sht_nmb <- format(mergedata$indbf.sht_nmb,digit=17)
  write.csv(mergedata,file=csvname,row.names = F)
  write.table(mergedata,file=txtname,row.names = F)
  print(paste0(' done:  #', i,': tile ',list_tiles[i]))
}
colnames(mergedata)[2]<-'sht_nmb'
abby_data <- read.csv(file='E:/program/neon_test/neon_test/neon_data_abby_v1.csv',header = T)
data <- merge(abby_data,mergedata[c(2,108:298)],by='sht_nmb')


s2_path <- 'F:/gee/gee-export'
s2_list <- c(dir(s2_path,pattern = 'WREF'))
s2_max <- read.csv(paste(s2_path,s2_list[1],sep='/'),header = T)
colnames(s2_max)[2:length(colnames(s2_max))]<-paste0(colnames(s2_max)[2:length(colnames(s2_max))],'_max')
s2_mean <- read.csv(paste(s2_path,s2_list[2],sep='/'),header = T)
colnames(s2_mean)[2:length(colnames(s2_mean))]<-paste0(colnames(s2_mean)[2:length(colnames(s2_mean))],'_mean')
s2_min <- read.csv(paste(s2_path,s2_list[3],sep='/'),header = T)
colnames(s2_min)[2:length(colnames(s2_min))]<-paste0(colnames(s2_min)[2:length(colnames(s2_min))],'_min')
s2_sd <- read.csv(paste(s2_path,s2_list[4],sep='/'),header = T)
colnames(s2_sd)[2:length(colnames(s2_sd))]<-paste0(colnames(s2_sd)[2:length(colnames(s2_sd))],'_sd')
mergedata <- data.frame(mergedata,s2_max[2:26],s2_min[2:26],s2_mean[2:26],s2_sd[2:26])
data <- merge(abby_data,mergedata[c(2,108:398)],by='sht_nmb')

colnames(data)
colnames(data)[310:length(colnames(data))]<-paste0('s2',colnames(data)[310:length(colnames(data))])
colnames(data)

getwd()
setwd('E:/program/neon_test/neon_test')
write.csv(data,file = './abby_GEDI_s2l8to.csv',row.names = F)

names(mergedata)[1:6]<-c('beam','sht_nmb','snstvty','slr_lvt','lt_lwst','in_lwst')
write.csv(mergedata,file = './neon_gedi_s2l8torh.csv',row.names = F)
write.table(mergedata,file = './neon_gedi_s2l8torh.txt',row.names = F)