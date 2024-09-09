rm(list = ls());gc()
library(foreign)
library(stringr)
library(base)

abby_ht <- read.csv(file='./neon_data_abby_v1.csv',header = T)
path_neon = 'F:/NEON/ABBY/NEON_struct-ecosystem/ABBY_GEDI.dbf'
abby_ht <- read.dbf(file=path_neon)
abby_ht[abby_ht==-9999]<-NA
abby <- na.omit(abby_ht)
abby.test = filter(abby, abby$RASTERVALU !=0)
abby.test = abby[abby$RASTERVALU !=0,]
data <- read.csv(file='E:/program/neon_test/neon_test/abby_rh95_s2l8to_pred.csv',header = T)
library(ggplot2)

rmse <- function(x,y){
  sqrt(mean((x-y)^2))
}

r2 <- function(x,y){
  cor(x,y)^2
}

#### func_eq_r2(m) - Return R2 equations, cut to 2-digt ####
func_eq_r2 = function(m){
  r2 = sprintf('%.2f',summary(m)$r.squared)  
  eq <- substitute(italic(R)^2 == r2)
  as.character(as.expression(eq)) 
}


p_data <- data.frame(data$rh75, data$neon_ht)
p_data <- data.frame(data$rf.pred,data$neon_ht)
p_data <- data.frame(data$lm.pred,data$neon_ht)
colnames(p_data)<- c('x','y')
p_data <- na.omit(p_data)
x_label <- 'lm.pred'
y_label='NEON_ht'
x_lim=c(0,40)
y_lim=c(0,40)
x_max=40; y_max=40
p_title <- 'NEON_ABBY_ht & lm.pred'
y_step=y_max/10
diff = p_data$y - p_data$x
p_RMSE=round(sqrt(mean(diff^2)),2)
p_nS=dim(p_data)[1]
p2 <- ggplot(aes(y=y,x=x),data=p_data) +
  xlab(x_label) +  ylab(y_label) +
  xlim(x_lim) + ylim(y_lim)	+
  ggtitle(p_title) + 
  theme(text=element_text(size=20))   
p2 <- p2 + geom_point(alpha=0.5,size=2,show.legend=NA) + theme_bw() + geom_blank() 
p2 <- p2 + 
  geom_abline(intercept=0,colour='red',size=1) 
#p_r2 <- r2(p_data$x, p_data$y)
p_r <- cor(p_data$x, p_data$y)

p2 <- p2 +	
  geom_text(x=0,y=y_max-y_step*0.6,  label=paste('N = ',p_nS,sep=''),hjust=0,size=5) + # N
  #geom_text(x=0,y=y_max-y_step*2,label=paste('R2 = ',sprintf('%.2f',p_r2),sep=''),hjust=0,size=5) + # R2
  geom_text(x=0,y=y_max-y_step*1.2,label=paste('RMSE = ',sprintf('%.2f',p_RMSE),sep=''),hjust=0,size=5) + # RMSE
  geom_text(x=0,y=y_max-y_step*1.8,label=paste('r = ',sprintf('%.2f',p_r),sep=''),hjust=0,size=5)
# geom_text(x=x_max*0.75,y=y_max-y_step*4,label=paste('MBE = ',sprintf('%.1f',p_MBE),sep=''),hjust=0,size=5)     # MB  

p2

plot(abby_ht$rh95, abby_ht$RASTERVALU, type="p",xlab='RH95',ylab='NEON_ht',
     xlim=c(0,55),ylim=c(0,55))



#data<- read.csv(file='./harv_chm_gedi_forest.csv',header = T)
p_data <- data.frame(as.double(data$rh75), as.double(data$neon_ht))
colnames(p_data)<- c('x','y')
p_data <- na.omit(p_data)
x_label <- 'RH95'
y_label='NEON_ht'
x_lim=c(0,50)
y_lim=c(0,50)
x_max=40; y_max=40
p_title <- 'NEON_HARV_ht & RH95'
y_step=y_max/10
diff = p_data$y - p_data$x
p_RMSE=round(sqrt(mean(diff^2)),2)
p_nS=dim(p_data)[1]
p2 <- ggplot(aes(y=y,x=x),data=p_data) +
  xlab(x_label) +  ylab(y_label) +
  xlim(x_lim) + ylim(y_lim)	+
  ggtitle(p_title) + 
  theme(text=element_text(size=20))   
p2 <- p2 + geom_point(alpha=0.5,size=2,show.legend=NA) + theme_bw() + geom_blank() 
p2 <- p2 + 
  geom_abline(intercept=0,colour='red',size=1) 
#p_r2 <- r2(p_data$x, p_data$y)
p_r <- cor(p_data$x, p_data$y)

p2 <- p2 +	
  geom_text(x=0,y=y_max-y_step*0.6,  label=paste('N = ',p_nS,sep=''),hjust=0,size=5) + # N
  #geom_text(x=0,y=y_max-y_step*2,label=paste('R2 = ',sprintf('%.2f',p_r2),sep=''),hjust=0,size=5) + # R2
  geom_text(x=0,y=y_max-y_step*1.2,label=paste('RMSE = ',sprintf('%.2f',p_RMSE),' (m)',sep=''),hjust=0,size=5) + # RMSE
  geom_text(x=0,y=y_max-y_step*1.8,label=paste('r = ',sprintf('%.2f',p_r),sep=''),hjust=0,size=5)
# geom_text(x=x_max*0.75,y=y_max-y_step*4,label=paste('MBE = ',sprintf('%.1f',p_MBE),sep=''),hjust=0,size=5)     # MB  

p2




library(tidyverse)
library(RColorBrewer)
library(ggthemes)
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
  scale_x_continuous(limits = c(0,40),expand = c(0,0))+
  scale_y_continuous(limits = c(0,40),expand = c(0,0))+
  guides(fill = guide_colorbar(title = "Counts", title.position = "top",title.hjust = .5,ticks = T))+
  theme_bw() + geom_blank()+
  labs(x ='GEDI RH75 (m)',y="NEON ht (m)",
       title = "NEON_ABBY_ht & RH75")+
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

