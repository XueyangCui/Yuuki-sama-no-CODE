library(readxl)
library(ggplot2)
windowsFonts(KT=windowsFont('楷体'))

data_1 <- read_excel("C:/Users/240yuuka/Desktop/tissue.xlsx")

tissue_data <- as.data.frame(data_1)  #dataframe数据才能进行画图

ebtop<-function(x){
  
  return(mean(x)+sd(x)/sqrt(length(x)))
  
}   #errorbar计算的函数

ebbottom<-function(x){
  
  return(mean(x)-sd(x)/sqrt(length(x)))
  
}

p1<- ggplot(data=tissue_data,aes(x=identity,y=conc,fill=time))+
  
  
  stat_summary(fun = 'mean', geom = 'bar', aes(width=0.4) #传递bar的大小方法
               )+     #stat_summary()进行参数计算的画图时的快捷方法，后续还需要补充
  
  stat_summary(geom = "errorbar",
               
               fun.min = ebbottom,
               
               fun.max = ebtop,
               
               position = position_dodge(0.9),
               
               width=0.2)+
  
  theme_classic(base_family = 'serif')+
  
  theme(panel.grid = element_blank(),
        plot.title = element_text(hjust=0.5,size = 15),
        axis.title.y = element_text(size= 15, hjust = 0.5, vjust = 1),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 10),
        legend.title = element_text()
        )+
  
  
  scale_fill_manual(values = c("#81B29A","#F2CC8F","#3D405B"),
                    
                    name = 'Dose Time')+      #对fill、color、shape进行分类数据进行手动调整的快捷方法
  
  labs(x='', y = 'Concentration(ng/mL,ng/g)', title = 'Compound#41 Tissue Distribution',)+
  scale_y_continuous( breaks = seq(0,150,25),labels = seq(0, 150, 25),
                      expand = c(0,0))+   #seq()进行数列生成
  coord_cartesian(ylim = c(0, 150 ))
  

(p1)