library(readxl)
library(ggplot2)
PK <- read_excel("C:/Users/240yuuka/Desktop/PK.xlsx") #数据集为不区分identity的数据（同一个体）
PK_DATA <- as.data.frame(PK)
labelx <- data.frame(time= c(0,5, 10, 15, 25)) #专门的标签数据集
labely <- data.frame(time= c(3, 6, 9,12,14))

p1 <- ggplot(PK_DATA, aes(x= time, y = mean))+
  geom_point(size = 4,shape = 17)+
  geom_line(size = 0.6)+     
  
  #显示数值
  geom_errorbar(aes( ymin = mean, ymax = mean+sd),linewidth = 0.6,width = 0.4)+
  
  theme_classic(base_family =  'serif')+
  
  labs(x='Time(h)', y = 'Concentration(ng/mL,ng/g)', title = 'Compound#10--i.p.(20mg/kg)',)+
  
  theme(panel.grid = element_blank(),
        plot.title = element_text(hjust=0.5,size = 15),
        axis.title.y = element_text(size= 15, hjust = 0.5, vjust = 1),
        axis.title.x = element_text(size= 15, hjust = 0.5, vjust = 1),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 10),
        legend.title = element_text()
  )+
  
  scale_x_continuous(limits = c(0,25),breaks = labelx$time, labels=labelx$time
  )+  #强制从0开始，但是会有遮盖效果
  scale_y_log10(breaks = c(1,10,100,1000),labels = c(1,10,100,1000) 
  )+
  coord_cartesian(ylim = c(1, 1000 ))

#添加坐标轴点，可以通过别的数据集优化

p1
