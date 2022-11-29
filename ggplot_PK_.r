library(readxl)
library(ggplot2)
PK <- read_excel("C:/Users/240yuuka/Desktop/PK.xlsx")
PK_DATA <- as.data.frame(PK)
labelx <- data.frame(time= c(0,5, 10, 15, 25)) #专门的标签数据集
labely <- data.frame(time= c(3, 6, 9,12,14)) 

p1 <- ggplot(PK_DATA,aes(x= time, y = mean))+
  geom_point(aes(shape=identity),size = 4)+      #
  geom_line(aes(group = identity),size = 0.6)+  # aes()中的group用来分类进行连接，shape用来对不同变量进行指定
  
  
  #显示数值
  geom_errorbar(aes(ymin = mean, ymax = mean+sd),   #errbar增加bar值，如何进行函数计算优化还要探究，不直接输入mean和sd进行输出的方法
                linewidth = 0.6,width = 0.4)+
  
  theme_classic(base_family =  'serif')+
  
  labs(x='Time(h)', y = 'Concentration(ng/mL,ng/g)', title = 'Compound#11',)+
  scale_shape_manual(name= NULL,
                     values = c(15,  18), 
                     labels = c('i.g.', 'i.p.'))+   #scale_shape_manual()   对legend进行设定和修改参数
  
  
  theme(panel.grid = element_blank(),
        plot.title = element_text(hjust=0.5,size = 15),
        axis.title.y = element_text(size= 15, hjust = 0.5, vjust = 1),
        axis.title.x = element_text(size= 15, hjust = 0.5, vjust = 1),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 10),
        legend.position = c(0.9, 0.9),
        legend.text = element_text(size = 11)
  )+
  
  scale_x_continuous(limits = c(0,25),breaks = labelx$time, labels=labelx$time
  )+  #强制从0开始，但是会有遮盖效果
  scale_y_continuous(breaks = seq(0,100,25),labels = seq(0, 100, 25)
  )+
  coord_cartesian(ylim = c(0, 100 ))
 


#添加坐标轴点，可以通过别的数据集优化

p1