data("ToothGrowth")
ToothGrowth$dose <- factor(ToothGrowth$dose, level = c(0.5, 1, 2), labels = c('D0.5', 
                                                                              'D1',
                                                                              'D2'))   
                                                                              #fator对因子数据进行区分，level和label对数据进行调整

str(ToothGrowth)

library('ggpubr')
ggviolin(ToothGrowth, x = 'dose', y= 'len', color = 'supp',
         add='dotplot', palette=c('red', 'blue'))   #ggpubr包画图的方法，后续需要补充

aov1 <- aov(len~dose*supp,data= ToothGrowth)     #aov()对数据集进行方差分析，
summary(aov1)