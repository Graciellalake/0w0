https://finance.yahoo.com/screener?.tsrc=fin-srch
library(quantmod)
library(xlsx)
library(ggplot2)
library(ggpubr)

data1 <- read.xlsx(file="E:/COVID-19哈佛数据分析/cases.xlsx",1)
stocknames <- c("AAPL","FB","V","MSFT","GOOGL","JNJ","MPW")

for (i in 1:3) {
  getSymbols(stocknames,
                        auto.assign = TRUE,
                        from = "2020-02-01",
                        to = "2020-06-01")
}
plus <- function(x)
{
  as.data.frame(x)
}

AAPL <- plus(AAPL)
FB <- plus(FB)
GOOGL <- plus(GOOGL)
JNJ <- plus(JNJ)
MSFT <- plus(MSFT)
V <- plus(V)
MPW <- plus(MPW)

condi <- c("newdeaths","newconfirmed")#1为新增死亡病例，2为新增确诊病例
corre <- function(x,y,z,w)
{
  test1 <- cbind.data.frame(x,y)
  plot <- ggplot(data=test1, aes(x=x, y=y))+geom_point(color="red")+stat_cor(data=test1, method = "pearson")+labs(x=condi[w],y=stocknames[z])
  return(plot)
}

corre(data1$newdeaths,AAPL$AAPL.Adjusted,1,1)
corre(data1$newconfirmedcases,AAPL$AAPL.Adjusted,1,2)
#生成散点图

stockprice <- cbind(AAPL$AAPL.Adjusted,FB$FB.Adjusted,GOOGL$GOOGL.Adjusted,JNJ$JNJ.Adjusted,MPW$MPW.Adjusted,MSFT$MSFT.Adjusted,TMDI$TMDI.Adjusted,V$V.Adjusted)
stockprice <- as.matrix(stockprice)
colnames(stockprice) <- c("AAPL","FB","GOOGL","JNJ","MPW","MSFT","TMDI","V")

res <- cor(stockprice)
corrplot(res, method = "shade",shade.col = NA, tl.col ="black", tl.srt = 45, order = "AOE") # 绘制相关系数矩阵图

dist.r<-dist(stockprice,method="euclidean") # 欧氏距离
dist.r
heatmap(as.matrix(dist.r))
