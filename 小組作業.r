###data resource:
###公投:    https://data.gov.tw/dataset/95883
###學歷:    https://data.moi.gov.tw/MoiOD/Data/DataDetail.aspx?oid=4E7FFDCC-17EC-4E5C-9DD7-780C2890AF6B
###人口組成:https://data.moi.gov.tw/MoiOD/Data/DataDetail.aspx?oid=2C7688CB-B505-4D00-B11C-66C4D31B024F


{
library("readr", lib.loc="~/R/win-library/3.5")
library("tidyr", lib.loc="~/R/win-library/3.5")
library("dplyr", lib.loc="~/R/win-library/3.5")
library('xts')
library('zoo')
library('graphics')
library('ppcor')
library('PerformanceAnalytics')
library('car')
library(psych)
library(leaps)
}

###1.資料前處理

##Y:同意率 -------------------190 agree
##X: 
##1.老年人/非老年人 ------------193 oyratio
##2.性別比----------------------196 fmratio
##3.六都------------------------197 bigcity
##4.大學生比例(182+185+186+187)-199 uratio
##5.結婚率----------------------200 married
##6.收入中位數------------------189 V6
##7.老年人/總人數---------------205 Oall
##8.年情人/總人數---------------204 Yall  
##9.投票率----------------------203 voterate
##10.年情人/老年人---------------202 Yoratio



salary=read.csv(file('C:\\Users\\Hardy\\Desktop\\linearmodel\\salary_mid.csv',encoding = 'utf-8'))
.dta=read.csv(file('C:\\Users\\Hardy\\Desktop\\linearmodel\\rowdata.csv',encoding = 'utf-8'))
.firstdata=.dta
##6
{.firstdata=left_join(.firstdata, salary[,c(4,5)], by = c("site_id"))
##1 2 
.firstdata$agree=.firstdata$同意票數/.firstdata$有效票數
.firstdata$young=rowSums(.firstdata[,12:105])
.firstdata$old=rowSums(.firstdata[,106:177])
.firstdata$oyratio=.firstdata$old/.firstdata$young
.firstdata$female=rowSums(.firstdata[,seq(13,177,2)])
.firstdata$male=rowSums(.firstdata[,seq(12,176,2)])
.firstdata$fmratio=.firstdata$female/.firstdata$male
##  may be problem 
##婚姻統計人數與年齡統計相符   但與學歷統計不符

## 3 4 5
.firstdata$bigcity=ifelse(.firstdata$縣市=="桃園市"|.firstdata$縣市=="臺中市"|.firstdata$縣市=="臺北市"|.firstdata$縣市=="新北市"|.firstdata$縣市=="臺南市"|.firstdata$縣市=="高雄市",1,0)  
.firstdata$undergraduate=rowSums(.firstdata[,c(182,185,186,187)])
.firstdata$uratio=.firstdata$undergraduate/rowSums(.firstdata[,182:188])
.firstdata$married=1-(.firstdata$未婚/rowSums(.firstdata[178:181]))

##10
## 18~30 的人 年齡分類[可討論]  

.firstdata$Young=rowSums(.firstdata[,12:37])
.firstdata$Yoratio=.firstdata$Young/.firstdata$old
## 9
.firstdata$voterate=.firstdata$投票人數/.firstdata$投票權人數

##7 8
.firstdata$Yall=.firstdata$Young/rowSums(.firstdata[,12:177])
.firstdata$Oall=.firstdata$old/rowSums(.firstdata[,12:177])
##11   40~65
.firstdata$Midall=rowSums(.firstdata[,58:107])/rowSums(.firstdata[,12:177])
##Data=firstdata[,c(189,190,196,197,199,200,203,204,205)]
##12   
.firstdata$YMratio=.firstdata$Yall/.firstdata$Midall
}
##Y:同意率 -------------------190 agree
##X: 
##1.老年人/非老年人 ------------193 oyratio
##2.性別比----------------------196 fmratio
##3.六都------------------------197 bigcity
##4.大學生比例(182+185+186+187)-199 uratio
##5.結婚率----------------------200 married
##6.收入中位數------------------189 V6
##7.老年人/總人數---------------205 Oall
##8.年情人/總人數---------------204 Yall  
##9.投票率----------------------203 voterate
##10.年情人/老年人--------------202 Yoratio
##11.中年人/總人數--------------206 midall
##12.年情人/中年人--------------207 YMratio
###目前決定用 2  3 4 5 6  9 10 預測
Data=.firstdata[,c(189,190,196,197,199,200,203,202)]



###2.模型建立 

##模型標記
##model1st: 全部變數做回歸
##model2nd: 標準化後計算回歸
##model3rd: 找尋交互作用 

##2.1 
model1st=lm((agree)~.,Data)##R sq(adjusted):0.47
summary(model1st)
Anova(model1st,type = "2")


##check of residual
K=model1st$residuals
plot(model1st$fitted.values,model1st$residuals)
qqnorm(model1st$residuals)
qqline(model1st$residuals)
##2.2 
##may need adjust
##find outlier
## outlier split out  還沒教到 不確定要怎麼做
Data=Data[-4,]


### V6不顯著 not sure if we need to keep  

###check again ,better now

{
  model1st=lm((agree)~.,Data)
  summary(model1st)
  K=model1st$residuals
  plot(model1st$fitted.values,model1st$residuals)
  qqnorm(model1st$residuals)
  qqline(model1st$residuals)
  cor(K,qnorm(ppoints(length(K)))[order(order(K))])
}



##2.3 Box Cox  (may not need)   可討論
{
{
SSE=as.vector(mode = "numeric",length(100))


for(i in c(-10:40)){
  
  lamba=i/10
  K2=geometric.mean(Data$agree)
  K1=1/(lamba*K2^(lamba-1))
  if(lamba==0){ W=K2*(log(Data$agree))}
  else  {W=K1*((Data$agree^lamba)-1)}
  
  model1=lm(W~Data$V6+Data$Yoratio+Data$fmratio+Data$bigcity+Data$uratio+Data$married,Data)
  Error=sum(model1$residuals^2)
  SSE[i+11]=c(Error)
  print(Error)
}
 
plot(seq(-1,4,0.1),SSE,xlab = "lamba")

}


##lambda adjusted  (lambda=3.7)   how to explain?
{
  model1st=lm((agree)^3.7~.,Data)##R sq(adjusted):
  summary(model1st)
  Anova(model1st,type = "2" )
}

##check again
{
  model1st=lm((agree)^3.7~.,Data)
  K=model1st$residuals
  plot(model1st$fitted.values,model1st$residuals)
  qqnorm(model1st$residuals)
  qqline(model1st$residuals)
  
}

}
##2.4 standardized    (may not need)   可討論
{
D1=scale(Data)
D1=data.frame(D1)

model2nd=lm(agree~.,D1)##R sq(adjusted):
summary(model2nd)

Anova(model2nd,type = "2" )
##chart.Correlation(D1)
##pcor(D1)
}
##2.5
##Display

chart.Correlation(Data)

## partial correlation
pcor(Data)


##2.6

## find better model
{
  
  best <- function(model, ...) {
    subsets <- regsubsets(formula(model), model.frame(model), ...)
    subsets <- with(summary(subsets),
                    cbind(p = as.numeric(rownames(which)), which, rss, rsq, adjr2, cp, bic))
    
    return(subsets)
  }  
  
  tab=round(best(lm(agree~.,Data), nbest = 4), 4)
}
## decide to split out V6

Data=Data[,-1]
model1st=lm((agree)~.,Data)
summary(model1st)



##2.7
##seek for interaction

plot(Data$married,Data$agree)
DBcity=Data[Data$bigcity==1,]
points(x=DBcity$Yall,y=DBcity$agree,pch=16,col="red")

plot(Data$voterate*Data$married,model1st$residuals)
 




##所有交互作用的可能性
model3rd=lm(agree~.+.*.,Data)
summary(model3rd)
Anova(model3rd,type=2)
##發現男女比在Anova 顯著  但多元回歸不顯著
##去除男女比看看
Data_fmout=Data[,-2]
model1st=lm((agree)~.,Data_fmout)
summary(model1st)
Anova(model1st)
##結果好像不好  ,決定保留


##投票率 六都 交互作用 好像不錯  
model3rd=lm(agree~.+voterate*.,Data)
##model3rd=lm(agree~.+bigcity*.,Data) 六都不怎麼樣
summary(model3rd)

Anova(model3rd,type=2)
##男女比在加入交互作用後不顯著
##try split out fmratio 
model3rd=lm(agree~.+voterate*.,Data_fmout)
summary(model3rd)

Anova(model3rd,type=2)
##maybe need to think twice
##u*f   Y*b b*m
model3rd=lm(agree~.+voterate*bigcity+voterate*uratio+voterate*married+married*bigcity,Data)
summary(model3rd)

Anova(model3rd,type=2)
anova(model3rd)
K=model3rd$residuals
plot(model3rd$fitted.values,K)
qqnorm(K)
qqline(K)
cor(K,qnorm(ppoints(length(K)))[order(order(K))])

## check Residual



pairs(Data)



######目前不知為何uratio Yoratio 係數為負   ??
