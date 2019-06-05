### problem not yet done  3,4, cross validation
{

###data resource:
###公投:    https://data.gov.tw/dataset/95883
###學歷:    https://data.moi.gov.tw/MoiOD/Data/DataDetail.aspx?oid=4E7FFDCC-17EC-4E5C-9DD7-780C2890AF6B
###人口組成:https://data.moi.gov.tw/MoiOD/Data/DataDetail.aspx?oid=2C7688CB-B505-4D00-B11C-66C4D31B024F
  ##Y:同意率 -------------------190 agree
  ##X: 
  ##1.老年人/非老年人 ------------193 oyratio
  ##2.性別比----------------------196 mfratio
  ##3.六都------------------------197 bigcity
  ##4.大學生比例(182+185+186+187)-199 uratio
  ##5.結婚率----------------------200 married
  ##6.收入中位數------------------189 V6
  ##7.老年人/總人數---------------205 Oall
  ##8.年情人/總人數---------------204 Yall  
  ##9.投票率----------------------203 voterate
  ##10.年情人/老年人--------------202 Yoratio
  ##11.中年人/總人數--------------206 midall
  ##12.年情人/中年人--------------207 YMratio  try
  ##13.離島-----------------------208 island
  ##14.年齡中位數-----------------209 medage
  
{
  library("readr")
  library("tidyr")
  library("dplyr")
  library('xts')
  library('zoo')
  library('graphics')
  library('ppcor')
  library('PerformanceAnalytics')
  library('car')
  library(psych)
  library(leaps)
  library(olsrr)
}

##模型標記
##model1st: 全部變數做回歸
##model2nd: 標準化後計算回歸
##model3rd: 找尋交互作用 
##model4th: weighted model

### Problem set

###1.Preprocessing &	First model&standardized first model ,plot of data partial correlation  ----------------------
{
  #1.1data process
  {
  .salary=read.csv(file('C:\\Users\\Hardy\\Desktop\\linearmodel\\salary_mid.csv',encoding = 'utf-8'))
  .dta=read.csv(file('C:\\Users\\Hardy\\Desktop\\linearmodel\\rowdata.csv',encoding = 'utf-8'))
  .firstdata=.dta
  ## exclude island
  #.firstdata=filter(.firstdata,縣市!="連江縣")
  #.firstdata=filter(.firstdata,縣市!="金門縣")
  #.firstdata=filter(.firstdata,縣市!="澎湖縣")
  
  ##6
{.firstdata=left_join(.firstdata, .salary[,c(4,5)], by = c("site_id"))
    ##1 2 
    .firstdata$agree=.firstdata$同意票數/.firstdata$有效票數
    .firstdata$young=rowSums(.firstdata[,12:105])
    .firstdata$old=rowSums(.firstdata[,106:177])
    .firstdata$oyratio=.firstdata$old/.firstdata$young
    .firstdata$female=rowSums(.firstdata[,seq(13,177,2)])
    .firstdata$male=rowSums(.firstdata[,seq(12,176,2)])
    .firstdata$mfratio=.firstdata$male/.firstdata$female
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
    ##13  離島
    .firstdata$island=ifelse(.firstdata$縣市=="連江縣"|.firstdata$縣市=="金門縣"|.firstdata$縣市=="澎湖縣",1,0)
    ##14
    Medi=data.frame()
      for(j in 1:368 ){
      series=c(0)
      
     for (i in 18:100){
      series=c(series,rep(i,.firstdata[[j,2*i-24]]+firstdata[[j,2*i-23]]))
      
      
     }
      Med=median(series[-1])
      Medi[j,"medage"]=Med
      
      }
    .firstdata=cbind(.firstdata,"medage"=Medi)
    
    
  
  }
  
  ##Y:同意率 -------------------190 agree
  ##X: 
  ##1.老年人/非老年人 ------------193 oyratio
  ##2.性別比----------------------196 mfratio
  ##3.六都------------------------197 bigcity
  ##4.大學生比例(182+185+186+187)-199 uratio
  ##5.結婚率----------------------200 married
  ##6.收入中位數------------------189 V6
  ##7.老年人/總人數---------------205 Oall
  ##8.年情人/總人數---------------204 Yall  
  ##9.投票率----------------------203 voterate
  ##10.年情人/老年人--------------202 Yoratio
  ##11.中年人/總人數--------------206 midall
  ##12.年情人/中年人--------------207 YMratio  try
  ##13.離島-----------------------208 island
  ##14.年齡中位數-----------------209 medage
  ###目前決定用 2  3 4 5 6  9 10 預測
  
  
  ##Data=.firstdata[,c(190,189,193,196,197,199,200,202,203,204,205,206,207,208)]
  OriginalData=.firstdata[,c(190,196,197,199,200,203,204,206,209)]
  
  ##分組
  {
    #set.seed(101) # Set Seed so that same sample can be reproduced in future also
    
    # Now Selecting 50% of data as sample from total 'n' rows of the data
    #sample <- sample.int(n = nrow(OriginalData), size = floor(.50*nrow(OriginalData)), replace = F)
    #Data <- OriginalData[sample, ]
    #TestData <- OriginalData[-sample, ]
    Data=OriginalData
  }
  
}
  #1.2first  model plot pcor
  {
   pairs(Data)
  chart.Correlation(Data)
  pcor(Data)
  
  
  model1st=lm((agree)~.,Data)
  summary(model1st)
  Anova(model1st,type = "2" )
  }
  
  #1.3standard model plot pcor
  {
    D1=scale(Data)
    D1=data.frame(D1)
    Mean=colSums(Data)/dim(Data)[1]
    Sd=sqrt(diag(var(Data)))
    model2nd=lm(agree~.,D1)##R sq(adjusted):
    summary(model2nd)
    
    Anova(model2nd,type = "2" )
    pcor(D1)
    
    
    chart.Correlation(D1)
    ##pcor(D1)
  }
  
}

###2.	Residual plot,QQplot to check assumption------------
{
  
  #2.1Residual plot & Q-Qplot 
  {
    K=model1st$residuals
    plot(model1st$fitted.values,model1st$residuals)
    qqnorm(model1st$residuals)
    qqline(model1st$residuals)
    cor(K,qnorm(ppoints(length(K)))[order(order(K))])
  }
  #2.2plot of residual vs Xvariable
  {
    
  }
  #2.3 Box Cox  (may not need)   可討論
  {
    {
      SSE=as.vector(mode = "numeric",length(100))
      
      
      for(i in c(-10:40)){
        
        lamba=i/10
        K2=geometric.mean(Data$agree)
        K1=1/(lamba*K2^(lamba-1))
        if(lamba==0){ W=K2*(log(Data$agree))}
        else  {W=K1*((Data$agree^lamba)-1)}
        
        model1=lm(W~Data$voterate+Data$Yoratio+Data$mfratio+Data$bigcity+Data$uratio+Data$married,Data)
        Error=sum(model1$residuals^2)
        SSE[i+11]=c(Error)
        print(Error)
      }
      
      plot(seq(-1,4,0.1),SSE,xlab = "lamba")
      
    }
    
    
    ##lambda adjusted  (lambda=3.7)   how to explain?
    {
      model1st=lm((agree)^2~.,Data)##R sq(adjusted):
      summary(model1st)
      Anova(model1st,type = "2" )
    }
    
    ##check again
    {
      model1st=lm((agree)^2~.,Data)
      K=model1st$residuals
      plot(model1st$fitted.values,model1st$residuals)
      qqnorm(model1st$residuals)
      qqline(model1st$residuals)
      cor(K,qnorm(ppoints(length(K)))[order(order(K))])
      
    }
    
  }


}
###3.	Add variable plot to find interaction term----------
{
  ## add variable plot  高次項
  { 
    par(mfcol = c(3,3))
    for(i in 1:8){
      
      .Data=Data[,-(i+1)]
      modelY=lm(agree~.,.Data)
      .X=dplyr:: select(Data,-agree)
      
      modelX=lm(.X[,names(.X)[i]]~.,.X[,-i])
      
      plot(modelX$residuals,modelY$residuals,xlab = names(Data)[i+1],ylab = "e(Y|other variable)",main = "add variable plot")
    }
    
  }
  #判斷不需要高次項
  ##interaction -residual plot
  {
  
  }
  
 
  ##所有交互作用的可能性
  model3rd=lm(agree~.+.*.,D1)
  summary(model3rd)
  Anova(model3rd,type=2)
  ##投票率 六都 交互作用 好像不錯  

  
  
  model3rd=lm(agree~.+mfratio*uratio,D1)
  summary(model3rd)
  Anova(model3rd,type=2)
  
  
}

###4.	Best subsets algorithm to check interaction -------
###term and higher order term
{
  #Best subsets
  {
    
   best <- function(model,nbest,nvmax...) {
      subsets <- regsubsets(formula(model), model.frame(model),nbest=nbest,nvmax=nvmax...)
      subsets <- with(summary(subsets),
                      cbind(p = as.numeric(rownames(which)), which, rss, rsq, adjr2, cp, bic))
      
      return(subsets)
    }  
    # interaction term
   {
     Interact <- function(model,nbest,nvmax,force.in...){
       subsets <- regsubsets(formula(model), model.frame(model),nbest=nbest,nvmax=nvmax,force.in=force.in...)
       subsets <- with(summary(subsets),
                       cbind(p = as.numeric(rownames(which)), which, rss, rsq, adjr2, cp, bic))
       
       return(subsets)
     }
   }
    Tab=round(best(lm(agree~.,Data), nbest = 2,nvmax=29), 4)

    InterTab=data.frame(round(Interact(lm(agree~.+.*.,Data), nbest = 2,nvmax=29,force.in=names(Data)[-1]), 4))
    Cprank=order(order(InterTab$cp))
    Bicrank=order(order(InterTab$bic))
    adjr2rank=order(order(InterTab$adjr2))
    
  }
}

###5.	Find the candidates of model------------------------
{
  
  
  K=model3rd$residuals
  plot(model3rd$fitted.values,K)
  qqnorm(K)
  qqline(K)
  cor(K,qnorm(ppoints(length(K)))[order(order(K))])
}


###6.	Check X Y outlier standardized delete residual leverage------
{
  #6.1 check X outlier
  {
    Xmat=Data[,-1]
  X=dplyr:: select(mutate("intercept"=rep(1,dim(Data)[1]),Xmat),intercept,everything())
  
  X=as.matrix(X)
  Hat_matrix=X%*%solve(t(X)%*%X)%*%t(X)
  Leverage=diag(Hat_matrix)
  plot(Leverage,main = "Leverage plot")
  abline(h=2*sum(Leverage)/length(Leverage),col="red")##  2p/n
  }
  #6.2 check Y outlier
  {
    SSE=sum(model3rd$residuals^2)
    t=model3rd$residuals*sqrt((dim(Data)[1]-length(model3rd$coefficients)-1)/(SSE*(1-Leverage)-model3rd$residuals^2))
    ## bonferroni two tail t-test alpha=0.1
    criterion=qt(1-0.05/dim(Data)[1],dim(Data)[1]-length(model3rd$coefficients)-1)
    plot(abs(t),ylim = c(0,4),main = "studentized deleted residual plot")
    abline(h=criterion,col="red")
    #sort(abs(t))
  }
}

###7.influential index	COOK distance DFFITS DFBETAS------------------------------
{
  #7.1DDFITS
  {#criterion 
    DDFTIS=t*sqrt(Leverage/(1-Leverage))
    plot(DDFTIS,main="DDFITS plot")
    
    Dcriterion=2*sqrt(length(model3rd$coefficients)/dim(Data)[1])#=2*sqrt(p/n)
    lines(DDFTIS)
    abline(h=Dcriterion,col="red")
  }
  #7.2COOK
  {#criterion 
    COOKD=((model3rd$residuals)^2/(4*(SSE/(dim(Data)[1]-length(model3rd$coefficients)))))*Leverage/((1-Leverage)^2)
    plot(COOKD,main="Cook's distance plot",ylab = "D")
    lines(COOKD)
    Ccriterion=qf(0.5,length(model3rd$coefficients),(dim(Data)[1]-length(model3rd$coefficients)))
    abline(h=Ccriterion,col="red")
  }
  #7.3DFBETAS
  {#criterion 
    ols_plot_dfbetas(model3rd)
    BETA=dfbetas(model3rd)
    plot(model3rd$fitted.values,model3rd$residuals)
  }
}
##三個指標都顯示outlier 有一定影響力
###8.	Multicollinearity   VIF----------------------------------
{
  #criterion 10
  vif(model3rd)
  ##原本以為共線性高，但發現標準化後就沒有
  
  ## indicate the multicolinearity may distort the regression result in uratio and interaction term
  #mfratio uratio mfratio*uratio
  }

  ##conclusion of 6,7,8:  


###9.	If unequal weight need?- if need implement ------------------
{
  ## 沒有明顯的變異數不同質的問題，故不需要。
}

##
##變異數不同質問題應該不嚴重，故不需要
###10. if ridge regression need?(depend on VIF)- if need implement ----------
{
  D1X=dplyr:: select(D1,-agree)
  rxx=cor(D1X)
  ryx=cor(D1)[-1,1]
  bR=data.frame()
  Vif=data.frame()
  Rsquare=data.frame()
  
  for (i in 0:100){
    c=i*0.005
    br=solve(rxx+c*as.matrix(diag(dim(Xmat)[2])))%*%ryx
    br=t(br)
    bR=rbind(bR,br)
    bias = solve(rxx + c*diag(ncol(rxx)))
    vif=t(diag(bias %*% rxx %*% bias)) 
    Vif=rbind(Vif,vif)
    ##不知道為什麼算不出來Rsquare 
   ## rsq=1-(sum((D1[,1]-rowSums(br*D1X))^2)/(dim(D1)[1]-1))
  #  rsq=1-(sum((PSS[,1]-rowSums(br*PSS[,c(2,3,4)]))^2)/(46-1))
   ## Rsquare=rbind(Rsquare,rsq)
  }
  c=seq(0,0.5,0.005)
  bR=cbind(c,bR,Vif)
  
  plot(bR[,length(model1st$coefficients)+1]~c,bR,type = "l",ylim=c(-1,5),ylab = "bR",main="ridge trace plot",lty=5,col="red")
  lines(bR[,length(model1st$coefficients)+2]~c,bR,lty=5,col="orange")
  lines(bR[,length(model1st$coefficients)+3]~c,bR,lty=5,col="yellow")
  lines(bR[,length(model1st$coefficients)+4]~c,bR,lty=5,col="green")
  lines(bR[,length(model1st$coefficients)+5]~c,bR,lty=5,col="blue")
  lines(bR[,length(model1st$coefficients)+6]~c,bR,lty=5,col="purple")
  legend(x = 1, y = 0,lty=5,col= c("red","orange","yellow","green","blue","purple"),legend = names(D1)[-1])
}

##標準化後的VIF不大應該不需要
##標準化後問題不嚴重，應該不需要
###11.	If robust regression need or remove the outlier-------------
###(depend on DFFITS DFBETA COOK) ?- if need implement 
{

  
  IRLS <- function(model, i = 1) 
  {
    e   <- resid(model)
    MAD <- median(abs(e - median(e))) / 0.6745  # (11.46)
    u   <- e / MAD  # (11.47)
    w   <- apply(data.frame(u), 1, function(x) if (abs(x) <= 1.345) 1 else 1.345/abs(x))  # (11.44)
    
    model <- update(model, weights = w)
    if (i > 1) return(IRLS(model, i-1)) else return(model)  # Recursive return definition
  }
  
  tab <- cbind(
    "e0" = resid(model3rd),
    "u0" = resid(model3rd) / (median(abs(resid(model3rd) - median(resid(model3rd)))) / 0.6745),
    "w1" = IRLS(model3rd, 1)$weights,
    "e1" = resid( IRLS(model3rd, 1) ),
    "w2" = IRLS(model3rd, 2)$weights,
    "e2" = resid( IRLS(model3rd, 2) ),
    "w7" = IRLS(model3rd, 7)$weights,
    "e7" = resid( IRLS(model3rd, 7) ))
  
  round(tab, 4)
  plot(tab[,7],main = "robust weight")
  lines(tab[,7])
  
  model4th=lm(agree~.+mfratio*uratio,D1,weights =IRLS(model3rd, 7)$weights)
  summary(model4th)
  Anova(model4th,type=2)
  plot(model4th$fitted.values,model4th$residuals)
  
  qqnorm(model4th$residuals)
  qqline(model4th$residuals,col="red")
  
  ##結果中年人不顯著
  
 # K=model4th$residuals*model4th$weights
  #plot(model4th$fitted.values,K)
  #qqnorm(K)
  #qqline(K)
  #cor(K,qnorm(ppoints(length(K)))[order(order(K))])
  
  
}

## construct CI  fixed X sample
{
  bootstrap <- function(model,coef, times = 1000, alpha = 0.05) 
  {
    b     <- coef(model)[[coef]]
    n     <- nrow(model.frame(model))
    coefs <- vector(mode = "numeric", length = times)
    for (i in seq(times))
    {
      estar <- sample(resid(model), size = n, replace = TRUE)
      fstar <- fitted(model) + estar
      bootmodel  <- lm(fstar ~ .-agree, data = model.frame(model))
      coefs[i] <- coef(bootmodel)[[coef]]
    }
    
    p <- quantile(coefs, probs = c(alpha/2, 1-alpha/2))
    
    statistics <- cbind(
      "mean"      = mean(coefs),
      "sd"        = sd(coefs),
      "b(a/2)"    = p[[1]],
      "b(1-a/2)"  = p[[2]])
    
    confint <- cbind(
      'd1'  = b - p[[1]],
      'd2'  = p[[2]] - b,
      'lwr' = 2*b - p[[2]],
      'upr' = 2*b - p[[1]])
    
    return (list(coefs = coefs, statistics = statistics, confint = confint))
  }
    Stat=vector(mode="numeric",length = 4)
    CI=vector(mode="numeric",length = 4)
   
  par(mfcol = c(3,3))
  for(i in 1:9){
    
      z <- bootstrap(model4th, coef=i)
      hist(z$coefs, breaks = 40, main = "",xlab = names(model4th$coefficients)[i])
      title("bootstrap result")
      Stat=rbind(Stat,z$statistics)
      
      CI=rbind(CI,z$confint)

  }
    Stat=data.frame(row.names =names(model4th$coefficients),Stat[-1,])
    CI=data.frame(row.names =names(model4th$coefficients),CI[-1,] )


}

##應該需要
###12.	Fit candidate---------------------------------------
{
  
}


######目前不知為何uratio Yoratio 係數為負   ??
##write.csv(Data, file = "Data.csv")
  
  
}