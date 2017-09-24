rm(list=ls())
write('', file='.blank')
loadhistory('.blank')
cat('\014')
library(readxl)
#---------------------------------------------1: Find Factors and dominante Levels-------------------------------------
Findfactors=function(data){
  df=data.frame()
  for (i in 1:ncol(data)){
    if  (is.factor(data[,i])==TRUE){
      Name=colnames(data[i])
      sn=table(data[,i]) #find the levels
      s=data.frame(sn)
      Dlevel=s[s[,2]==max(s[,2]),]
      Nlevel=nlevels(data[,i])
      lev=levels(data[,i])
      levkinds=vector()
      for (i in 1:length(lev)){
        levkinds=paste(levkinds,lev[i])
      }
      pre=cbind(Name,Dlevel,Nlevel,levkinds)
      df=rbind(df,pre)
    }
    else{
      #print ('No factor variables found in the data')
    }
  }
return(View(df))
}

#-----------------------------------------------2: Find Outliers----------------------------------------------------------
DetectOutliers=function(data){
  df=data.frame()
  for (i in 1:ncol(data)){
    if  (is.integer(data[,i])==TRUE|is.numeric(data[,i])==TRUE){
      Name=colnames(data[i])
      belquan=quantile(data[,i],probs = 0.25)
      aboquan=quantile(data[,i],probs = 0.75)
      intdata=(aboquan-belquan)*1.5
      upplim=aboquan+intdata
      bellim=belquan-intdata
      subdata=subset(data,data[,i]>upplim|data[,i]<bellim)
      prob=round(nrow(subdata)/nrow(data),4)
      pre=cbind(Name,prob)
      df=rbind(df,pre)
    }
    else{}
  }
  return(View(df))
}  

#-------------------------------------------3:Find the most probable distribution--------------------------------------------
FindBestDist=function(x){
  x=x[!is.na(x)]
  x=x[x>=0]
  if(is.integer(x)==TRUE){
    dist=c('poisson','weibull','geometric')
  }
  else{
    dist=c('normal',"exponential","gamma","lognormal",'logistic')
  }
  df=data.frame()
  library(MASS)
  for ( i in dist ) {
    pars = fitdistr(x, i )
    result=pars$loglik
    s=cbind(i,result)
    df=rbind(df,s)
  }
  return(df[df$result==min(as.numeric(as.character(df$result))),])
}

#---------------------------------4: Calculate the Proportion of Character or Factor variable -------------------------------------
Findfp=function(data){
  lst=list()
  for (i in 1:ncol(data)){
    if  (is.factor(data[,i])==TRUE|is.character(data[,i])==TRUE){
      tb=list(prop.table(table(data[,i],exclude=NULL)))
      names(tb)=colnames(data[,i])
      lst=c(lst, tb)
    }
    else{
      #print ('No factor variables found in the data')
    }
  }
  return(lst)
}

#---------------------------------5: Missing Data Detect ---------------------------------
Missing = function(input){
  n = length(colnames(input)) 
  a <- NULL
  b <- NULL 
  c <- NULL
  for(i in 1:n) 
  {
    a[i]=sum(is.na(input[,i])) 
    b=a/nrow(input) 
    c[i]=length(unique(input[,i])) 
  }
  result=data.frame(colnames(input),a,b,c) 
  colnames(result) = c("column Name", "# Missing Values", "% Missing Value", "Unique Values")
  return(result) 
}

#--------------------------------------------6: Model validation --------------------------------------------
mdcom=function(a,m){
  metrics=c(MAD=0,MSE=0,MAPE=0,MPSE=0,R2=0,TMAD=0,p90=0)
  metrics["MAD"]=mean(abs(a-m))         # MAD: Mean absolute deviation
  metrics["MSE"]=mean((a-m)^2)          # MSE: Mean squared deviation
  metrics["MAPE"]=mean(abs((a-m)/a))    # MAPE: Mean absolute percentage error
  metrics["MPSE"]=mean(((a-m)/a)^2)     # MPSE: Mean percentage squared error
  SST=sum((a-mean(a))^2)                # SST: Total sum of squares
  SSE=sum((a-m)^2)                      # SSE: Sum of square due to error
  metrics["R2"]=1-(SSE/SST)
  metrics["TMAD"]=mean(abs(a-m),trim=0.05)
  metrics["p90"]=quantile(abs(a-m),probs=0.9)
  return(metrics)}

#--------------------------------------------7: Sample data------------------------------------------------------
split=function(data){
  set.seed(777)
  library(caTools)
  spl <- sample.split(team$R, 0.8)
  train <- subset(team, spl == TRUE) # Get the training data set
  test <- subset(team, spl == FALSE) # Get the test data set
  return(c(train,test))
}

