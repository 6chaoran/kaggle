setwd("~/Dropbox//kaggle/digit recognize")
library(readr)
train<-read_csv('train.csv')
test<-read_csv('test.csv')
library(caret)

reduceDim<-function(data){
  pos<-matrix(0:783,28,28,byrow=T)
  offset<-seq(1,28,2)
  n=0
  train.reduced<-data.frame(index=1:nrow(data))
  if(!is.null(data$label)) train.reduced$label<-data$label
  data$label<-NULL
  for (i in offset){
    for (j in offset){
      px<-as.numeric(pos[i:(i+1),j:(j+1)])
      px<-apply(data[,px],1,max)
      index<-paste0('px',n)
      n=n+1
      train.reduced[index]<-px
    }
  }
  train.reduced$index<-NULL
  return (train.reduced)
}

#reduce dimension
train.reduced<-reduceDim(train)
label<-train$label
train.reduced$label<-NULL
train.reduced<-round(train.reduced/255.0,3)
test.reduced<-reduceDim(test)
test.reduced<-round(test.reduced/255.0,3)

#sample
inTrain<-createDataPartition(label,p=1,list=F)
train.re<-train.reduced[inTrain,]
test.re<-test.reduced[inTrain,]
label.re<-factor(label[inTrain])

#pre-process
nzv<-nzv(rbind(train.re,test.re),99/1)
train.re<-train.re[,-nzv]
test.re<-test.re[,-nzv]
pca<-preProcess(rbind(train.re,test.re),method='pca',pcaComp=39)
train.re<-predict(pca,train.re)
test.re<-predict(pca,test.re)

#svmPoly
ctrl<-trainControl(method='cv',number = 10)
tune<-expand.grid(
                  scale=0.1,
                  C=0.25,
                  degree=2
                  )
run_time<-system.time(fit<-train(label.re~.,data=train.re,
            trControl = ctrl,
            tuneGrid = tune,
            method='svmPoly'))

run_time<-system.time(fit<-train(label.re~.,data=train.re,
                                 trControl = ctrl,
                                 #tuneGrid = tune,
                                 method='rf'))

#random forest
#mtry  Accuracy   Kappa      Accuracy SD  Kappa SD  
#39   0.8435501  0.8249940  0.03938595   0.04394278
#library(randomForest)
#rf.time<-system.time(fit.rf<-randomForest(label.re~.,data=train.re,mtry=15,ntree=200))

#pca=39
#Accuracy   Kappa      Accuracy SD  Kappa SD   
#0.9732862  0.9703079  0.003337582  0.003709799

#write prediction
rslt<-predict(fit,test.re,type='raw')
predictions <- data.frame(ImageId=1:nrow(test), Label=rslt)
write_csv(predictions,'submission_pca39.csv')

