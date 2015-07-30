##setting the working directory
setwd("~/Dropbox//kaggle/digit recognize")

##reading train/test data
library(readr)
train<-read_csv('train.csv')
test<-read_csv('test.csv')

##Visualize the data
display=function(data,dim=28){
  n=1
  col=NULL
  for (i in 1:10){
    row=NULL
    for (j in 1:10){
      x=as.integer(data[n,])
      x=rev(x)
      n=n+1
      x=matrix(x,dim,dim,byrow=F)
      x=x[seq(dim,1,-1),]
      row=cbind(row,x)
    }
    col=rbind(col,row)
  }
  image((col),col=gray((0:255)/255),axes=F)
}

##separate the label and features
label<-train$label
train$label<-NULL

##Dimension Redcution 1 - combine adjcent 2x2 pixels
reduceDim<-function(data){
  pos<-matrix(1:784,28,28,byrow=T)
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
train.reduced=reduceDim(train)
test.reduced=reduceDim(test)

##Dimension Redcution 2 - PCA
library(caret)
nzv<-nzv(rbind(train.reduced,test.reduced),99/1)
train.nzv<-train.reduced[,-nzv]
test.nzv<-test.reduced[,-nzv]
pca<-preProcess(rbind(train.nzv,test.nzv),method='pca',thresh=0.9)
train.pca<-predict(pca,train.nzv)
test.pca<-predict(pca,test.nzv)

##Train with Linear SVM
ctrl<-trainControl(method='cv',number = 10)
inTrain=sample(42000,500)
run_time<-system.time(fit<-train(factor(label[inTrain])~.,data=train.pca[inTrain,],
            trControl = ctrl,
            method='svmLinear'))


##write prediction
rslt<-predict(fit,test.reduced,type='raw')
predictions <- data.frame(ImageId=1:nrow(test), Label=rslt)
write_csv(predictions,'submission.csv')

##Generate markdown
library(knitr)
knit2html('digit-recognizor.Rmd')

