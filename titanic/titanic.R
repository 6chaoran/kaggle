#create/set the working directory
if (!file.exists('~/Desktop/kaggle_titanic')) dir.create('~/Desktop/kaggle_titanic',recursive = T)
setwd("~/Desktop/kaggle_titanic")

#download the data set from github
if (!file.exists('train.csv')) download.file('https://raw.githubusercontent.com/6chaoran/kaggle/master/titanic/train.csv'
,'train.csv',method='curl')
if (!file.exists('test.csv')) download.file('https://raw.githubusercontent.com/6chaoran/kaggle/master/titanic/test.csv'
,'test.csv',method='curl')

#reading train/test data
train<-read.csv('train.csv',na.strings=c('NA',''), stringsAsFactors=F)
test<-read.csv('test.csv',na.strings=c('NA',''),stringsAsFactors=F)

#combine train/test data for pre-processing
train$Cat<-'train'
test$Cat<-'test'
test$Survived<-NA
full<-rbind(train,test)

#checking the missing data
require(Amelia)
missmap(train,col=c('yellow','black'),main='Titanic Train Data',legend=F)

#Create Features for model fitting
#Extract Title from Name
full$Title<-sapply(full$Name,function(x) strsplit(x,'[.,]')[[1]][2])
full$Title<-gsub(' ','',full$Title)
aggregate(Age~Title,full,median)
full$Title[full$Title %in% c('Capt', 'Don', 'Major','Jonkheer')] <- 'Sir'
full$Title[full$Title %in% c('Dona','the Countess' )] <- 'Lady'
full$Title[full$Title %in% c('Mlle','Mme','Ms')] <- 'Miss'

#Adding FamilySize
full$FamilySize<-full$Parch+full$SibSp+1

#Adding Mother
full$Mother<-0
full$Mother[full$Sex=='female' & full$Parch>0 & full$Age>18 & full$Title!='Miss']<-1

#Adding Child
full$Child<-0
full$Child[full$Parch>0 & full$Age<=12]<-1

#Adding FamilyId
Surname<-sapply(full$Name,function(x) strsplit(x,'[.,]')[[1]][1])
FamilyId<-paste0(full$FamilySize,Surname)
full$FamilyId<-factor(FamilyId)
Family<-data.frame(table(FamilyId))
SmallFamily<-Family$FamilyId[Family$Freq<=2]
FamilyId[FamilyId %in% SmallFamily]<-'Small'
full$FamilyId2<-factor(FamilyId)

#Exact Deck from Cabin number
full$Deck<-sapply(full$Cabin, function(x) strsplit(x,NULL)[[1]][1])
#deck.fit<-rpart(Deck~Pclass+Fare,data=full[!is.na(full$Deck),])
#full$Deck[is.na(full$Deck)]<-as.character(predict(deck.fit,full[is.na(full$Deck),],type='class'))
full$Deck[is.na(full$Deck)]<-'UNK'

#Excat Cabin Posistion from Cabin number
full$CabinNum<-sapply(full$Cabin,function(x) strsplit(x,'[A-Z]')[[1]][2])
full$num<-as.numeric(full$CabinNum)
num<-full$num[!is.na(full$num)]
Pos<-kmeans(num,3)
full$CabinPos<-'UNK'
full$CabinPos[!is.na(full$num)]<-Pos$cluster
full$CabinPos<-factor(full$CabinPos)
full$num<-NULL

#Dealing with missing data (Emarked/Fare/Age)
#Embarked
full$Embarked[is.na(full$Embarked)]<-'S'

#Fare
#simply replace the one missing Fare data with median, due to skewed distribution of Fare
full$Fare[is.na(full$Fare)]<-median(full$Fare,na.rm=T)

#Age
#regression tree method to predict missing Age data
library(rpart)
fit.Age<-rpart(Age[!is.na(Age)]~Pclass+Title+Sex+SibSp+Parch+Fare,data=full[!is.na(full$Age),],method='anova')
full$Age[is.na(full$Age)]<-predict(fit.Age,full[is.na(full$Age),])

#factorize the categorical data
full<-transform(full,
                Pclass=factor(Pclass),
                Sex=factor(Sex),
                Embarked=factor(Embarked),
                Title=factor(Title),
                Mother=factor(Mother),
                Child=factor(Child),
                FamilyId2=factor(FamilyId2),
                Deck=factor(Deck)
)

#split into train/test dataset
train<-full[full$Cat=='train',]
test<-full[full$Cat=='test',]
train$Survived<-factor(train$Survived)

#Train using Caret package
#setting 10 folds cross validation for a fair model evaluation
library(caret)
set.seed(99)
control=trainControl(method='repeatedcv',number=10,repeats=10)
#optimized setting for neural network, prefine for fast re-run
grid.nnet<-expand.grid(size=1,decay=0.1)

#train with decision tree model
fit.rp<-train(Survived~FamilySize+CabinPos+Deck+Pclass+Sex+Age+SibSp+Parch+Fare+Embarked+Title+Mother+Child,
              data=train,method='rpart',trControl=control)

#train with logistic regression
fit.glm<-train(Survived~FamilySize+CabinPos+Deck+Pclass+Sex+Age+SibSp+Parch+Fare+Embarked+Title+Mother+Child,
               data=train,method='glm',trControl=control,family=binomial(link='logit'))

#train with bayesian logistic regression
fit.bl<-train(Survived~FamilySize+CabinPos+Deck+Pclass+Sex+Age+SibSp+Parch+Fare+Embarked+Title+Mother+Child,
              data=train,method='bayesglm',trControl=control,family=binomial(link='logit'))

#train with random forest
fit.rf<-train(Survived~FamilySize+CabinPos+Deck+Pclass+Sex+Age+SibSp+Parch+Fare+Embarked+Title+Mother+Child,
              data=train,method='rf',trControl=control)

#train with neural network
fit.nnet<-train(Survived~FamilySize+CabinPos+Deck+Pclass+Sex+Age+SibSp+Parch+Fare+Embarked+Title+Mother+Child,
                data=train,method='nnet',trControl=control,maxit=1000,trace=F,tuneGrid=grid.nnet)

#tabulating the prediction accuracy
acc.glm<-paste0(round(max(fit.glm$results$Accuracy),4)*100,'%')
acc.bl<-paste0(round(max(fit.bl$results$Accuracy),4)*100,'%')
acc.rp<-paste0(round(max(fit.rp$results$Accuracy),4)*100,"%")
acc.rf<-paste0(round(max(fit.rf$results$Accuracy),4)*100,"%")
acc.nnet<-paste0(round(max(fit.nnet$results$Accuracy),4)*100,'%')

cat('prediction accucary with logisitc regreesion is ',acc.glm,'.\n')
cat('prediction accucary with bayesian logisitc regreesion is ',acc.bl,'.\n')
cat('prediction accucary with decision tree model is ',acc.rp,'.\n')
cat('prediction accucary with random forest  is ',acc.rf,'.\n')
cat('prediction accucary with nerual network is ',acc.nnet,'.\n')

#writing submission with nerual network method
test$Survived<-predict(fit.nnet,test)
submission<-test[,1:2]
write.csv(submission,'submission_nnet.csv',row.names=F)
