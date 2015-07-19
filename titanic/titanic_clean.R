setwd("~/Dropbox/kaggle/titanic")
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
levels(full$Survived)=c('Perished','Survived')
train<-full[full$Cat=='train',]
test<-full[full$Cat=='test',]
train$Survived<-factor(train$Survived)