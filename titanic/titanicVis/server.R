library(shiny)
library(rpart)
library(rattle)
# Define server logic required to summarize and view the selected
# dataset

model.building<-function(test0){
  setwd("~/Dropbox/kaggle/titanic")
  
  #reading train/test data
  train<-read.csv('train.csv',na.strings=c('NA',''), stringsAsFactors=F)
  test<-read.csv('test.csv',na.strings=c('NA',''),stringsAsFactors=F)
  
  #checking the missing data
  #combine train/test data for pre-processing
  train$Cat<-'train'
  test$Cat<-'test'
  test$Survived<-NA
  full<-rbind(train,test)
  
  #Embarked
  full$Embarked[is.na(full$Embarked)]<-'S'
  #Extract Title from Name
  full$Title<-sapply(full$Name,function(x) strsplit(x,'[.,]')[[1]][2])
  full$Title<-gsub(' ','',full$Title)
  full$Title[full$Title %in% c('Capt', 'Don', 'Major', 'Sir')] <- 'Sir'
  full$Title[full$Title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer')] <- 'Lady'
  #Adding FamilySize
  full$FamilySize<-full$Parch+full$SibSp+1
  #Fare
  #simply replace the one missing Fare data with median, due to skewed distribution of Fare
  full$Fare[is.na(full$Fare)]<-median(full$Fare,na.rm=T)
  #full$Fare<-cut(full$Fare,c(0,20,50,Inf))
  #Age
  #decision tree (regression) method to predict missing Age data
  library(rpart)
  fit.Age<-rpart(Age[!is.na(Age)]~Pclass+Title+Sex+SibSp+Parch+Fare,data=full[!is.na(full$Age),],method='anova')
  full$Age[is.na(full$Age)]<-predict(fit.Age,full[is.na(full$Age),])
  
  
  #Adding Mother
  full$Mother<-0
  full$Mother[full$Sex=='female' & full$Parch>0 & full$Age>18 & full$Title!='Miss']<-1
  #Adding Child
  full$Child<-0
  full$Child[full$Parch>0 & full$Age<=12]<-1
  full<-transform(full,
                  Pclass=factor(Pclass),
                  Sex=factor(Sex),
                  Embarked=factor(Embarked),
                  Title=factor(Title),
                  Mother=factor(Mother),
                  Child=factor(Child)
  )
  
  train<-full[full$Cat=='train',]
  test<-full[full$Cat=='test',]
  train$Survived<-factor(train$Survived)
  levels(train$Survived)<-c('Perished','Survived')
  library(rpart)
  fit.rpart<-rpart(Survived~FamilySize+Pclass+Sex+Age+SibSp+Parch+Fare+Embarked+Title+Mother+Child,data=train,control=rpart.control(cp=0.01))
  test<-test[1,]
  test$FamilySize<-test0$FamilySize
  test$Title<-test0$Title
  test$Pclass<-test0$Pclass
  test$Age<-test0$Age
  test$Fare<-test0$Fare
  rslt<-predict(fit.rpart,test,type='class')
  rslt<-as.character(rslt)
  return(list(fit.rpart,rslt))
}

shinyServer(function(input, output) {
  getData<-reactive({
    model.building(data.frame(
      Title=input$Title,
      Pclass=input$Pclass,
      FamilySize=input$FamilySize,
      Fare=input$Fare,
      Age=input$Age
    ))[[2]]
  })
  getPlot<-reactive(fancyRpartPlot(model.building(data.frame(
    Title=input$Title,
    Pclass=input$Pclass,
    FamilySize=input$FamilySize,
    Fare=input$Fare,
    Age=input$Age
  ))[[1]]))
  output$msg1<-renderText(
    paste('Hi',input$Title,input$user,',')
  )
  output$msg2<-renderText(
    getData()
  )
  output$plot1<-renderPlot(getPlot())
  
})
