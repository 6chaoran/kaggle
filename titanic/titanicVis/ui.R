library(shiny)
# Define UI for dataset viewer application
shinyUI(fluidPage(
  titlePanel("Titanic Survival Prediction"),
  sidebarLayout(
    sidebarPanel(
      textInput("user", "Input Your Name:", "Chaoran"),
      selectInput(
        'Title','Select your Title:',
        c('Mr'='Mr','Miss'='Miss','Master'='Master','Dr'='Dr','Mrs'='Mrs')
      ),
      selectInput(
        'Pclass','Select your Pclass:',
        c('1'=1,'2'=2,'3'=3)
      ),
      sliderInput(
        'Fare','Select your Fare:',
        min=0,max=300,value=20,step=1
      ),
      sliderInput(
        'Age','Select your Age:',
        min=0,max=80,value=27,step=1
      ),
      sliderInput(
        'FamilySize','Select your FamilySize:',
        min=1,max=11,value=1,step=1
      ),
      submitButton('Update')
    ),
    mainPanel(
      tabsetPanel(
        tabPanel(
          title='Decision Tree',
          plotOutput('plot1')
          ),
        tabPanel(
          title='Your Prediction',
          textOutput('msg1'),
          br(),
          p('with the given information:'),
          p('you are most likely to be '),
          textOutput('msg2')  
          )
        ) 
    )
  )
))