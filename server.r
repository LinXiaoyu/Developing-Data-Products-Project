####################################################
######### Developing Data Products Project #########
### server.r
library(shiny)
library(ggplot2)
library(plyr)
require(rpart.plot)
data(ptitanic)

data = ptitanic

#### Transform numerical variables to categorical variables
data$agegrp = cut(data$age,breaks = c(0,10,20,30,40,50,60,70,80), right = T,
                  labels=c("0-10 yrs","10-20 yrs","20-30 yrs","30-40 yrs","40-50 yrs","50-60 yrs","60-70 yrs","70-80 yrs"))

data$sibspyn[data$sibsp==0] <- "No"
data$sibspyn[data$sibsp>0] <- "Yes"

data$parchyn[data$parch==0] <- "No"
data$parchyn[data$parch>0] <- "Yes"

# Define server logic for random distribution application
shinyServer(function(input, output) {
  
  data1 <- reactive({
    ch=input$ch
    table0 <- table(data[,ch],data$survived)
    table_sum <- data.frame(matrix(,nrow=dim(table0)[1],ncol=dim(table0)[2]+1))
    rownames(table_sum) = rownames(table0)
    colnames(table_sum) = c("Number of Passengers Died","Number of Passengers Survived","Survival Rate")
    table_sum[1:dim(table0)[1],1:2] = table0
    table_sum[1:dim(table0)[1],3] = round(prop.table(table0,1),2)[,2]
    table_sum
    
  })
  
  data2 <- reactive({
    ch=input$ch
    table0 = data.frame(table(data[,ch],data$survived))
    names(table0)=c(ch,"survived","Freq")
    table_plot <- ddply(table0, ch, transform,
                        survival_rate = round(Freq / sum(Freq),2))
    table_plot
    
  })
  
  survival_rate <- reactive({
    
    data0 = data[(!is.na(data$age))&(data$pclass==input$class)&(data$sex==input$sex)&(data$agegrp==input$agegrp)
                  &(data$sibspyn==input$sibspyn)&(data$parchyn==input$parchyn),]
    survival_rate = list(0,0)
    if(dim(data0)[1]!=0){
      survival_rate[[1]] = dim(data0)[1]
      survival_rate[[2]]=sum(data0$survived=="survived")/dim(data0)[1]
    
    }
    else{
      survival_rate = list("NA","NA")
    }
    survival_rate
  
  })
  
  
  Values <- reactive({
    
    # Compose data frame
    rate = data.frame(matrix(numeric(),nrow=7,ncol=2))
    names(rate) = c("Variables","You Selected:")                  
    rate[,1] = c("Passenger Class", 
               "Gener",
               "Age Group",
               "Passenger Aboard with Siblings/Spouses",
               "Passenger Aboard with Parents/Children",
               "Number of Passengers in the selected group",
               "Survival Rate of the selected group")
    rate[1:5,2] = as.character(c(input$class, 
                             input$sex,
                             input$agegrp,
                             input$sibspyn,
                             input$parchyn))
    rate[6,2] = survival_rate()[[1]]
    rate[7,2] = round(survival_rate()[[2]],2)
    rate

  }) 
  
  output$table <- renderTable({
    data1()
    
  })
  
  output$plot <- renderPlot({
    
    table_plot = data2()
    ch=input$ch
    dist = input$dist
    choice_name = ifelse(ch=="pclass","Passenger Clasee",
                         ifelse(ch=="sex","Gender",
                                ifelse(ch=="agegrp","Age Group",
                                       ifelse(ch=="sibspyn","Aboard with Siblings/Spouses",
                                              ifelse(ch=="parchyn","Aboard with Parents/Children")))))
    if(dist=="num"){
      
      p = ggplot(table_plot, aes(x=table_plot[,1], y=Freq, fill=survived,ymax=max(table_plot$Freq)),environment=environment()) +
        geom_bar(stat="identity",position="dodge") +
        scale_fill_brewer(palette="Pastel1") +
        geom_text(aes(label=Freq),vjust=1.5,
                  position=position_dodge(.9), size=3) +
        ylim(0,max(table_plot$Freq)+50) +
        xlab(choice_name) +
        ylab("Number of Survivors")
      print(p)
    }
    
    if(dist=="rate"){
      p = ggplot(table_plot, aes(x=table_plot[,1], y=survival_rate, fill=survived,ymax=max(table_plot$survival_rate)),environment=environment()) +
        geom_bar(stat="identity",position="dodge") +
        scale_fill_brewer(palette="Pastel1") +
        geom_text(aes(label=survival_rate),vjust=1.5,
                  position=position_dodge(.9), size=3) +
        ylim(0,1) +
        xlab(choice_name) +
        ylab("Survival Rate")
      print(p)
    }
    
  })
  
  output$summary <- renderTable({
    Values()
  })
  
  output$intro <- renderTable({

     intro = data.frame(matrix(,4,2))
     names(intro)=c("","Discription")
     intro [,1] = c("Background","Data","Function","Prerequisite")
     intro[1,2] = "The sinking of the Titanic is one of the most infamous shipwrecks in history.
     On April 15, 1912, during her maiden voyage, the Titanic sank after colliding with an iceberg, 
     killing 1502 out of 2224 passengers and crew. 
     One of the reasons that the shipwreck led to such loss of life was that there were not enough 
     lifeboats for the passengers and crew. Although there was some element of luck involved in surviving 
     the sinking, some groups of people were more likely to survive than others, such as women, 
     children, and the upper-class. -- Kaggle"

     intro[2,2] = "The data set used in the application is 'ptitanic' from 'rpart.plot' package in R. 
      The description of the data set can be found at:
     'http://artax.karlin.mff.cuni.cz/r-help/library/rpart.plot/html/ptitanic.html'. 
      Variables included are passenger class, died or survived, gender, age, number of siblings or spouses aboard,
     number of parents or children aboard. Data transformation were done on some of the variables to simplify the application."
     
     intro[3,2] = "In this Shiny application, you can view the distribution of passengers who survied the tragedy with certain characteristics ('Distribution - Table' tab) 
      and visualize the resutls ('Distribution - Plot' tab), 
     as well as caculate the survival rate of a group of passengers ('Survival Rate' tab). 
     The application can give you a general idea of what sorts of passengers were likely to survive."
     
     intro[4,2] = "Please install R packages of 'ggplot2' and 'plyr' before running this application."
     intro
     })
  
})
