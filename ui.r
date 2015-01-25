##################################################
###### Developing Data Products Project #######
####### Titanic Survivors #################
library(shiny)

# Define UI for random distribution application 
shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Titanic Survivors"),
  
  # Sidebar with controls to select the random distribution type
  # and number of observations to generate. Note the use of the br()
  # element to introduce extra vertical spacing
  sidebarPanel(
    
    h3("Distribution of Survivors"),
    radioButtons("ch","Choose a Passenger Characteristic:",
                 list("Passenger Clasee"="pclass",
                      "Gender"="sex",
                      "Age Group"="agegrp",
                      "Aboard with Siblings/Spouses"="sibspyn",
                      "Aboard with Parents/Children"="parchyn")),
    
    radioButtons("dist", "Bar Chart Displays:",
                 list("Number of Survivors" = "num",
                      "Survival Rate" = "rate")),
#     #     submitButton("Submit"),
    br(),

    h3("Find Survival Rate of Passengers:"),
    
    selectizeInput("class", 
                "Choose a Passenger Class:", 
                choices = c("1st","2nd","3rd")),

    selectInput("sex", 
                "Choose a Passenger Gender:", 
                choices = c("female","male")),

    selectInput("agegrp", 
            "Choose a Passenger Age Group:", 
            choices = c("0-10 yrs","10-20 yrs","20-30 yrs","30-40 yrs","40-50 yrs","50-60 yrs","60-70 yrs","70-80 yrs")),

    selectInput("sibspyn", 
                "Choose Passenger Aboard with Siblings/Spouses:", 
                choices = c("Yes","No")),
    
    selectInput("parchyn", 
                "Choose Passenger Aboard with Parents/Children:", 
                choices = c("Yes","No"))
  ),
  
  # Show a tabset that includes a plot, summary, and table view
  # of the generated distribution
  mainPanel(
    
    #     h3(textOutput("To look at the distribution of survivors, please click the 'Distribution - Plot' or'Distribution - Table' tab")),
    
    tabsetPanel( 
      tabPanel("Introduction", tableOutput("intro")),
      tabPanel("Distribution - Plot", plotOutput("plot")), 
      tabPanel("Distribution - Table", tableOutput("table")),
      tabPanel("Survival Rate", tableOutput("summary"))

    )
  )
))
