#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(dplyr)
server <- function(input, output, session) {
  
  
  p1 <- read.csv("/Users/kimayashah/Downloads/insurance.csv", header = TRUE, sep = ",")
  #Summarize Data and then Plot
  data<-reactive({
    #req(input$insured_sex)
    req(input$collision_type)  
    d<-p1%>% filter(collision_type %in% input$collision_type)%>% group_by(incident_hour_of_the_day)%>% summarise(count=n())
  })
  
  #Update SelectInput Dynamically
  observe({
    updateSelectInput(session, "collision_type", choices = p1$collision_type)
  })
  
  
  #Plot
  output$plot <- renderPlot({ 
    ggplot(data(),aes(x=incident_hour_of_the_day,y=count))+
      geom_bar(stat = "identity",fill="lightblue") +theme_dark()+ geom_text(aes(label=count), vjust=2, color="black", size=4.5)+xlab("Hour of the day")+ylab("Count")+ggtitle("Types of Crashes by Time")
  })
  
}

ui <- basicPage(
  selectInput(inputId = "collision_type",
              label = "Select collision type",
              "Names"),
  plotOutput("plot")
  
)

shinyApp(ui = ui, server = server)