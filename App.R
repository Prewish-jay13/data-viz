#please reload all libraries in case some functions cannot be found!!!
library(shiny)
library(shinythemes)
library(data.table)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(shinydashboard)

#read data from csv file
data <- read.csv("C://Users//prewi//Desktop//vizualization//dataviz assignment//data//Speed Dating Data.csv") 
 
#filter the data and pick the right attributes needed
#place the important data in a different table
newColom <- data %>% 
  select(iid, id, pid, partner, gender, match, attr1_1, attr2_1, attr, like, dec_o, dec, income)
  
#clean out incomplete records
completeRecords <- na.omit(newColom) 
#records men and women filtering
recordsMen <- completeRecords %>% select(iid, id, pid, partner, gender, match, attr1_1, attr2_1, attr, like, dec_o, dec, income) %>% filter(gender == 1)
recordsWomen <- completeRecords %>% select(iid, id, pid, partner, gender, match, attr1_1, attr2_1, attr, like, dec_o, dec, income) %>% filter(gender == 0)

#put the amount of matches and total dates in a different column overall, men and women
impactNormal <- aggregate(x = completeRecords$match,
                    by = list(completeRecords$attr),
                    FUN = function(x) c(matches = sum(x), dates = length(x)))

impactMen <- aggregate(x = recordsMen$match,
                       by = list(recordsMen$attr),
                       FUN = function(x) c(matches = sum(x), dates = length(x)))


impactWomen <- aggregate(x = recordsWomen$match,
                         by = list(recordsWomen$attr),
                         FUN = function(x) c(matches = sum(x), dates = length(x)))

#create new dataframe 
dating <- data.frame(attractiveness = impactNormal$Group.1, matches = impactNormal$x[,1], total = impactNormal$x[,2], 
                     ratio = impactNormal$x[,1]/impactNormal$x[,2])

datingMen <- data.frame(attractiveness = impactMen$Group.1, matches = impactMen$x[,1], total = impactMen$x[,2], 
                     ratio = impactMen$x[,1]/impactMen$x[,2])

datingWomen <- data.frame(attractiveness = impactWomen$Group.1, matches = impactWomen$x[,1], total = impactWomen$x[,2], 
                        ratio = impactWomen$x[,1]/impactWomen$x[,2])
ui <- fluidPage(theme = shinytheme("cerulean"),
                navbarPage(
                  theme = "cerulean",  
                  "Data Vizualization app",
                  tabPanel("Assignment", 
                           "In this app we will be looking at vizualized data of how important people 
                           think attractiveness is when it comes to dating vs the actual impact"), 
                  tabPanel("Navbar 2",
                           sidebarPanel(
                             selectInput("input", "pick the type of impact you wish to see", 
                                         choices = c("overall", " based on gender")),
                             
                             actionButton("show", "show graph")
                             
                           ), # sidebarPanel
                           mainPanel(
                             
                             renderPlot(plot(cars, main = input$input, cex = input$choices, pch = 19),
                                        width = 600, height = 400)
                           ) 
                           
                  ),
                  tabPanel("Different perspective",
                           sidebarPanel(
                             selectInput("input", "pick the type of impact you wish to see", 
                                         choices = c("overall", " based on gender")),
                             sliderInput('size', 'Point size', min = 0.2, max = 5, value = 1),
                             
                             actionButton("show", "show graph")
                             
                           ), # sidebarPanel
                           mainPanel(
                             
                             renderPlot(plot(cars, main = input$input, cex = input$choices, pch = 19),
                                        width = 600, height = 400)
                           ) 
                           
                  )
                ) 
) 


# Define server function  
server <- function(input, output) {
  
  datasetInput <- reactive({
    switch(input$dataset,
           "overall" = overall,
           "based on gender" = gender)
  })
  output$table <- renderTable({
    datasetInput()
  })
} 








shinyApp(ui = ui, server = server)


#barplot(impactNormal$x, names.arg = impactNormal$Group.1, xlab = 'attractiveness' , ylab = 'amount of matches', col = 'orange')                                                                                       
 
