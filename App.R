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
spec <- data.frame(iid=newColom$iid, attr_1= newColom$attr1_1)
spec <- na.omit(spec)

completeRecords <- na.omit(newColom) 
#records men and women filtering
recordsMen <- completeRecords %>% select(iid, id, pid, partner, gender, match, attr1_1, attr2_1, attr, like, dec_o, dec, income) %>% filter(gender == 1)
recordsWomen <- completeRecords %>% select(iid, id, pid, partner, gender, match, attr1_1, attr2_1, attr, like, dec_o, dec, income) %>% filter(gender == 0)


speculation <- aggregate(x=spec$iid,
                         by=list(spec$attr_1),
                         FUN=length)

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

#create new dataframes
dating <- data.frame(attractiveness = impactNormal$Group.1, matches = impactNormal$x[,1], total = impactNormal$x[,2], 
                     ratio = (impactNormal$x[,1]/impactNormal$x[,2])*100)

datingMen <- data.frame(attractiveness = impactMen$Group.1, matches = impactMen$x[,1], total = impactMen$x[,2], 
                     ratio = (impactMen$x[,1]/impactMen$x[,2])*100)

datingWomen <- data.frame(attractiveness = impactWomen$Group.1, matches = impactWomen$x[,1], total = impactWomen$x[,2], 
                        ratio = (impactWomen$x[,1]/impactWomen$x[,2])*100)


barplot(dating$matches)
barplot(datingMen$matches)
barplot(datingWomen$matches)

barplot(datingMen$ratio)
barplot(datingWomen$ratio)


#ggplot(dating, aes(x=attractiveness, y=matches)) + 
#  geom_bar(stat = "identity") +
#  coord_flip()

#ggplot(dating, aes(x=attractiveness, y=total)) + 
#  geom_bar(stat = "identity") +
 # coord_flip()

#ggplot(dating, aes(fill=total, y=matches, x=attractiveness)) + 
 # geom_bar(position="dodge", stat="identity")

ggplot(data = dating, mapping = aes(fill=matches,x=attractiveness, y=matches),color="blue")+
  geom_bar(position="dodge", stat="identity")

m<-ggplot()+
  geom_line(data = dating, mapping = aes(x=attractiveness, y=matches), color= "blue")+
  geom_point(data = dating, mapping = aes(x=attractiveness, y=matches), color= "blue")+
  geom_line(data = datingMen, mapping = aes(x=attractiveness, y=matches), color= "orange")+
  geom_point(data = datingMen, mapping = aes(x=attractiveness, y=matches), color= "orange")+
  geom_line(data = datingWomen, mapping = aes(x=attractiveness, y=matches), color= "red")+
  geom_point(data = datingWomen, mapping = aes(x=attractiveness, y=matches), color= "red")+
  labs(x="Attractiveness", y="ratio in %", title="% of matches compared to toal dates for each attractiveness level")

g<-ggplot()+
  geom_line(data = dating, mapping = aes(x=attractiveness, y=ratio), color= "green")+
  geom_point(data=dating, mapping = aes(x=attractiveness, y=ratio), color = "green")+
  geom_line(data = datingMen, mapping = aes(x=attractiveness, y=ratio), color= "red")+
  geom_point(data=datingMen, mapping = aes(x=attractiveness, y=ratio), color = "red")+
  geom_line(data = datingWomen, mapping = aes(x=attractiveness, y=ratio), color= "yellow")+
  geom_point(data=datingWomen, mapping = aes(x=attractiveness, y=ratio), color = "yellow")+
  labs(x="Attractiveness", y="ratio in %", title="% of matches compared to toal dates for each attractiveness level")

  ui <- fluidPage(theme = shinytheme("cerulean"),
                navbarPage(
                  theme = "cerulean",  
                  "Data Vizualization app",
                  tabPanel("Assignment", 
                           "In this app we will be looking at vizualized data of how important people 
                           think attractiveness is when it comes to dating vs the actual impact"), 
                  tabPanel("Navbar 2",
                           sidebarPanel(
                             selectInput(inputId = "input", label = "pick the type of impact you wish to see", 
                                         choices = c("overall", " based on gender")),
                             
                             actionButton("show", "show graph")
                             
                           ), # sidebarPanel
                           mainPanel(
                             
                             #renderPlot(plot(cars, main = input$input, cex = input$choices, pch = 19),
                                        #width = 600, height = 400)
                           ) 
                           
                  ),
                  tabPanel("Different perspective",
                           sidebarPanel(
                             selectInput("input", label = "pick the type of impact you wish to see", 
                                         choices = c("overall", " based on gender")),
                             sliderInput('size', 'Point size', min = 0.2, max = 5, value = 1),
                             
                             actionButton("show", "show graph")
                             
                           ), # sidebarPanel
                           
                           mainPanel(
                             
                            
                           ) 
                           
                  )
                ) 
) 


# Define server function  
server <- function(input, output) {
  
  datasetInput <- reactive({
    switch(input$input,
           "overall" = overall,
           "based on gender" = gender)
  })
  
  output$table <- renderTable({
    datasetInput()
  })
} 








shinyApp(ui = ui, server = server)


#barplot(impactNormal$x, names.arg = impactNormal$Group.1, xlab = 'attractiveness' , ylab = 'amount of matches', col = 'orange')                                                                                       
 barplot(speculation$x)
 
