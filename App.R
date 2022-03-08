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
  select(iid, id, pid, partner, gender,age, match, attr1_1, attr2_1, attr, like, dec_o, dec, income)

#clean out incomplete records
spec <- data.frame(iid=newColom$iid, attr_1= newColom$attr1_1)
spec <- na.omit(spec)

#records overall, men and women filtering
completeRecords <- na.omit(newColom) 
recordsMen <- completeRecords %>% select(iid, id, pid, partner, gender, match, attr1_1, attr2_1, attr, like, dec_o, dec, income) %>% filter(gender == 1)
recordsWomen <- completeRecords %>% select(iid, id, pid, partner, gender, match, attr1_1, attr2_1, attr, like, dec_o, dec, income) %>% filter(gender == 0)

#real matches
allRealMatches <- completeRecords %>% select(iid, id,gender, match, attr, dec_o,dec) %>% filter(match == 1) %>% filter(dec_o == 1)
allRealMatchesMen <- completeRecords %>% select(iid, id, gender, match, attr, dec_o,dec) %>% filter(match == 1) %>% filter(dec_o == 1) %>% filter(gender== 1)
allRealMatchesWomen <- completeRecords %>% select(iid, id, gender, match, attr, dec_o,dec) %>% filter(match == 1) %>% filter(dec_o == 1) %>% filter(gender == 0)


speculation <- aggregate(x=spec$iid,
                         by=list(spec$attr_1),
                         FUN=length)

#put the amount of matches and total dates in a different column overall, men and women
realNormalImpact <- aggregate(x = allRealMatches$match,
                              by = list(allRealMatches$attr),
                              FUN = function(x) c(matches = sum(x), dates = length(x)))

realImpactMen <- aggregate(x = allRealMatchesMen$match,
                           by = list(allRealMatchesMen$attr),
                           FUN = function(x) c(matches = sum(x), dates = length(x)))

reallImpactWomen <- aggregate(x = allRealMatchesWomen$match,
                              by = list(allRealMatchesWomen$attr),
                              FUN = function(x) c(matches = sum(x), dates = length(x)))

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
attrSpec <- data.frame(points = speculation$Group.1, people = speculation$x)
dating <- data.frame(attractiveness = impactNormal$Group.1, matches = impactNormal$x[,1], total = impactNormal$x[,2], 
                     ratio = (impactNormal$x[,1]/impactNormal$x[,2])*100)
datingMen <- data.frame(attractiveness = impactMen$Group.1, matches = impactMen$x[,1], total = impactMen$x[,2], 
                        ratio = (impactMen$x[,1]/impactMen$x[,2])*100)
datingWomen <- data.frame(attractiveness = impactWomen$Group.1, matches = impactWomen$x[,1], total = impactWomen$x[,2], 
                          ratio = (impactWomen$x[,1]/impactWomen$x[,2])*100)

#dates that where both partners matched
realDating <-data.frame(attractiveness = realNormalImpact$Group.1, matches = realNormalImpact$x[,1], total = realNormalImpact$x[,2], 
                        ratio = (realNormalImpact$x[,1]/realNormalImpact$x[,2])*100)
realDatingMen <-data.frame(attractiveness = realNormalImpactMen$Group.1, matches = realNormalImpactMen$x[,1], total = realNormalImpactMen$x[,2], 
                           ratio = (realNormalImpactMen$x[,1]/realNormalImpactMen$x[,2])*100)
realDatingWomen <-data.frame(attractiveness = realNormalImpactWomen$Group.1, matches = realNormalImpactWomen$x[,1], total = realNormalImpactWomen$x[,2], 
                             ratio = (realNormalImpactWomen$x[,1]/realNormalImpactWomen$x[,2])*100)


# Define server function  
server <- function(input, output) {
  select_input = reactive({input$select_input})
  based = reactive({input$based})
  slider_input = reactive({input$slider_input})
  select_input_mutual = reactive({input$select_input_mutual})
  #fist plot method
  
  output$dating_data_one <- renderPlot({
    if(select_input() == "overall" && input$based == "one-sided-matches"){
      g <- ggplot(data = dating, mapping = aes(x=attractiveness, y=matches))
      g + geom_bar(position="dodge", stat="identity")+
        labs(x="Attractiveness", y="Number of matches", title="amount of matches for each attractiveness level overall")
    } 
    else if(select_input() == "overall" && input$based == "mutual-matches"){
      g <- ggplot(data = realDating, mapping = aes(x=attractiveness, y=matches))
      g + geom_bar(position="dodge", stat="identity")+
        labs(x="Attractiveness", y="Number of matches", title="amount of matches for each attractiveness level overall")
    }
    else  if(select_input() == "for men" && input$based == "one-sided-matches"){
      g <- ggplot(data = datingMen, mapping = aes(x=attractiveness, y=matches))
      g + geom_bar(position="dodge", stat="identity")+
        labs(x="Attractiveness", y="Number of matches", title="amount of matches for each attractiveness level for men")
    }
    else if(select_input() == "for men" && input$based == "mutual-matches"){
      g <- ggplot(data = realDatingMen, mapping = aes(x=attractiveness, y=matches))
      g + geom_bar(position="dodge", stat="identity")+
        labs(x="Attractiveness", y="Number of matches", title="amount of matches for each attractiveness level for men")
    }
    else if(select_input() == "for women" && input$based == "one-sided-matches") {
      g <- ggplot(data = datingWomen, mapping = aes(x=attractiveness, y=matches))
      g + geom_bar(position="dodge", stat="identity")+
        labs(x="Attractiveness", y="Number of matches", title="amount of matches for each attractiveness level for women")
    }
    else if(select_input() == "for women" && input$based == "mutual-matches") {
      g <- ggplot(data = realDatingWomen, mapping = aes(x=attractiveness, y=matches))
      g + geom_bar(position="dodge", stat="identity")+
        labs(x="Attractiveness", y="Number of matches", title="amount of matches for each attractiveness level for women")
    }
  })
  #end first plot method
  
  #first page plot
  output$speculation <- renderPlot({
    ggplot(data=attrSpec, mapping=aes(x = attrSpec$points, y = people))+ 
      geom_bar(position = "dodge", stat = "identity", color = rainbow(94))+
      xlim(input$depth[1],input$depth[2])+
      labs(x="Importance of attractiveness", y="Frequency of answers ", title="% of matches compared to toal dates for each attractiveness level")
  }) 
  #end first page plot
  
  #secondtab code begin
  output$dating_data_two <- renderPlot({
    if(select_input() == "overall"  && input$based == "one-sided-matches"){
      ggplot()+
        geom_point(data = dating, mapping = aes(x=attractiveness, y=matches),color="black")+
        labs(x="Attractiveness", y="Number of matches", title="amount of matches for each attractiveness level overall")
    } 
    else if(select_input() == "overall"  && input$based == "mutual-matches"){
      ggplot()+
        geom_point(data = realDating, mapping = aes(x=attractiveness, y=matches),color="black")+
        labs(x="Attractiveness", y="Number of matches", title="amount of matches for each attractiveness level overall")
    }else  if(select_input() == "for men"  && input$based == "one-sided-matches"){
      ggplot()+
        geom_point(data = datingMen, mapping = aes(x=attractiveness, y=matches), color= "purple")+
        labs(x="Attractiveness", y="Number of matches", title="amount of matches for each attractiveness level for men")+
        stat_smooth(method = "lm")
    }else if(select_input() == "for men"  && input$based == "mutual-matches"){
      ggplot()+
        geom_point(data = realDatingMen, mapping = aes(x=attractiveness, y=matches), color= "purple")+
        labs(x="Attractiveness", y="Number of matches", title="amount of matches for each attractiveness level for men")+
        stat_smooth(method = "lm")
    }
    else if(select_input() == "for women" && input$based == "one-sided-matches"){
      ggplot()+
        geom_point(data = datingWomen, mapping = aes(x=attractiveness, y=matches), color= "red")+
        labs(x="Attractiveness", y="Number of matches", title="amount of matches for each attractiveness level for women")+
        stat_smooth(method = "lm")
    }
    else if(select_input() == "for women" && input$based == "mutual-matches"){
      ggplot()+
        geom_point(data = realDatingWomen, mapping = aes(x=attractiveness, y=matches), color= "red")+
        labs(x="Attractiveness", y="Number of matches", title="amount of matches for each attractiveness level for women")+
        stat_smooth(method = "lm")
    }
  })
  #end second tab
  output$dating_data_three <- renderTable({
    if(select_input() == "overall"  && input$based == "one-sided-matches"){
  dating 
    } 
    else if(select_input() == "overall"  && input$based == "mutual-matches"){
    realDating
    }else  if(select_input() == "for men"  && input$based == "one-sided-matches"){
    datingMen
    }else if(select_input() == "for men"  && input$based == "mutual-matches"){
realDatingMen
    }
    else if(select_input() == "for women" && input$based == "one-sided-matches"){
     datingWomen
    }
    else if(select_input() == "for women" && input$based == "mutual-matches"){
  realDatingWomen
    }
  })

} 

ui <- fluidPage(theme = shinytheme("cerulean"),
                navbarPage(
                  "Data Vizualization app",
                  tabPanel("Assignment",
                           fluidRow(tags$div(class="header", checked=NA,
                                             tags$p("To view information about the adult dating dataset"),
                                             tags$a(href="https://archive-beta.ics.uci.edu/ml/datasets/adult", "Click Here!")),
                                    br()),
                           fluidRow(tags$div(class="header", checked=NA,
                                             tags$p("The purpose of this dashboard is to show how important attractiveness is when it comes 
                                              to dating. Vs the actual impact of attractiveness."),
                                             tags$p("For the dataset people were given forms and they were supposed to distribute 100 points over multiple attributes they found important in a person
                                                    when it comes to dating. One of those attributes was attractiveness. That is why I have taken the amount of points each person filled in at the attractiveness attribute.
                                                    "),br(),
                                             tags$p("On this page is the barplot of the amount of people that have given a rating to how important they found attractiveness when it comes 
                                                    to dating from a scale from 0 to a 100. You can use the slider to zoom in on the barplot to better inspect the values in the barplot."),
                                             tags$p("The distribution of how important people find attractiveness is pretty high on the lower side of the scale of how important people
                                                    find attractiveness but as the importance increases it is noticeable that less people find attractiveness majorly important. Because of the number of bars I decides to use the rainbow color scheme because 
                                                    not all bars were visible when only one color was used.")
                           )),fluidRow(sidebarPanel(
                             sliderInput("depth", "Depth:", min = 0, max = 100, value = c(0,100))
                           ),
                           mainPanel(
                             
                             plotOutput("speculation")
                           ) 
                           )), 
                  
                  tabPanel("Matches based on attraction", 
                           fluidRow(tags$div(class="header", checken=NA,
                  tags$p("During the speed dating event each participant gave their date a rating between 1 and 10 based on how attractive they found their date. I also wanted to use 
                         the income attribute as well as the other perspective but there were a lot of missing values so I could not use that attribute."))),
                  tags$p("On this page I plotted two kinds of data.
                           The first being the total matches each partner of a certain attractiveness level got. And 
                           the amount of mutual matches people of a certain attractiveness level got. After plotting the data in a barplot and scatterplot you can see that the amount of matches overall increase up till an attractiveness value of 8 but lowers after that.
                        The same can be noticed for men and women. The same can be seen looking at the matches that were mutual. Based on this it can be concluded that attractiveness does
                        have an impact on dating but after an attractiveness value of 8 it lowers which can mean that there are also other attributes that are also important, otherwise the amount of matches
                        would keep on increasgin as the attractiveness level got higher but this is not the case"),br(),
                           #begin sidebar
                          fluidRow(sidebarPanel(
                             selectInput(inputId = "select_input", label = "impact of attractivenes when it comes to dating:", 
                                         choices = c("overall", "for men", "for women")), 
                             radioButtons(inputId = "based", label = "Based on", 
                                          choices =  c("one-sided-matches", "mutual-matches"))
                           ),
                           mainPanel(
                             tabsetPanel(
                               tabPanel("Bar plot", plotOutput("dating_data_one")), 
                               tabPanel("Scatter plot", plotOutput("dating_data_two")),
                               tabPanel("Tabel", tableOutput("dating_data_three"))
                             ))) 
                  )
                  
                  
                ) 
) 






shinyApp(ui = ui, server = server)








