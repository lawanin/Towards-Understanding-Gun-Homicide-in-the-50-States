#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
source("crimegdpdata.R")

# Define UI for application that draws a histogram
ui <- navbarPage(
  "Exploring Variables of Gun Violence",
  tabPanel("Graphics",
                  mainPanel(imageOutput("crimegraph")),
                  mainPanel(dataTableOutput("gdporder"))),
  tabPanel("Model", 
           mainPanel(imageOutput("crimegdpfit")),
           h3("Discussion"), 
           p("The data in the graphics section, namely the gun deaths per capita
             and ranking of GDP per capita in each state from 2004 to 2016, was 
             fitted to a linear regression and the graphed posterior for the value 
             of the parameter gdp_1 was obtained. This value represents the 
             change in crime per 100,000 people for each change in ranking of 
             state GDP. The model yields a most likely value of -.019, with a 95% 
             confidence roughly between -.031 and -.007. The model suggests,
             however, as the graph's line shows, that there is definitely negative 
             correlation between crime and GDP. For context, the model suggests
             that a difference of 30 in GDP per capita ranking results in roughly
             1 fewer gun death per 100,000 people in the state.")),
  tabPanel("Discussion",
           titlePanel("Potential Sources"),
           h3("Gun Violence Statistics"),
           p("The most important part of my project 
               will involve statistics on gun violence. I found a Wikipedia 
               tbale which has information on violence in 2015. The source is 
               the FBI's online summary of Crime in the U.S. This is an anunual
               report and I have checked and there is a 2019 version. Generally, 
               I want to avoid 2020 in fear of any anomalies created by the 
               pandemic. The wikipedia table also has a source for 2013 gun 
               ownership rates, but the study it linked to seems to have done
               these caluclations using firearm suicide as a proxy. This sounds
               dubious to me. Link to Wikipedia page:
               https://en.wikipedia.org/wiki/Gun_violence_in_the_United_States_by_state#cite_note-4"),
           h3("Gun Ownerhsip Statistics"),
           p("Not only is the study cited old, but I really am sketptical of 
               its methodology, though it is apparently not an uncommon one. I 
               found a CBS news article which published gun ownership rates by 
               state and whose source was a long-term study released in April of 
               2020 by researches at the RAND Corporation. The base of the 
               calculations appear to come from surveys, combined with
               'administrative data on firearm suicides, hunting licenses, 
               subscriptions to Guns & Ammo magazine, and background checks.' 
               This sounds much more trustworthy. Though it was released early in
               2020, so I don't think my COVID fears really apply here.
               Link: https://www.rand.org/pubs/tools/TL354.html"),
           h3("States by GDP"),
           p("I found a Wikipedia article with a list of states by their GDP. 
               It lists states for the third quarter of 2020, but again I am 
               conerned that data from this time will obscure results because 
               of impact from COVID. The source is the Bureau of Economic 
               Analysis, Department of Commerce. They seem to have regular 
               quarterly reports of this kind. Information on the territories of 
               the U.S. was gotten from the World Bank, and I don't think I will 
               be including these in my observation, though I should make sure 
               other sources of mine do not. I also wonder whether GDP is a better
               approximation of what I am looking for than income. I will want 
               to compare the difference between these two in further analysis."),
           h3("Census Data"),
           p("Census Data contains many of the factors I am interested in using
             in these observations: urbanity, race, and possibly, hosuehold income. 
             I am interested in the first two because I often here these invoked
             in conversation gun violence. Particulary by those who push back 
             against corellation by gun violence. Though PMUS data exists and I
              would love to do something on a more local level, perhaps in 
              Virginia, I do not know whether the preceeding data exixts for 
              states.")),
  tabPanel("About", 
           titlePanel("About"),
           h3("Project Background and Motivations"),
           p("Hello, this is my final project on Gun Violence in progress. Right
             now, I am settling into ways of displaying the various factors on 
             Gun Violence which I am interested in exploring. I am using one
             such factor, state GDP per capita, as a working test of how to 
             process and display these reults. It should be noted that the data 
             currently on the site deals with crime at large and not gun crime. 
             I do have this data, but have not worked with it yet. x"),
           h3("About Me"),
           p("My name is Nosa and I study Comparative Literature. 
             You can reach me at nlawani@college.harvard.edu."),
           h3("Repo Link"),
           p("https://github.com/lawanin/gov1005-recitation-week-4")))

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$crimegraph <- renderPlot({
    crimegdpfile %>%
      ggplot(aes(x = gdp_id, y = big_crime_per_capita)) +
      geom_point(alpha = 0.5) + 
      labs(title = "Gun Deaths per 100k by States in Order of Asecending GDP per Capita
from 2004-2016", 
           subtitle = "The Linear Fit Suggests a Slight Decrease in Crime as
States grow Richer, but this is hard to observe", 
           x = "States in order of ascending GDP per capita", 
           y = "Crime per capita") +
      scale_x_continuous(breaks = c(1 : 50)) +
      geom_smooth(method = "lm") 
  })
  output$gdporder <- renderDataTable({
    gdp_order
  })
  output$crimegdpfit <- renderPlot({
    fit_1_plot
  })
}

# Run the application 
shinyApp(ui = ui, server = server)