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
           h3("Gun Death Statistics"),
           p("This is the most important data in my project, as I will be estimating
           how it is influenced by different factos across the 50 states. My source
           for this has primarily been the annual FBI statistics. In this annual report, 
           there are data for all crime by state and type, and then data for crime
           with suppleental info, most importantly with type of weapon, by state 
           and type. Different states do not report supplemental info for every 
           crime committed, and so the proportion of homicide deaths with the 
           weapon specified varies to  homicide deaths in the general crime 
           statistics vary from year to year and state to state. For states and years
           where this proportion is greater than 75%, I have used the rate of 
           gun homicide to homicide to estimate the total gun homicides for those states 
           and years. In IL for almost every year and AL for some years, the FBI 
           notes that these states have underreported crimes with supplemental info. 
           By our 75% standard, a total of 61 states and years were not given 
           a value for gun homicide per capita, this includes states other 
           than IL and AL, and some years of these states were representative 
           enough to be given figures in our method. In every year, Florida was 
           excluded because its supplemental info was not compliant with FBI 
           requirements. On the state website for Florida, however, they have their
           own supplemental info for crime in all years, which have been used to 
           form the estimates for total gun deaths in this state. More thought 
           will go into validity of our method of estimation and the effect of 
           exclusion of these states on representativeness."),
           h3("States by GDP"),
           p("This data is from the Buereau of Economic Analysis. I have used their 
             data of annual Real GDP by state, that is the annual GDP for each state 
             in 2012 dollars. To obtain the GDP per capita I have used the state
             population provided for each state and year in the FBI crime 
             statistics. I have ranked the GDP per capita for each year from 1 to 
             however many states for which an estimate for gun homicide (vide supra)
             was able to be made. This is the only dependent variable I have 
             ranked and analyzed with gun homicide per capita. It serves as a 
             working model for how I will treat the remainder of my variables."),
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
           h3("Census Data"),
           p("Census Data contains many of the factors I am interested in using
             in these observations: urbanity, race, and possibly, hosuehold income. 
             I am interested in the first two because I often here these invoked
             in conversation gun violence. Particulary by those who push back 
             against corellation by gun violence. Though PMUS data exists and I
              would love to do something on a more local level, perhaps in 
              Virginia, I do not know whether the preceeding data exists for 
              state counties")),
  tabPanel("About", 
           titlePanel("About"),
           h3("Project Background and Motivations"),
           p("Hello, this is my final project on Gun Violence in progress. Right
             now, I am settling on ways of displaying the various factors on 
             Gun Violence which I am interested in exploring. I am using one
             such factor, state GDP per capita, as a working test of how to 
             process and display these reults."),
           h3("About Me"),
           p("My name is Nosa Lawani. I like to read  and plan to major in 
           Comparative Literature. You can reach me at nlawani@college.harvard.edu"),
           h3("Repo Link"),
           p("https://github.com/lawanin/Towards-Understanding-Gun-Homicide-in-the-50-States")))

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$crimegraph <- renderPlot({
    crimegdpfile %>%
      ggplot(aes(x = gdp_id, y = big_crime_per_capita)) +
      geom_point(alpha = 0.5) + 
      labs(title = "Gun Deaths per 100k by States in Order of Asecending GDP per Capita from 2004-2016", 
           subtitle = "The Linear Fit Suggests a Slight Decrease in Crime as States grow Richer, but this is hard to observe", 
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