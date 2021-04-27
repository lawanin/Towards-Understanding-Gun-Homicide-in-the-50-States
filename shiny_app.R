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
library(gt)
library(ggdist)
library(gtsummary)
library(rstanarm)
library(plotly)
source("model.R")
source("crimeeverythingdata.R") 

# Define UI for application that draws a histogram
ui <- navbarPage(
  "Towards Understanding Gun Homicide in the 50 States",
  tabPanel("Model", 
           h3("A Finding: The Interacting Effect of Single Parenthood and Urbanicity on Gun Deaths"),
           p("Below we have a graph of posteriors (i.e. shaded curves) showing  a finding from our 
             model of gun violence across the 50 states: the interaction between urbanicity
             and single parenthood on gun violence. Each shaded curve represents a hypothetical
             state with a ranking for single parenthood and urbanicity as shown by its color 
             and position on the y-axis, respectively. The range of values covered by the 
             curve on the x axis represents the range of values predicted for such a state; the
             x value at the peak of the graph corresponds to the gun death value of the highest
             liklihood, our best estimate. As we have done for all of our variables, 
             values for urbanicity and single parenthood are ranked from 1-50, indicating their 
             relative position among the United States. For the other variables in our model, a 
             ranking of 25 is assumed."),
           mainPanel(imageOutput("modelposteriors"), width = 12),
           p("Looking vertically at any one color, in states of increased  
ranking of urbanicity one observes an increase in gun deaths. Looking across any one 
row, one also observes an increase in gun deaths in states of increased single parenthood.
However, the rate of increase. These independent rates of increase, however, interact with 
one another to produce a greater overall increase. When comparing the row with 
urbanicity of rank 1 to that of urbanicity of rank 50, one sees that the increase
due to increasing ranks of single parenthood in row 1 is much lower than it is 
in row 50. In that view, one cay say the more urban  a state is the stronger
the effect of single parenthood on crime. Alternatively, if one compares, for example,  
the red and blue color groups across rows, one sees that in states with the lowest
single parenthood, the difference between the most and least urban state is less than 
one death per hundred, while in states of single parenthood rank 31, the difference 
between the most and least urban state is greater than 2 deaths per hundreed thousand.
In this view, one can say the greater the presence of single parenthood in a state, 
the more that state's cities will increase its number of gun deaths. This 
trend underscores the broader results of our model and work with the data: gun 
deaths across the United States are influenced not by any one factor, particularly
not only by gun ownership, but by many interacting moral factors."),
           h3("Our Model"),
           withMathJax(helpText('$$y_{i} = \\beta_0 + \\beta_1 urban\\_id_{i} + \\beta_2 gun\\_id_{i} + \\beta_3 single\\_id_{i} +
                                \\beta_4 black\\_id_{i} + \\beta_5 poverty\\_id_{i} +
                                 \\beta_6 urban\\_id_{i} * single\\_id_{i} + \\epsilon_{i}$$')),
           p("The following table shows the full results of our model. Above is the regression 
formula, for each characteristic a single parameter value has been calculated. Each value is to be
mutliplied by the character values specific to the given state (i). The intercept 
represents a nonsensical value for a state for which  all parameters were 0. The value 
for each parameter is the increase in gun deaths (added to the intercept) given one 
increase in a ranking. For example, when comparing a state with a difference of 10 
in gun ownership ranking,", em("ceteris paribus"), ", we expect to observe achange in 
.51  gun deaths per 100K people. For a state of the population of my home state 
of Virginia, 8.5 million, this would correspond to roughly 43 more gun deaths. 
The value for urban_id, as is visualized by the graphic above, should not be 
interpreted alone but with the addition of the interaction term urban_id*single_id. 
The value for urban_id should be multiplied by that for single_id and then by the 
value of the paramter for the interaction. The single most impactful parameter is 
black_id, which itself is indicative of the complex interaction of  
moral causes, many of which will particularly impact the American blacks due to 
their historical situation. Among this mix of variables, poverty_id is comparatively
insignificant and possibly 0, as we can see from the 95% Confidence Interval."),
           mainPanel(gt_output("regressiontable")),
           mainPanel(width = 12,    
           p(""),
           p("Before making conclusions from the findings of the model, one should
             carefully consider its limitations. The data from which the model is 
             created is large and exactly relevant for the population about which we 
             seek to make inferences: the contemporary United States. With few exceptions, the data includes
             values for every state over twelve years collected by trusted, most often, 
             governmental, organizations. These few exceptions are, however, worth
             interrogation to find out how omission of certain states and years
             from data sets may contribute to data systematically skewed from the ideal, complete
             set. Additionally, much of these data, including our principal measure of
             gun ownership, are themselves estimates. The fact that we do  use ranks
             instead of exact values lessens the importance of the fact, though we still
             assume that whatever comes from estimating is consistently distributed 
             among the states in a way such that it does not affect the states' relative order. 
             Though we have attempted to select a diverse range of relevant 
             factors in making our model, the factors we chose to analyze are far  
             from including all the factors that contribute to this issue in real 
             life. This absence affects both the factors that end up 
             yielding significance and the parameter values for these factors. Relatedly, 
             our model mostly only tracks indepent values for each factor and ignores
             the complex interaction between these that might occur in real life. This 
             inherent inexactitude of our model is significant and is represented 
             by the epsilon in our regression equation. Finally, even when simply 
             interpreting the mathematical results of our model and not comparing 
             them with the real world, there are significant interpretational questions, 
             as was seen in the two ways one could interpret causation in the interaction graphed above. 
             Another significant one, for example: by making this model we have implicitly assumed that 
             gun deaths is depedent on gun ownership and have found significant correlation. 
             Could we reasonably interpret the prevalence of guns (e.g. from law-abiding citizens
             who seek to protect themselves in dangerous areas, or dangerous people 
             who bring guns into their areas) as a function of gun violence?"))),
  tabPanel("Data",
           mainPanel(h2("Gun Deaths and Factors"),
                     p("On this page are plots of most of the variables examined,
                       including those not in the model, ranked and plotted against
                       gun deaths per 100K. No plot in any year will have data for 
                       a full 50 states, due first to limitations in FBI crime data
                       containing the supplementary information of weapons, and 
                       second to varied limitations in the data sets of the factors.
                       See the about page for details. For the time being, this page
                       provides the most comprehensive information on the source
                       and method of dealing with the data sets of each factor.")), 
  mainPanel(h3("Gun Deaths and Gun Ownership"),
             p("Plotted are gun deaths against states ranked by gun 
             ownership. Each ranking is determined by comparison with the 50 states 
             in a given year. The data for gun ownership are estimates based 
             on a 2020 RAND study which estimates household firearm ownership by
             state from 1980-2016. In that study, the average value for household 
             firearm ownership was found to be 38.5% and the average standard 
             error, 3.2%. Calculation of a linear fit of these data, yields a best 
             estimate of", strong("-.0016"), "for the slope, with a 95% confidence interval between
             -.014 and .00104. Below is a plot in which one can view each year's 
             set of rankings individually."),
             plotOutput("gungraph")),
  mainPanel(plotlyOutput("gungraphfiltered"),
            sliderInput("slidergun", "Year:",
                        min = 2004, max = 2016,
                        value = 2011, 
                        sep = "")),
  mainPanel(h3("Gun Deaths and Black Population"),
                     p("Plotted are gun deaths against states ranked by the proportion
                     of blacks in their population. Each ranking is determined by 
                     comparison with the 50 states in a given year. The data for
                     black population are based on 1-year estimates 
                     from the American Community Survey from 2005-2016. In those 
                     combined surveys, the average value for the black population 
                     was found to be 752269 and the average margin of error, 5240. 
                     Though trends exist among other racial demographics, the 
                     trend is by far the strongest and simplest among blacks, and 
                     so we have presented them here as the racial population which
                     might be most helpful in identifying causes of gun deaths. 
                     Calculation of a linear fit of these data, yields a best 
                     estimate of", strong(".1038"), "for the slope, with a 95% confidence
                     interval between .0096 and .1116. Below is a plot in which
                     one can view each year's set of rankings individually."),
                     plotOutput("racegraph")),
  mainPanel(plotlyOutput("racegraphfiltered"),
            sliderInput("sliderrace", "Year:",
                        min = 2005, max = 2016,
                        value = 2011,
                        sep = "")),
  mainPanel(h3("Gun Deaths and Urban Population"),
            p("Plotted are gun deaths against states ranked by the proportion
                     of urban-dwellers in the population. Each ranking is 
                     determined by comparison with the 50 states. The data for
                     urban population are the counts from the 2010 census. Thus, one 
                     set of rankings has been generated from the census and used
                     for each year. Calculation of a linear fit of these data, yields 
                     a best estimate of", strong(".0188"), "for the slope, with a 95% 
                     confidence interval between .0072 and .0304. Below is a 
                     plot in which one can view each year's set of rankings 
                     individually."),
            plotOutput("urbangraph")),
  mainPanel(plotlyOutput("urbangraphfiltered"),
            sliderInput("sliderurban", "Year:",
                        min = 2004, max = 2016,
                        value = 2011,
                        sep = "")),
  mainPanel(h3("Gun Deaths and Single Parenthood"),
            p("Plotted are gun deaths against states ranked by the proportion
                     of children living with single parents in their population. 
                     Each ranking is determined by comparison with the 50 states
                     in a given year. The data for children of single parents 
                     are based on 1-year estimates from the American 
                     Community Survey from 2005-2016. In those combined surveys, the average value for the black population 
                     was found to be 579152 and the average margin of error, 
                     14840. Calculation of a linear fit of these data, yields a 
                     best estimate of", strong(".0977"), "for the slope, with a 95% 
                     confidence interval between .0893 and .1061. Below is a 
                     plot in which one can view each year's set of rankings 
                     individually."),
            plotOutput("singlegraph")),
  mainPanel(plotlyOutput("singlegraphfiltered"),
            sliderInput("slidersingle", "Year:",
                        min = 2005, max = 2016,
                        value = 2011,
                        sep = "")),
  mainPanel(h3("Gun Deaths and the Poverty Line"),
            p("Plotted are gun deaths against states ranked by the proportion
                     of the population living below the poverty line. Each ranking
                     is determined by comparison with the 50 states in a given 
                     year. The data for those below the poverty line are 
                     based on 1-year estimates from the American Community Survey 
                     from 2005-2016. In those combined surveys, the average value 
                     for the black population was found to be 902146 and the 
                     average margin of error, 21429. Calculation of a linear fit 
                     of these data, yields a best estimate of", strong(".0656"), "for the 
                     slope, with a 95% confidence interval between .0552 and 
                     .0760 Below is a plot in which one can view each year's
                     set of rankings individually."),
            plotOutput("povertygraph")),
  mainPanel(plotlyOutput("povertygraphfiltered"),
            sliderInput("sliderpoverty", "Year:",
                        min = 2005, max = 2016,
                        value = 2011,
                        sep = "")),
  mainPanel(h3("Gun Deaths and 4th Grade Reading Scores"),
            p("Plotted are gun deaths against states ranked by the average 
                     reading achievement score  Each ranking is determined by 
                     comparison with the 50 states in a given year. The data for 
                     the reading assesment scores  are from the NAEP's report of
                     its biannual assesment in the years 1992, 1994, 1998, and 2002. 
                     Fourteen years were added to the year the test was taken, 
                     so that the average score in the new year represents the 
                     perfromanc of the year's 23-24 year olds as fourth graders. 
                     For years in which data was unavilable, data from the closest
                     year in which it was available was used as an estimate; no 
                     year was used to provide data for a year more than two years
                     from it. Calculation of a linear fit of these data, yields 
                     a best estimate of", strong("-.1057"), "for the slope, with
                     a 95% confidence interval between -.1181 and .0933 Below is
                     a plot in which one can view each year set 
                      of rankings individually."),
            plotOutput("readinggraph")),
  mainPanel(plotlyOutput("readinggraphfiltered"),
            sliderInput("sliderreading", "Year:",
                        min = 2004, max = 2016,
                        value = 2011,
                        sep = "")),
  mainPanel(h3("Gun Deaths and Averaged Freshman Graduation Rate (NCES)"),
            p("Plotted are gun deaths against states ranked by the rate
                     of the freshman graduate public school in four years. Each 
                     ranking is determined by comparison with the 50 states in a
                     given year. The data for the freshman graduation rate are 
                     based on estimates from the National Center of Education 
                     Statistics in the years 1990-2009. Six years were added to 
                     the year a rate was determined, so that the rate in the new year 
                     represents the rate at which the year's 23-24 year olds, among the most 
                     likely ages to commit murder, would have graduated from public
                     high school in four year. Calculation of a linear fit of 
                     these data, yields a best estimate of", strong("-.0653"), 
                     "for the slope, with a 95% confidence interval between -.0757
                     and .0552 Below is a plot in which one can view each year 
                     set of rankings individually."),
            plotOutput("graduationgraph")),
  mainPanel(plotlyOutput("graduationgraphfiltered"),
            sliderInput("slidergraduation", "Year:",
                        min = 2004, max = 2015,
                        value = 2011,
                        sep = "")),
  mainPanel(h3("Gun Deaths and GDP per Capita"),
            p("Plotted are gun deaths against states ranked by annual GDP  
                     capita. Each ranking is determined by comparison with the 50 
                     states in a given year. The data for GDP per capita are 
                     based on estimates from the Bureau of Labor Statistics
                     from 2004 to 2016 in 2012 dollars.  Calculation of a linear
                     fit of these data, yields a best estimate of", 
                     strong("-.0187"), "for the slope, with a 95% confidence interval 
                     between -.0303 and -.0071. Below is a plot in which one can 
                     view each year's set of rankings individually."),
            plotOutput("gdpgraph")),
  mainPanel(plotlyOutput("gdpgraphfiltered"),
            sliderInput("slidergdp", "Year:",
                        min = 2004, max = 2016,
                        value = 2011,
                        sep = "")),
  mainPanel(h3("Gun Deaths and Abortion"),
            p("Plotted are gun deaths against states ranked by the rate (abortions / women) of abortions
                     among women aged 20-24. Each ranking is determined by comparison 
                     with the 50 states in a given year. The data for abortion rates are 
                     based on estimates from the Guttmacher Institute. The age 20-24 displayed
                     the strongest positive correlation, though neighboring age groups
                     yield similar values. The most notable exceptions are at the extremes, 
                     for women 40+ the best estimate for the slope of the linear fit is
                     only .066, and for women under 15, this value is -.0111. Calculation of a linear
                     fit of these data, yields a best estimate of", 
              strong(".0318"), "for the slope, with a 95% confidence interval 
                     between .0210 and .0426. Below is a plot in which one can 
                     view each year's set of rankings individually."),
            plotOutput("abortiongraph")),
  mainPanel(plotlyOutput("abortiongraphfiltered"),
            sliderInput("sliderabortion", "Year:",
                        min = 2005, max = 2016,
                        value = 2011,
                        sep = "")),
           ),
  tabPanel("Tables",
            mainPanel(h3("States by Average Gun Ownership Ranking and Household Gun Ownership Rate"),
                      dataTableOutput("gunorder")),
           mainPanel(h3("States by Black Ranking and Average Percetange of Black Population"),
                     dataTableOutput("blackorder")),
           mainPanel(h3("States by Urban Ranking and Average Percetange of Population in Urban Areas"),
                     dataTableOutput("urbanorder")),
           mainPanel(h3("States by Single Parenthood Ranking and Average Percetange of Children Living With Single Parents"),
                     dataTableOutput("singleorder")),
           mainPanel(h3("States by 4th Grade Reading Ranking and Average Average Score of 4th Graders"),
                     dataTableOutput("reading4order")),
           mainPanel(h3("States by Freshman Graduation Ranking and Average Percent of Freshman Who Graduated in Four Years"),
                     dataTableOutput("graduationorder")),
           mainPanel(h3("States by GDP Ranking and Average GDP per Capita in Thousands"),
                     dataTableOutput("gdporder")),
           mainPanel(h3("States by 20-24 year old Abortion Ranking and Average 20-24 year old Abortion Rate"),
                     dataTableOutput("abortionorder")),
  ),
           
  tabPanel("About", 
           titlePanel("About"),
           h3("Project Background and Motivations"),
           p("This is my final project, Towards Understanding Gun Homicide in the 
             50 States. This is a topic I have often heard passionately discussed, 
             but, to me, often with a dissatisfying amount of data behind 
             claims. The debate is most often heard in connection to gun policy. 
             Very often, the U.S. is compared to other countries with much more
             gun restrictions and much less gun homicide. While this may be useful
             in comparing the effects of different policies, these often
             fail to take the influence of differing demographics on the rates, 
             and so does not convincingly argue for the primacy of gun ownership
             as a cause when compared to other demographic factors. On the other
             side, one often hears the mantra, guns don't kill people, people 
             kill people. While this is plausible, I have often failed to hear 
             citations of data along with this. Making a comparison across the 
             50 states, my project researches and models the effect of both 
             gun ownership and other demographic factors. The goal is not to 
             determine the efficacy of gun policy, but to examine potentially
             informative correlates of gun deaths in the United States in hope
             of better understanding its causes."),
           p("Moving from the things I had already heard and read and looking
             more closely at the relevant literature, I saw that studies using 
             data analysis to assess the efficacy of gun ownership exist, though
             with comparable controvery. Siegel et. al. (2013) and Kovandzic et. 
             al. (2013) have most recently written on this topic and come to differing
             conclusions. This page humbly begins to insert itself into their discussion,
             but with two key differences. First, both studies used the most well-established
             proxy of household firearm ownership, the ratio of firearm suicides to
             all suicides. This analysis, however, uses the recently relased RAND 
             estimates of household firearm ownership which incoorporates a host of 
             other, and in many cases, much more direct, measures of firearm ownership
             into its estimate. Second, both analyses focus on the effect of gun ownership.
             This page seeks more broadly to present information on some of the many
             correlates and possible causes of gun homicide in the United States."),
           h3("Note on FBI Data"),
           p("This is the most important data in my project, as I will be estimating
           how it is influenced by different factos across the 50 states. My source
           for this has primarily been the annual FBI statistics. In this annual report, 
           there are data for all crime by state and type of crime, and then data for crime
           with supplental info, whichincludes  with type of weapon, by state 
           and type of crime. Most states do not report supplemental info for every 
           crime committed, and so the proportion of homicide deaths with the 
           weapon specified varies to  homicide deaths in the general crime 
           statistics vary from year to year and from state to state. For states and years
           where this proportion is greater than 75%, I have used the rate of 
           gun homicide to homicide to estimate the total gun homicides for those states 
           and years. In IL for almost every year and AL for some years, the FBI 
           notes that these states have underreported crimes with supplemental info. 
           By our 75% standard, a total of 61 states and years were not given 
           a value for gun homicide per capita. This includes states other 
           than IL and AL, and some years of these two states were representative 
           enough to be given figures in our method. In every year, Florida was 
           excluded because its supplemental info was not compliant with FBI 
           requirements. On the state website for Florida, however, they have their
           own supplemental info for crime in all years, which have been used to 
           form the estimates for total gun deaths in this state."),
           h3("About Me"),
           p("My name is Nosa Lawani. I like to read  and plan to major in 
           Comparative Literature. You can reach me at nlawani@college.harvard.edu"),
           h3("Repo Link"),
           p("https://github.com/lawanin/Towards-Understanding-Gun-Homicide-in-the-50-States")))
# Define server logic required to draw a histogram
server <- function(input, output) {
  output$gungraph <- renderPlot({
    crimeeverythingfile %>%
      ggplot(aes(x = gun_id, y = big_crime_per_capita)) +
      geom_jitter(alpha = 0.5, 
                  height = 0.2, 
                  width = 0.2) + 
      labs(title = "Gun Deaths by States in Order of Proportion of Gun Owners from 2004-2016", 
           x = "States in order of Proportion of Gun Owners", 
           y = "Gun Deaths per 100K",
           caption = "FBI (2004-2016), RAND (2020)") +
      scale_x_continuous(breaks = c(1 : 50)) +
      geom_smooth()
  })
  output$gungraphfiltered <- renderPlotly({
    ggplotly(
    crimeeverythingfile %>%
      filter(year == input$slidergun) %>% 
      ggplot(aes(x = gun_id, y = big_crime_per_capita,
                 text = paste("State:", state, "\n",
                              "Year:", year, "\n", 
                              "Deaths:", round(big_crime_per_capita, digits = 3),
                              sep = ""))) +
      geom_jitter(alpha = 0.5, 
                  height = 0.2, 
                  width = 0.2) + 
      labs(title = "Gun Deaths by States in Order of Proportion of Gun Ownership from 2004-2016", 
           x = "States in order of Proportion of Gun Owners", 
           y = "Gun Deaths per 100K",
           caption = "FBI (2004-2016), RAND (2020)") +
      scale_x_continuous(breaks = c(1 : 50)),
    tooltip = "text")
  })
  output$racegraph <- renderPlot({
    crimeeverythingfile %>%
      ggplot(aes(x = black_id, y = big_crime_per_capita)) +
      geom_jitter(alpha = 0.5, 
                  height = 0.2, 
                  width = 0.2) + 
      labs(title = "Gun Deaths by States in Order of Proportion of Blacks in Population from 2005-2016", 
           x = "States in Order of Proportion of Blacks", 
           y = "Gun Deaths per 100K",
           caption = "FBI (2005-2016), American Community Survey (2005-2016)") +
      scale_x_continuous(breaks = c(1 : 50)) +
      geom_smooth() 
  })
  output$racegraphfiltered <- renderPlotly({
    ggplotly(
    crimeeverythingfile %>%
      filter(year == input$sliderrace) %>%
      ggplot(aes(x = black_id, y = big_crime_per_capita,
                 text = paste("State:", state, "\n",
                              "Year:", year, "\n", 
                              "Deaths:", round(big_crime_per_capita, digits = 3),
                              sep = ""))) +
      geom_jitter(alpha = 0.5, 
                  height = 0.2, 
                  width = 0.2) + 
      labs(title = "Gun Deaths by States in Order of Proportion of Blacks in Population from 2005-2016", 
           x = "States in Order of Proportion of Blacks", 
           y = "Gun Deaths per 100K",
           caption = "FBI (2005-2016), American Community Survey (2005-2016)") +
      scale_x_continuous(breaks = c(1 : 50)) +
      geom_smooth(), 
    tooltip = "text")
  })
  
  output$urbangraph <- renderPlot({
    crimeeverythingfile %>%
      ggplot(aes(x = urban_id, y = big_crime_per_capita)) +
      geom_jitter(alpha = 0.5, 
                  height = 0.2, 
                  width = 0.2) + 
      labs(title = "Gun Deaths by States in Order of  Proportion of Urban Population from 2004-2016", 
           x = "States in Order of Proportion of Urban Population", 
           y = "Gun Deaths per 100K",
           caption = "FBI (2004-2016), U.S. Census (2011)") +
      scale_x_continuous(breaks = c(1 : 50)) +
      geom_smooth() 
  })
  output$urbangraphfiltered <- renderPlotly({
    ggplotly(
    crimeeverythingfile %>%
      filter(year == input$sliderurban) %>%
      ggplot(aes(x = urban_id, y = big_crime_per_capita,
                 text = paste("State:", state, "\n",
                              "Year:", year, "\n", 
                              "Deaths:", round(big_crime_per_capita, digits = 3),
                              sep = ""))) +
      geom_jitter(alpha = 0.5, 
                  height = 0.2, 
                  width = 0.2) + 
      labs(title = "Gun Deaths by States in Order of Proportion of Urban Population from 2004-2016",
           x = "States in Order of Proportion of Urban Population", 
           y = "Gun Deaths per 100K",
           caption = "FBI (2004-2016), U.S. Census (2011)") +
      scale_x_continuous(breaks = c(1 : 50)) +
      geom_smooth(), 
    tooltip = "text")
  })
  
  output$singlegraph <- renderPlot({
    crimeeverythingfile %>%
      ggplot(aes(x = single_id, y = big_crime_per_capita)) +
      geom_jitter(alpha = 0.5, 
                  height = 0.2, 
                  width = 0.2) + 
      labs(title = "Gun Deaths by States in Order of Proportion of Children with Single Parents from 2005-2016", 
           x = "States in Order of Proportion of Children with Single Parents", 
           y = "Gun Deaths per 100k", 
           caption = "FBI (2005-2016), American Community Survey (2005-2016)") +
      scale_x_continuous(breaks = c(1 : 50)) +
      geom_smooth() 
  })
  output$singlegraphfiltered <- renderPlotly({
    ggplotly(
    crimeeverythingfile %>%
      filter(year == input$slidersingle) %>%
      ggplot(aes(x = single_id, y = big_crime_per_capita,
                 text = paste("State:", state, "\n",
                              "Year:", year, "\n", 
                              "Deaths:", round(big_crime_per_capita, digits = 3),
                              sep = ""))) +
      geom_jitter(alpha = 0.5, 
                  height = 0.2, 
                  width = 0.2) + 
      labs(title = "Gun Deaths by States in Order of Proportion of Children with Single Parents from 2005-2016",
           x = "States in Order of Proportion of Children with Single Parents", 
           y = "Gun Deaths per 100K",
           caption = "FBI (2005-2016), American Community Survey (2005-2016)") +
      scale_x_continuous(breaks = c(1 : 50)) +
      geom_smooth(),
    tooltip = "text")
  })
  
  output$povertygraph <- renderPlot({
    crimeeverythingfile %>%
      ggplot(aes(x = poverty_id, y = big_crime_per_capita)) +
      geom_jitter(alpha = 0.5, 
                  height = 0.2, 
                  width = 0.2) + 
      labs(title = "Gun Deaths by States in Order of Proportion Below Poverty Line from 2005-2016", 
           x = "States in Order of Proportion Below Poverty Line", 
           y = "Gun Deaths per 100K",
           caption = "FBI (2005-2016), American Community Survey (2005-2016)") +
      scale_x_continuous(breaks = c(1 : 50)) +
      geom_smooth() 
  })
  output$povertygraphfiltered <- renderPlotly({
    ggplotly(
    crimeeverythingfile %>%
      filter(year == input$sliderpoverty) %>%
      ggplot(aes(x = poverty_id, y = big_crime_per_capita,
                 text = paste("State:", state, "\n",
                              "Year:", year, "\n", 
                              "Deaths:", round(big_crime_per_capita, digits = 3),
                              sep = ""))) +
      geom_jitter(alpha = 0.5, 
                  height = 0.2, 
                  width = 0.2) + 
      labs(title = "Gun Deaths by States in Order of Proportion Below Poverty Line from 2005-2016",
           x = "States in Order of Proportion of Proportion Below Poverty Line", 
           y = "Gun Deaths per 100K",
           caption = "FBI (2005-2016), American Community Survey (2005-2016)") +
      scale_x_continuous(breaks = c(1 : 50)) +
      geom_smooth(), 
    tooltip = "text")
  })
  
  output$readinggraph <- renderPlot({
    crimeeverythingfile %>%
      ggplot(aes(x = reading4_id, y = big_crime_per_capita)) +
      geom_jitter(alpha = 0.5, 
                  height = 0.2, 
                  width = 0.2) + 
      labs(title = "Gun Deaths by States in Order of 4th Grade Reading Scores from 2004-2016", 
           x = "States in Order of Freshman Graduation Rate", 
           y = "Gun Deaths per 100K",
           caption = "FBI (2004-2016), NAEP (1992, 1994, 1998, 2002)") +
      scale_x_continuous(breaks = c(1 : 50)) +
      geom_smooth() 
  })
  output$readinggraphfiltered <- renderPlotly({
    ggplotly(
    crimeeverythingfile %>%
      filter(year == input$sliderreading) %>%
      ggplot(aes(x = reading4_id, y = big_crime_per_capita,
                 text = paste("State:", state, "\n",
                              "Year:", year, "\n", 
                              "Deaths:", round(big_crime_per_capita, digits = 3),
                              sep = ""))) +
      geom_jitter(alpha = 0.5, 
                  height = 0.2, 
                  width = 0.2) + 
      labs(title = "Gun Deaths by States in Order of 4th Grade Reading Scores from 2004-2016",
           x = "States in Order of Freshman Graduation Rate",
           y = "Gun Deaths per 100K",
           caption = "FBI (2004-2016), NAEP (1992, 1994, 1998, 2002)") +
      scale_x_continuous(breaks = c(1 : 50)) +
      geom_smooth(), 
    tooltip = "text")
  })
  
  output$graduationgraph <- renderPlot({
    crimeeverythingfile %>%
      ggplot(aes(x = graduation_id, y = big_crime_per_capita)) +
      geom_jitter(alpha = 0.5, 
                  height = 0.2, 
                  width = 0.2) + 
      labs(title = "Gun Deaths by States in Order of Freshman Graduation Rate from 2004-2015", 
           x = "States in Order of Freshman Graduation Rate", 
           y = "Gun Deaths per 100K",
           caption = "FBI (2004-2015), NCES (1998-2010)") +
      scale_x_continuous(breaks = c(1 : 50)) +
      geom_smooth() 
  })
  output$graduationgraphfiltered <- renderPlotly({
    ggplotly(
    crimeeverythingfile %>%
      filter(year == input$slidergraduation) %>%
      ggplot(aes(x = graduation_id, y = big_crime_per_capita,
                 text = paste("State:", state, "\n",
                              "Year:", year, "\n", 
                              "Deaths:", round(big_crime_per_capita, digits = 3),
                              sep = ""))) +
      geom_jitter(alpha = 0.5, 
                  height = 0.2, 
                  width = 0.2) + 
      labs(title = "Gun Deaths by States in Order of Freshman Graduation Rate from 2004-2015",
           x = "States in Order of Freshman Graduation Rate",
           y = "Gun Deaths per 100K",
           caption = "FBI (2004-2015), NCES (1998-2010)") +
      scale_x_continuous(breaks = c(1 : 50)) +
      geom_smooth(), 
    tooltip = "text")
  })
  
  output$gdpgraph <- renderPlot({
    crimeeverythingfile %>%
      ggplot(aes(x = gdp_id, y = big_crime_per_capita)) +
      geom_jitter(alpha = 0.5,
                  height = 0.2,
                  width = 0.2) +
      labs(title = "Gun Deaths by States in Order of GDP per Capita  from 2004-2016",
           x = "States in Order of GDP per Capita",
           y = "Gun Deaths per 100K",
           caption = "FBI (2004-2016), BLS (2004-2016)") +
      scale_x_continuous(breaks = c(1 : 50)) +
      geom_smooth()
  })
  output$gdpgraphfiltered <- renderPlotly({
    ggplotly(
    crimeeverythingfile %>%
      filter(year == input$slidergdp) %>%
      ggplot(aes(x = gdp_id, y = big_crime_per_capita,
                 text = paste("State:", state, "\n",
                              "Year:", year, "\n", 
                              "Deaths:", round(big_crime_per_capita, digits = 3),
                              sep = ""))) +
      geom_jitter(alpha = 0.5,
                  height = 0.2,
                  width = 0.2) +
      labs(title = "Gun Deaths by States in Order of GDP per Capita from 2004-2016",
           x = "States in Order of GDP per Capita",
           y = "Gun Deaths per 100K",
           caption = "FBI (2004-2016), BLS (2004-2016)") +
      scale_x_continuous(breaks = c(1 : 50)) +
      geom_smooth(),
    tooltip = "text")
  })
  
  output$abortiongraph <- renderPlot({
    crimeeverythingfile %>%
      ggplot(aes(x = abortion_id, y = big_crime_per_capita)) +
      geom_jitter(alpha = 0.5,
                  height = 0.2,
                  width = 0.2) +
      labs(title = "Gun Deaths by States in Order of Abortion Rate of 20-24 year olds from 2004-2016",
           x = "States in Order of 20-24 year old Abortion Rate",
           y = "Gun Deaths per 100K",
           caption = "FBI (2005-2016), Guttmacher Inst. (2005-2016)") +
      scale_x_continuous(breaks = c(1 : 50)) +
      geom_smooth()
  })
  output$abortiongraphfiltered <- renderPlotly({
    ggplotly(
    crimeeverythingfile %>%
      filter(year == input$sliderabortion) %>%
      ggplot(aes(x = abortion_id, y = big_crime_per_capita,
                 text = paste("State:", state, "\n",
                              "Year:", year, "\n", 
                              "Deaths:", round(big_crime_per_capita, digits = 3),
                              sep = ""))) +
      geom_jitter(alpha = 0.5,
                  height = 0.2,
                  width = 0.2) +
      labs(title = "Gun Deaths by States in Order of Abortion Rate of 20-24 year olds from 2004-2016",
           x = "States in Order of 20-24 year old Abortion Rate",
           y = "Gun Deaths per 100K",
           caption = "FBI (2005-2016), Guttmacher Inst. (2005-2016)") +
      scale_x_continuous(breaks = c(1 : 50)) +
      geom_smooth(), 
    tooltip = "text")
  })


  output$gunorder <- renderDataTable(
    gun_order,
    options = list(pageLength  = 5,
                   lengthMenu = c(5, 10, 15, 25, 50))
  )
  
  output$blackorder <- renderDataTable(
    black_order,
    options = list(pageLength  = 5,
                   lengthMenu = c(5, 10, 15, 25, 50))
)
  
  output$urbanorder <- renderDataTable(
    urban_order,
    options = list(pageLength  = 5,
                   lengthMenu = c(5, 10, 15, 25, 50))
  )
  
  output$singleorder <- renderDataTable(
    single_order,
    options = list(pageLength  = 5,
                   lengthMenu = c(5, 10, 15, 25, 50))
  )
  
  output$povertyorder <- renderDataTable(
    poverty_order,
    options = list(pageLength  = 5,
                   lengthMenu = c(5, 10, 15, 25, 50))
  )
  
  output$reading4order <- renderDataTable(
    reading4_order,
    options = list(pageLength  = 5,
                   lengthMenu = c(5, 10, 15, 25, 50))
  )
  
  output$graduationorder <- renderDataTable(
     graduation_order,
     options = list(pageLength  = 5,
                    lengthMenu = c(5, 10, 15, 25, 50))
  )
  
  output$gdporder <- renderDataTable(
    gdp_order,
    options = list(pageLength  = 5,
                   lengthMenu = c(5, 10, 15, 25, 50))
  )
  
  output$abortionorder <- renderDataTable(
    abortion_order,
    options = list(pageLength  = 5,
                   lengthMenu = c(5, 10, 15, 25, 50))
  )
  
  output$modelposteriors <- renderPlot({
    model_posteriors
  })
  output$regressiontable <- render_gt({
    rtable
  })
}

# Run the application 
shinyApp(ui = ui, server = server)