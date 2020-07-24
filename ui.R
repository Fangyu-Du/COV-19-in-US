
# load library
library(tidyverse)
library(dplyr)
library(ggplot2)
library(plotly)
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(scales)
library(stringr)
library(readr)
library(DT)
library(leaflet)
library(sf)
library(sp)
library(rgeos)
library(maptools)
library(ggalt)
library(viridis)
library(ggmap)

# read updated data file
CsvUrl <- url("https://data.covidactnow.org/latest/us/states.OBSERVED_INTERVENTION.timeseries.csv")
data <- read.csv(CsvUrl)
# read local data file
Sys.setlocale("LC_ALL", "C")
pop <- read.csv("population.csv")

# data cleaning
data <- data %>% merge(pop, by.x = "stateName", by.y = "State") %>%
    select(c(1:6, 11:15, 22, 24)) %>%
    mutate(date = as.Date(data$date), lastUpdatedDate = as.Date(data$lastUpdatedDate)) 
colnames(data) <- c("State", "Date", "BedReq", "BedCap", "ICUUse", "ICUCap", 
                    "CumDeath", "CumInf", "CurInf", "CurSus", "CurExp", "UpdatedDate","Pop")

# calculate some new rate index
data$CurInfRate <- data$CurInf / data$Pop
data$CumInfRate <- data$CumInf / data$Pop
data$CumDeathRate <- data$CumDeath / data$Pop
data$BedRate <- data$BedReq / data$BedCap
data$ICURate <- data$ICUUse / data$ICUCap
data$Type <- case_when(data$Date<data$UpdatedDate ~ "Record", 
                       data$Date>=data$UpdatedDate ~ "Forecast")


# define details
palette1 <- c("dodgerblue1", "black")
palette2 <- c("dodgerblue2", "black")

##########
### UI ###
##########

ui <- dashboardPagePlus(
    
    # dashboard header
    dashboardHeaderPlus(title = "State Alarm"),
    
    # dashboard sidebar
    dashboardSidebar(
        sidebarMenu(
            menuItem("Introduction", tabName = "Intro", icon = icon("dashboard")),
            menuItem("State Ranking", tabName = "BarChart", icon = icon("bar-chart")),
            menuItem("Situation Map", tabName = "Map", icon = icon("map-marker")),
            menuItem("State Exploration", tabName = "LineChart", icon = icon("line-chart"))
        )
    ),
    
    # dashboard body
    dashboardBody(
        tabItems(
            
            ####################
            ### tab 1: Intro ###
            ####################
            tabItem(tabName = "Intro",
                    fluidPage(
                        tags$h1("About",style = "font-family:Impact"),
                        tags$p("COVID-19 is still rampant in the United States. According to the coronavirus map from Johns Hopkins University, the total positive cases have reached 3.8 million by July,20,2020. We are experiencing this disaster and the current situation is not quite optimistic. In that case, we try to analyze the COVID-19 situation by collecting data from March until predicted October."),
                        tags$p("This interactive web application is built to explore data to make a warning system about each state based on the infection growth rate, positive test rate and ICU capacity. Our first tab you will see the state ranking. We try to show the top 10 of the states by comparing monthly death rate, monthly infected percentage and monthly growth rate. The second tab features a nationwide map where you could select a feature or multiple features to plot like the cumulative infected and choose a specific date. When you click on one of the states, the detailed information about the index will show. The third tab highlights various characteristics including current infection rate, cumulative infection rate, cumulative death rate, hospital bed usage rate and ICU bed usage rate. We have three sub-tabs to show the nationwide, the comparison of three specific states and exploration of one state based on the above indexes."),
                        tags$hr(),
                        tags$h1("Research Question",style = "font-family:Impact"),
                        tags$p("1.	What are the top 10 states we need to warn of about the most monthly death rate, monthly infected percentage and monthly case growth rate?"),
                        tags$p("2.	What are the nationwide COVID-19 situation based on the chosen indexes and specific date?"),
                        tags$p("3.	What trends could we explore when we compare each state with the above characteristics?"),
                        tags$hr(),
                        tags$h1("Demo",style = "font-family:Impact"),
                        tags$p("For specific details on this project, including how to interact with the visualizations, please watch the below video."),
                        HTML('<iframe width="50%" height="300" 
                  src="https://www.youtube.com/embed/IrbMJ0vCT4k"
                  frameborder="0" allowfullscreen></iframe>'),
                        tags$hr(),
                        tags$h1("COVID-19 Data Set Information",style = "font-family:Impact"),
                        p("Our data set is related to COVID-19 cases in the United States and comes from ",
                          a("covidactnow.org",
                            href = "https://covidactnow.org/?s=733125"),", an open source system about COVID-19 data and detailed exploration of the cases. The dataset we choose is within the range of the United States documenting those key indexes related to COVID-19 like current infection case number, cumulative infection case number, cumulative death case number, number of hospital bed in use and number of ICU bed in use. The dataset we used includes observations with variables. The full dataset is available ",
                          a("at this link.",
                            href = "https://blog.covidactnow.org/export-covid-act-now-data-spreadsheet/")),
                        tags$hr(),
                        h1("References and Further Reading",style = "font-family:Impact"),
                        p("References and additional reading items."),
                        p("-",
                          a("https://coronavirus.jhu.edu/us-map",
                            href = "https://coronavirus.jhu.edu/us-map")),
                        p("-",
                          a("American COVID warning system",
                            href = "https://covidactnow.org/?s=733125")),
                        tags$hr(),
                        h1("Team",style = "font-family:Impact"),
                        column(12, 
                               widgetUserBox(
                                   title = "Zhuohan(Hannah) Lei",
                                   subtitle = "Johns Hopkins University Carey Business School (BARM)",
                                   width = 6, 
                                   type = NULL,
                                   color = "orange",
                                   "A lovely girl ~",
                                   footer = "Hannah Lei is currently pursuing a master degree at Johns Hopkins University, majoring in Business Analytics and Risk Management. She is passionate about works related to data analytics and risk management and seeks to combine analytical skills with International trade and economy knowledge gaining during undergraduate period in her future career path. She has just completed an internship as a risk analyst in Credit Suisse Securities(NY) and benefited a lot."
                               ),
                               widgetUserBox(
                                   title = "Xingzhou(Jason) Zhu",
                                   subtitle = "Johns Hopkins University Carey Business School (BARM)",
                                   width = 6, 
                                   type = NULL,
                                   "A hansome boy ~",
                                   color = "aqua-active",
                                   footer = "Jason is a master candidate at Johns Hopkins Carey Business school and focus on business analytics and risk management. He is looking forward to finding a job related to consulting and data analysis. He is humorous and always tells jokes to his teammates. He cares about the things happen nationwide and international which he will comment on and write articles about. He could find a good balance between learning and relaxing."
                               )),
                        column(12,
                               widgetUserBox(
                                   title = "Yanyi(Cora) Li",
                                   subtitle = "Johns Hopkins University Carey Business School (BARM)",
                                   width = 6, 
                                   type = NULL,
                                   color = "orange",
                                   "A smart girl ~",
                                   footer = "Cora Li is a graduating student in the Business Analytics and Risk Management program at Johns Hopkins Carey Business School. She is interested in data analytics and has internship experiences in a startup data company. She will enjoy her career and life-long journey beginning from the Industrial and Commercial Bank of China (ICBC)."
                               ),
                               widgetUserBox(
                                   title = "Fangyu(Cherry) Du",
                                   subtitle = "Johns Hopkins University Carey Business School (BARM)",
                                   width = 6, 
                                   type = NULL,
                                   color = "aqua-active",
                                   "A beautiful girl ~",
                                   footer = "Fangyu Du is currently an MS in Business Analytics and Risk Management student at Johns Hopkins Carey Business School with Finance background. Skilled at business development, product optimization, and data analytics. Her past experience was financial consultant in China Merchants Bank."
                               ))
                    )
            ),
            
            ####################
            ### tab 2: Bar   ###
            ####################      
            tabItem(tabName = "BarChart",
                    h2("State Ranking"),
                    fluidRow(width = 12,
                             box(plotlyOutput("plot1", height = 250), solidHeader = TRUE, width = 9),
                             box(solidHeader = TRUE, status = "primary", width= 3,
                                 sliderInput("month", "Month(2020):", 
                                             min = 3, max = 7, value = 1, step = 1, 
                                             animate = animationOptions(interval = 2000, loop = FALSE)))
                    ),
                    fluidRow(width = 12,
                             box(plotlyOutput("plot2", height = 250), solidHeader = TRUE, width = 9),
                             box(background = "red", uiOutput("text1"), width = 3, height = 270)
                    ),
                    fluidRow(width = 12,
                             box(plotlyOutput("plot3", height = 250), solidHeader = TRUE, width = 9)
                    )
                    
            ),
            
            ##################
            ### tab 3: Map ###
            ##################
            tabItem(tabName = "Map",
                    h2("Situation Map"),
                    fluidRow(
                        boxPlus(
                            width = 12,
                            title = "US COV-19 Map", 
                            closable = TRUE, 
                            status = "warning", 
                            solidHeader = FALSE, 
                            collapsible = TRUE,
                            enable_sidebar = TRUE,
                            sidebar_width = 25,
                            sidebar_start_open = TRUE,
                            
                            sidebar_content = tagList(  
                                selectInput("Statecharacter","Select a feature to plot",c("Cumulative Infected"="cumulativeInfected","Current Infected"="currentInfected","Cumulative Death"="cumulativeDeaths"),selected ="cumulativeInfected"),
                                dateInput("date",
                                          label = 'Select Date :',
                                          value = Sys.Date() )
                                
                                
                            ),
                            column(width=10,
                                   leafletOutput("myMap", width="100%")),
                            column(width = 2,
                                   plotOutput("boxSidebarPlot"))
                        )),  
                    
                    
                    fluidRow(
                        gradientBox(
                            title = "State level",
                            width = 12,
                            selectInput("singlestate","Choose a State",c("Alaska"="Alaska","Alabama"="Alabama","Arizona"="Arizona","Arkansas"= "Arkansas" ,  "California"="California","Colorado"= "Colorado","Connecticut"= "Connecticut",        
                                                                         "District of Columbia"="District of Columbia", "Georgia"="Georgia", "Illinois"="Illinois",  "Indiana"="Indiana", "Louisiana"="Louisiana" ,          
                                                                         "Minnesota"="Minnesota","Mississippi"="Mississippi", "Montana"="Montana","New Mexico"="New Mexico","North Dakota"="North Dakota",        
                                                                         "Oklahoma"="Oklahoma", "Pennsylvania"="Pennsylvania",  "Tennessee"="Tennessee","Virginia"="Virginia","Delaware"="Delaware" ,           
                                                                         "West Virginia"="West Virginia","Wisconsin"="Wisconsin","Wyoming"="Wyoming", "Florida"="Florida" ,            
                                                                         "Idaho"="Idaho" ,"Kansas"="Kansas","Maryland"="Maryland"," New Jersey"= "New Jersey"," North Carolina"= "North Carolina",      
                                                                         "South Carolina"="South Carolina", "Washington"="Washington", "Vermont"="Vermont" ,"Utah"="Utah" ,"Iowa"="Iowa",                
                                                                         "Kentucky"="Kentucky" , "Maine"="Maine", "Massachusetts"="Massachusetts",  "Michigan"="Michigan", "Missouri"="Missouri" ,           
                                                                         "Nebraska"="Nebraska", "Nevada"="Nevada", "New Hampshire"="New Hampshire" , "New York"="New York", "Ohio"="Ohio",                
                                                                         "Oregon"="Oregon", " Rhode Island "=" Rhode Island " ,"South Dakota"="South Dakota" ,"Texas"="Texas",                             
                                                                         "Hawaii"="Hawaii"  )),
                            icon = "fa fa-heart",
                            gradientColor = "blue", 
                            boxToolSize = "xs", 
                            closable = TRUE,
                            footer =  plotlyOutput("plot4")
                            
                        ))),
            
            ########################
            ### tab 4: LineChart ###
            ########################
            tabItem(tabName = "LineChart",
                    
                    h2("State Exploration"),
                    
                    tabBox(
                        title = span("Visualized States Exploration", style = "color: SteelBlue"),
                        id = "LineChartBox",
                        height = "250px",
                        width = 16,
                        
                        ### subtab1: all states
                        tabPanel(span("Skim All States", style = "color: SteelBlue"),
                                 
                                 # radio
                                 fluidRow(
                                     box(
                                         title = span("Select an index you want to compare",
                                                      style = "color: SteelBlue"),
                                         width = 12, status = "primary", 
                                         radioButtons("radio1", label = NULL,
                                                      choices = c("Current infection rate",
                                                                  "Cummulative infection rate",
                                                                  "Cummulative death rate", 
                                                                  "Hospital bed usage rate",
                                                                  "ICU bed usage rate"), 
                                                      selected = "Current infection rate")
                                     )
                                 ),
                                 
                                 # line chart panel
                                 fluidRow(
                                     box(
                                         title = "Dashboard for all states",
                                         width = 12, status = "primary", solidHeader = TRUE,
                                         fluidRow(plotOutput("plotl1")),
                                         fluidRow(column(12,"The black line represents records, and blue line represents forecasts."))
                                     )
                                 )
                        ),
                        
                        ### subtab2: three states
                        tabPanel(span("Compare Three States", style = "color: SteelBlue"),
                                 
                                 # radio
                                 fluidRow(
                                     box(
                                         title = span("Select an index you want to compare",
                                                      style = "color: SteelBlue"),
                                         width = 12, status = "primary", 
                                         radioButtons("radio2", label = NULL,
                                                      choices = c("Current infection rate",
                                                                  "Cummulative infection rate",
                                                                  "Cummulative death rate", 
                                                                  "Hospital bed usage rate",
                                                                  "ICU bed usage rate"), 
                                                      selected = "Current infection rate")
                                     )
                                 ),
                                 
                                 # the whole box with graph
                                 fluidRow(
                                     box(
                                         title = "Comparison panel for three states",
                                         width = 12, status = "primary", solidHeader = TRUE,
                                         
                                         # select box
                                         fluidRow(column(4, selectInput(inputId = "select1", label = NULL,
                                                                        choices = c("State"=""))),
                                                  column(4, selectInput(inputId = "select2", label = NULL,
                                                                        choices = c("State"=""))),
                                                  column(4, selectInput(inputId = "select3", label = NULL,
                                                                        choices = c("State"="")))
                                         ),
                                         
                                         # action button
                                         fluidRow(column(2, actionButton("action1", label = "Compare"),
                                                         div(style = "height:20px"))
                                         ),
                                         
                                         # line chart panel
                                         fluidRow(
                                             # box(width = 4, plotOutput("plotl2", height = "200px")),
                                             box(width = 4, imageOutput("plotl2", height = "200px")),
                                             box(width = 4, plotOutput("plotl3", height = "200px")),
                                             box(width = 4, plotOutput("plotl4", height = "200px"))
                                         ),
                                         fluidRow(column(12,"The black line represents records, and blue line represents forecasts."))
                                     )
                                 )
                        ),
                        
                        ### subtab3: one state
                        tabPanel(span("Explore One State", style = "color: SteelBlue"),
                                 
                                 # items
                                 fluidRow(
                                     box(
                                         title = span("Select the state you want to explore",
                                                      style = "color: SteelBlue"),
                                         width = 12, status = "primary", 
                                         fluidRow(column(2, selectInput(inputId = "select4", label = NULL,
                                                                        choices = c("State"=""), selected = FALSE))
                                         ),
                                         fluidRow(column(2, actionButton("action2", label = "Explore")))
                                     )
                                 ),
                                 
                                 # the whole box with graph
                                 fluidRow(
                                     box(
                                         title = "Panel for the state",
                                         width = 12, status = "primary", solidHeader = TRUE,
                                         
                                         # line chart panel
                                         fluidRow(
                                             box(width = 6, plotlyOutput("plotl5", height = "250px")),
                                             box(width = 6, plotlyOutput("plotl6", height = "250px"))
                                         ),
                                         fluidRow(
                                             box(width = 6, plotlyOutput("plotl7", height = "250px")),
                                             box(width = 6, plotlyOutput("plotl8", height = "250px"))
                                         ),
                                         fluidRow(
                                             box(width = 6, plotlyOutput("plotl9", height = "250px")),
                                             box(width = 6, plotlyOutput("plotl10", height = "250px"))
                                         ),
                                         fluidRow(column(12,"The black line represents records, and blue line represents forecasts."))
                                     )
                                     
                                 )
                                 
                                 
                        )
                    )
            )
        )
    )
)
