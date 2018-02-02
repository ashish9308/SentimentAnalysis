library(shinydashboard)
dashboardPage( skin = "blue",
               dashboardHeader(title = "Sentimental Analysis tool",titleWidth = 250,
                               dropdownMenu(type = "messages",
                                            messageItem(
                                              from = "Sales Dept",
                                              message = "Sales are steady this month."
                                            ),
                                            messageItem(
                                              from = "New User",
                                              message = "How do I register?",
                                              icon = icon("question"),
                                              time = "13:45"
                                            ),
                                            messageItem(
                                              from = "Support",
                                              message = "The new server is ready.",
                                              icon = icon("life-ring"),
                                              time = "2014-12-01"
                                            )
                               )),
               
               dashboardSidebar(
                 width = 230,
                 sidebarMenu(
                   menuItem("Tweet Analysis", tabName = "usecase1", icon = icon("twitter")),
                   menuItem("Playstore Sentiment", tabName = "usecase1", icon = icon("google")),
                   menuItem("AppStore Sentiment", tabName = "usecase1", icon = icon("apple"))
                   
                 ),
                 # Custom CSS to hide the default logout panel
                 tags$head(tags$style(HTML('.shiny-server-account { display: none; }'))),
                 
                 # The dynamically-generated user panel
                 uiOutput("userpanel")
               ),
               dashboardBody(
                 tabItems(
                   tabItem(tabName = "usecase1",
                           fluidRow(
                             box(
                               title = icon("twitter"), width = 12, solidHeader = TRUE, status = "primary",
                               textInput(width = '300px',"text", " What topic would you like to analyze?"),
                               sliderInput(width = '600px',"n", " How many tweets do you want analyzed?", min= 10, max= 1000, value = 200, step = 5),
                               dateRangeInput(width = '300px','dateRange',
                                              label = 'Select Date Range for which tweets are to be Analyzed: ',
                                              start = Sys.Date() - 5, end = Sys.Date() 
                               ),
                               actionButton("executetweet","Fetch and Analyze!")
                             ),
                             box(
                               tabBox(
                                 id = "tabset1", height = "600px", width = "900px",
                                 tabPanel("Histogram", plotOutput("plot1"),verbatimTextOutput("text")),
                                 
                                 tabPanel("Sentiment Analysis", plotOutput("plot2")),
                                 tabPanel("Polarity Analysis", plotOutput("plot3")),
                                 tabPanel("Word-Cloud", plotOutput("plot4"))
                               )
                             )
                             
                           )
                   )
                 )
               )
)










