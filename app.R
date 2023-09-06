library(shiny)
library(shinyalert)
library(shinythemes)
library(shinycssloaders)
library(shinyjs)
library(httr)
library(bslib)
library(thematic)
library(gtrendsR)
library(plotly)
library(dplyr)
library(ggplot2)

options(spinner.color = "lightblue",
        spinner.color.background = "#ffffff",
        spinner.size = 2)

my_theme <- bs_theme(
    bg = "#fdfefe",
    fg = "blue",
    primary = "red",
    base_font = font_google("PT Sans Caption"),
    "font-size-base" = "0.9rem",
    version = 5,
    "navbar-bg" = "blue"
)

thematic_shiny()

ui <- list(useShinyjs(),navbarPage(windowTitle = "TrendChecker",
                                   title = strong("TrendChecker"),theme = my_theme,
                                   tabPanel(title = strong("Trend Over Time"),icon = icon("chart-line"),#firebaseUIContainer(),
                                            sidebarLayout(
                                                sidebarPanel(width = 3,actionButton("info",strong("About TrendChecker",icon("info"))),hr(),
                                                             hidden(tags$div(id = "about",h5("TrendChecker is a web application that enables users to monitor the search popularity of any subject of interest over time,
                                                                                          and across different countries by calling the Google trend api. Search hit of 100 is the indicator of optimum popularity, while other hits are measured relative to the optimum."))),h4(strong("Controls")),hr(),
                                                             textInput("text",strong("Enter Search Term")),
                                                             checkboxGroupInput("check",strong("Select Country(ies)"),choices = c("USA" = "US","UK" = "GB","Germany" = "DE","Netherlands" = "NL","Nigeria" = "NG","Japan" = "JP")),
                                                             radioButtons("radio",strong("Choose Trend Source"),choices = c("Web","News","YouTube","Images")),
                                                             radioButtons("time",strong("Select Time Frame"),choices = c("Last Hour","Last Four Hours","Last Day","Last Seven Days","Past 30 Days","Past 90 Days","Past 12 Months","Last Five Years")),
                                                             actionButton("run",strong("Run Search"),icon("caret-right"))
                                                             ),
                                                mainPanel(
                                                          withSpinner(plotOutput("plot", width = "110%", height = "450px"),type = 8),
                                                downloadButton("plot_download","Download",icon = icon("download"))),
                                              
                                                          
                                            ))
)) 

# Define server logic required to run query

server <- function(input, output,session) {
  
## APP info button toggle activation
    
observeEvent(input$info,{
    toggle("about")
})    


## Create reactive input switch functionality
    
check_input <- reactive(input$check)
    

radio_input <- reactive(switch(input$radio,
                      "Web" = "web",
                      "News" = "news",
                      "YouTube" = "youtube",
                      "Images" = "images"))

radio_time <- reactive(switch(input$time,
                     "Last Hour" = "now 1-H",
                     "Last Four Hours" = "now 4-H",
                     "Last Day" = "now 1-d",
                     "Last Seven Days" = "now 7-d",
                     "Past 30 Days" = "today 1-m",
                     "Past 90 Days" = "today 3-m",
                     "Past 12 Months" = "today 12-m",
                     "Last Five Years" = "today+5-y"))


    
text_input <- reactive(input$text)




## Write the function

trend <- function(){
    
    gt <- gtrends(keyword = text_input(),geo = check_input(),gprop = radio_input(),
            time = radio_time())
    
    p <- plot(gt)
        
    return(p)
}
    
## Convert to reactive function

trend2 <- eventReactive(input$run,{
  trend()
})

## Create interactive plot

output$plot <- renderPlot({
    
    withProgress(message = "Fetching data",
                 detail = "This may take a while...",value = 0,{
                     
                     for (i in 1:40){
                         
                         incProgress(1/40)
                         Sys.sleep(0.4)
                     }
                 })    

    req(input$run)
    trend2()

})

output$plot_download <- downloadHandler(
  filename = function(){
    paste("graph",".jpg",sep = "")
  },
  content = function(file){
    jpeg(file)
    trend()
    dev.off()
  }
)
       
}

# Run the application 
shinyApp(ui = ui, server = server)
