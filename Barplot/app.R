#!/usr/bin/env Rscript

# This example comes from the r-shiny examples github repo.
# https://github.com/rstudio/shiny-examples/blob/master/001-hello/app.R

library(shiny)

serverNames <- c('mhealth','mhealth-01', 'mhealth-02')

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  titlePanel("Sessions assigned to the same ordering and interpreting"),
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "selectedServer", 
                  label = "Select server:",
                  choices = serverNames,
                  selected = "mhealth"),
      checkboxInput(inputId = "ExcludeOld",
                    label = "exclude _old accounts",
                    value = TRUE),
      width = 2
    ),
    mainPanel(
      plotlyOutput("barplot", width = "120%", height = "950px")
    )
  )
)


# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  output$barplot = renderPlotly({

    data <- readRDS(file =  paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/SameData.rds" ))

    serverdata <-  data %>% filter(servername == input$selectedServer)
    if(input$ExcludeOld){
      serverdata <- serverdata %>% filter(!grepl("_old",interpreting))
    }
    
    plotly_obj <- serverdata %>%
      group_by(year, interpreting) %>%
      summarise(sessions=n()) %>%
      plot_ly(x=~year, y=~sessions, group =~interpreting, color=~interpreting,  type = "bar") %>% 
      layout(showlegend = TRUE) %>% 
      layout(legend=list(title=list(text='<b> Interpreting </b>')))
    plotly_obj
  })
  
}

# If you want to automatically reload the app when your codebase changes - should be turned off in production
options(shiny.autoreload = TRUE)

options(shiny.host = '0.0.0.0')
options(shiny.port = 5050)

# Create Shiny app ---- 
shinyApp(ui = ui, server = server)