library(shiny)
library(ggplot2)
library(data.table)
library(dplyr)
library(plotly)
library(visNetwork)
library(igraph)


serverNames <- c('mhealth','mhealth-01', 'mhealth-02')
avaliableyears <- c('2018', '2019', '2020')

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  titlePanel("Ordering and Interpreting relations"),
  
  sidebarLayout(
    position = "left",
    sidebarPanel(
      selectInput(inputId = "selectedServer", 
                  label = "Select server:",
                  choices = serverNames,
                  selected = "mhealth-01"),
      selectInput(inputId = "selectedyear", 
                  label = "Select year:",
                  choices = avaliableyears,
                  selected = "2019"),
      checkboxInput(inputId = "ExcludeOld",
                    label = "exclude _old accounts",
                    value = TRUE),
      checkboxInput(inputId = "ExcludeSameOI",
                    label = "exclude same ordering and interpreting cases",
                    value = TRUE),
      
      width = 2
    ),
    mainPanel(
      visNetworkOutput("network",  width = "120%", height = "750px"), 
      dataTableOutput("nodes_data_from_shiny"),
      uiOutput('dt_UI'),
    )
  )
)


# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  data <- readRDS(file =  paste0(dirname(rstudioapi::getSourceEditorContext()$path), "/data.rds" ))
  
  filteredData <- reactive({
    filteredData <-  data %>% filter(servername == input$selectedServer &  year == input$selectedyear)
    
    if(input$ExcludeOld){
      filteredData <- filteredData %>% filter(!grepl("_old",interpreting))
    }
    
    if(input$ExcludeSameOI){
      filteredData <- filteredData %>% filter(filteredData$ordering != filteredData$interpreting)
    }
    
    filteredData
    
  })
  
  
  output$network <- renderVisNetwork({
    filteredData <- filteredData()
    dt <- filteredData %>% 
      group_by(ordering, interpreting) %>%  
      summarise(width = n())
    doctors <- data.frame(doctors = unique(c( dt$ordering, dt$interpreting)))
    
    # Graph
    nodes <- data.frame(id = unique(doctors$doctor), label = unique(doctors$doctor)) 
    edges <- data.frame(dt[, c('ordering', 'interpreting', 'width')])
    edges$title <- paste0("Sessions: ", edges$width)
    graph <- graph_from_data_frame(edges, vertices = nodes,  directed = FALSE)
    vn <- toVisNetworkData(graph)
    vn$nodes$color <-  ifelse(nodes$label %in% unique(dt$interpreting), "tomato", 'grey')
    vn$nodes <- vn$nodes %>% arrange(nodes)
    # build visNetwork
    visNetwork(nodes = vn$nodes, edges = vn$edges, 
               height = "1000px", width = "100%", main = "") %>%
      visOptions(highlightNearest = list(enabled = T, degree = 1, hover = F),  nodesIdSelection = TRUE)  %>% 
      visEdges(dashes = FALSE, color = list(highlight = "tomato"), smooth = list(enabled = FALSE, type = "horizontal"),  labelHighlightBold =TRUE) %>%
      visEvents(select = "function(nodes) {
            Shiny.onInputChange('current_node_id', nodes.nodes);  
            ;}")
    
  }) 
  
  
  
  output$nodes_data_from_shiny <- renderDataTable({
    filteredData <- filteredData()
    
    dt <- filteredData %>% group_by(ordering, interpreting) %>%  summarise(sessions = n( ))
    dt %>% filter((ordering %in% input$current_node_id)|(interpreting %in% input$current_node_id)) %>% data.table()
    
  })
  
  
  output$dt_UI <- renderUI({
    if (!is.null(input$current_node_id) && !is.null(input$network)) {
      dataTableOutput('nodes_data_from_shiny')
    } else{}
  })
  
}

# If you want to automatically reload the app when your codebase changes - should be turned off in production
options(shiny.autoreload = TRUE)

options(shiny.host = '0.0.0.0')
options(shiny.port = 8080)

# Create Shiny app ---- 
shinyApp(ui = ui, server = server)