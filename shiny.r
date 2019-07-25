library(shiny)
library(tidyverse)
library(visNetwork)
library(DT)
library(shinyalert)

# global

books <<- list("data.csv")

# Define UI for data upload app ----install
ui <- fluidPage(
  
  # App title ----
  titlePanel("Visualizing Interaction Data"),
  
  # Info.md
  includeMarkdown("info.md"),
  

  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Select a test file --
      
      selectInput("selection", "Choose a dataset:",
                  choices = books),
      actionButton("update", "Change"),
      
      # Horizontal line ----
      tags$hr(),
      
      # Input: Select a file ----
      fileInput("file1", "Choose CSV File",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      
      # Horizontal line ----
      tags$hr(),
      
      # Input: Checkbox if file has header ----
      checkboxInput("header", "Header", TRUE),
      
      # Input: Select separator ----
      radioButtons("sep", "Separator",
                   choices = c(Comma = ",",
                               Semicolon = ";",
                               Tab = "\t"),
                   selected = ","),
      
      # Input: Select quotes ----
      radioButtons("quote", "Quote",
                   choices = c(None = "",
                               "Double Quote" = '"',
                               "Single Quote" = "'"),
                   selected = '"'),
      
      # Horizontal line ----
      tags$hr(),
      
      # Input: Select number of rows to display ----
      radioButtons("disp", "Display",
                   choices = c(Head = "head",
                               All = "all"),
                   selected = "all")
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: table and network ----
      tabsetPanel(type = "tabs",
                  tabPanel("Table", DT::dataTableOutput("contents")),
                  tabPanel("Network", visNetworkOutput("network"))
      ),
      
      verbatimTextOutput("shiny_return"),
      
      # Button
      useShinyalert(),  # Set up shinyalert
      actionButton("preview", "Preview")
    )
  )
)

# Define server logic to read selected file ----
server <- function(input, output, session) {
  rawData <- eventReactive(input$file1, {
    read.csv(input$file1$datapath)
  })
  
  output$contents <- DT::renderDataTable({
    
    # when reading semicolon separated files,
    # having a comma separator causes `read.csv` to error
    tryCatch(
      {
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    
    if(input$disp == "head") {
      return(head(df))
    }
    else {
      return(df)
    }
    
  })
  
  # network render 
  
  output$network <- renderVisNetwork({
    
      instructors <- rawData() %>%
        distinct(Instructors) %>%
        rename(label = Instructors) 
      
      instructors <- mutate(instructors, group = "Librarian")
      
      faculty <- rawData() %>%
        distinct(Faculty) %>%
        rename(label = Faculty)
      
      faculty <- mutate(faculty, group = "Faculty")
      
      nodes <- full_join(instructors, faculty)
     
      nodes <- nodes %>%
        rowid_to_column("id")
      
      nodes <- mutate(nodes, title = label)
      
      per_act <- rawData() %>%
        group_by(Instructors, Faculty) %>%
        summarise(weight = n()) %>%
        ungroup()
      
      edges <- per_act %>% 
        left_join(nodes, by = c("Instructors" = "label")) %>% 
        rename(from = id)
      
      edges <- edges %>% 
        left_join(nodes, by = c("Faculty" = "label")) %>% 
        rename(to = id)
      
      edges <- select(edges, from, to, weight)
      
      edges <- mutate(edges, title = "Instruction")
    
    network <- visNetwork(nodes, edges) %>%
      visPhysics(solver = "forceAtlas2Based") %>%
      visInteraction(hover = TRUE) %>%
      visEdges(smooth = FALSE) %>%
      visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE, selectedBy = "group") %>%
      visEvents(hoverNode = "function(nodes) {
                Shiny.onInputChange('current_node_id', nodes);
                ;}")
  })
  
  output$shiny_return <- renderPrint({
    input$current_node_id
  })
  
  observeEvent(input$preview, {
    # Show a modal when the button is pressed
    shinyalert("Want to export?", "html export is in the works. Just screenshot for now!", type = "info")
  })
}

# Create app

shinyApp(ui, server)
