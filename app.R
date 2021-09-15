# Shiny App for WCM Script woRdcomplex-2.1

library(shiny)

ui <- fluidPage(
  
  # App title 
  headerPanel("Word Complexity Measure"),
  
  # Sidebar panel with inputs 
  sidebarPanel(
    textAreaInput("sample", "Transcript:", placeholder="Paste English orthography transcript here...", height = '250px', width = "100%")
  ),
  
  # Main panel for displaying outputs 
  mainPanel(
    fluidRow(
      column(width = 12),
    #  box(width = NULL, #height = "200px",
    #       column(width = 6, align = "center",
    #              h4("Transcript Averages", style = "text-align:center"),
    #              tableOutput("average_table")
    #       ),
    #       column(width = 6, align = "center",
    #              h4("Word by Word", style = "text-align:center"),
    #              tableOutput("results")
    #       )
    #   )
    ),
  )
  
)

# Define server logic to plot various variables against mpg ----
server <- function(input, output) {

}

shinyApp(ui, server)