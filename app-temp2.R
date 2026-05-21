#Try to use datatable (DT) function
#Use this as model: https://yihui.shinyapps.io/DT-click/
#Return link to summary and other information to appear on the second tab or card after selection
#Will need to create a new table that has all the relevant variables plus the summary links
    #Create string descriptions to replace the numbers
    #Subset to just a few columns for the display
#Also create some selection tools to subset the table based on actors and PCs

library(shiny)
library(DT)
library(bslib)

#Load full table with links
full<-read.csv("ICB_Crisis_List_temp.csv")

#Subset to core information without links
core<-full[,1:5]

ui <- navbarPage(
  
  title = 'ICB Data Viewer Beta', id = 'x0',
  
  tabPanel('Table', DT::dataTableOutput('x1')),
  
  tabPanel('Selection', textInput('x2', 'Crisis Name'))
 #uiOutput("x4") 
)

# Define server logic ----

server <- shinyServer(function(input, output, session) {
  
  # add CSS style 'cursor: pointer' to the 1st column (i.e. crisis name)
  output$x1 = DT::renderDataTable({
    datatable(
      core, selection = 'none', class = 'cell-border strip hover'
    ) %>% formatStyle(1, cursor = 'pointer')
  })
  
  observeEvent(input$x1_cell_clicked, {
    info = input$x1_cell_clicked
    # do nothing if not clicked yet, or the clicked cell is not in the 1st column
    if (is.null(info$value) || info$col != 1) return()
    updateTabsetPanel(session, 'x0', selected = 'Selection')
    updateTextInput(session, 'x2', value = info$value)
  })
  
  
})



# Run the app ----
shinyApp(ui = ui, server = server)