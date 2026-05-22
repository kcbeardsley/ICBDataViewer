#Try to use datatable (DT) function
#Use this as model: https://yihui.shinyapps.io/DT-click/
#Once functional, adjust the aesthetics: https://rstudio.github.io/DT/
#Will need to create new crisis-level and actor-level tables with core variables
    #Create string descriptions to replace the numbers
#Display key crisis-level variables as text
#Display key actor-level variables as a datatable

library(shiny)
library(DT)
library(bslib)
library(dplyr)

#Load tables
full<-read.csv("ICB_Crisis_List_with_Summaries_urls.csv")
system<-read.csv("ICB_system_core.csv")
actor<-read.csv("ICB_actor_core.csv")

#Subset to core information without links
core<-full[,1:5]

ui <- navbarPage(
  
  title = 'ICB Data Viewer Beta', id = 'x0',
  
  tabPanel('Crisis List', DT::dataTableOutput('x1')),
  
  tabPanel('Select Crisis Information', 
           h3(textOutput('name')), 
           uiOutput('summary'),
           h4('Crisis Actors'), textOutput('actors'),  
           h4('Trigger Date'), textOutput('trigdate'), 
           h4('Termination Date'), textOutput('termdate'),  
           h4('Outcome'), textOutput('outcome'), 
           h4('Threat Gravity'), textOutput('gravity'), 
           h4('Violence'), textOutput('violence'), 
           h4('Protracted Conflict'), textOutput('pc'), 
           h4('Mediation'), textOutput('mediation')),
  
# tabPanel('Select Crisis Information', DT::dataTableOutput('systemtable')),

  tabPanel('Select Actor Information', DT::dataTableOutput('actortable'))
)

# Define server logic ----

server <- shinyServer(function(input, output, session) {
  
  # add CSS style 'cursor: pointer' to the 1st column (i.e. crisis name)
  output$x1 = DT::renderDataTable({
    datatable(
      core, selection = 'none', class = 'cell-border strip hover',
      colnames = c('Name', 'Number', 'Actors', 'Trigger Date', 'Termination Date'),
      options = list(pageLength = 600), rownames = FALSE,
      caption='Click on the crisis name for more information'
    ) %>% formatStyle(1, cursor = 'pointer')
  })
  
  observeEvent(input$x1_cell_clicked, {
    info = input$x1_cell_clicked
    # do nothing if not clicked yet, or the clicked cell is not in the 1st column
    if (is.null(info$value) || info$col != 0) return()
    updateTabsetPanel(session, 'x0', selected = 'Select Crisis Information')
#    updateTextInput(session, 'name', value = info$value)
#    updateTextInput(session, 'crisno', value = full$Crisis.Num.[full$Crisis.Name==info$value])
#    updateTextInput(session, 'actors', value = full$Actors[full$Crisis.Name==info$value])
#    updateTextInput(session, 'trigger', value = system$trigger[system$crisname==info$value])
#    updateTextInput(session, 'trigdate', value = system$trigger_date[system$crisname==info$value])
#    updateTextInput(session, 'termdate', value = system$termination_date[system$crisname==info$value])
#    updateTextInput(session, 'gravity', value = system$gravity[system$crisname==info$value])
#    updateTextInput(session, 'violence', value = system$violence[system$crisname==info$value])
#    updateTextInput(session, 'outcome', value = system$outcome[system$crisname==info$value])
#    updateTextInput(session, 'pc', value = system$protracted_conflict[system$crisname==info$value])
#    updateTextInput(session, 'mediation', value = system$mediation[system$crisname==info$value])
    output$name<-renderText({info$value})
    output$actors<-renderText({full$Actors[full$Crisis.Name==info$value]})
    output$trigdate<-renderText({system$trigger_date[system$crisname==info$value]})
    output$termdate<-renderText({system$termination_date[system$crisname==info$value]})
    output$gravity<-renderText({system$gravity[system$crisname==info$value]})
    output$violence<-renderText({system$violence[system$crisname==info$value]})
    output$outcome<-renderText({system$outcome[system$crisname==info$value]})
    output$pc<-renderText({system$protracted_conflict[system$crisname==info$value]})
    output$mediation<-renderText({system$mediation[system$crisname==info$value]})
    output$summary <- renderUI({
      url <- a(h4("Link to crisis summary"), href = full$Summary.URL[full$Crisis.Name==info$value])
      tagList(url)
    })
#    output$systemtable = DT::renderDataTable({datatable(system[system$crisname==info$value,5:12],
#                                                       colnames = c('Trigger', 'Trigger Date', 'Termination Date', 'Threat Gravity', 'Violence', 'Outcome', 'Protracted Conflict', 'Mediation'),
#                                                       rownames = FALSE, caption=info$value)
#    })
    output$actortable = DT::renderDataTable({datatable(actor[actor$crisname==info$value,1:9],
                                                       colnames = c('Actor', 'Trigger Date', 'Termination Date', 'Triggering Entity', 'Source of threat', 'Trigger', 'Major Response', 'Crisis Management', 'Violence'),
                                                       rownames = FALSE, caption=info$value)
                                            }) 
  })



})



# Run the app ----
shinyApp(ui = ui, server = server)