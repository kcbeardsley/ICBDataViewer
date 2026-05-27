#Use slider range for dates
#Once functional, adjust the aesthetics: https://rstudio.github.io/DT/


library(shiny)
library(DT)
library(bslib)
library(dplyr)

#Change the theme
#my_theme <- bs_theme(
#  bootswatch = 'zephyr'
#)

#Load tables
full<-read.csv("ICB_Crisis_List_with_Summaries_urls.csv")
system<-read.csv("ICB_system_core.csv")
actor<-read.csv("ICB_actor_core.csv")
core<-full[,1:5]
#Create lists of states and pcs and add "ALL"
countries<-actor$actorname
countries<-countries[order(unlist(countries))]
countries<-c("All", unique(countries))
pcs<-actor$protracted_conflict
pcs<-pcs[order(unlist(pcs))]
pcs<-c("All", unique(pcs))

ui <- navbarPage(
#  theme = my_theme,
  
  title = 'ICB Data Viewer Beta', id = 'x0',
  
  tabPanel('Crisis List', 
           selectInput(
             "state",
             "Select Crisis Actor",
             choices = setNames(as.list(countries), countries),
             selected = "All"
           ),
           selectInput(
             "pc",
             "Select Protracted Conflict",
             choices = setNames(as.list(pcs), pcs),
             selected = "All"
           ),
           sliderInput( 
             "trigyr", "Crisis Trigger Year", 
             min =  min(actor$trigger_year, na.rm=T), max =  max(actor$trigger_year, na.rm=T), 
             value = c(min(actor$trigger_year, na.rm=T), max(actor$trigger_year, na.rm=T)),
             sep = ""
           ), 
           DT::dataTableOutput('x1')),
  
  tabPanel('Select Crisis Information', 
           h3(textOutput('name')), 
           uiOutput('summary'),
           DT::dataTableOutput('actortable'),
#          h4('Crisis Actors'), textOutput('actors'),  
#          h4('Trigger Date'), textOutput('trigdate'), 
#          h4('Termination Date'), textOutput('termdate'),  
           h4('Crisis Outcome'), textOutput('outcome'), 
           h4('Crisis Threat Gravity'), textOutput('gravity'), 
#          h4('Violence'), textOutput('violence'), 
           h4('Protracted Conflict'), textOutput('pc'), 
           h4('Crisis Mediation'), textOutput('mediation')),
  
# tabPanel('Select Crisis Information', DT::dataTableOutput('systemtable')),

#  tabPanel('Select Actor Information', DT::dataTableOutput('actortable'))

)
# Define server logic ----

server <- shinyServer(function(input, output, session) {

  # add CSS style 'cursor: pointer' to the 1st column (i.e. crisis name)
  output$x1 = DT::renderDataTable({
    datatable(
      core[core$Crisis.Name %in% actor$crisname[(actor$actorname==input$state | actor$all==input$state) & (actor$protracted_conflict==input$pc | actor$all==input$pc) & actor$trigger_year>=min(input$trigyr) & actor$trigger_year<=max(input$trigyr)],], selection = 'none', class = 'cell-border strip hover',
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