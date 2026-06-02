#Use slider range for dates
#Once functional, adjust the aesthetics: https://rstudio.github.io/DT/


library(shiny)
library(DT)
library(bslib)
library(dplyr)
library(rvest)
library(stringr)
library(xml2)

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


#Prepare crisis summaries
html_file <- "ICB Crisis Summaries.html"

extract_summary <- function(article) {
  children <- html_elements(article, xpath = "./*")
  labels <- html_text2(children)
  
  start <- which(labels == "Background:")
  end <- which(labels == "Update:")
  
  if (length(start) == 0 || length(end) == 0 || end <= start) {
    return("Summary not found.")
  }
  
  summary_paragraphs <- children[(start):(end + 1)] %>%
    html_text2()
  
  paste(summary_paragraphs, collapse = "\n\n")
}

get_crisis_summary <- function(crisis_no, file = html_file) {
  page <- read_html(file)
  
  articles <- html_elements(page, "article.summary")
  
  crisnos <- articles %>%
    html_element("p.crisno") %>%
    html_text2() %>%
    str_extract("\\d+") %>%
    as.integer()
  
  match_index <- which(crisnos == crisis_no)
  
  if (length(match_index) == 0) {
    return(paste("No crisis found for CRISNO", crisis_no))
  }
  
  article <- articles[[match_index[1]]]
  
  title <- article %>%
    html_element("h2") %>%
    html_text2()
  
  summary_text <- extract_summary(article)
  
  paste0(
    "CRISNO ", crisis_no, ": ", title,
    "\n\n",
    summary_text
  )
}



#Prepare PC summaries
html_file_pc <- "ICB Protracted Conflict Summaries.html"

extract_pc <- function(article) {
  children <- html_elements(article, xpath = "./*")
  labels <- html_text2(children)
  
  start <- which(labels == "Introduction")
  end <- which(labels == "End")
  
  if (length(start) == 0 || length(end) == 0 || min(end) <= max(start)) {
    return("This crisis did not occur within a protracted conflict.")
  }
  
#  start <- start[1]
#  end <- end[end > start][1]
  
  pc_text <- children[start:(end - 1)] %>%
    html_text2()
  
  paste(pc_text, collapse = "\n\n")
}

get_article_pcid <- function(article) {
  pcid_node <- html_element(article, "p.pcid")
  
  if (length(pcid_node) > 0 && !is.na(html_text2(pcid_node))) {
    return(as.integer(str_extract(html_text2(pcid_node), "\\d+")))
  }
  
  # Fallback for cases where PCID appears immediately before article
  prev <- xml_find_first(article, "preceding-sibling::p[contains(@class, 'pcid')][1]")
  
  if (!is.na(xml_name(prev))) {
    return(as.integer(str_extract(html_text2(prev), "\\d+")))
  }
  
  NA_integer_
}

get_pc_summary <- function(pc_no, file = html_file_pc) {
  page <- read_html(file)
  
  articles <- html_elements(page, "article")
  
  pcids <- sapply(articles, get_article_pcid)
  
  match_index <- which(pcids == pc_no)
  
  if (length(match_index) == 0) {
    return("This crisis did not occur within a protracted conflict.")
  }
  
  article <- articles[[match_index[1]]]
  
  title <- article %>%
    html_element("h2") %>%
    html_text2() %>%
    str_squish()
  
  summary_pc <- extract_pc(article)
  
  paste0(
    "PCID ", pc_no, ": ", title,
    "\n\n",
    summary_pc
  )
}
#####################################

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
  
  tabPanel('Crisis Information', 
           h3(textOutput('name')), 
#           uiOutput('summary'),
           h4('----------'),
           h4('System Level'),
           DT::dataTableOutput('systemtable'),
           h4('----------'),
           h4('Actor Level'),
           DT::dataTableOutput('actortable'),
#          h4('Crisis Actors'), textOutput('actors'),  
#          h4('Trigger Date'), textOutput('trigdate'), 
#          h4('Termination Date'), textOutput('termdate'),  
#           h4('Crisis Outcome'), textOutput('outcome'), 
#           h4('Crisis Threat Gravity'), textOutput('gravity'), 
#          h4('Violence'), textOutput('violence'), 
#           h4('Protracted Conflict'), textOutput('pc'), 
#           h4('Crisis Mediation'), textOutput('mediation'),
  ),
# tabPanel('Crisis Information', DT::dataTableOutput('systemtable')),

#  tabPanel('Select Actor Information', DT::dataTableOutput('actortable'))

  tabPanel('Crisis Summary', uiOutput("summary")),

  tabPanel('Protracted Conflict Summary', uiOutput("summary_pc"))

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
    updateTabsetPanel(session, 'x0', selected = 'Crisis Information')
    output$name<-renderText({info$value})
#    output$actors<-renderText({full$Actors[full$Crisis.Name==info$value]})
#    output$trigdate<-renderText({system$trigger_date[system$crisname==info$value]})
#    output$termdate<-renderText({system$termination_date[system$crisname==info$value]})
#    output$gravity<-renderText({system$gravity[system$crisname==info$value]})
#    output$violence<-renderText({system$violence[system$crisname==info$value]})
#    output$outcome<-renderText({system$outcome[system$crisname==info$value]})
#    output$pc<-renderText({system$protracted_conflict[system$crisname==info$value]})
#    output$mediation<-renderText({system$mediation[system$crisname==info$value]})
#    output$summary <- renderUI({
#      url <- a(h4("Link to crisis summary"), href = full$Summary.URL[full$Crisis.Name==info$value])
#      tagList(url)
#    })
    output$systemtable = DT::renderDataTable({datatable(system[system$crisname==info$value,6:13],
                                                       colnames = c('Trigger', 'Trigger Date', 'Term. Date', 'Threat Gravity', 'Violence', 'Outcome', 'Protracted Conflict', 'Mediation'),
                                                       rownames = FALSE, options=list(dom = 'ltipr', lengthChange = FALSE, info = FALSE, paging = FALSE))
    })
    output$actortable = DT::renderDataTable({datatable(actor[actor$crisname==info$value,1:10],
                                                       colnames = c('Actor', 'Trigger Date', 'Term. Date', 'Triggering Entity', 'Source of threat', 'Trigger', 'Major Response', 'Response Date', 'Crisis Mgmt', 'Violence'),
                                                       rownames = FALSE, options=list(dom = 'ltipr', lengthChange = FALSE, info = FALSE, paging = FALSE))
                                            }) 
    output$summary <- renderUI({
      tags$div(
        style = "
        white-space: pre-wrap;
        font-size: 16px;
        line-height: 1.5;
        max-width: 850px;
      ",
        get_crisis_summary(core$Crisis.Num.[core$Crisis.Name==info$value])
      )
    })
    output$summary_pc <- renderUI({
      tags$div(
        style = "
        white-space: pre-wrap;
        font-size: 16px;
        line-height: 1.5;
        max-width: 850px;
      ",
        get_pc_summary(system$pcid[system$crisname==info$value])
      )
    })
  })



})



# Run the app ----
shinyApp(ui = ui, server = server)