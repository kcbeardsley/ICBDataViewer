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
full<-read.csv("ICB_Crisis_List_with_Summaries_urls_year.csv")
system<-read.csv("ICB_system_core.csv")
actor<-read.csv("ICB_actor_core.csv")
core<-full[,1:4]
#Create lists of states and pcs and add "ALL"
countries<-actor$actorname
countries<-countries[order(unlist(countries))]
countries<-c("All", unique(countries))
pcs<-actor$protracted_conflict
pcs<-pcs[order(unlist(pcs))]
pcs<-c("All", unique(pcs))


#Prepare crisis summaries
html_file <- "ICB Crisis Summaries.html"

crisis_node_to_ui <- function(node) {
  node_type <- xml2::xml_name(node)
  
  if (node_type == "h3") {
    tags$h3(html_text2(node))
  } else if (node_type == "p") {
    tags$p(html_text2(node))
  } else if (node_type == "ol") {
    items <- node %>%
      html_elements("li") %>%
      html_text2()
    
    tags$ol(
      class = "crisis-list",
      lapply(items, tags$li)
    )
  } else {
    NULL
  }
}

extract_crisis_ui <- function(article) {
  children <- html_elements(article, xpath = "./*")
  labels <- html_text2(children)
  
  start <- which(labels == "Overview:")
  end <- which(labels == "Update:")
  
  if (length(start) == 0 || length(end) == 0 || end <= start) {
    return(tags$p("Summary not found."))
  }
  
  crisis_nodes <- children[start:(end + 1)]
  
  tagList(
    lapply(crisis_nodes, crisis_node_to_ui)
  )
}

get_crisis_summary_ui <- function(crisis_no, file = html_file) {
  page <- read_html(file)
  
  articles <- html_elements(page, "article.summary")
  
  crisnos <- articles %>%
    html_element("p.crisno") %>%
    html_text2() %>%
    str_extract("\\d+") %>%
    as.integer()
  
  match_index <- which(crisnos == crisis_no)
  
  if (length(match_index) == 0) {
    return(tags$p(paste("No crisis found for CRISNO", crisis_no)))
  }
  
  article <- articles[[match_index[1]]]
  
  title <- article %>%
    html_element("h2") %>%
    html_text2() %>%
    str_squish()
  
  tagList(
    tags$div(
      class = "crisis-card",
      tags$div(class = "crisis-number", paste("CRISNO", crisis_no)),
      tags$h2(title),
      extract_crisis_ui(article)
    )
  )
}



#Prepare PC summaries
html_file_pc <- "ICB Protracted Conflict Summaries.html"

node_to_ui <- function(node) {
  node_type <- xml2::xml_name(node)
  
  if (node_type == "h3") {
    tags$h3(html_text2(node))
  } else if (node_type == "p") {
    tags$p(html_text2(node))
  } else if (node_type == "ol") {
    items <- node %>%
      html_elements("li") %>%
      html_text2()
    
    tags$ol(
      class = "pc-list",
      lapply(items, tags$li)
    )
  } else {
    NULL
  }
}

extract_pc_ui <- function(article) {
  children <- html_elements(article, xpath = "./*")
  labels <- html_text2(children)
  
  start <- which(labels == "Introduction")
  end <- which(labels == "End")
  
  if (length(start) == 0 || length(end) == 0 || min(end) <= max(start)) {
    return(tags$h4("This crisis did not occur within a protracted conflict."))
  }
  
  start <- start[1]
  end <- end[end > start][1]
  
  pc_nodes <- children[start:(end - 1)]
  
  tagList(
    lapply(pc_nodes, node_to_ui)
  )
}

get_article_pcid <- function(article) {
  pcid_node <- article %>% html_element("p.pcid")
  
  if (length(pcid_node) == 0 || is.na(html_text2(pcid_node))) {
    return(NA_integer_)
  }
  
  html_text2(pcid_node) %>%
    stringr::str_extract("\\d+") %>%
    as.integer()
}

get_pc_summary_ui <- function(pc_no, file = html_file_pc) {
  page <- read_html(file)
  
  articles <- html_elements(page, "article")
  pcids <- sapply(articles, get_article_pcid)
  
  match_index <- which(pcids == pc_no)
  
  if (length(match_index) == 0 || is.na(pc_no)) {
    return(tags$h4("This crisis did not occur within a protracted conflict."))
  }
  
  article <- articles[[match_index[1]]]
  
  title <- article %>%
    html_element("h2") %>%
    html_text2() %>%
    str_squish()
  
  tagList(
    tags$div(
      class = "pc-card",
      tags$div(class = "pc-number", paste("PCID", pc_no)),
      tags$h2(title),
      extract_pc_ui(article)
    )
  )
}
#####################################

ui <- navbarPage(
#  theme = my_theme,

#Define a consistent heading style:
      
  title = 'ICB Data Viewer (v16)', id = 'x0',
  
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
           fluidRow(
             column(
               10,
               offset = 1,
           h2(textOutput('name'), style = "
    margin-top: 0;
    margin-bottom: 22px;
    font-size: 28px;
    font-weight: 700;
    color: #1f2937;
  "), 
#           uiOutput('summary'),
#           tags$hr(),
           h3('Key System-Level Variables', style="margin-top: 28px;
    margin-bottom: 12px;
    font-size: 20px;
    font-weight: 700;
    color: #003366;
    border-bottom: 1px solid #ddd;
    padding-bottom: 6px;"),
           DT::dataTableOutput('systemtable'),
#           tags$hr(),
           h3('Key Actor-Level Variables', style="margin-top: 28px;
    margin-bottom: 12px;
    font-size: 20px;
    font-weight: 700;
    color: #003366;
    border-bottom: 1px solid #ddd;
    padding-bottom: 6px;"),
           DT::dataTableOutput('actortable'),
#           tags$hr(),

           h4("Resources", style="margin-top: 28px;
    margin-bottom: 12px;
    font-size: 16px;
    font-weight: 700;
    color: #003366;
    border-bottom: 1px solid #ddd;
    padding-bottom: 6px;"),

           p(
            "The complete ICB datasets, including more than 200 system/crisis-level and actor-level variables, ",
            "can be accessed from the ICB Project website:"
            ),

          tags$ul(
            tags$li(
              tags$a(
               href = "https://sites.duke.edu/icbdata/data-collections/",
               target = "_blank",
                "ICB Data Collections"
                    )
                   )
                  ),
           p(
            "Codebooks describing all variables in the system/crisis-level and actor-level datasets are available at the following links:"
            ),

            tags$ul(
              tags$li(
               tags$a(
                 href = "https://duke.box.com/s/ravjtawv20aszptx3xhn43p7gd2q02dq",
                  target = "_blank",
                  "System-Level Variables Codebook"
                      )
                    ),
              tags$li(
                tags$a(
                  href = "https://duke.box.com/s/d7zxijj57ukexpc08j1039vvw4cvrpzy",
                  target = "_blank",
                  "Actor-Level Variables Codebook"
                      )
                    )
                  ),
             )
           )
  ),
#          h4('Crisis Actors'), textOutput('actors'),  
#          h4('Trigger Date'), textOutput('trigdate'), 
#          h4('Termination Date'), textOutput('termdate'),  
#           h4('Crisis Outcome'), textOutput('outcome'), 
#           h4('Crisis Threat Gravity'), textOutput('gravity'), 
#          h4('Violence'), textOutput('violence'), 
#           h4('Protracted Conflict'), textOutput('pc'), 
#           h4('Crisis Mediation'), textOutput('mediation'),
# tabPanel('Crisis Information', DT::dataTableOutput('systemtable')),

#  tabPanel('Select Actor Information', DT::dataTableOutput('actortable'))

  tabPanel('Crisis Summary',
           tags$head(
             tags$style(HTML("
  .crisis-card {
    background: white;
    max-width: 900px;
    padding: 30px 38px;
    margin: 25px auto;
    border-radius: 12px;
    box-shadow: 0 2px 12px rgba(0,0,0,0.08);
    line-height: 1.6;
    font-size: 16px;
  }

  .crisis-number {
    font-weight: bold;
    color: #555;
    text-transform: uppercase;
    letter-spacing: 0.04em;
    margin-bottom: 8px;
  }

  .crisis-card h2 {
    margin-top: 0;
    margin-bottom: 22px;
    font-size: 28px;
    font-weight: 700;
    color: #1f2937;
  }

  .crisis-card h3 {
    margin-top: 28px;
    margin-bottom: 12px;
    font-size: 20px;
    font-weight: 700;
    color: #003366;
    border-bottom: 1px solid #ddd;
    padding-bottom: 6px;
  }

  .crisis-card p {
    margin-bottom: 16px;
    color: #222;
  }

  .crisis-list {
    margin-top: 10px;
    margin-bottom: 24px;
    padding-left: 30px;
  }

  .crisis-list li {
    margin-bottom: 8px;
    line-height: 1.5;
  }

  .crisis-list li::marker {
    font-weight: bold;
    color: #003366;
  }
    "))),
           uiOutput("summary")
           ),

tabPanel(
  "Protracted Conflict Summary",
  
  tags$head(
    tags$style(HTML("
      .pc-card {
        background: white;
        max-width: 900px;
        padding: 30px 38px;
        margin: 25px auto;
        border-radius: 12px;
        box-shadow: 0 2px 12px rgba(0,0,0,0.08);
        line-height: 1.6;
        font-size: 16px;
      }

      .pc-number {
        font-weight: bold;
        color: #555;
        text-transform: uppercase;
        letter-spacing: 0.04em;
        margin-bottom: 8px;
      }

      .pc-card h2 {
        margin-top: 0;
        margin-bottom: 22px;
        font-size: 28px;
        font-weight: 700;
        color: #1f2937;
      }

      .pc-card h3 {
        margin-top: 28px;
        margin-bottom: 12px;
        font-size: 20px;
        font-weight: 700;
        color: #003366;
        border-bottom: 1px solid #ddd;
        padding-bottom: 6px;
      }

      .pc-card p {
        margin-bottom: 16px;
        color: #222;
      }

      .pc-list {
        margin-top: 10px;
        margin-bottom: 24px;
        padding-left: 30px;
      }

      .pc-list li {
        margin-bottom: 8px;
        line-height: 1.5;
      }

      .pc-list li::marker {
        font-weight: bold;
        color: #003366;
      }
    "))
  ),
  
  uiOutput("summary_pc")
),

tabPanel(
  "Instructions",
  
  fluidRow(
    column(
      10,
      offset = 1,
      
      h2("Welcome to the ICB Data Viewer", style="margin-top: 0;
        margin-bottom: 22px;
        font-size: 28px;
        font-weight: 700;
        color: #1f2937;"),
      
      p(
        "This application provides an interactive interface for exploring data from the ",
        strong("International Crisis Behavior (ICB) Project"),
        "."
      ),
      
      p(
        "To begin, navigate to the ",
        strong("Crisis List"),
        " tab. There you can select a crisis by clicking on its name. The toggles 
        will subset the crisis list to specific states, time periods or 
        protracted conflicts. The search box can be used to find crises when you
        know parts of the crisis names."
        ),
      
      p(  
        "Once you click on a crisis name, 
        the viewer takes you to the ",
        strong("Crisis Information"),
        " tab, which displays a subset of key variables about the crisis and crisis actors.
        Narrative summaries related to the selected crisis are available on additional tabs:"
      ),
      
      tags$ul(
        tags$li(strong("Crisis Summary:"), "A summary of the crisis and contextual background information"),
        tags$li(strong("Protracted Conflict Summary:"),
          "Background information about the associated protracted conflict, when applicable"
        )
      ),
      
 #     tags$hr(),
      
      h3("Citations", style="margin-top: 28px;
        margin-bottom: 12px;
        font-size: 20px;
        font-weight: 700;
        color: #003366;
        border-bottom: 1px solid #ddd;
        padding-bottom: 6px;"),
      
      p(
        "When using the ICB data, please cite the following sources:"
      ),
      
      tags$blockquote(
        p(
          "Brecher, Michael and Jonathan Wilkenfeld (1997). ",
          em("A Study of Crisis"),
          ". Ann Arbor: University of Michigan Press."
        ),
        
        p(
          "Brecher, Michael, Jonathan Wilkenfeld, Kyle Beardsley, ",
          "Patrick James and David Quinn (2025). ",
          em("International Crisis Behavior Data Codebook, Version 16"),
          ". ",
          tags$a(
            href = "https://sites.duke.edu/icbdata/data-collections/",
            target = "_blank",
            "https://sites.duke.edu/icbdata/data-collections/"
          )
        )
      ),
      
#      tags$hr(),
 
      h3("Additional Data Resources", style="margin-top: 28px;
        margin-bottom: 12px;
        font-size: 20px;
        font-weight: 700;
        color: #003366;
        border-bottom: 1px solid #ddd;
        padding-bottom: 6px;"),
      
      p(
        "The complete ICB datasets, including more than 200 system/crisis-level and actor-level variables, ",
        "can be accessed from the ICB Project website:"
      ),
      
      tags$ul(
        tags$li(
          tags$a(
            href = "https://sites.duke.edu/icbdata/data-collections/",
            target = "_blank",
            "ICB Data Collections"
          )
        )
      ),
      
      p(
        "Codebooks describing all variables in the system/crisis-level and actor-level datasets are available at the following links:"
      ),
      
      tags$ul(
        tags$li(
          tags$a(
            href = "https://duke.box.com/s/ravjtawv20aszptx3xhn43p7gd2q02dq",
            target = "_blank",
            "System-Level Variables Codebook"
          )
        ),
        tags$li(
          tags$a(
            href = "https://duke.box.com/s/d7zxijj57ukexpc08j1039vvw4cvrpzy",
            target = "_blank",
            "Actor-Level Variables Codebook"
          )
        )
      ),
      
#      tags$hr(),
      
           
      h3("Acknowledgment", style="margin-top: 28px;
        margin-bottom: 12px;
        font-size: 20px;
        font-weight: 700;
        color: #003366;
        border-bottom: 1px solid #ddd;
        padding-bottom: 6px;"),
      
      p(
        "We are grateful for the work of ",
        strong("Alex Jonas,"),
        " who developed the original ICB Data Viewer. ",
        "His vision and skill laid the foundation for the current version of the viewer and made this resource possible."
      )
    )
  )
)

)
# Define server logic ----

server <- shinyServer(function(input, output, session) {

  # add CSS style 'cursor: pointer' to the 1st column (i.e. crisis name)
  output$x1 = DT::renderDataTable({
    datatable(
      core[core$CrisisName %in% actor$crisname[(actor$actorname==input$state | actor$all==input$state) & (actor$protracted_conflict==input$pc | actor$all==input$pc) & actor$trigger_year>=min(input$trigyr) & actor$trigger_year<=max(input$trigyr)],], selection = 'none', class = 'cell-border strip hover',
      colnames = c('Name', 'Number', 'Trigger Year', 'Actors'),
      options = list(pageLength = 600, scrollY = "375px", scrollCollapse=TRUE, info = FALSE, paging = FALSE), rownames = FALSE,
      caption='Click on the crisis name for more information'
    ) %>% formatStyle(1, cursor = 'pointer')
  })
  
  #Start with base output of the first crisis
  output$name<-renderText({system$crisname[1]})
  output$systemtable = DT::renderDataTable({datatable(system[1,6:13],
                                                      colnames = c('Trigger', 'Trigger Date', 'Term. Date', 'Threat Gravity', 'Violence', 'Outcome', 'Protracted Conflict', 'Mediation'),
                                                      rownames = FALSE, options=list(dom = 'ltipr', lengthChange = FALSE, info = FALSE, paging = FALSE))
  })
  output$actortable = DT::renderDataTable({datatable(actor[actor$crisname==system$crisname[1],1:10],
                                                     colnames = c('Actor', 'Trigger Date', 'Term. Date', 'Triggering Entity', 'Source of threat', 'Trigger', 'Major Response', 'Response Date', 'Crisis Mgmt', 'Violence'),
                                                     rownames = FALSE, options=list(dom = 'ltipr', lengthChange = FALSE, info = FALSE, paging = FALSE))
  }) 
  output$summary <- renderUI({
    get_crisis_summary_ui(system$crisno[1])
  })
  output$summary_pc <- renderUI({
    get_pc_summary_ui(system$pcid[1])
  })
  
  
  #Adjust output to correspond to the crisis clicked
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
      get_crisis_summary_ui(system$crisno[system$crisname == info$value])
    })
    output$summary_pc <- renderUI({
      get_pc_summary_ui(system$pcid[system$crisname == info$value])
    })
  })

})



# Run the app ----
shinyApp(ui = ui, server = server)