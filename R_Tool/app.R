##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Author: Lilith Kramer & Willem Stolte
## Goal: example R Shiny app
## Version: 0.1 -- 08-apr-2019 -- script set up
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


##~~~~~~~~~~~~~~~~~~~~~
##==== PREPARATION ====
##~~~~~~~~~~~~~~~~~~~~~


##==legend=====

## COMMENTS
## ##      = comment
## #       = outcommented code
## #!#     = should be checked if location of script changes 
## #?#     = to do
## ##==    = block of info separator

## OBJECTS
## fnXXX   = filename
## dirXXX  = directory (path) / foldername
## oXXX    = loaded object
## dfXXX   = dataframe      
## dtXXX   = datatable 



##==notes to self============
## tooltip: https://stackoverflow.com/questions/36670065/tooltip-in-shiny-ui-for-help-text



##==install packages and open libraries ====

library(shiny)         
library(shinydashboard)
library(leaflet)
library(plotly)
library(shinyjs)
library(shinycssloaders)
library(tidyverse)
library(rgdal)



##==settings=======

options(scipen = 999)  ## no scientific numbering
options(shiny.reactlog = T) # for help with debugging
options(shiny.maxRequestSize = 30*1024^2) ## maximum upload size is now 30Mb, standard for R shiny is 5Mb. 
## rshiny shinyapps.io max limit size = 32MB. 

# jsResetCode <- "shinyjs.reset = function() {history.go(0)}" # Define the js method that resets the page

##== load functions ====

source('functies/testfuncties.R')

##==set paths ========

#!# directories
dirDATA <- "data/"

#!# files 


##== set variables =======
variables = reactiveValues(iets = NULL)


##==load files=========

##== USER INTERFACE ==================

header  <- dashboardHeader(title = "e.g. tool")

sidebar <- dashboardSidebar(
  sidebarMenu(id = "side_tabs",
              menuItem(text = "Startpagina", tabName = "start", icon = icon("home")),
              menuItem(text = "Testpagina", tabName = "test", icon = icon("upload")),
              menuItem(text = "Help", tabName = "help", icon = icon("question"))
  ))

## rows have a grid width of 12, so a box with width = 4, takes up one third of the space
## tops will be lined out, bottoms not
## heights are in pixels.. 

body    <- dashboardBody(
  
  tabItems(
    #===start_tab==============
    tabItem(tabName = "start",
            fluidRow(
              box(title = "Welkom bij de webapplicatie van X",
                  solidHeader = T,
                  status = "success",
                  collapsible = F,
                  p("Deze applicatie heeft als doel.."),
                  p("Deze applicatie is gebouwd door Deltares en gefinancierd door Rijkswaterstaat."),
                  img(src='deltares_logo.png'),
                  img(src='rijkswaterstaat_logo.png'),
                  width = 12))),

    #== huidig_tab==============
    tabItem(tabName = "test", 
            fluidRow(
              box(title = "Testpagina",
                  solidHeader = T,
                  status = "success",
                  collapsible = T,
                  collapsed = F,
                  width = 12,
                  p("Het inlezen van de data gaat als volgt:"), 
                    tags$ol(tags$li("Selecteer x"),
                            tags$li("Selecteer y")),
                  p("De onderstaande boxen zijn te openen en sluiten met een klik op het '+'-teken. Dit teken vindt u aan de rechterbovenzijde van de boxen.")
                  ),
              
              ##==== reset applicatie ========
              # box(title = "Herstart applicatie",
              #     solidHeader = T,
              #     status = "warning",
              #     collapsible = T,
              #     collapsed = T,
              #     width = 12,
              #     p("Druk op de knop om de applicatie opnieuw te starten. Hierbij gaat alle informatie die tot nu toe ingeladen was of weergegeven werd, verloren."),
              #     useShinyjs(),                                           # Include shinyjs in the UI
              #     extendShinyjs(text = jsResetCode),                      # Add the js code to the page
              #     actionButton("reset_button", "Herstart applicatie")
              #     ),
              
              
              ##==== ecologische soortgroep kiezen =====
              box(title = "1. Selecteer iets",
                  solidHeader = T,
                  status = "success",
                  collapsible = T,
                  collapsed = T,
                  width = 12,
                  p("Klik op een radiobutton."),
                  radioButtons(inputId = "rb_iets", 
                               label = NULL, 
                               choices = c("Iets 1", "Iets 2", "Iets 3")),
                  textOutput("selected_iets")),
              
            
              ##==== inlezen opname data ========
              box(title = "2. Upload iets",
                  solidHeader = T, 
                  status = "success",
                  collapsible = T,
                  collapsed = T,
                  width = 12,
                  p("Upload hier X. De maximale bestandsgrootte is 30MB."),
                  fileInput("testInvoerbestand", NULL,
                            multiple = FALSE,
                            accept = c("text/csv",
                                       "text/plain"),
                            buttonLabel = "Kies bestand",
                            placeholder = "Er is nog geen bestand geselecteerd."),
                  tabBox(id = "logHuidigInvoerSoortenBestand",
                         title = "",
                         side = "left", 
                         width = 12,
                         selected = "Tonen",
                         tabPanel("Tonen", 
                                  tableOutput("testTonen") %>% withSpinner(type = 3, color = "#01A65A", color.background = "#ffffff")),
                         tabPanel("Downloaden", 
                                  downloadButton("downloadData", "Download data weer")),
                         tabPanel("Extra", 
                                  tableOutput("testTonenIntern"))
                         )),
        
              ##==== tonen data ========
              box(title = "3. Toon iets",
                  solidHeader = T, 
                  status = "success",
                  collapsible = T,
                  collapsed = T,
                  width = 12,
                  tabBox(width = 12,
                         tabPanel("GGPlot",
                                  plotOutput("ggplotje")),
                         tabPanel("Plotly",
                                  plotlyOutput("plotly1")),
                         tabPanel("kaart",
                                  leafletOutput("leaflet_kaart")
                         )
                  )
                  
              ))),
    
    ##====help_tab====
    tabItem(tabName = "help",
            fluidRow(
              box(title = "Help",
                  solidHeader = T,
                  status = "success",
                  collapsible = T,
                  collapsed = F,
                  width = 12,
                  h3("Help"),
                  downloadButton("testdocument", "Download testdocument")
                  )))))
    

ui <- dashboardPage(skin = "green", header, sidebar, body,  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"), tags$style(".leaflet-top {z-index:999!important;}"))) ## tags$head etc is noodzakelijk om de zoombuttons van leaflet achter de dropdown box te krijgen. 
# ui <- dashboardPage(header, sidebar, body,  tags$head(tags$style(".leaflet-top {z-index:999!important;}"))) ## tags$head etc is noodzakelijk om de zoombuttons van leaflet achter de dropdown box te krijgen. 


##== SERVER=========


server <- function(input, output, session) {
  
  
  ##========= reset shiny app ==================
  # observeEvent(input$reset_button, {js$reset()}) 
  
  ##========= toekennen Variabelen ==================
  observeEvent(input$rb_iets, {
    
    if(input$rb_iets == 'Iets 1'){
    variables$iets = "Iets nummer 1"
    } else if(input$rb_iets == 'Iets 2'){
      variables$iets = "Iets nummer 2"
    } else if(input$rb_iets == 'Iets 3'){
      variables$iets = "Iets nummer 3"}})
    
 
  ##========= weergave text ==================
  output$selected_iets <- renderText({
    req(input$rb_iets)
    paste("Geselecteerd iets: ", input$rb_iets, sep = "")
    })

  testbestand <- reactive({
    req(input$testInvoerbestand)
    testfile <- inlezen(input$testInvoerbestand$datapath)
  })
  
  ##========= weergave tabel ==================
  output$testTonen <- renderTable({
    req(testbestand())
    df <- as.data.frame(testbestand())
    if(nrow(df)>10){df <- df[1:10, ]}
    return(df)
  })
  
  ##========= maak extra kolommen in tabel ===========
  output$testTonenIntern <- renderTable({
    req(testbestand())
    testbestand() %>%
      mutate(logMin = log10(min), logMax = log10(max))
  })

  ##========= weergave plot ==================

  output$ggplotje <- renderPlot({
    req(testbestand())
    
    p <- ggplot(testbestand(), aes(min, max)) +
      geom_point() +
      geom_smooth(method = "lm")
    
    return(p)
    
  })
  
  ##========= weergave interactieve plot ==================
  
  output$plotly1 <- renderPlotly({
    req(testbestand())
    
    p <- ggplot(testbestand(), aes(min, max)) +
      geom_point() +
      geom_smooth(method = "lm")
    
    return(ggplotly(p))
    
  })
  
  
  ##==== weergave interactieve kaart ========
  output$leaflet_kaart <- renderLeaflet({
    req(testbestand())
  
    df <- testbestand()
    coordinates(df) <- (~locX+locY)
    proj4string(df) <- CRS("+init=epsg:28992")
    df.wgs <- spTransform(df, CRS("+proj=longlat +datum=WGS84"))
    
    leaflet(df.wgs) %>%
      addTiles() %>%
      # addTiles('http://{s}.tile.openstreetmap.de/tiles/osmde/{z}/{x}/{y}.png') %>%
      addCircleMarkers(color = "grey", label = ~ paste(plant, waarde), labelOptions = options(noHide = T, textsize = "15px"))
    
  })

##====downloads======================

output$downloadData <- downloadHandler(
  filename = function(){
    paste(input$testInvoerbestand, ".csv", sep = "")
  },
  content = function(file){  
  write.csv(testbestand(), file, row.names = FALSE)
  }
)  

output$testdocument <- downloadHandler(
  filename = function(){paste("Verberk Habitat en milieupreferenties macrofauna.pdf")},
  content <- function(file){file.copy("documentatie/Verberk Habitat en milieupreferenties macrofauna.pdf", file)},
  contentType = "application/pdf"
)
}

shinyApp(ui, server)


