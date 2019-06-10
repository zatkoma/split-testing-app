#
# SEO Split Testing Evaluation
#
# By: Martin Žatkovič
# Last update: 10.6.2019
# Repo:
#


# Libraries
library(shiny)
library(googleAuthR)
library(searchConsoleR)
library(shinyjs)
library(shinyTime)
library(chron)
library(ggplot2)
library(gridExtra)
library(CausalImpact)
library(data.table)
library(stringr)
library(foreach)

# Server Setings
options(shiny.port = 7775)
options(shiny.host = "127.0.0.1")

# UI
ui <- fluidPage(
  
  shinyjs::useShinyjs(),
  
  # For debugging
  # verbatimTextOutput("console_text"),
  
  # App Title
  titlePanel("SEO Split Testing Evaluation"),
  
  # Text before
  p("Tato aplikace byla vytořena, abyste mohli jednoduše pracovat s vyhodnocováním testů v SEO. Její základní výhody jsou v tom, že 
    se může připojit na Google Search Consoli, odkud tahá data a ty porovnává mezi sebou. Testování v SEO vychází z toho, že si vyberete
    dataset URL adres (skupina A), kde nic nezměníte a vše necháte tak jak má být. A následně dataset URL adres (skupina B), kde provedete
    jisté změny a úpravy."),
  
  hr(),
  
  # Sidebar
  sidebarLayout(
    
    sidebarPanel(
      
      actionButton(inputId = "prihlasitse",label = "Přihlaste se do GSC", style="margin-bottom: 25px"),
      
      uiOutput("choose_dataset"),
      
      dateInput("timeFrom", "Sledované období od:", value = "2019-01-01"),
      
      dateInput("timeChange", "Datum provedení změny:", value = "2019-03-30"),
      
      dateInput("timeTo", "Sledované období do:", value = strptime(Sys.time(), format="%Y-%m-%d", tz="GMT") - (4 * 86400)),
      
      textAreaInput("skupinaA", "Skupina A (nezměněno)", value ="", width = "100%"),
      
      checkboxInput("exact1", "Srovnávej adresáře", value = FALSE),
      
      textAreaInput("skupinaB", "Skupina B (změněno)", value ="", width = "100%"),
      
      checkboxInput("exact2", "Srovnávej adresáře", value = FALSE),
      
      hr(),
      
      uiOutput("choose_metrics"),
      
      numericInput("limit", "Maximální počet řádek", value=100000),
      
      hr(),
      
      actionButton(inputId = "propocitat",label = "Spočítat změnu", style="margin-bottom: 25px")
      
    ),
    
    # Main
    mainPanel(
      
      
      h3("Propočítaná data (odchylka od skupiny A)"),
      plotOutput("firstPlot"),
      
      h3("Informace o dopadu"),
      verbatimTextOutput("secondPlot"),
      
      h3("Report"),
      verbatimTextOutput("thirdPlot")
    )
  )
  )

# Backend
server <- function(input, output) {
  
  shinyjs::disable("propocitat")
  
  ## What is this?
  info <- reactiveValues()
  
  ## Login to GSC
  observeEvent(input$prihlasitse,{
    
    ## Create new user and get websites
    scr_auth(new_user = TRUE)
    scr_websites <- list_websites()
    
      ## TODO: Print just verified websites
      ## Try catch or what?
      info$account_list <- list_websites()
    
    ## Disable login and enable app
    shinyjs::disable("prihlasitse")
    shinyjs::enable("propocitat")
    
  })
  
  ## Render websites
  output$choose_dataset <- renderUI({
    selectInput("dataset", "Dostupné weby:", as.list(info$account_list[1]))
  })
  
  ## Render variables to count
  output$choose_metrics <- renderUI({
    selectInput("datasetMetrics", "Srovnávané metriky:", as.list(c("Imprese", "Clicks")))
  })
  
  
  ## Evaluation
  observeEvent(input$propocitat,{
    {
      
      shinyjs::disable("propocitat")
      
      ## Get informations from inputs
      sc_params_from <- input$timeFrom
      sc_params_change <- input$timeChange
      sc_params_to <- input$timeTo
      sc_params_property <- input$dataset
      sc_params_limit <- input$limit
      
      ####
      ## GET DATA GROUP A
      ####
      promena_temp <- unlist(strsplit(x =input$skupinaA,split = '[\r\n]' ))
      vstupni_temp <- data.frame("URL"= promena_temp)
      adresare_temp <- input$exact1
      
      if(adresare_temp == TRUE){
        
        x <- foreach(i=vstupni_temp$URL) %do% search_analytics(sc_params_property, sc_params_from, sc_params_to, dimensions = c("page","date"), dimensionFilterExp = c(paste("page~~", i, "", sep="")), searchType = "web", rowLimit = sc_params_limit)
        temp <- as.data.frame(do.call("rbind", x), stringsAsFactors = FALSE)
        
      }else{
        
        x <- foreach(i=vstupni_temp$URL) %do% search_analytics(sc_params_property, sc_params_from, sc_params_to, dimensions = c("page","date"), dimensionFilterExp = c(paste("page==", i, "", sep="")), searchType = "web", rowLimit = sc_params_limit)
        temp <- as.data.frame(do.call("rbind", x), stringsAsFactors = FALSE)
        
      }
      
      ## Set sc_skupina_a
      sc_skupina_a <- temp
      
      ## Cleaning is caring
      promena_temp <- NULL
      vstupni_temp <- NULL
      adresare_temp <- NULL
      x <- NULL
      temp <- NULL

      ## Prepare GROUP A for Impact.
      if(input$datasetMetrics == "Imprese"){
        
        sc_skupina_a$page = NULL;
        sc_skupina_a$clicks = NULL;
        sc_skupina_a$ctr = NULL;
        sc_skupina_a$position = NULL;
        #sc_skupina_a$impresions = NULL;
        
      }else if(input$datasetMetrics == "Clicks"){
        
        sc_skupina_a$page = NULL;
        #sc_skupina_a$clicks = NULL;
        sc_skupina_a$ctr = NULL;
        sc_skupina_a$position = NULL;
        sc_skupina_a$impresions = NULL;
        
      }else if(input$datasetMetrics == "Positions"){
        
        sc_skupina_a$page = NULL;
        sc_skupina_a$clicks = NULL;
        sc_skupina_a$ctr = NULL;
        #sc_skupina_a$position = NULL;
        sc_skupina_a$impresions = NULL;
        
      }else if(input$datasetMetrics == "CTR"){
        
        sc_skupina_a$page = NULL;
        sc_skupina_a$clicks = NULL;
        #sc_skupina_a$ctr = NULL;
        sc_skupina_a$position = NULL;
        sc_skupina_a$impresions = NULL;
        
      }
      
      
      sc_skupina_a <- aggregate(. ~date, data=sc_skupina_a, sum, na.rm=TRUE)
      
      
      ####
      ## GET DATA GROUP B
      ####
      promena_temp <- unlist(strsplit(x =input$skupinaB,split = '[\r\n]' ))
      vstupni_temp <- data.frame("URL"= promena_temp)
      adresare_temp <- input$exact2
      
      if(adresare_temp == TRUE){
        
        x <- foreach(i=vstupni_temp$URL) %do% search_analytics(sc_params_property, sc_params_from, sc_params_to, dimensions = c("page","date"), dimensionFilterExp = c(paste("page~~", i, "", sep="")), searchType = "web", rowLimit = sc_params_limit)
        temp <- as.data.frame(do.call("rbind", x), stringsAsFactors = FALSE)
        
      }else{
        
        x <- foreach(i=vstupni_temp$URL) %do% search_analytics(sc_params_property, sc_params_from, sc_params_to, dimensions = c("page","date"), dimensionFilterExp = c(paste("page==", i, "", sep="")), searchType = "web", rowLimit = sc_params_limit)
        temp <- as.data.frame(do.call("rbind", x), stringsAsFactors = FALSE)
        
      }
      
      ## Set sc_skupina_b
      sc_skupina_b <- temp
      
      ## Cleaning is caring
      promena_temp <- NULL
      vstupni_temp <- NULL
      adresare_temp <- NULL
      x <- NULL
      temp <- NULL
      
      
      if(input$datasetMetrics == "Imprese"){
        
        sc_skupina_b$page = NULL;
        sc_skupina_b$clicks = NULL;
        sc_skupina_b$ctr = NULL;
        sc_skupina_b$position = NULL;
        #sc_skupina_b$impresions = NULL;
        
      }else if(input$datasetMetrics == "Clicks"){
        
        sc_skupina_b$page = NULL;
        #sc_skupina_b$clicks = NULL;
        sc_skupina_b$ctr = NULL;
        sc_skupina_b$position = NULL;
        sc_skupina_b$impresions = NULL;
        
      }else if(input$datasetMetrics == "Positions"){
        
        sc_skupina_b$page = NULL;
        sc_skupina_b$clicks = NULL;
        sc_skupina_b$ctr = NULL;
        #sc_skupina_b$position = NULL;
        sc_skupina_b$impresions = NULL;
        
      }else if(input$datasetMetrics == "CTR"){
        
        sc_skupina_b$page = NULL;
        sc_skupina_b$clicks = NULL;
        #sc_skupina_b$ctr = NULL;
        sc_skupina_b$position = NULL;
        sc_skupina_b$impresions = NULL;
        
      }
      
      sc_skupina_b <- aggregate(. ~date, data=sc_skupina_b, sum, na.rm=TRUE)
      
      
      #### Set impact ####
      
      ## Merge GROUPs
      dataImpact <- merge(sc_skupina_b, sc_skupina_a, by="date", all = T)
      dataImpact[is.na(dataImpact)] <- 0
      names(dataImpact)[2] = "y"
      names(dataImpact)[3] = "x1"
      
      
      ## Magic whit dates
      pre_start <- strptime(sc_params_from, format="%Y-%m-%d")
      pre_end <- strptime(sc_params_change, format="%Y-%m-%d")
      
      post_start <- strptime(sc_params_change, format="%Y-%m-%d", tz="GMT") + 86400
      post_end <- strptime(sc_params_to, format="%Y-%m-%d")
      
      post_start <- as.POSIXlt(post_start)
      pre.period <- as.Date(c(pre_start,pre_end))
      post.period <- as.Date(c(post_start,post_end))
      dataImpact = zoo(dataImpact[, -1], dataImpact$date)
      
      ## CausalImpact
      impact <- CausalImpact(dataImpact, pre.period, post.period)
      
      ## Print
      output$firstPlot <- renderPlot({
        plot(impact)
      })
      
      output$secondPlot <- renderPrint({
        summary(impact)
      })
      
      output$thirdPlot <- renderPrint({
        summary(impact, "report")
      })
      
      ## Pročistit Impact
      shinyjs::enable("propocitat")
      
    }
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
