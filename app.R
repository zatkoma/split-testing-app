#
# ShinyApp - Console Casual Impact
#
#

##Sys.setlocale("LC_CTYPE", "en_US.UTF-8")



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
##devtools::install_version("MASS", "7.3-51.1")

options(shiny.port = 7775)
options(shiny.host = "127.0.0.1")

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  shinyjs::useShinyjs(),
  
  verbatimTextOutput("console_text"),
  
  # Název aplikace
  titlePanel("SEO Testing - Explorer"),
  
  # Úvodní slůvko
  p("Tato aplikace byla vytořena, abyste mohli jednoduše pracovat s vyhodnocováním testů v SEO. Její základní výhody jsou v tom, že 
    se může připojit na Google Search Consoli, odkud tahá data a ty porovnává mezi sebou. Testování v SEO vychází z toho, že si vyberete
    dataset URL adres (skupina A), kde nic nezměníte a vše necháte tak jak má být. A následně dataset URL adres (skupina B), kde provedete
    jisté změny a úpravy."),
  
  hr(),
  
  # Sidebar with a slider input for number of bins 
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
      
      hr(),
      
      actionButton(inputId = "propocitat",label = "Spočítat změnu", style="margin-bottom: 25px")
      
    ),
    
    # Show a plot of the generated distribution
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

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  shinyjs::disable("propocitat")
  
  ## Testing výpis:
  info <- reactiveValues()
  
  ## Přihlášení do GSC:
  observeEvent(input$prihlasitse,{
    
    scr_auth(new_user = TRUE)
    scr_websites <- list_websites()
    
    info$account_list <- list_websites()
    
    shinyjs::disable("prihlasitse")
    shinyjs::enable("propocitat")
    
  })
  
  ## Inicialiazace webů po přihlášení
  output$choose_dataset <- renderUI({
    selectInput("dataset", "Dostupné weby:", as.list(info$account_list[1]))
  })
  
  output$choose_metrics <- renderUI({
    selectInput("datasetMetrics", "Srovnávané metriky:", as.list(c("Imprese", "Clicks", "Positions", "CTR")))
  })
  
  
  ## Propočítávání dat
  observeEvent(input$propocitat,{
    {
      
      shinyjs::disable("propocitat")
      
      ## Nepotřebná validace 
      ## output$selected_var <- reactive(input$dataset)
      
      ## Disable this a přidat tlačítko upravil jsem data o projektu(změny)
      
      ## Zisk dat z konfigurátoru:
      sc_params_from <- input$timeFrom
      sc_params_change <- input$timeChange
      sc_params_to <- input$timeTo
      sc_params_property <- input$dataset
      
      
      ## Stáhnutí overview dat:
      sc_all <- search_analytics(sc_params_property, sc_params_from, sc_params_to, dimensions = c("page", "date"), searchType = "web", rowLimit = 100000)
      ## TODO: Validace existence LPs
      
      ## Getnutí LPs, které jsou ve skupině A
      pages_temp = sc_all
      pages_temp$impressions = NULL;
      pages_temp$clicks = NULL;
      pages_temp$ctr = NULL;
      pages_temp$position = NULL;
      
      pages_temp = aggregate(. ~ page, pages_temp, sum, na.rm=TRUE, na.action="na.pass")
      pages_temp["obsahuje_url"] <- NA
      
      ## Include te hlavni URL
      
      promena_temp <- unlist(strsplit(x =input$skupinaA,split = '[\r\n]' ))
      
      vstupni_temp <- data.frame("URL"= promena_temp)
      
      adresare_temp <- input$exact1
      if(adresare_temp == TRUE){
        vstupni_temp$URL <- paste(".*", vstupni_temp$URL, ".*", sep="")
      }else{
        vstupni_temp$URL <- paste(vstupni_temp$URL, sep="")
      }
      
      
      pages_temp$obsahuje_url <- str_match(pages_temp$page, vstupni_temp$URL)
      
      pages_temp$date <- NULL
      pages_temp$page <- NULL
      pages_temp$data <- 1
      
      pages_temp = aggregate(. ~ obsahuje_url, pages_temp, sum, na.rm=TRUE, na.action="na.pass")
      pages_temp$data <- NULL
      
      sc_skupina_a <- sc_all[sc_all$page %in% pages_temp$obsahuje_url,]
      
      pages_temp <- NULL
      vstupni_temp <- NULL
      promena_temp <- NULL
      
      ##sc_skupina_a <- sc_all[sc_all$page == input$skupinaA,]
      
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
      
      
      ####### Skupina B #########
      
      ##indices <- which(sc_all$page == input$skupinaB)
      ##sc_skupina_b <- sc_all[indices, ]
      
      ## Getnutí LPs, které jsou ve skupině A
      pages_temp = sc_all
      pages_temp$impressions = NULL;
      pages_temp$clicks = NULL;
      pages_temp$ctr = NULL;
      pages_temp$position = NULL;
      
      pages_temp = aggregate(. ~ page, pages_temp, sum, na.rm=TRUE, na.action="na.pass")
      pages_temp["obsahuje_url"] <- NA
      
      ## Include te hlavni URL
      promena_temp <- unlist(strsplit(x =input$skupinaB,split = '[\r\n]' ))
      
      vstupni_temp <- data.frame("URL"= promena_temp)
      
      adresare_temp <- input$exact2
      if(adresare_temp == TRUE){
        vstupni_temp$URL <- paste(".*", vstupni_temp$URL, ".*", sep="")
      }else{
        vstupni_temp$URL <- paste(vstupni_temp$URL, sep="")
      }
      
      pages_temp$obsahuje_url <- str_match(pages_temp$page, vstupni_temp$URL)
      
      pages_temp$date <- NULL
      pages_temp$page <- NULL
      pages_temp$data <- 1
      
      pages_temp = aggregate(. ~ obsahuje_url, pages_temp, sum, na.rm=TRUE, na.action="na.pass")
      pages_temp$data <- NULL
      
      sc_skupina_b <- sc_all[sc_all$page %in% pages_temp$obsahuje_url,]
      
      pages_temp <- NULL
      vstupni_temp <- NULL
      promena_temp <- NULL
      
      ##sc_skupina_b <- sc_all[sc_all$page == input$skupinaB,]
      
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
      
      dataImpact <- merge(sc_skupina_b, sc_skupina_a, by="date", all = T)
      dataImpact[is.na(dataImpact)] <- 0
      ##dataImpact <- aggregate(. ~date, data=dataImpact, sum, na.rm=TRUE)
      ##dataImpact <- mean(dataImpact, na.rm=TRUE)
      
      names(dataImpact)[2] = "y"
      names(dataImpact)[3] = "x1"
      
      
      
      pre_start <- strptime(sc_params_from, format="%Y-%m-%d")
      pre_end <- strptime(sc_params_change, format="%Y-%m-%d")
      
      post_start <- strptime(sc_params_change, format="%Y-%m-%d", tz="GMT") + 86400
      post_end <- strptime(sc_params_to, format="%Y-%m-%d")
      
      post_start <- as.POSIXlt(post_start)
      
      
      preend <- difftime(strptime(sc_params_change, format="%Y-%m-%d"),strptime(sc_params_from, format="%Y-%m-%d"),units="days")
      preend <- as.double(preend)
      
      postend <- preend + difftime(strptime(sc_params_to, format="%Y-%m-%d"),strptime(sc_params_change, format="%Y-%m-%d"),units="days")
      postend <- as.double(postend)
      
      ##pre.period <- c(1,preend)
      ##post.period <- c(preend + 1,postend)
      
      pre.period <- as.Date(c(pre_start,pre_end))
      post.period <- as.Date(c(post_start,post_end))
      
      
      dataImpact = zoo(dataImpact[, -1], dataImpact$date)
      
      
      impact <- CausalImpact(dataImpact, pre.period, post.period)
      
      output$firstPlot <- renderPlot({
        plot(impact)
      })
      
      output$secondPlot <- renderPrint({
        summary(impact)
      })
      
      output$thirdPlot <- renderPrint({
        summary(impact, "report")
      })
      
      shinyjs::enable("propocitat")
      
    }
  })
  
  
  observeEvent(input$exportovat,{
    
    
    ## Zisk dat z konfigurátoru:
    sc_params_from <- input$timeFrom
    sc_params_to <- input$timeTo
    sc_params_property <- input$dataset
    
    sc_all <- search_analytics(sc_params_property, sc_params_from, sc_params_to, searchType = "web")
    
    
  }
  )
  
}

# Run the application 
shinyApp(ui = ui, server = server)
