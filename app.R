library(shiny)
library(quantmod)
library(rvest)
library(DT)
library(corrplot)


ui <- fluidPage(
  headerPanel("Stock Query"),
  
  sidebarLayout(
    sidebarPanel(textInput("tick", "Ticker", "GOOG"),
                 dateRangeInput("dates", 
                                "Date Range",
                                start = "2017-01-01", 
                                end = "2017-12-31"),
                 actionButton("get", "Get Plot")),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Charts", plotOutput("plot")), 
        tabPanel("Correlation", tableOutput("corr")),
        id = "tab"
      )
    )
  )
)

server <- function(input, output){
  
  # get S&P 500 tickers from wikipedia
  url <- "https://en.wikipedia.org/wiki/List_of_S%26P_500_companies"
  SP500 <- url %>%
    read_html() %>%
    html_nodes(xpath='//*[@id="mw-content-text"]/div/table[1]') %>%
    html_table()
  SP500 <- SP500[[1]]
  sp_500 <- SP500$`Ticker symbol`
  #sp_500 <- sp_500[1:100]
 
  
  # get data
  stock <- reactive({
    getSymbols(Symbols = input$tick, src = "yahoo", 
               from = input$dates[1],
               to = input$dates[2], 
               auto.assign = FALSE)
  })
  
  # candlestick plot
  output$plot <- renderPlot({
    chartSeries(stock(),
                name = input$tick,
                type = "candlestick",
                theme = "white",
                TA = NULL
    )
  })
  
  stock_return <- reactive({
    dailyReturn(getSymbols(Symbols = input$tick, src = "yahoo", 
                           from = input$dates[1],
                           to = input$dates[2], 
                           auto.assign = FALSE), type = 'log')
  })
  
  # correlation table
  output$corr <- renderTable({
    
    peer_return <- data.frame()
    symb <- list()
    for (tick in sp_500){
      tryCatch({
        peers <- getSymbols(tick, from=input$dates[1], to=input$dates[2], auto.assign = FALSE)
        if (any(is.na(Ad(peers)))){
          next
        }
        if (ncol(peer_return) == 0){
          peer_return = dailyReturn(Ad(peers), type = "log")
        }
        else{
          peer_return <- cbind(peer_return, dailyReturn(Ad(peers), type = "log"))
        }
        symb <- c(symb, tick)
      }, error=function(e){})
    }
    peer_return <- setNames(data.frame(peer_return), symb)
    
    
    i <- 1
    corr_list = c()
    while (i <= length(symb)){
      corr_list <- c(corr_list, cor(stock_return(), peer_return[,symb[[i]]]))
      i = i+1
    }
    corr_list <- t(data.frame(corr_list))
    corr_list <- setNames(corr_list, symb)
    top6 <- names(corr_list[order(corr_list,decreasing = TRUE)][1:6])
    #top6
    cor(peer_return[,top6])
    
  })
  
}

shinyApp(ui = ui, server = server)