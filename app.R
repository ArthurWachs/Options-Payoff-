# Aplicativo desenvolvido por Arthur Wachslicht (@arthurwachs) e Pedro Osorio (@pedroosorio98)
# Para acessar o aplicativo, https://arthurwachs.shinyapps.io/opcoes/

library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)

#Calculates stock profit
stock_profit <- function(S0, St, n){
  return ((St - S0)*n)
}
#Calculates stock payoff
stock_payoff <- function(St, n){
  return (St*n)
}
#Calculates call profit
call_profit <- function(strike, price, St, n){
    return ((max(0, St - strike, na.rm = TRUE) - price)*n)
    
}
#Calculates call payoff
call_payoff <- function(strike, St, n){
    return ((max(0, St - strike, na.rm = TRUE))*n)
    
}
#Calculates put profit
put_profit <- function(strike, price, St, n){
    return ((max(0, strike - St, na.rm = TRUE) - price)*n)
}
#Calculates stock payoff
put_payoff <- function(strike, St, n){
    return ((max(0, strike - St, na.rm = TRUE))*n)
    
}


#User Interface
ui <- dashboardPage(
    dashboardHeader(title = "Opções"),
    dashboardSidebar(div("Arthur Wachslicht"),
                     div("Pedro Osorio"),
                     sliderInput(inputId = "slider", 
                                 label = "Número de Opções", 
                                 min = 0, max = 10, 
                                 value = 1, 
                                 step = 1),
                     sliderInput(inputId = "slider_acoes", 
                                 label = "Número de Ações", 
                                 min = 0, max = 10, 
                                 value = 0, 
                                 step = 1),
                     sliderInput(inputId = "slider_St", 
                                 label = "Intervalo de St", 
                                 min = 0, max = 150, 
                                 value = c(0,100), 
                                 step = 2.5)
                     ),
    dashboardBody(
        # Boxes need to be put in a row (or column)
      
        fluidRow(box(title = "Instruções", 

                       paste(
                         "O programa foi desenvolvido para calcular métricas relevantes de
                     estratégias envolvendo ações e opções.", "Os gráficos e a tabela são relativas ao dia do 
                     vencimento das opções. Além disso, todas as opções devem ter o mesmo ativo subjacente e mesma 
                     data de vencimento.","Inicie selecionando o número de ativos que deseja e qual o intervalo do 
                     preço do ativo objeto no vencimento. Em seguida, preencha o campo direito com os instrumentos 
                     financeiros desejados.", 
                     sep = "\n"
                       )
                     )),
        fluidRow(
            tabBox(width = 6,
            tabPanel("Lucro", plotOutput("total_profit_graph")),
            tabPanel("Payoff", plotOutput("total_payoff_graph")),
            tabPanel(textOutput("cash_flow"), tableOutput("table_output"),
                     title = "Tabela")
            
            )
            ,
            #box(tableOutput("test_table")),
            column(width = 5,
            uiOutput("dynamic_boxes")
            )
        
        )
    
))

# Define server logic required to draw a histogram
server <- function(input, output) {

#Returns payoff function
payoff_function <- function(x){
    options_df <- get_options_df()
    stock_df <- get_stock_df()
    a <- 0
    
    if (nrow(options_df) > 0) {
      for (i in 1:nrow(options_df)) {
        
        if (options_df$call_put[i] == "Call") {
          
          a <- a + call_payoff(strike = options_df$strike[i],
                               St = x,
                               n = options_df$quantity[i])
          
        } else{
          a <- a + put_payoff(strike = options_df$strike[i],
                              St = x,
                              n = options_df$quantity[i])
          
        }
        
      }
      
    }
    
    if (nrow(stock_df) > 0) {
      for (i in 1:nrow(stock_df)) {
        a <- a + stock_payoff(St = x,
                              n = stock_df$quantity[i])
        
      }
      
    }
    
    
    
    a
  }

#Returns profit function
profit_function <- function(x){
    options_df <- get_options_df()
    stock_df <- get_stock_df()
    
    a <- 0
    
    if(nrow(options_df) > 0){
      for (i in 1:nrow(options_df)) {
        
        if (options_df$call_put[i] == "Call") {
          
          a <- a + call_profit(strike = options_df$strike[i],
                               price = options_df$price[i],
                               St = x,
                               n = options_df$quantity[i])
          
        } else{
          a <- a + put_profit(strike = options_df$strike[i],
                              price = options_df$price[i],
                              St = x,
                              n = options_df$quantity[i])
          
        }
      
    }
    
    }
    
    if (nrow(stock_df)>0) {
      for (i in 1:nrow(stock_df)) {
        a <- a + stock_profit(St = x,
                              S0 = stock_df$price[i],
                              n = stock_df$quantity[i])
        
      }
      
    }
    
    a
}


#Return dynamic input boxes    
output$dynamic_boxes <- renderUI({
        tags <- tagList()
        for (i in seq_len(input$slider)) {
            tags[[i]] <- box(
                title = paste("Opção", i),
                textInput(paste0("nome", i), "Ticker",value = "Ticker"),
                selectInput(paste0("cp", i), "Call/Put", c("Call", "Put")),
                numericInput(paste0("qtd", i), "Quantidade", step = 1,value = 1),
                numericInput(paste0("strike", i), "Strike", value = 0),
                dateInput(paste0("vencimento", i), "Vencimento"),
                numericInput(paste0("preco", i), "Preço", value = 0)
                  
                
                
            )
        }
        if (input$slider_acoes > 0) {
          
          for (i in (input$slider+1):(input$slider+input$slider_acoes)) {
            tags[[i]] <- box(
              title = paste("Ação", i - input$slider),
              textInput(paste0("nome", i), "Ticker",value = "Ticker"),
              numericInput(paste0("qtd", i), "Quantidade", step = 1,value = 1),
              numericInput(paste0("preco", i), "Preço", value = 0)
              
              
              
            )
            
          }
          
        }
        
        
        tags
    })
    
#Returns dataframe with all option informations    
get_options_df <- reactive({
    options_df <- data.frame(ticker = as.character(),
                             call_put = as.character(),
                             quantity = as.numeric(),
                             strike = as.numeric(),
                             vencimento = as.Date(as.character()),
                             price = as.numeric())
    
    for (i in seq_len(input$slider)) {
        options_df <- options_df %>% 
            add_row(
                data.frame(
                    ticker = input[[paste0("nome", i)]],
                    call_put = input[[paste0("cp", i)]],
                    quantity = input[[paste0("qtd", i)]],
                    strike = input[[paste0("strike", i)]],
                    vencimento = input[[paste0("vencimento", i)]],
                    price = input[[paste0("preco", i)]],
                    stringsAsFactors = FALSE
                )
            )
        
    }
    
    options_df
    
})

#Returns dataframe with all stock informations    
get_stock_df <- reactive({
  stock_df <- data.frame(ticker = as.character(),
                           quantity = as.numeric(),
                           price = as.numeric())
  if (input$slider_acoes > 0) {
    for (i in (input$slider+1):(input$slider+input$slider_acoes)) {
      stock_df <- stock_df %>% 
        add_row(
          data.frame(
            ticker = input[[paste0("nome", i)]],
            quantity = input[[paste0("qtd", i)]],
            price = input[[paste0("preco", i)]],
            stringsAsFactors = FALSE
          )
        )
      
    }
    
  }
  
  
  
  stock_df
  
})

#Returns profit graph    
output$total_profit_graph <- renderPlot({
    options_df <- get_options_df()
    
    
    
    p <- Vectorize(profit_function)
    
    ggplot(data = data.frame(x = 0), mapping = aes(x = x))+
    stat_function(fun = p)+
    xlim(input$slider_St[1], input$slider_St[2])+
    geom_hline(yintercept = 0, col = "red", linetype = "dashed")+
    theme_minimal()+
    labs(title = "Lucro no Vencimento",
         x = "Preço do Ativo Subjacente (St)",
         y = "Lucro / Prejuízo")


    
    
})

#Retruns payoff graph
output$total_payoff_graph <- renderPlot({
    stock_df <- get_stock_df()
    options_df <- get_options_df()
    
    
    
    p <- Vectorize(payoff_function)
    
    ggplot(data = data.frame(x = 0), mapping = aes(x = x))+
        stat_function(fun = p)+
        xlim(input$slider_St[1], input$slider_St[2])+
        geom_hline(yintercept = 0, col = "red", linetype = "dashed")+
        theme_minimal()+
        labs(title = "Payoff no Vencimento",
             x = "Preço do Ativo Subjacente (St)",
             y = "Payoff")
    
    

})

#Returns table with payoff and profit
output$table_output <- renderTable({
  stock_df <- get_stock_df()
  
    options_df <- get_options_df()
    
    sequence <- seq(from = input$slider_St[1], 
                    to = input$slider_St[2], 
                    length.out = 6) %>% 
        round(2)
    
    initial_CF <- (-sum(options_df$price*options_df$quantity,na.rm = TRUE) - sum(stock_df$price*stock_df$quantity,na.rm = TRUE)) %>% 
        as.numeric() %>% 
        round(2)
    
    payoff <- lapply(sequence, payoff_function) %>% 
        as.numeric() %>% 
        round(2)
    profit <- lapply(sequence, profit_function) %>% 
        as.numeric() %>% 
        round(2)
    
    ret <- if (initial_CF != 0) {
        (100*profit/abs(initial_CF)) %>% 
            round(2) %>% 
            as.character() %>% 
            paste0("%")
    } else{
        rep("Nan", 6)
    }

        
    
    matriz <- c(sequence, payoff, profit, ret) %>% 
        matrix(ncol = 6, byrow = TRUE) %>% 
        as.data.frame() 
    
   row.names(matriz) <- c("St","Payoff", "Lucro", "Retorno")
    
    matriz
    
    
    

    
    
    
},
colnames = FALSE,
rownames = TRUE,
digits = 2)

#Returns initial cash flow from the operation
output$cash_flow <- renderText({
    options_df <- get_options_df()
    stock_df <- get_stock_df()
    
    initial_CF <- (-sum(options_df$price*options_df$quantity) -sum(stock_df$price*stock_df$quantity)) %>% 
        as.numeric() %>% 
        round(2)
    
    return(paste0("Fluxo de Caixa Inicial: R$", initial_CF))
    
    
})



  

   
}

# Run the application 
shinyApp(ui = ui, server = server)
