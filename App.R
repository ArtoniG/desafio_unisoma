library(shiny)
library(readxl)
library(Rcpp)
library(sqldf)
library(lpsolve)
CCP <- read_xlsx("~/Downloads/Modelo de Dados_CCP_v1.xlsx",col_names = T)

ui <- pageWithSidebar(headerPanel("CCP Project"), 
                      sidebarPanel(
                        selectInput(inputId = "variable", label = "Variável:", choices = c())
                      ), 
                      mainPanel())

server <-  function(input, output){
  
}

shinyApp(ui, server)
