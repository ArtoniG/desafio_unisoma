# CARREGA OS PACOTES
library(sqldf)
library(dplyr)
library(readxl)
library(Rcpp)
library(timevis)
library(lubridate)
library(tidyverse)

# IMPORTAÇÃO DOS DADOS
read_excel_allsheets <- function(filename, tibble = T) {
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
  if(!tibble) x <- lapply(x, as.data.frame)
  names(x) <- sheets
  x
}

mysheets <- read_excel_allsheets("~/Downloads/Modelo de Dados_CCP_v1.xlsx")

# TODOS OS HORÁRIOS QUE A CLÍNICA OFERECE ATENDIMENTO
horario <- select(auxiliar, "HORÁRIO DE ATENDIMENTO")
horario <- hour(horario$`HORÁRIO DE ATENDIMENTO`) + minute(horario$`HORÁRIO DE ATENDIMENTO`)/60 # ALTERA O FORMATO DAS HORAS PARA A VARIÁVEL 'HORA FINAL'

# IDENTIFICAÇÃO DE TODAS AS CRIANÇAS
id.crianca <- unique(disponibilidade$IDENTIFICAÇÃO) 

# SELECIONA OS REGULARES
id.crianca <- unique(regular$IDENTIFICAÇÃO) 
id.crianca <- as_tibble(id.crianca) # CONVERTE PARA TIBBLE
colnames(id.crianca) <- "IDENTIFICAÇÃO" # NOMEA A VARIÁVEL

# SELECIONA OS REGULARES COM DISPONIBILIDADE APENAS DE UM HORÁRIO ESPECÍFICO
prioridade <- filter(disponibilidade, disponibilidade$PERÍODO == "Horário") %>% right_join(y = id.crianca, by = "IDENTIFICAÇÃO", copy = F) 
prioridade <- prioridade[complete.cases(prioridade),] # SELECIONA APENAS OS DADOS DE INTERESSE (REMOVE OS NA's)
prioridade$`HORA INICIAL` <- hour(prioridade$`HORA INICIAL`) + minute(prioridade$`HORA INICIAL`)/60 # ALTERA O FORMATO DAS HORAS PARA A VARIÁVEL 'HORA INICIAL'
prioridade$`HORA FINAL` <- hour(prioridade$`HORA FINAL`) + minute(prioridade$`HORA FINAL`)/60 # ALTERA O FORMATO DAS HORAS PARA A VARIÁVEL 'HORA FINAL'


# SELECIONA OS PRIORITÁRIOS (REGULARES C/ HORÁRIOS RESTRITOS)
id.crianca <- unique(prioridade$IDENTIFICAÇÃO) 
id.crianca <- as_tibble(id.crianca) # CONVERTE PARA TIBBLE
colnames(id.crianca) <- "IDENTIFICAÇÃO" # NOMEA A VARIÁVEL

# SELECIONA OS TIPOS DE ATENDIMENTO NECESSÁRIO PARA OS PRIORITÁRIOS
especialidade <- right_join(regular, id.crianca, "IDENTIFICAÇÃO") 

by()


# SELECIONA APENAS AS  PLANILHAS DOS FUNCIONÁRIOS
funcionarios <- grep(pattern = "Atendimento", x = nome, value = T, invert = T)
funcionarios <- grep(pattern = "da Criança", x = nome, value = T, invert = T)
funcionarios <- funcionarios[-1]

# ECONTRA A TABELA DO PROFISSIONAL DESEJADO (PRECISA SER MELHORADA)
find.speciality <- function(especialidade){
  for (i in 1:length(funcionarios)) 
    ifelse (colnames(mysheets[[funcionarios[i]]])[1] == especialidade, return(mysheets[[funcionarios[i]]]), NA)
}

id.crianca <- unique(mysheets[[2]][,1])
for (i in 1:length(funcionarios)) {
  aux <- filter(mysheets[[funcionarios[i]]], SEG %in% id.crianca$IDENTIFICAÇÃO)
  if_else(dim(aux) )
  }






