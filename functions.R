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
funcionarios <- names(mysheets)
funcionarios <- grep(pattern = "Atendimento", x = funcionarios, value = T, invert = T)
funcionarios <- grep(pattern = "da Criança", x = funcionarios, value = T, invert = T)
funcionarios <- funcionarios[-1] # REMOVE A PLANILHA AUXILIAR (PERGUNTAR SE ESTA EXISTE APENAS PARA ESSE BD MODELO)

# ENCONTRA AS TABELAS DA ESPECIALIDADE DESEJADA
find.speciality <- function(especialidade){
  x <- list()
    for (i in 1:length(funcionarios)){ 
   ifelse (colnames(mysheets[[funcionarios[i]]])[1] == especialidade, x[[i]] <- mysheets[[funcionarios[i]]], x[[i]] <- NA)}
  x <- x[!is.na(x)]
  x
}

check.speciality <- function(find.speciality) ifelse(length(find.speciality) == 0, print("Não há"), NA)

id.crianca <- unique(mysheets[["Cadastro da Criança"]][,1])
for (i in 1:length(funcionarios)) {
  aux <- filter(mysheets[[funcionarios[i]]], SEG %in% id.crianca$IDENTIFICAÇÃO)
  if_else(dim(aux) )
  }

check.no.registered <- function(){
  # VERIFICA SE HÁ CRIANÇAS NÃO CADASTRADAS NA PLANILHA ATENDIMENTO REGULAR
  verify <- filter(mysheets[['Cadastro da Criança']]['IDENTIFICAÇÃO'], mysheets[['Cadastro da Criança']]['STATUS DE ATENDIMENTO'] != "Legado") %>% right_join(y = mysheets[["Atendimento Regular"]]["IDENTIFICAÇÃO"], "IDENTIFICAÇÃO") 
  if(length(verify$IDENTIFICAÇÃO) != length(mysheets[["Atendimento Regular"]]$IDENTIFICAÇÃO)){
    print("Há crianças na planilha Atendimento Regular que não estão cadastradas.")
  }
  
  # VERIFICA SE HÁ CRIANÇAS NÃO CADASTRADAS NA PLANILHA ATENDIMENTO ESPORÁDICO
  verify <- filter(mysheets[['Cadastro da Criança']]['IDENTIFICAÇÃO'], mysheets[['Cadastro da Criança']]['STATUS DE ATENDIMENTO'] != "Legado") %>% right_join(y = mysheets[["Atendimento Esporádico"]]["IDENTIFICAÇÃO"], "IDENTIFICAÇÃO") 
  if(length(verify$IDENTIFICAÇÃO) != length(mysheets[["Atendimento Esporádico"]]$IDENTIFICAÇÃO)){
    print("Há crianças na planilha Atendimento Esporádico que não estão cadastradas.")
  }
 

}


  # VERIFICA SE HÁ 2 CRIANÇAS AGENDADAS NO MESMO HORÁRIO COM PROFISSIONAIS DIFERENTES 
  combn(funcionarios, 2, function(comb){  # APLICA SOBRE TODAS AS COMBINAÇÕES 2 À 2 POSSÍVEIS DE FUNCIONÁRIOS AS VERIFICAÇÕES ABAIXO
  
  # VERIFICA SE O 1º OU 2º FUNCIONÁRIO É DA NUTRIÇÃO
  if_else(condition = colnames(mysheets[[comb[1]]])[1] == "Nutrição" | colnames(mysheets[[comb[2]]])[1] == "Nutrição", 
    
        # SE FALSO: COMPARA AS TABELAS VERIFICANDO SE HÁ ALGUMA CRIANÇA AGENDADA NO MESMO HORÁRIO
        false = if(table((mysheets[[comb[1]]][,1:6] == mysheets[[comb[2]]][,1:6]))["TRUE"] != 18){
          toString(c("Existe(m) criança(s) agendada(s) no mesmo horário com os profissionais:", comb[1], comb[2]))}, 
        
        # SE VERDADEIRO: VERIFICA SE O 1º FUNCIONÁRIO É DA NUTRIÇÃO
        true = if_else(condition = colnames(mysheets[[comb[1]]])[1] == "Nutrição", 
                   
                   # SE VERDADEIRO: COMPARA APENAS A PARTE DA PLANILHA QUE ESTA SENDO PLANEJADA COM A PLANILHA DO 2º FUNCINÁRIO 
                   true = if(table(mysheets[[comb[1]]][,1:6] == mysheets[[comb[2]]][,1:6])["TRUE"] != 18){
                      toString(c("Existe(m) criança(s) agendada(s) no mesmo horário com os profissionais:", comb[1], comb[2]))},
                   
                   # SE FALSO: COMPARA APENAS A PARTE DA PLANILHA 
                   false = if(table(mysheets[[comb[1]]][,1:6] == mysheets[[comb[2]]][,1:6])["TRUE"] != 18){
                      toString(c("Existe(m) criança(s) agendada(s) no mesmo horário com os profissionais:", comb[1], comb[2]))}))
  }, simplify = F)

  # VERIFICA SE HÁ 2 CRIANÇAS AGENDADAS NO MESMO HORÁRIO COM PROFISSIONAIS DIFERENTES  (CHECAR PQ 11 SAÍDAS E NÃO 21)
  verify <- combn(funcionarios, 2, function(comb){
    if(table(mysheets[[comb[1]]][,1:6] == mysheets[[comb[2]]][,1:6])["TRUE"] != 18){
              toString(c("Existe(m) criança(s) agendada(s) no mesmo horário com os profissionais:", comb[1], comb[2]))}} , simplify = F)
 
  