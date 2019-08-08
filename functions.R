# CARREGA OS PACOTES
library(dplyr)
library(readxl)
library(lubridate)
library(purrr)
library(stringr)
library(hms)

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
#horario <- select(auxiliar, "HORÁRIO DE ATENDIMENTO")
#horario <- hour(horario$`HORÁRIO DE ATENDIMENTO`) + minute(horario$`HORÁRIO DE ATENDIMENTO`)/60 # ALTERA O FORMATO DAS HORAS PARA A VARIÁVEL 'HORA FINAL'

# IDENTIFICAÇÃO DE TODAS AS CRIANÇAS
#id.crianca <- unique(disponibilidade$IDENTIFICAÇÃO) 

# SELECIONA OS REGULARES
#id.crianca <- unique(regular$IDENTIFICAÇÃO) 
#id.crianca <- as_tibble(id.crianca) # CONVERTE PARA TIBBLE
#colnames(id.crianca) <- "IDENTIFICAÇÃO" # NOMEA A VARIÁVEL

# SELECIONA OS REGULARES COM DISPONIBILIDADE APENAS DE UM HORÁRIO ESPECÍFICO
#prioridade <- filter(disponibilidade, disponibilidade$PERÍODO == "Horário") %>% right_join(y = id.crianca, by = "IDENTIFICAÇÃO", copy = F) 
#prioridade <- prioridade[complete.cases(prioridade),] # SELECIONA APENAS OS DADOS DE INTERESSE (REMOVE OS NA's)
#prioridade$`HORA INICIAL` <- hour(prioridade$`HORA INICIAL`) + minute(prioridade$`HORA INICIAL`)/60 # ALTERA O FORMATO DAS HORAS PARA A VARIÁVEL 'HORA INICIAL'
#prioridade$`HORA FINAL` <- hour(prioridade$`HORA FINAL`) + minute(prioridade$`HORA FINAL`)/60 # ALTERA O FORMATO DAS HORAS PARA A VARIÁVEL 'HORA FINAL'


# SELECIONA OS PRIORITÁRIOS (REGULARES C/ HORÁRIOS RESTRITOS)
#id.crianca <- unique(prioridade$IDENTIFICAÇÃO) 
#id.crianca <- as_tibble(id.crianca) # CONVERTE PARA TIBBLE
#colnames(id.crianca) <- "IDENTIFICAÇÃO" # NOMEA A VARIÁVEL

# SELECIONA OS TIPOS DE ATENDIMENTO NECESSÁRIO PARA OS PRIORITÁRIOS
#especialidade <- right_join(regular, id.crianca, "IDENTIFICAÇÃO") 

#by()


# SELECIONA APENAS AS  PLANILHAS DOS FUNCIONÁRIOS
funcionarios <- names(mysheets) %>% 
  grep(pattern = "Atendimento", value = T, invert = T) %>% 
  grep(pattern = "da Criança", value = T, invert = T) %>% 
  grep(pattern = "Auxiliar", value = T, invert = T)

# ENCONTRA AS TABELAS DA ESPECIALIDADE DESEJADA
find.speciality <- function(especialidade){
  x <- list()
    for (i in 1:length(funcionarios)){ 
   ifelse (colnames(mysheets[[funcionarios[i]]])[1] == especialidade, x[[i]] <- mysheets[[funcionarios[i]]], x[[i]] <- NA)}
  x <- x[!is.na(x)]
  ifelse(length(x) == 0, return(c("Não há profissionais atuando na especialidade:", especialidade)), return(x))
}


# SUBSTITUI OS VALORES DOS HORÁRIOS NAS PLANILHAS
#sapply(funcionarios, function(professional.speech){
# mysheets[[professional.speech]][[1]][[1]] <- hms(minutes = minute(mysheets[[professional.speech]][[1]][[1]]), hours = hour(mysheets[[professional.speech]][[1]][[1]]))
#})

# ARMAZENA A IDENTIFICAÇÃO DE TODAS AS CRIANÇAS CADASTRADAS
id.crianca.cat <- tibble(id.crianca = sort(unique(mysheets[["Cadastro da Criança"]][,1][[1]])), id.cat = seq_along(sort(unique(mysheets[["Cadastro da Criança"]][,1][[1]]))))


#for (i in 1:length(funcionarios)) {
#  aux <- filter(mysheets[[funcionarios[i]]], SEG %in% id.crianca$IDENTIFICAÇÃO)
#  if_else(dim(aux) )
#  }

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
                      return(print(c("Existe(m) criança(s) agendada(s) no mesmo horário com os profissionais:", comb[1], comb[2])))}))
  }, simplify = F)

  # VERIFICA SE HÁ 2 CRIANÇAS AGENDADAS NO MESMO HORÁRIO COM PROFISSIONAIS DIFERENTES  (CHECAR PQ 11 SAÍDAS E NÃO 21)
  verify <- combn(funcionarios, 2, function(comb){
    if((table(mysheets[[comb[1]]][,1:6] == mysheets[[comb[2]]][,1:6])["TRUE"]) != 19){
              return(print(c("Existe(m) criança(s) agendada(s) no mesmo horário com os profissionais:", comb[1], comb[2])))}} , simplify = F)
 

# RETORNA PARA CADA FUNCIONÁRIO OS DIAS DA SEMANA EM QUE HÁ ATENDIMENTO CADASTRADO DE FORMA CATEGORIZADA SEG-1, TER-2, QUA-3, QUI-4 e SEX-5
#dias.da.semana <- function(funcionarios){
#  sapply(funcionarios, function(funci){
#    data <- c(funci)
#    week <- c()
#    ifelse (which(!is.na(mysheets[[data]][,2:6])) >= 1 & which(!is.na(mysheets[[data]][,2:6])) <= 19,  week <- c(week, 1),                      # SEGUNDA
#            ifelse(which(!is.na(mysheets[[data]][,2:6])) >= 20 & which(!is.na(mysheets[[data]][,2:6])) <= 38, week <- c(week, 2),               # TERÇA
#                   ifelse(which(!is.na(mysheets[[data]][,2:6])) >= 39 & which(!is.na(mysheets[[data]][,2:6])) <= 57, week <- c(week, 3),        # QUARTA
#                          ifelse(which(!is.na(mysheets[[data]][,2:6])) >= 58 & which(!is.na(mysheets[[data]][,2:6])) <= 76, week <- c(week, 4), # QUINTA
#                                 week <- c(week, 5)))))                                                                                         # SEXTA
#  }, USE.NAMES = T)  
#}

#horarios.atend <- function(funcionarios){
#  semana <- dias.da.semana(funcionarios)
#  horarios <- list()
#  for(i in seq_along(semana)){
#    if(length(semana[[i]]) > 0){
#      dias <- unique(semana[[i]])
#      sapply(dias, function(j){
#        horarios[[funcionarios[i]]] <<- which(!is.na(mysheets[[funcionarios[i]]][[dias[j]+1]]))
#      })
#    }
#  }
#  return(horarios)
#}

# PROFISSÃO DE CADA PROFISSIONAL
speciality <- mysheets[funcionarios] %>% map(~ colnames(.x)[1])

# OS DIAS AGENDADOS
hour.seg <- mysheets[funcionarios] %>% map(~ which(!is.na(.$SEG)))
hour.ter <- mysheets[funcionarios] %>% map(~ which(!is.na(.$TER)))
hour.qua <- mysheets[funcionarios] %>% map(~ which(!is.na(.$QUA)))
hour.qui <- mysheets[funcionarios] %>% map(~ which(!is.na(.$QUI)))
hour.sex <- mysheets[funcionarios] %>% map(~ which(!is.na(.$SEX)))

# OS PERÍODOS AGENDADOS 
per.seg <- hour.seg %>% map(~ ifelse(test = .x <= 9 & .x>= 1, yes = periodo <- 1, no = periodo <- 2))
per.ter <- hour.ter %>% map(~ ifelse(test = .x <= 9 & .x>= 1, yes = periodo <- 1, no = periodo <- 2))
per.qua <- hour.qua %>% map(~ ifelse(test = .x <= 9 & .x>= 1, yes = periodo <- 1, no = periodo <- 2))
per.qui <- hour.qui %>% map(~ ifelse(test = .x <= 9 & .x>= 1, yes = periodo <- 1, no = periodo <- 2))
per.sex <- hour.sex %>% map(~ ifelse(test = .x <= 9 & .x>= 1, yes = periodo <- 1, no = periodo <- 2))

# QUEM JÁ ESTÁ AGENDADO
who.seg <- mysheets[funcionarios] %>% map(~ .$SEG[!is.na(.$SEG)])
who.ter <- mysheets[funcionarios] %>% map(~ .$TER[!is.na(.$TER)])
who.qua <- mysheets[funcionarios] %>% map(~ .$QUA[!is.na(.$QUA)])
who.qui <- mysheets[funcionarios] %>% map(~ .$QUI[!is.na(.$QUI)])
who.sex <- mysheets[funcionarios] %>% map(~ .$SEX[!is.na(.$SEX)])

# LOCALIZAÇÃO DOS HORÁRIOS INDISPONÍVEIS
indisp.seg <- who.seg %>% map(~ which(str_detect(.x, "Indispon")))
indisp.ter <- who.ter %>% map(~ which(str_detect(.x, "Indispon")))
indisp.qua <- who.qua %>% map(~ which(str_detect(.x, "Indispon")))
indisp.qui <- who.qui %>% map(~ which(str_detect(.x, "Indispon")))
indisp.sex <- who.sex %>% map(~ which(str_detect(.x, "Indispon")))

# ARMAZENA TODAS AS INFORMAÇÕES EM UM ÚNICO OBJETO
seg <- tibble(hour.seg, per.seg, who.seg, indisp.seg) %>% 
  pmap(~ cbind(..1, ..2, ..3, ..4)) 
ter <- tibble(hour.ter, per.ter, who.ter, indisp.ter) %>% 
  pmap(~ cbind(..1, ..2, ..3, ..4))
qua <- tibble(hour.qua, per.qua, who.qua, indisp.qua) %>% 
  pmap(~ cbind(..1, ..2, ..3, ..4))
qui <- tibble(hour.qui, per.qui, who.qui, indisp.qui) %>% 
  pmap(~ cbind(..1, ..2, ..3, ..4))
sex <- tibble(hour.sex, per.sex, who.sex, indisp.sex) %>% 
  pmap(~ cbind(..1, ..2, ..3, ..4))

child <- pwalk(list(who.seg, who.ter, who.qua, who.qui, who.sex), ~ unique)
  
child <- pwalk(child, ~ pwalk(~unlist))

aux <- c()
children <- sapply(seq_along(funcionarios), function(i){
  aux <- unique(c(aux, unlist(who.seg[[funcionarios[i]]]), unlist(who.ter[[funcionarios[i]]]), unlist(who.qua[[funcionarios[i]]]), unlist(who.qui[[funcionarios[i]]]), unlist(who.sex[[funcionarios[i]]])))
}, simplify = T)

aux <- c()
ch <- sapply(seq_along(children), function(func){
  aux <- c(aux, children[[func]])
}, simplify = T)
 
