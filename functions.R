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


# SUBSTITUI OS VALORES DOS HORÁRIOS NAS PLANILHAS (NOT WORKING)
newtimetable <- function(){
  sapply(funcionarios, function(professional.speech){
    mysheets[[professional.speech]][[1]][[1]] <<- hms(minutes = minute(mysheets[[professional.speech]][[1]][[1]]), hours = hour(mysheets[[professional.speech]][[1]][[1]]))
  })  
}

# ARMAZENA A IDENTIFICAÇÃO DE TODAS AS CRIANÇAS CADASTRADAS EM ORDEM ALFABÉTICA 
id.crianca.cat <- tibble(IDENTIFICAÇÃO = sort(unique(mysheets[["Cadastro da Criança"]][,1][[1]])), IDENTIFICAÇÃO.CAT = seq_along(sort(unique(mysheets[["Cadastro da Criança"]][,1][[1]]))))


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
 
  if(length(mysheets[['Cadastro da Criança']][['IDENTIFICAÇÃO']]) != length(unique(mysheets[['Cadastro da Criança']][['IDENTIFICAÇÃO']]))){
    print(c("Há crianças cadastradas com identificadores iguais. O número de vezes em que o indentificador aparece representa o número de repetições do mesmo.", mysheets[['Cadastro da Criança']][['IDENTIFICAÇÃO']][duplicated(mysheets[['Cadastro da Criança']][['IDENTIFICAÇÃO']])]))
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

# CATEGORIZA AS ESPECIALIDADES EM ORDEM ALFABÉTICA
speciality.cat <- as_tibble(cbind(sort(unique(sapply(speciality, "[[", 1))), seq_along(unique(sapply(speciality, "[[", 1))))) %>% set_names("TIPO DE ATENDIMENTO", "ATENDIMENTO.CAT")

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

# RETORNA AS CRIANÇAS QUE ESTÃO AGENDADAS
registered.children <- function(){
  children <- c()
  sapply(funcionarios, function(func){
    if(length(who.seg[[func]]) != 0){
      children <<- c(children, who.seg[[func]])
    }
    if(length(who.ter[[func]]) != 0){
      children <<- c(children, who.ter[[func]])
    }
    if(length(who.qua[[func]]) != 0){
      children <<- c(children, who.qua[[func]])
    }
    if(length(who.qui[[func]]) != 0){
      children <<- c(children, who.qui[[func]])
    }
    if(length(who.sex[[func]]) != 0){
      children <<- c(children, who.sex[[func]])
    }
  })
  children <- as_tibble(grep(x = unique(children), pattern = "Indispon", value = T, invert = T)) %>% set_names("IDENTIFICAÇÃO")
  return(children)
}

#registered.children.result <- registered.children()

# VERIFICA AS CRIANÇAS AGENDADAS QUE NÃO ESTÃO CADASTRADAS
realize.child <- function(registered.children.result){
  if(table(registered.children.result[[1]] %in% id.crianca.cat[[1]])["FALSE"] != 0){
    return(c("As seguintes crianças estão agendadas, mas não estão cadastradas:", registered.children.result[[1]][which(!registered.children.result[[1]] %in% id.crianca.cat[[1]])]))
  }
}

# CRUZA AS INFORMAÇÕES DAS CRIANÇAS AGENDADAS E CADASTRADAS COM SUAS RESPECTIVAS CATEGORIAS
id.registered.children <- semi_join(id.crianca.cat, registered.children.result)

needed.and.availability <- function(){
  regular.children.needed <<- filter(mysheets[["Atendimento Regular"]],  `QTD. DE ATENDIMENTO SEMANAL` != 0) %>% semi_join(x = id.crianca.cat)
  regular.speciality.needed <<- semi_join(mysheets[["Atendimento Regular"]]["TIPO DE ATENDIMENTO"], x = speciality.cat["TIPO DE ATENDIMENTO"]) 
  if(table(unique(mysheets[["Atendimento Regular"]]["TIPO DE ATENDIMENTO"][[1]]) %in% regular.speciality.needed["TIPO DE ATENDIMENTO"][[1]])["FALSE"] != 0){
    return(c("Não há profissionais atuando na(s) área(s) de:", unique(mysheets[["Atendimento Regular"]]["TIPO DE ATENDIMENTO"][[1]])[!unique(mysheets[["Atendimento Regular"]]["TIPO DE ATENDIMENTO"][[1]]) %in% regular.speciality.needed["TIPO DE ATENDIMENTO"][[1]]]))
  }
  biweekly.regular.needed <- filter(mysheets[["Atendimento Regular"]], `FREQUENCIA DE ATENDIMENTO` == "Quinzenal" & `QTD. DE ATENDIMENTO SEMANAL` == 1 & `TIPO DE ATENDIMENTO` == "Nutrição")
  if(nrow(filter(mysheets[["Atendimento Regular"]], `FREQUENCIA DE ATENDIMENTO` == "Quinzenal" & `QTD. DE ATENDIMENTO SEMANAL` > 1)) > 0){
    print(c("A(s) criança(s) abaixo possue(m) necessidade(s) de atendimento(s) quizenal(is) com frequência maior que uma visita na semana:",
            unique(filter(mysheets[["Atendimento Regular"]], `FREQUENCIA DE ATENDIMENTO` == "Quinzenal" & `QTD. DE ATENDIMENTO SEMANAL` > 1)[["IDENTIFICAÇÃO"]])))
  }
  if(nrow(filter(mysheets[["Atendimento Regular"]], `FREQUENCIA DE ATENDIMENTO` == "Quinzenal" & `TIPO DE ATENDIMENTO` != "Nutrição")) > 0){
    print(c("A(s) criança(s) abaixo possue(m) necessidade(s) de atendimento(s) quizenal(is) em uma especialidade que não é Nutrição:",
               unique(filter(mysheets[["Atendimento Regular"]], `FREQUENCIA DE ATENDIMENTO` == "Quinzenal" & `TIPO DE ATENDIMENTO` != "Nutrição")[["IDENTIFICAÇÃO"]])))
  }  
}



for (i in seq_along(funcionarios)) {
  if_else(condition = length(who.seg[[i]]) != 0, 
          true = if_else(condition = length(indisp.seg[[i]]) != 0,
                         true = sapply(indisp.seg[[i]], function(indice){
                           cri[indice] <- 0
                           esp[indice] <- 
                         }),
                         false = ),
          false = )
}
filter()



#as.vector(rbind(1, 2, 3, ...))


#FILTRAR NUMERO DE ATENDIMENTO NECESSARIOS NA SEMANA
#CHECAR SE HÁ ALGUÉM COM DEMANDA QUINZENAL EM UMA ESPECIALIDADE QUE NÃO É NUTRIÇÃO
#CHECAR SE NUMERO DE ATENDIMENTOS SEMANAIS PARA NUTRIÇÃO > 1
#CONVERTER OS RESULTADOS DA OTIMIZAÇÃO EM TABELAS
#CRIAR MODELO DO RELATÓRIO DE SAÍDA
#PASSAR TODAS AS INFORMAÇÕES DAS AGENDAS DOS FUNCIONÁRIOS PARA O SETBOUNDS