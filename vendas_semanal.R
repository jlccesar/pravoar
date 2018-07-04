library(xtable)
library(plyr)
library(tidyverse)
library(forcats)
library(readxl)
library(magrittr)
library(lubridate)
library(knitr)
library(kableExtra)
library(tinytex)

vendas <- read_excel("C:/Users/João/Desktop/pravoar/vendas.xlsx")

vendas <- filter(vendas,data_reserva>=today()-7)




#levels utilizados no codigo
periodo_levels <- c("0H-5H59","6H-11H59","12H-17H59","18H-23H59")
dia_levels <- c("Domingo","Segunda","Terca","Quarta","Quinta", "Sexta", "Sabado")
estadia_levels <- c("OW","1-3","4-7","8 ou +")
antecipacao_levels <- c("1-7","8-14","15-30","31 ou +")
idade_levels <- c("1-17","18-24","25-35","36-45","46 ou +")
sexo_levels <- c("Feminino", "Masculino")
owrt_levels <- c("OW","RT")
cia_levels <- c("Azul","Avianca","Gol","Latam","Passaredo")
bagagem_levels <- c("Sim","Não")
escala_levels <- c("0","1","2","3")




#calculando tempo de emissÃ£o em minutos
vendas$tempo_emissao <- as.numeric(difftime(vendas$data_emissao,vendas$data_reserva,units = "mins"))

#calculando tempo de ida total do voo
vendas$tempo_ida <- difftime(vendas$data_chegada_ida,vendas$data_ida,units = "hours") 

#calculando tempo de volta total do voo
vendas$tempo_volta <- difftime(vendas$data_chegada_volta,vendas$data_volta,units = "hours") 




#periodo da compra
vendas <- mutate(vendas,periodo_compra=hour(data_reserva))

vendas$periodo_compra <- if_else(vendas$periodo_compra<6,"0H-5H59",if_else(vendas$periodo_compra<12,"6H-11H59",if_else(vendas$periodo_compra<18,"12H-17H59","18H-23H59")))
vendas$periodo_compra <- factor(vendas$periodo_compra,levels =periodo_levels )

#periodo da ida
vendas <- mutate(vendas,periodo_ida=hour(data_ida))
vendas$periodo_ida <- if_else(vendas$periodo_ida<6,"0H-5H59",if_else(vendas$periodo_ida<12,"6H-11H59",if_else(vendas$periodo_ida<18,"12H-17H59","18H-23H59")))
vendas$periodo_ida <- factor(vendas$periodo_ida,levels =periodo_levels )

#periodo da volta
vendas <- mutate(vendas,periodo_volta=hour(data_volta))
vendas$periodo_volta <- if_else(vendas$periodo_volta<6,"0H-5H59",if_else(vendas$periodo_volta<12,"6H-11H59",if_else(vendas$periodo_volta<18,"12H-17H59","18H-23H59")))
vendas$periodo_volta <- factor(vendas$periodo_volta,levels =periodo_levels )

#criando tabela de apresentação de count de periodo compra;ida;volta
periodo_compra <-vendas %>% count(periodo_compra)

if (periodo_compra[1,1]!="0H-5H59"){
  x <- c("0H-5H59","-")
  periodo_compra <- rbind(x,periodo_compra) 
}

periodo_ida <- vendas %>% count(periodo_ida)
if (periodo_ida[1,1]!="0H-5H59"){
  x <- c("0H-5H59","-")
  periodo_ida <- rbind(x,periodo_ida) 
}


periodo_volta <- vendas %>% count(periodo_volta)
if (!is.na(periodo_volta[1,1])){
  if (periodo_volta[1,1]!="0H-5H59"){
    x <- c("0H-5H59","-")
    periodo_volta <- rbind(x,periodo_volta)
  }
} else {x <- c("0H-5H59","-")
periodo_volta <- rbind(x,periodo_volta) 
}

# if(!is.na(periodo_volta[2,1])){
#   if(periodo_volta[2,1]="6H-11H59"){
#     x <- c("6H-11H59","-")
#     periodo_volta <- rbind(periodo_volta[1,2],x,periodo_volta[2,2])
#   } 
# } else {print("shit")
#   }


periodo_tabela <- bind_cols(periodo_compra[1],periodo_compra[2],periodo_ida[2],periodo_volta[2])
colnames(periodo_tabela)[1] <- "Periodo"
colnames(periodo_tabela)[2] <- "Compra"
colnames(periodo_tabela)[3] <- "Ida"
colnames(periodo_tabela)[4] <- "Volta"