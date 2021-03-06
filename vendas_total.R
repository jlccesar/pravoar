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

periodo_ida <- vendas %>% count(periodo_ida)
if (periodo_ida[1,1]!="0H-5H59"){
  x <- c("0H-5H59","-")
  periodo_ida <- rbind(x,periodo_ida) 
}

periodo_volta <- vendas %>% count(periodo_volta)
if (periodo_volta[1,1]!="0H-5H59"){
  x <- c("0H-5H59","-")
  periodo_volta <- rbind(x,periodo_volta) 
}
periodo_volta <- periodo_volta[1:4,]


periodo_tabela <- bind_cols(periodo_compra[1],periodo_compra[2],periodo_ida[2],periodo_volta[2])
colnames(periodo_tabela)[1] <- "Periodo"
colnames(periodo_tabela)[2] <- "Compra"
colnames(periodo_tabela)[3] <- "Ida"
colnames(periodo_tabela)[4] <- "Volta"




#contagem de origem, destino, OD e ow/rt

#calculando trecho de ida
vendas <- mutate(vendas, trecho_ida=paste(origem_ida,destino_ida,sep = "")) 

#calculando trecho de volta e fazendo replace
vendas <- mutate(vendas, trecho_volta=paste(origem_volta,destino_volta,sep = "")) 
#vendas$trecho_volta <- replace(vendas$trecho_volta,vendas$trecho_volta=="NANA","-") # faz o replace de itens do vetor, por outros itens

#calculando OD da viagem
vendas$origem_ida2 <- revalue(vendas$origem_ida, c(GRU="SAO",CGH="SAO", SDU="RIO", GIG="RIO", CNF="BHZ"))

vendas$destino_ida2 <- revalue(vendas$destino_ida, c(GRU="SAO",CGH="SAO", SDU="RIO", GIG="RIO", CNF="BHZ"))
vendas <- mutate(vendas, od_viagem=paste(origem_ida2,destino_ida2, sep = ""))


origem_tabela <- vendas %>% count(origem_ida)
colnames(origem_tabela)[1] <- "Origem"
colnames(origem_tabela)[2] <- "Qtd Vendas"

destino_tabela <- vendas %>% count(destino_ida)
colnames(destino_tabela)[1] <- "Destino"
colnames(destino_tabela)[2] <- "Qtd Vendas"


od_tabela <- vendas %>% count(od_viagem)
colnames(od_tabela)[1] <- "OD"
colnames(od_tabela)[2] <- "Qtd Vendas"

vendas$ow_rt <- factor(vendas$ow_rt,levels = owrt_levels)
owrt_tabela <- vendas%>% count(ow_rt)
colnames(owrt_tabela)[1] <- "OW / RT"
colnames(owrt_tabela)[2] <- "Total"




#tabela com resumo de algumas variáveis

#calculando nÃºmero de passageiros da compra
vendas$qtd_pax <- is.na(vendas$nome_pax2)
vendas$qtd_pax <- if_else(vendas$qtd_pax==TRUE,1,2) 

# calculando quantidade de voos por passageiro, sem levar em conta a escala levar em conta
vendas$qtd_voos <- if_else(vendas$ow_rt=="OW",vendas$qtd_pax*1,vendas$qtd_pax*2) 

# calculando preço final do cliente
#vendas <-  mutate(vendas, preco_cliente=preco_wooba-invoice_du+margem) 

total_voos <- sum(vendas$qtd_voos)
total_pax <- sum(vendas$qtd_pax)
total_vendas <- nrow(vendas)
total_gmv <- sum(vendas$preco_cliente)
resumo_total <- c(total_voos,total_pax,total_vendas,total_gmv)
resumo_total <- tibble(resumo_total)
resumo_total_names <- c("Voos","Pax", "Vendas", "GMV")
resumo_total_names <- tibble(resumo_total_names)

resumo_tabela <- bind_cols(resumo_total_names[1],resumo_total[1])
colnames(resumo_tabela)[1] <- "Variáveis"
colnames(resumo_tabela)[2] <- "Total"




#calculando estadia da viagem
vendas$estadia <-  if_else(vendas$ow_rt=="OW",0,round(as.numeric(vendas$data_volta-vendas$data_ida,units="days"),digits = 0))
vendas$estadia2 <-  if_else(vendas$ow_rt=="OW","OW", if_else(vendas$estadia<=3,"1-3",if_else(vendas$estadia<=7,"4-7","8 ou +")))
vendas$estadia2 <- factor(vendas$estadia2,levels=estadia_levels)

estadia_tabela <- vendas %>% count(estadia2)
colnames(estadia_tabela)[1] <- "Estadia"
colnames(estadia_tabela)[2] <- "Total"

#calculando antecipação
vendas <-  mutate(vendas, antecipacao=round(as.numeric(data_ida-data_reserva,units="days"),digits=0))
vendas$antecipacao2 <-  if_else(vendas$antecipacao<8,"1-7",if_else(vendas$antecipacao<15,"8-14",if_else(vendas$antecipacao<31,"15-30","31 ou +")))

antecipacao_tabela <- vendas %>% count(antecipacao2)
colnames(antecipacao_tabela)[1] <- "Antecipação"
colnames(antecipacao_tabela)[2] <- "Total"




# tabelas de cia ida e volta

vendas$cia_ida <- factor(vendas$cia_ida,levels = cia_levels)
vendas$cia_volta <- factor(vendas$cia_volta,levels = cia_levels)

ciaida_tabela <- vendas %>% count(cia_ida)
colnames(ciaida_tabela)[1] <- "Cia Ida"
colnames(ciaida_tabela)[2] <- "Total"

ciavolta_tabela <- vendas %>% count(cia_volta)
colnames(ciavolta_tabela)[1] <- "Cia Volta"
colnames(ciavolta_tabela)[2] <- "Total"
if (is.na(ciavolta_tabela[5,1]){
  ciavolta_tabela[5,1] <- "Passaredo"
  ciavolta_tabela[5,2] <- "-"
}

cia_tabela <- bind_cols(ciaida_tabela[1],ciaida_tabela[2],ciavolta_tabela[2])
colnames(cia_tabela)[1] <- "Cia"
colnames(cia_tabela)[2] <- "Ida"
colnames(cia_tabela)[3] <- "Volta"

#tabelas de bagagem ida e bagagem volta
vendas$bagagem_ida <- factor(vendas$bagagem_ida,levels = bagagem_levels)
vendas$bagagem_volta <- factor(vendas$bagagem_volta,levels = bagagem_levels)

bagagem_ida <- vendas %>% count(bagagem_ida)

bagagem_volta <- vendas %>% count(bagagem_volta)
bagagem_volta <- bagagem_volta[-3,]

bagagem_tabela <- bind_cols(bagagem_ida[1],bagagem_ida[2],bagagem_volta[2])
colnames(bagagem_tabela)[1] <- "Bagagem"
colnames(bagagem_tabela)[2] <- "Ida"
colnames(bagagem_tabela)[3] <- "Volta"




#dia da compra
vendas <-  mutate(vendas,dia_compra=wday(data_reserva))
vendas$dia_compra[vendas$dia_compra==1] <-"Domingo"
vendas$dia_compra[vendas$dia_compra==2] <-"Segunda"
vendas$dia_compra[vendas$dia_compra==3] <-"Terca"
vendas$dia_compra[vendas$dia_compra==4] <-"Quarta"
vendas$dia_compra[vendas$dia_compra==5] <-"Quinta"
vendas$dia_compra[vendas$dia_compra==6] <-"Sexta"
vendas$dia_compra[vendas$dia_compra==7] <-"Sabado"
vendas$dia_compra <- factor(vendas$dia_compra,levels=dia_levels)

# tabela de dia da compra
diacompra_tabela <- vendas %>% count(dia_compra)
colnames(diacompra_tabela)[1] <- "Dia da Compra"
colnames(diacompra_tabela)[2] <- "Total"

#dia da ida
vendas <- mutate(vendas, dia_ida=wday(data_ida))
vendas$dia_ida[vendas$dia_ida==1] <-"Domingo"
vendas$dia_ida[vendas$dia_ida==2] <-"Segunda"
vendas$dia_ida[vendas$dia_ida==3] <-"Terca"
vendas$dia_ida[vendas$dia_ida==4] <-"Quarta"
vendas$dia_ida[vendas$dia_ida==5] <-"Quinta"
vendas$dia_ida[vendas$dia_ida==6] <-"Sexta"
vendas$dia_ida[vendas$dia_ida==7] <-"Sabado"
vendas$dia_ida <- factor(vendas$dia_ida,levels=dia_levels)

# tabela de dia da ida
diaida_tabela <- vendas %>% count(dia_ida)
colnames(diaida_tabela)[1] <- "Dia da Ida"
colnames(diaida_tabela)[2] <- "Total"

#dia da volta
vendas <- mutate(vendas, dia_volta=wday(data_volta))
vendas$dia_volta[vendas$dia_volta==1] <-"Domingo"
vendas$dia_volta[vendas$dia_volta==2] <-"Segunda"
vendas$dia_volta[vendas$dia_volta==3] <-"Terca"
vendas$dia_volta[vendas$dia_volta==4] <-"Quarta"
vendas$dia_volta[vendas$dia_volta==5] <-"Quinta"
vendas$dia_volta[vendas$dia_volta==6] <-"Sexta"
vendas$dia_volta[vendas$dia_volta==7] <-"Sabado"
vendas$dia_volta <- factor(vendas$dia_volta,levels=dia_levels)

# tabela de dia da volta
diavolta_tabela <- vendas %>% count(dia_volta)
colnames(diavolta_tabela)[1] <- "Dia da Volta"
colnames(diavolta_tabela)[2] <- "Total"




#SEXO
vendas$sexo_pax1 <- factor(vendas$sexo_pax1,levels=sexo_levels)
vendas$sexo_pax2 <- factor(vendas$sexo_pax2,levels=sexo_levels)
sexo1 <- count(vendas,sexo_pax1)
sexo2 <- count(vendas,sexo_pax2)
sexo2 <- sexo2[-3,]

feminino <- sexo1[1,2]+sexo2[1,2]
masculino <- sexo1[2,2]+sexo2[2,2]

sexo_total <-  c(feminino, masculino)
sexo_total <- tibble(sexo_total)
sexo_total_names <- c("Feminino","Masculino")
sexo_total_names <- tibble(sexo_total_names)

sexo_tabela <- bind_cols(sexo_total_names[1],sexo_total[1])
colnames(sexo_tabela)[1] <- "Sexo"
colnames(sexo_tabela)[2] <- "Total"




#IDADE
vendas <- mutate(vendas,idade_pax1=year(Sys.Date())-year(nascimento_pax1) )
vendas$faixa_idade1 <- if_else(vendas$idade_pax1<18,"1-17",if_else(vendas$idade_pax1<25,"18-24",if_else(vendas$idade_pax1<36,"25-35",if_else(vendas$idade_pax1<46,"36-45","46 ou +"))))
vendas$faixa_idade1 <- factor(vendas$faixa_idade1,levels=idade_levels)

faixa1_tabela <- vendas %>% count(faixa_idade1)
colnames(faixa1_tabela)[1] <- "Faixa Idade 1"
colnames(faixa1_tabela)[2] <- "Total"

vendas <- mutate(vendas,idade_pax2=year(Sys.Date())-year(nascimento_pax2) )
vendas$faixa_idade2 <- if_else(vendas$idade_pax2<18,"1-17",if_else(vendas$idade_pax2<25,"18-24",if_else(vendas$idade_pax2<36,"25-35",if_else(vendas$idade_pax2<46,"36-45","46 ou +"))))
vendas$faixa_idade2 <- factor(vendas$faixa_idade2,levels=idade_levels)

faixa2_tabela <- vendas %>% count(faixa_idade2)
colnames(faixa2_tabela)[1] <- "Faixa Idade 2"
colnames(faixa2_tabela)[2] <- "Total"




#ESCALA
vendas$escala_ida <- factor(vendas$escala_ida,levels = escala_levels)
escalaida_tabela <- vendas %>% count(escala_ida)
colnames(escalaida_tabela)[1] <- "Escala Ida"
colnames(escalaida_tabela)[2] <- "Total"


vendas$escala_volta <- factor(vendas$escala_volta,levels = escala_levels)
escalavolta_tabela <- vendas %>% count(escala_volta)
colnames(escalavolta_tabela)[1] <- "Escala Volta"
colnames(escalavolta_tabela)[2] <- "Total"




# TABELAS
# kable(periodo_tabela,align = rep('c')) %>% kable_styling(bootstrap_options = c("striped","hover","condensed"),full_width = F,position = "center") #%>% column_spec(1,width="7em") %>% column_spec(2,width="7em") %>% column_spec(3,width="7em") %>% column_spec(4,width="7em")
# 
# kable(origem_tabela,align = rep('c')) %>% kable_styling(bootstrap_options = c("striped","hover","condensed"),full_width = F,position = "left")
# 
# kable(destino_tabela,align = rep('c')) %>% kable_styling(bootstrap_options = c("striped","hover","condensed"),full_width = F,position = "left")
# 
# kable(od_tabela,align = rep('c')) %>% kable_styling(bootstrap_options = c("striped","hover","condensed"),full_width = F,position = "left") #%>% scroll_box(width = "30%", height = "300px")
# 
# kable(resumo_tabela,align = rep('c'), digits=0) %>% kable_styling(bootstrap_options = c("striped","hover","condensed"),full_width = F,position = "left")
# 
# kable(estadia_tabela,align = rep('c')) %>% kable_styling(bootstrap_options = c("striped","hover","condensed"),full_width = F,position = "left")
# 
# kable(antecipacao_tabela,align = rep('c')) %>% kable_styling(bootstrap_options = c("striped","hover","condensed"),full_width = F,position = "left")
# 
# kable(owrt_tabela,align = rep('c')) %>% kable_styling(bootstrap_options = c("striped","hover","condensed"),full_width = F,position = "left")
# 
# kable(cia_tabela_tabela,align = rep('c')) %>% kable_styling(bootstrap_options = c("striped","hover","condensed"),full_width = F,position = "left")
# 
# kable(bagagem_tabela,align = rep('c')) %>% kable_styling(bootstrap_options = c("striped","hover","condensed"),full_width = F,position = "left")
# 
# kable(diacompra_tabela,align = rep('c')) %>%  kable_styling(bootstrap_options = c("striped","hover","condensed"),full_width = F,position = "left")
# 
# kable(diaida_tabela,align = rep('c')) %>%  kable_styling(bootstrap_options = c("striped","hover","condensed"),full_width = F,position = "left")
# 
# kable(diavolta_tabela,align = rep('c')) %>%  kable_styling(bootstrap_options = c("striped","hover","condensed"),full_width = F,position = "left")
# 
# kable(sexo_tabela,align = rep('c')) %>%  kable_styling(bootstrap_options = c("striped","hover","condensed"),full_width = F,position = "left")
# 
# kable(faixa1_tabela,align = rep('c')) %>%  kable_styling(bootstrap_options = c("striped","hover","condensed"),full_width = F,position = "left")
# 
# kable(faixa2_tabela,align = rep('c')) %>%  kable_styling(bootstrap_options = c("striped","hover","condensed"),full_width = F,position = "left")
# 
# kable(escalaida_tabela,align = rep('c')) %>%  kable_styling(bootstrap_options = c("striped","hover","condensed"),full_width = F,position = "left")
# 
# kable(escalavolta_tabela,align = rep('c')) %>%  kable_styling(bootstrap_options = c("striped","hover","condensed"),full_width = F,position = "left")
# 
