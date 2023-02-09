################################################################################
# UFRPE - Universidade Federal Rural de Pernambuco                             #     
# Estatistica - 1VA Projeto RStudio - Analise de Dados                         #
# Prof.: Kleber                                                                #
# Aluno: Augusto Cézar de Souza Miranda                                        #
#                                                                              #
################################################################################


########################### CARREGANDO DADOS ###################################

#install.packages("data.table")

#FUNÇÃO FILE.CHOOOSE()


# LENDO ARQUIVOS XLS/XLSX - EXCEL

#intall.packages("readxl")

library(readxl)
dadosPraia = read_excel(file.choose())
dadosPraia

View(dadosPraia)

str(dadosPraia)
str(Cidade)

#VERIFICANDO NOMES DAS TABELAS
names(dadosPraia)


#FILTAR DADOS - AUGUSTO MIRANDA - PRAIA PERUÍBE

intall.packages("dplyr")
library(dplyr)

Cidade <-filter(dadosPraia, City == "PERUÍBE")

View(Cidade)

########################### TRANSFORMANDO DADOS ################################

#dadosPraia[dadosPraia$City == "PERUÍBE" & !is.na(dadosPraia$City), ]

# install.packages("tidyverse")
library(tidyverse)




###############################################################################
###### CONVERTENDO ULTIMA COLUNA PARA NUMERIC

Cidade$Enterococcus  <- as.numeric(as.character(Cidade$Enterococcus))


sapply(Cidade, class)

str(Cidade)

############################################################################# dadosPraia %>% reframe(dadosPraia, media = mean(Enterococcus))
# Questão 1:
#   Encontre média, desvio-padrão, mediana, Q1, Q3, 
#   mínimo e máximo dos enterococos de cada praia (summarise)
#
#
#
# 
## RESPOSTA: 
# MEDIA, MEDIANA, DESVIO PADRÃO


#MÉDIA
Cidade %>% group_by(Beach) %>% summarise(Media = mean(Enterococcus))
#MEDIANA
Cidade %>% group_by(Beach) %>% summarise(Mediana = median(Enterococcus))
## DESVIO PADRÃO
Cidade %>% group_by(Beach) %>% summarise(DesvioP = (Enterococcus - mean(Enterococcus)))
#MÉDIA E MEDIANA JUNTAS
Cidade %>% group_by(Beach) %>% summarise(Media = mean(Enterococcus), Mediana = median(Enterococcus))

#Quartil - 
quantile(Cidade$Enterococcus, probs = seq(0,1, 0.25))

Cidade %>% summarise(quantile(Enterococcus, probs = seq(0,1, 0.25)))


############################################################################# 
# Questão 2:
#   Faça um gráfico de barras com a variável Beach, ordenando da praia 
#   com maior quantidades de amostras para a menor. Colorir gráfico com 
#   base na praia. Anotar as porcentagens no topo das barras (ggplot2).
#
# install.packages("ggplot2")
library(tidyverse)
library(ggplot2)

Cidade %>% group_by(Beach) %>% summarise(Contagem = n()) %>% ggplot(aes(x = Beach, y = Contagem, color = Beach, label = Contagem, fill = Beach)) + geom_bar(stat = "Identity") + geom_label() + coord_flip() 

############################################################################# 
# Questão 3:
#   Repita a questão 2, fazendo desta vez um gráfico de pizza (ggplot2).
#
#
Cidade %>% group_by(Beach) %>% summarise(Contagem = n()) %>% ggplot(aes(x = Beach, y = Contagem, color = Beach, label = Contagem, fill = Beach)) + geom_bar(stat = "Identity") + geom_label() + coord_polar("y", start = 0) 

############################################################################# 
# Questão 4:  
# Fazer um histograma com todos os dados de enterococos das praias da sua cidade (ggplot2).
#
Cidade %>% group_by(Beach) %>% summarise(Contagem = n()) %>% ggplot(aes(x = Beach, y = Contagem, color = Beach, label = Contagem, fill = Beach)) + geom_histogram(stat = "Identity")

############################################################################# 
# Questão 5:
#   Fazer box-plots de todas as praias da sua cidade num único gráfico (ggplot2).
#
boxplot(Cidade$Enterococcus ~ Cidade$Beach,
        main = "Questão 5 - BoxPloat")

Cidade %>% group_by(Beach) %>% summarise(Contagem = n()) %>% ggplot(aes(x = Beach, y = Contagem, color = Beach, label = Contagem, fill = Beach)) + geom_boxplot()

Cidade %>% ggplot(aes(reorder(Beach, Enterococcus), Enterococcus)) + geom_boxplot(fill = "red", alpha = 0.5)

