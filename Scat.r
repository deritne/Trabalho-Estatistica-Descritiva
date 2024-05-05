#Limpando environment
rm(list=ls())

#Instalação pacote
if(!require(caret)){
  install.packages("caret", dependencies=c("Depends","Suggests"))
}
#Carregar o pacote instalado
library(caret)

#Reid (2015) coletou dados sobre fezes de animais na costa da Califórnia.
#Os dados consistem em designações de espécies verificadas por DNA, bem como campos relacionados 
#à hora e local da coleta e às próprias fezes. O dataframe scat contém dados sobre as três principais espécies.

#Carregar data.frame scat
data(scat) #Species Month Location
#Criando corte para ignorar valor irrelevante (zero)
ValorCorte<-0


#---- Species/Years ----# Quantitativa
#Tradução Species

#Criação novos nomes
nomeEspecies<-c("Lince","Coiote","Raposa_Cinza")
#Substituição nome "Species"
levels(scat$Species)<-nomeEspecies
#Criando vetor numerico que recebe quantos animais tem de cada especie
Especies<-table(scat$Species,scat$Year)
#Criação gráfico de barra "Species" onde se denomina quantas fezes de cada animal foram coletadas
barplot(Especies,beside=TRUE,main="Quantidade de fezes coletadas por animal em seus respectivos anos",xlab="Anos",ylab="Quantidade",legend.text=levels(scat$Species),
        col=rainbow(length(levels(scat$Species))))

#-- Criação gráfico de pizza "Species" --#
#Criação data percentual
EspeciesPerc<-round(100*prop.table(Especies),2)
#Criação nome para incluir no gráfico de pizza
EspeciesPie<-paste(nomeEspecies,"-",EspeciesPerc,"%")
#Criação tabela de pizza onde se denomina a porcentagem dos animais que tiveram suas fezes coletadas
pie(EspeciesPerc,label=EspeciesPie,main="Porcentagem dos animais que tiveram suas fezes coletadas",col=rainbow(length(Especies)))


#---- Month ----#
#Tradução Meses

#Criação novos nomes
nomeMeses<-c("Abril","Agosto","Fevereiro","Janeiro","Junho","Maio","Novembro","Outubro","Setembro")
#Substituição nome "Month"
levels(scat$Month)<-nomeMeses
#Vetor numerico que recebe quantidade de cada mês
Meses<-table(scat$Month)
#Removendo categorias com ocorrência insignificante
Meses <- Meses[Meses>ValorCorte]
#Criação gráfico de barra "Month" onde se mostra a quantidade de fezes coletadas por mês
barplot(Meses,main="Quantidade de fezes coletadas por mês",xlab="Meses",ylab="Quantidade",col=rainbow(length(Meses)))

#-- Criação gráfico de pizza "Month" --#
#Criação data percentual
MesesPerc<-round(100*prop.table(Meses),2)
#Criação nome para incluir no gráfico de pizza
MesesPie<-paste(nomeMeses,"-",MesesPerc,"%")
#Criação tabela de pizza onde se denomina a porcentagem dos meses em que as fezes foram coletadas
pie(MesesPerc,label=MesesPie,main="Porcentagem dos meses em que as fezes foram coletadas",col=rainbow(length(Meses)))


#---- Location ----#
#Tradução localização

#Criação novos nomes
NomeLocal<-c("Borda","Meio","Fora da borda")
#Substituição nome "Location"
levels(scat$Location)<-NomeLocal
#Vetor númerico que define a quantidade de fezes que foram coletadas por local
Locais<-table(scat$Location)
#Criação gráfico de barras "Locations"
barplot(Locais,main="Locais em que as fezes foram coletadas",xlab="Locais",ylab="Quantidade", col=rainbow(length(Locais)))

#-- Criação gráfico de pizza "locations" --#
#Criação data percentual
LocaisPerc<-round(100*prop.table(Locais),2)
#Criação nomes
LocaisPie<-paste(NomeLocal,"-",LocaisPerc,"%")
#Criação tabela de pizza onde se demonstra a porcentagem de quais locais as fezes foram coletadas
pie(LocaisPerc,label=LocaisPie,main="Porcentagem dos locais em que as fezes foram coletadas",col=rainbow(length(Locais)))