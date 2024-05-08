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
#Criando vetor numerico que recebe quantos animais tem de cada especie/Ano
Especies<-table(scat$Species,scat$Year)
#Criação gráfico de barra "Species" onde se denomina quantas fezes de cada animal foram coletadas
barplot(Especies,beside=TRUE,main="Quantidade de fezes coletadas por animal em seus respectivos anos",
        xlab="Anos",ylab="Quantidade",legend.text=levels(scat$Species),
        col=rainbow(length(levels(scat$Species))))

#-- Criação gráfico de pizza "Species" --#
#Criando vetor numerico que recebe quantos animais tem de cada especie
EspeciesNum<-table(scat$Species)
#Criação data percentual
EspeciesPerc<-round(100*prop.table(EspeciesNum),2)
#Criação nome para incluir no gráfico de pizza
EspeciesPie<-paste(nomeEspecies,"-",EspeciesPerc,"%")
#Criação tabela de pizza onde se denomina a porcentagem dos animais que tiveram suas fezes coletadas
pie(EspeciesPerc,label=EspeciesPie,main="Porcentagem dos animais que tiveram suas fezes coletadas",col=rainbow(length(Especies)))


#---- Month ----#
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
NomeLocal<-c("Borda da costa da California","Meio da costa da California","Fora da borda da costa da California")
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
pie(LocaisPerc,label=LocaisPie,main="Em quais locais as fezes foram mais coletadas",col=rainbow(length(Locais)))


#---- Site ----#
#Criação vetor quantidade de pesquisas que cada site fez
Sites<-table(scat$Site)
#Criação tabela
barplot(Sites,xlab="Sites que realizaram a pesquisa",ylab="Quantidade",main="Quantidade de fezes pesquisadas por site",col=rainbow(length(levels(scat$Site))))

#-- Criação gráfico de pizza "Site" --#
#Criação vetor quantidade de pesquisas que cada site fez
Sites<-table(scat$Site)
#Criação vetor percentual sites
SitesPerc<-round(100*prop.table(Sites))
#Criação nomes
SitesPie<-paste(levels(scat$Site),"-",SitesPerc,"%")
#Criação gráfico de pizza
pie(SitesPerc,labels=SitesPie,main="Porcentagem de fezes pesquisadas por site",col=rainbow(length(levels(scat$Site))))

#---- Ropey ----$
#Criação vetor quantidade de fezes pegajosas
Pegajoso<-table(scat$ropey)
#Criação separação não pegajoso/pegajoso
NomeRopey<-c("Não Pegajoso","Pegajoso")
#Criação tabela
barplot(Pegajoso,names.arg=NomeRopey,col=rainbow(length(Pegajoso)))

#-- Criação gráfico de pizza "Ropey"
#Criação vetor quantidade aspecto das fezes
Pegajoso<-table(scat$ropey)
#Criação separação não pegajoso/pegajoso
NomeRopey<-c("Não Pegajosa","Pegajosa")
#Criação vetor percentual
PegajosoPerc<-round(100*prop.table(Pegajoso))
#Criação nomes
PegajosoPie<-paste(NomeRopey,"-",PegajosoPerc,"%")
#Criação gráfico pizza
pie(PegajosoPerc,main="Aspecto das fezes",labels=PegajosoPie,col=rainbow(length(Pegajoso)))

#---- Mass ----#
#Filtrando ocorrências
dadosgraf05 <- data.frame(subset(scat, scat$Mass >= 0 & scat$Mass <= 5))[12:12] #Separando ocorrências da massa entre x e x

dadosgraf50 <- data.frame(subset(scat, scat$Mass >= 5 & scat$Mass <= 10))[12:12]

dadosgraf1015 <- data.frame(subset(scat, scat$Mass >= 10 & scat$Mass <= 15))[12:12]

dadosgraf1520 <- data.frame(subset(scat, scat$Mass >= 15 & scat$Mass <= 20))[12:12]

dadosgraf2025 <- data.frame(subset(scat, scat$Mass >= 20 & scat$Mass <= 25))[12:12]

dadosgraf2530 <- data.frame(subset(scat, scat$Mass >= 25 & scat$Mass <= 30))[12:12]

dadosgrafacima30 <- data.frame(subset(scat, scat$Mass >= 30))[12:12]

#Unindo valores
dadosgrafico<-c(nrow(dadosgraf05),nrow(dadosgraf50),nrow(dadosgraf1015),nrow(dadosgraf1520),nrow(dadosgraf2025),
                nrow(dadosgraf2530),nrow(dadosgrafacima30))

#Criação ocorrências para legenda
OcorrFez<-c("0 a 5","5 a 10","10 a 15","15 a 20","20 a 25","25 a 30","30+")
#Criação tabela ocorrências de fezes por massa
barplot(dadosgrafico,legend.text=OccorFez,ylab="Quantidade de fezes com x libras",
        main="Ocorrência de fezes em libras",col=rainbow(length(dadosgrafico)),names.arg=teste)

