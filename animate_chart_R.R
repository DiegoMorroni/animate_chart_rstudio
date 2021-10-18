# gráfico linear animado
# autor: Diego Morroni
# Data driação 18/10/2021
#ultima edição: 18/10/2021
# fonte dos dados: dados abertos PMAP-PR :http://propesq-pr.fundepag.br/usuarioexterno/

###################################################################################
# pacotes necessarios
install.packages("dplyr")
install.packages("zoo")
install.packages("animation")
install.packages("ggplot")

#carregando dados
dados<- read.csv("relatorio30_.csv", h= T, dec=",", sep= ";")

# alterando formato de dados
dados$Nível.Taxonômico<- as.factor(dados$Nível.Taxonômico)
dados$Pescado<- as.factor(dados$Pescado)
dados$Aparelho.de.Pesca<- as.factor(dados$Aparelho.de.Pesca)
dados$kg.no.Período<-as.numeric(dados$kg.no.Período)
dados$valor.estimado.no.período<-as.numeric(dados$valor.estimado.no.período)#  o "R$" precisa ser eliminado antes

#filtragem e segmentação do periodo, escolha das espécies
library(dplyr)
dados<- dados%>%
  filter( Ano != 2021 & Ano != 2016, Pescado == "Caranguejo-uçá")%>%
  mutate(trimestre = case_when(Mês %in% c(1:3) ~ "Q1",
                               Mês  %in% c(4:6)~"Q2",
                               Mês  %in%c(7:9)~"Q3",
                               Mês  %in% c(10:12)~"Q4"))

#concatenando colunas                             
dados$period<-paste(dados$Ano,dados$trimestre)  

#ordenando colunas
dados_sort<-dados%>%
  group_by(period)%>%
  summarise(kg=sum(kg.no.Período)/1000, Pr= sum(valor.estimado.no.período)/1000000)%>%
  mutate(T_sa =cumsum(kg), P_sa = cumsum(Pr) )

#modificando formato data
library(zoo) 

dados_sort$period<-gsub(" ", "", dados_sort$period) # eliminando espaço
dados_sort$period<- as.yearqtr(dados_sort$period, format = "%YQ%q")

# gráfico animado padrao R - volume acumulado
library(animation)
saveGIF(movie.name= "Caranguejo_R_pattern.gif",interval= .2,{
  for (i in 2:nrow(dados_sort)) {
    
    plot(dados_sort$period[1:i], dados_sort$T_sa[1:i],
         type="l",
         col="red",
         xlim=range(dados_sort$period),
         ylim= range(dados_sort$T_sa), 
         xlab = "período",
         ylab= " Toneladas",
         main="Total de caranguejo-uçá capturado no Paraná, 2017-2020" )
    
  }
})

###########################################################################


# gráfico animado padrao GGPlot - valor acumulado
library(ggplot2)
saveGIF(movie.name= "caranguejo_ggplot_pattern.gif",interval= .2,{
  for (i in 2:nrow(dados_sort)) {
    plot<-ggplot(dados_sort[1:i,], aes(x=period, y= P_sa))+
      geom_line( color="#69b3a2", size=1.2 )+
      scale_y_continuous(limits = range(dados_sort$P_sa))+
      scale_x_continuous(limits = range(dados_sort$period))+
      ggtitle("Valor(R$) acumulado entre 2017-2020 para Caranguejo-uçá") +
      ylab("milhões de R$")+
      xlab(NULL)
    theme_bw()
    print(plot)
    
  }
})
