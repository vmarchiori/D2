
#Se necessario remover pacotes
remove.packages("tseries")
remove.packages("readxl")
remove.packages("aTSA")

#Pacotes necessarios
install.packages("readxl", dependencies = T)
install.packages("strucchange")
install.packages("aTSA", dependencies = T)
install.packages("tseries", dependencies = T)
install.packages("urca")
install.packages("forecast")

#Carrega pacotes
library(readxl)
library(aTSA)
library(tseries)
library(urca)
library(strucchange)
library("forcats")

#Criando a serie temporal e plotando grafico
binance_st <- ts(BINANCE$Fechado, start = 2017, frequency = 365)
plot(binance_st)

#Criar FAC  e FACP

acf(BINANCE$Fechado,lend=2, lwd=5,col="darkblue",main= "Função Autocorrelação - FAC")
#Melhorando aspecto da FAC
axis(1,tck = 1, col = "lightgrey", lty = "dotted")

pacf(BINANCE$Fechado,lend=60, lwd=5,col="darkblue",main= "Função Autocorrelação Parcial - FACP")
#Melhorando aspecto da PAC
axis(1,tck = 1, col = "lightgrey", lty = "dotted")



#Teste DF-DickFuller com drift e com tendencia
TesteADF_Binance_none <- ur.df(binance_st, "none", lags = 1)  
summary(TesteADF_Binance_none)

#Teste Phillips-Perron
pp.test(binance_st)

#Teste KPSS
kpss.test(binance_st)


#Teste de Chow
chow <- Fstats(binance_st~1)  #Executa o Teste de F de Chow
sctest(chow)                   #Retorna a Estatística de Teste e o p-valor

plot(binance_st)
lines(breakpoints(chow))
plot(chow)

#Teste Bai Perron
bp_ts <- breakpoints(binance_st ~ 1)
bp_ts
summary(bp_ts)

#ci_ts <- confint(bp_ts)
plot(binance_st)               
lines(bp_ts)            #Gráfico com os breakpoints


#Se não for estacionária, diferenciar a série
IntOrdem1 <- diff(BINANCE$Fechado)
IntegradaOrdem1 <- ts(IntOrdem1, start = 2017, frequency = 365)

plot(IntegradaOrdem1, type="l", main="Primeira Diferança do Binance", ylab="Preço", xlab="Data", col="Blue")
grid(col = "black", lty = "dotted")

#Verificar se a Série se tornou Estacionária
#FAC e FACP

acf(IntOrdem1,lend=2, lwd=5,col="darkblue",main= "Função Autocorrelação - FAC")              #Melhorando aspecto da FAC
axis(1,tck = 1, col = "lightgrey", lty = "dotted")

pacf(IntOrdem1,lend=60, lwd=5,col="darkblue",main= "Função Autocorrelação Parcial - FACP")   #Melhorando aspecto da PAC
axis(1,tck = 1, col = "lightgrey", lty = "dotted")

#Teste ADF
ur.df(IntegradaOrdem1, "none", lags = 1)

#Teste Philips-Perron
pp.test(IntegradaOrdem1)

#Teste KPSS
kpss.test(IntegradaOrdem1)

#Estimando Regressões e Tabelando Resultados
arima010<-arima(IntegradaOrdem1,order=c(0,1,0))
arima110<-arima(IntegradaOrdem1,order=c(1,1,0))
arima210<-arima(IntegradaOrdem1,order=c(2,1,0))
arima310<-arima(IntegradaOrdem1,order=c(3,1,0))
arima410<-arima(IntegradaOrdem1,order=c(4,1,0))
arima510<-arima(IntegradaOrdem1,order=c(5,1,0))
arima610<-arima(IntegradaOrdem1,order=c(6,1,0))



#Cria uma lista com os estimadores
estimacoes <- list(arima110, arima210,	arima310, arima410,	arima510,	arima610)

sapply(estimacoes, AIC)                 #Aplica o comando AIC na lista
sapply(estimacoes, BIC)                 #Aplica o comando BIC na lista

#Exemplo de criação de tabela com resultados - Extra
AIC <- sapply(estimacoes, AIC) 
BIC <- sapply(estimacoes, BIC)
Modelo <- c("arima110",	"arima210",	"arima310",	"arima410",	"arima510",	"arima610")
Resultados <- data.frame(Modelo, AIC, BIC)
View(Resultados)


#PrevisÃ£o - Notas na Aula 11

melhor_modelo <- arima610
predict(melhor_modelo,15)
forecast(melhor_modelo,15)

plot(forecast(arima610,15),type="o", main = "previsao Binance")
grid(col = "black", lty = "dotted")

