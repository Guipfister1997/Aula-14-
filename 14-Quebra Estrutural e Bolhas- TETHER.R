                    #Aula 14 - Quebra Estrural e Bolhas

install.packages("strucchange")

library(strucchange)
library(readxl)


tether <- read_excel("C:/Econometria/tether.xlsx")

tether <- ts <- ts(tether$Close, start = 2014, frequency = 365)

plot(tether)

#Teste de Chow

chow <- Fstats(tether~1)    #Executa o Teste de F de Chow
sctest(chow)                 #Retorna a Estat�stica de Teste e o p-valor

plot(tether)
lines(breakpoints(chow))

plot(chow)
    
#Teste Bai Perron

bp_ts <- breakpoints(tether~ 1)

bp_ts

summary(bp_ts)

#ci_ts <- confint(bp_ts)

plot(tether)               
lines(bp_ts)            #Gr�fico com os breakpoints


#Gr�fico com as linhas de tend�ncias para os tr�s per�odos

fm0 <- lm(tether ~ 1)
fm1 <- lm(tether ~ breakfactor(bp_ts, breaks = 1))
fm2 <- lm(tether ~ breakfactor(bp_ts, breaks = 2))
plot(tether)
lines(ts(fitted(fm0), start = 2014, freq=365), col = 3)
lines(ts(fitted(fm1), start = 2014, frequency=365), col = 4)
lines(ts(fitted(fm2), start = 2014, frequency=365), col = 1)
lines(bp_ts)


#Estimar o Melhor Modelo ARIMA

#Modelo Integrado de Ordem 1

MIO1 <- diff(tether)
plot(MIO1)

#� estacion�ria?

#FAC e FACP

#Qual a ordem do modelo ARIMA(p,d,q)

#Quais combina��es a serem estimadas?

#Os valores AIC e BIC dos modelos poss�veis.

#O melhor modelo

#Previs�o para os 6 pr�ximos meses do valor do Bitcoin utilizando o melhor modelo

plot(tether,
     ylab=expression(Y[t]),
     main='',
     bty='l',
     col='red')
grid(col='darkgrey', lwd=2)
lm(formula = diff(tether) ~ lag(tether, -1)[-length(tether)] - 1)
