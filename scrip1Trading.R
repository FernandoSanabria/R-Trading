
library(quantmod)
library(PerformanceAnalytics)


date <- "2019-1-15"
TV <- getSymbols.yahoo("TV", env=globalenv(), from=date)
TVClose <- getSymbols.yahoo("TV", from=date, auto.assign=F) [,6]
TVClose
#returns 
TVRets <- na.omit(dailyReturn(TVClose, type="log"))
# logarithmic returns vs time
chartSeries(TVRets)  


BBD <- getSymbols.yahoo("BBD", env=globalenv(), from=date)
BBDClose <- getSymbols.yahoo("BBD", from=date, auto.assign=F) [,6]

#returns 
BBDRets <- na.omit(dailyReturn(BBDClose, type="log"))
# logarithmic returns vs time
chartSeries(BBDRets) 




WALMEXMX <- getSymbols.yahoo("WALMEX.MX", env=globalenv(), from=date)
WALMEXMXClose <- getSymbols.yahoo("WALMEX.MX", from=date, auto.assign=F) [,6]

#returns 
WALMEXMXRets <- na.omit(dailyReturn(WALMEXMXClose, type="log"))
# logarithmic returns vs time
chartSeries(WALMEXMXRets)


GFNORTEOMX <- getSymbols.yahoo("GFNORTEO.MX", env=globalenv(), from=date)
GFNORTEOMXClose <- getSymbols.yahoo("GFNORTEO.MX", from=date, auto.assign=F) [,6]

#returns 
GFNORTEOMXRets <- na.omit(dailyReturn(GFNORTEOMXClose, type="log"))
# logarithmic returns vs time
chartSeries(GFNORTEOMXRets) 



FEMSAUBMX <- getSymbols.yahoo("FEMSAUB.MX", env=globalenv(), from=date)
FEMSAUBMXClose <- getSymbols.yahoo("FEMSAUB.MX", from=date, auto.assign=F) [,6]

#returns 
FEMSAUBMXRets <- na.omit(dailyReturn(FEMSAUBMXClose, type="log"))
# logarithmic returns vs time
chartSeries(FEMSAUBMXRets) 

library(dplyr)
library(caret)
library(tidyverse)




#ADD COLUMNS
FEMSA <- mutate (FEMSA, Company= "Femsa", Company_Ticker="KOF") 

FEMSA <- mutate (FEMSA, Company= "Femsa", Company_Ticker="KOF") 



#cambiamos el orden de las variables (columnas) para tener primero
#la compañia
FEMSA_2 <- FEMSA[,c(9,8,1,2,3,4,5,6,7)]         





#dataset GFBANORTE
GFBANORTE.MX <- mutate (GFBANORTE.MX, Company= "Grupo Financiero Banorte", 
                        Company_Ticker="GFNORTEO.MX") 


#cambiamos el orden de las variables (columnas) para tener primero
#la compañia
GFBANORTE.MX_2 <- GFBANORTE.MX[,c(9,8,1,2,3,4,5,6,7)]  




#Televisa
TELEVISA <- mutate (TELEVISA, Company= "Televisa", 
                    Company_Ticker="TV") 
TELEVISA_2 <- TELEVISA[,c(9,8,1,2,3,4,5,6,7)] 





#walmart México
WALMEX.MX <- mutate (WALMEX.MX, Company= "Walmart Méx", 
                     Company_Ticker="WALMEX.MX") 
WALMEX.MX_2 <- WALMEX.MX[,c(9,8,1,2,3,4,5,6,7)] 




#Bradesco. banco brasil
BBD.bradesco <- mutate (BBD.bradesco, Company= "Banco Bradesco", 
                        Company_Ticker="BBD") 
BBD.bradesco_2 <- BBD.bradesco[,c(9,8,1,2,3,4,5,6,7)] 




#Crear un gran dataset agregando filas (rows)
Latam_GEI_Index <- rbind(FEMSA_2, GFBANORTE.MX_2, TELEVISA_2, WALMEX.MX_2, 
                         BBD.bradesco_2)    



str(Latam_GEI_Index)





#si algo estuviera en caracter, entonces tocaria usar el convertidor as.numeric asi
#ej: si open no estuviera numerico


Latam_GEI_Index$Open = gsub(",", "", Latam_GEI_Index$Open) %>%
  as.numeric()  


summary(Latam_GEI_Index)



#desviacion estandar de precio de cierre ajustado de las cinco companias
sd(Latam_GEI_Index$Adj.Close)   


#subseting para sacar promedio, desviacion estandar y coeficiente por compania


library("dplyr")


meanFEMSA = mean(FEMSA$Adj.Close)
sdFEMSA = sd(FEMSA$Adj.Close)
Coef = (sdFEMSA/meanFEMSA)



FEMSA_3 <- Latam_GEI_Index %>%
  select(Company, Adj.Close) %>%
  filter(Company == "Femsa")  



#Clase 9      
#analisis que resume comportamiento historico de las cinco cias 
#desde marzo 2, 2019. Con precio cierre ajustado

date <- "2019-3-2"  
tickers <- c("GFNORTEO.MX", "TV", "WALMEX.MX", "BBD", "KOF")      


# crear tabla dinamicamente pasandole array de empresas/ticker symbol 

portfolioPrices <- NULL
for(ticker in tickers) {
  portfolioPrices <- cbind(portfolioPrices, getSymbols.yahoo(ticker,
      from="2019-3-2", periodicity="daily",auto.assign=FALSE)[,6])  
}



view(portfolioPrices)

library("tibble")
library("dplyr")
library("ggplot2")

colnames(portfolioPrices)




# se confirma si es dataframe
portfolioPrices <- as.data.frame(portfolioPrices)
class(portfolioPrices)   
rownames_to_column()

portfolioPrices <- rownames_to_column(portfolioPrices, var="Fecha")
view(portfolioPrices) 


#extraer este dataset.al escritorio

#linux - Ejemplo de mi escritorio
write.csv2(portfolioPrices , "/home/sonia/Desktop/portfolioPrices.csv")  




#ajustar el dataframe para hacer la grafica de lineas

library("tidyverse") 


df <- portfolioPrices %>% select(Fecha,GFNORTEO.MX.Adjusted,TV.Adjusted,
                                 WALMEX.MX.Adjusted,BBD.Adjusted, KOF.Adjusted) %>%
  gather (key="variable", value="value", -Fecha)


head(df)





#clase 11
#ahora grafica de lineas para ver comportamiento de precio de
#cierre ajustado 
ggplot(df, aes(x=Fecha, y=value)) + 
  geom_line(aes(group=variable, linetype=variable))+
  scale_color_manual(values=c("red", "blue", "black", "purple", "green"))+
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),    
        panel.grid.minor = element_blank())+
  labs(title="Fluctuación Precio Cierre Ajustado desde Marzo 2019")   







#clase 12         
#Vamos a ver el historico de RETORNO DIARIO SOBRE precios de cierre QUANTMOD
date <- "2019-3-2"    
TVClose <- getSymbols.yahoo("TV", from=date, auto.assign = F)[,6]


TVRets <- na.omit(dailyReturn(TVClose, type="log")) 
chartSeries(TVRets)   


#Femsa    
date <- "2019-3-2"  
KOFClose <- getSymbols.yahoo("KOF", from=date, auto.assign = F)[,6]


KOFRets <- na.omit(dailyReturn(KOFClose, type="log")) 
chartSeries(KOFRets) 


#Grupo financiero banorte

date <- "2019-3-2"  
GFNORTEO.MXClose <- getSymbols.yahoo("GFNORTEO.MX", from=date, auto.assign = F)[,6]


GFNORTEO.MXRets <- na.omit(dailyReturn(GFNORTEO.MXClose, type="log")) 
chartSeries(GFNORTEO.MXRets) 



#Walmart México
date <- "2019-3-2"  
WALMEX.MXClose <- getSymbols.yahoo("WALMEX.MX", from=date, auto.assign = F)[,6]


WALMEX.MXRets <- na.omit(dailyReturn(WALMEX.MXClose, type="log")) 
chartSeries(WALMEX.MXRets) 



#Bradesco
date <- "2019-3-2"  
BBDClose <- getSymbols.yahoo("BBD", from=date, auto.assign = F)[,6]


BBDCloseRets <- na.omit(dailyReturn(BBDClose, type="log")) 
chartSeries(BBDCloseRets)     




#Ver con velas japonesas 
getSymbols( Symbols="BBD", src="yahoo",
            from = "2019-03-02",
            to = "2020-03-02")

barChart(BBD, theme = "white")  

candleChart(BBD, multi.col = TRUE, theme = "white")


#
getSymbols( Symbols="WALMEX.MX", src="yahoo",
            from = "2019-03-02",
            to = "2020-03-02")

barChart(WALMEX.MX, theme = "white")  

candleChart(WALMEX.MX, multi.col = FALSE, theme = "white") 


#
getSymbols( Symbols="TV", src="yahoo",
            from = "2019-03-02",
            to = "2020-06-24")

barChart(TV, theme = "white")  

candleChart(TV, multi.col = TRUE, theme = "white") 

#
getSymbols( Symbols="GFNORTEO.MX", src="yahoo",
            from = "2019-03-02",
            to = "2020-03-02")

barChart(GFNORTEO.MX, theme = "white")  

candleChart(GFNORTEO.MX, multi.col = TRUE, theme = "white") 

#
getSymbols( Symbols="KOF", src="yahoo",
            from = "2019-03-02",
            to = "2020-03-02")

barChart(KOF, theme = "white")  

candleChart(KOF, multi.col = TRUE, theme = "white") 


install.packages("xts")

#clase 13 histogramas
getSymbols("BBD")
hist(BBD$BBD.Close, breaks= 60, col="blue")  
  

hist(KOFClose$KOF.Adjusted, breaks= 60, col="blue") 


hist(GFNORTEOMXClose$GFNORTEO.MX.Adjusted, breaks= 60, col="blue") 

hist(WALMEX.MXClose$WALMEX.MX.Adjusted, breaks= 60, col="blue") 

hist(TVClose$TV.Adjusted, breaks= 60, col="blue") 



#clase 14
#Analisis tecnico 
#TA: Technical analysis
#BBands es bandas de bollinger. Vo es volumen: 
#Nivel de actividad de un mercado. 
#MACD es moving average convergence divergence

library("tidyverse") 
library("dplyr")

KOFClose = getSymbols.yahoo("KOF", from=date, auto.assign = F)[,6]

KOFClose%>%Ad()%>%chartSeries()
KOFClose %>% chartSeries(TA='addBBands();addVo();
                         addMACD()',subset='2020')  
#Clase 15: solo slides

#Clase 16
#Ahora ademas el RSI

getSymbols("KOF")
chartSeries(KOF,subset = '2020',TA='addBBands();addVo();addMACD();addRSI()')

chartSeries(KOF,subset = '2020',TA='addBBands();addVo();addMACD();addRSI();addATR()')



getSymbols("BBD")
chartSeries(BBD,subset = '2020',TA='addBBands();addVo();addMACD()')

getSymbols( Symbols="GFNORTEO.MX", src="yahoo",
            from = "2019-03-02")

getSymbols("GFNORTEO.MX")
chartSeries(GFNORTEO.MX,subset = '2020',TA='addBBands();addVo();addMACD()')


getSymbols("TV")
chartSeries(TV,subset = '2020',TA='addBBands();addVo();addMACD()')

getSymbols("WALMEX.MX")
chartSeries(WALMEX.MX,subset = '2020',TA='addBBands();addVo();addMACD()')



#clase 18 medir rentabilidad de portafolio segun riesgo
#asumir que cada accion tiene el mismo peso
#LIBRERIA
library(PerformanceAnalytics)
library(PortfolioAnalytics)
tickers <- c("WALMEX.MX", "BBD", "KOF", "GFNORTEO.MX", "TV")      

weights <- c(.20, .20, 0.20, 0.20, 0.20)     

portfolioPrices <- NULL
for(ticker in tickers) {
  portfolioPrices <- cbind(portfolioPrices, getSymbols.yahoo(ticker,
                                                               from='2020-1-1', periodicity="daily",auto.assign=FALSE)[,6])  
}

head(portfolioPrices) 



#ROC: Rate of change. % de variacion entre precio actual y precio de
#periodo anterior. 
portfolioReturns <- na.omit(ROC(portfolioPrices))  
head(portfolioReturns)

portfolioReturns2 <- na.omit(ROC(portfolioPrices2))  
head(portfolioReturns)


#Benchmark: con ILF> i-shares Latin Funds

benchmarkPrices <- getSymbols.yahoo('ILF',
                                    from='2019-3-2', to='2020-6-1', periodicity='daily', auto.assign=FALSE)[,6]


benchmarkPrices2 <- getSymbols.yahoo('ILF',
                                    from='2020-6-1', periodicity='daily', auto.assign=FALSE)[,6]


benchmarkReturns <- na.omit(ROC(benchmarkPrices))   
benchmarkReturns2 <- na.omit(ROC(benchmarkPrices2)) 
head(benchmarkReturns) 


#Retornos del portafolio segun el peso de cada accion. 
#Sale una sola columna
#porque son los retornos del portafolio segun los pesos. 
#Funcion de returno de portafolio del paquete Performance analytics

RetornosPortafolio <- Return.portfolio(portfolioReturns)
#RetornosPortafolio

#Grafica ROC
portfolioReturns %>% chartSeries(TA="addROC()",subset="2020")

#portfolioReturns2 %>% chartSeries(TA="addROC()",subset="2020")
benchmarkReturns %>% chartSeries(TA="addROC()",subset="2020")

benchmarkReturns2 %>% chartSeries(TA="addROC()",subset="2020")



#Clase 19
#Midiendo rentabilidad segun retorno esperado y riesgo 
# si es mayor 1 tiene mayor riesgo contra el mercado comparativp
CAPM.beta(portfolioReturns, benchmarkReturns, .035/252)     

#Calcular retornos al año     
# si el sharpe es mas positivo tiene mejor adaptacion al riesgo
table.AnnualizedReturns(portfolioReturns)



#Clase 20      
#optimizacion portafolio. Maximizar retornos
#modificar los pesos
#los inputs: assets, constraints, objectives   
#objetivo:
library(PortfolioAnalytics)

portfolioReturns

portf <- portfolio.spec(colnames(portfolioReturns))

# el 1 hace referencia a que se cumpla para todos los elementos del trade
portf <- add.constraint(portf, type="weight_sum", min_sum=1,
                        max_sum=1)
# este tipo de restricción indica que ninguna acción tendra menos de 10% ni mas de 40% del presupuesto
portf <- add.constraint(portf, type="box", min=.10, max=.40)

# retorno monitoreado basado en los promedios
portf <- add.objective(portf, type="return", name="mean")

# riesgo monitoreado segun la desviación estandar
portf <- add.objective(portf, type="risk", name="StdDev")

library(GenSA)
?GenSA
# optimiza los pesos con el metodo Gensa generalized simulated aniling
optPort <- optimize.portfolio(portfolioReturns, portf, 
                              optimize_method ="GenSA")
optPort  


getSymbols.yahoo()

ticker

#Clase 21 - Back testing
# 1. Load libraries.
library(Quandl)
library(scales)
library(gridExtra)   
library(TTR)
library(jsonlite)
library(xtable)
library(gtable)
library(grid)
library(dplyr)

# 2. Load  
library(tidyverse)
library(httr)
library(readxl)
library(lubridate)
library(reshape2)
library(quantmod)

# 3. Set working directory. Estoy llamando mi codigo. 
setwd("/home/akasha/Documents/TRADING-R-PLATZI-master")

# 4. Quandl authentication key. Se debe generar una api key en Quandl
Quandl.api_key("VezxJGwvuyxLx3ByLrGo")

#Evaluar la estrategia de long y short en cada cia.



get_data <- function(ticker, from, to = Sys.Date()) {
  return(getSymbols.yahoo(ticker, from = from, to = to, auto.assign=F))
}


getSymbolsYahoo <- function(ticker,from) {
  data_full <- get_data(ticker,from)
  data_full <- data.frame(
    "Date"=as.Date(rownames(as.data.frame(data_full))),
    "Open"=as.data.frame(data_full)[,1],
    "High"=as.data.frame(data_full)[,2],
    "Low"=as.data.frame(data_full)[,3],
    "Close"=as.data.frame(data_full)[,4],
    "Volume"=as.data.frame(data_full)[,5],
    "Adj.Close"=as.data.frame(data_full)[,6]
  )
}
ILF = NULL
PFL = NULL




PFL <- getSymbolsYahoo("KOF",'1995-01-01')  %>%
filter(date >= "1995-01-01")





#iShares Latin America 40 ETF (ILF)
ILF <- getSymbolsYahoo("ILF",'2015-01-01') %>% 
  filter(date >= "2015-01-01")

ILF <- ILF %>%
  mutate( ticker = "ILF")

PFL <- PFL %>%
  mutate(signal = ifelse(as.numeric(format(Date, "%m")) <= 9,1,-1), ticker = "M01")

# 5. Calcular senal de trading para la estrategia M01. 
# PFL <- PFL %>%
#   mutate(signal = ifelse(as.numeric(format(date, "%m")) <= 9,1,-1), ticker = "M01")

# 6. Calcular retornos diarios, senal de returno, retornos acumulados, 
#rolling retornos acumulados, drawdown, 
# y sharpe ratio. 
head(PFL)
PFL <- PFL %>%
  mutate(daily_return = ifelse(row_number() == 1, 0, Adj.Close / lag(Adj.Close, 1) - 1), 
         signal_return = daily_return * signal, 
         #retornos acumulados
         cum_return = cumprod(1 + signal_return) - 1, 
         cum_return_3m = (cum_return + 1) / lag(cum_return + 1, 63) - 1, 
         cum_return_12m = (cum_return + 1) / lag(cum_return + 1, 252) - 1, 
         drawdown = (cum_return + 1) / cummax(cum_return + 1) - 1, 
         sd_12m = runSD(signal_return, n = 252)*sqrt(252), 
         sharpe_12m = SMA(cum_return_12m / sd_12m), 252)

ILF <- ILF %>% 
  mutate(daily_return = ifelse(row_number() == 1, 0, Adj.Close / lag(Adj.Close, 1) - 1), 
         cum_return = cumprod(1 + daily_return) - 1, 
         cum_return_3m = (cum_return + 1) / lag(cum_return + 1, 63) - 1, 
         cum_return_12m = (cum_return + 1) / lag(cum_return + 1, 252) - 1, 
         drawdown = (cum_return + 1) / cummax(cum_return + 1) - 1, 
         sd_12m = runSD(daily_return, n = 252)*sqrt(252), 
         sharpe_12m = SMA(cum_return_12m / sd_12m), 252)

combined= NULL
combined <- bind_rows(PFL, ILF)
tail(combined)


# 7. Plot equity curve versus benchmark.



###
(p1 <- ggplot(combined, aes(x = Date, y = cum_return)) + 
    geom_line(aes(colour = ticker)) + 
    labs(title = "Equity Curve Versus Benchmark", 
         subtitle = "Evaluación de desempeno de la estrategia", 
         y = "Cumulative Return", 
         x = "Date") + 
    geom_hline(yintercept = 0) + 
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()))






#Clase 22
(p2 <- ggplot(PFL, aes(x = Date, y = signal)) + 
    geom_line(size = 1, colour = "blue") + 
    labs(title = "Trading Signal", 
         subtitle = "Senal de mi estrategia.Entre  +1 y -1.", 
         y = "Position", 
         x = "Date") + 
    geom_hline(yintercept = 0))




(p3 <- ggplot(PFL, aes(x = Date, y = Adj.Close)) + 
    geom_line(aes(colour = signal)) + 
    scale_colour_gradient(low = "red") +
    labs(title = "PFL Closing Price con Trading Signal", 
         subtitle = "Cuando short y cuando long", 
         y = "Precio Cierre Ajustado", 
         x = "Fecha") + 
    geom_hline(yintercept = 0))




#Clase 23
(p4 <- ggplot(combined, aes(x = Date, y = cum_return_3m)) + 
    geom_line(aes(colour = ticker)) + 
    labs(title = "Rolling Returns (3 Meses)", 
         y = "Retorno", 
         x = "Fecha") + 
    scale_y_continuous(labels = percent, limits = c(-0.5, 0.75)) + 
    geom_hline(yintercept = 0))

(p5 <- ggplot(combined, aes(x = Date, y = drawdown)) + 
    geom_line(aes(colour = ticker)) + 
    labs(title = "Drawdown", 
         subtitle = "Frecuencia de caidas, tamano de maximas caidas y tiempo de recuperacion", 
         y = "Porcentaje drawdown", 
         x = "Fecha") + 
    scale_y_continuous(labels = percent) + 
    geom_hline(yintercept = 0))






#clase 24
(p6 <- ggplot(combined, aes(x = Date, y = sharpe_12m)) + 
    geom_line(aes(colour = ticker)) + 
    labs(title = "Sharpe Ratio (12 Meses)", 
         subtitle = "Sharpe ratio. Retornos por unidad de riesgo.", 
         y = "Sharpe Ratio", 
         x = "Fecha") + 
    geom_hline(yintercept = 0))





#clase 25 - Evaluando rentabilidad   
library(tidyverse)
library(tidyquant)
library(PerformanceAnalytics)
library(PortfolioAnalytics)
library(dplyr)


stocks <- c("KOF", "TV", "BBD", "WALMEX.MX", "GFNORTEO.MX")

stock_data <- tq_get(stocks,
                     get = "stock.prices",
                     from = Sys.Date() - months(12),
                     to = Sys.Date())

Ra <- stocks %>%
  tq_get(get  = "stock.prices",
         from = Sys.Date() - months(12),
         to = Sys.Date()) %>%
  group_by(symbol) %>%
  tq_transmute(select     = adjusted, 
               mutate_fun = periodReturn, 
               period     = "monthly", 
               col_rename = "Ra")
Ra
optimize.portfolio.rebalancing
rp <- random_portfolios(combined, 10000, "sample")
rando
init.investment <- 1000

growth <- Ra %>% arrange(date) %>%
  mutate(final_value = init.investment * cumprod(1 + returns)) %>%
  arrange(desc(final_value))
  growth %>% filter(date == max(date)) %>% select(-date)

growth %>% ggplot(aes(x = date, y = final_value, color = symbol)) +
  geom_line() +
  # geom_smooth(method = "loess") +
  labs(
    title = "Portafolio individual: Comparando el crecimiento de US1000",
    subtitle = "Visualizacion de desempeno",
    x = "",
    y = "Valor inversion"
  ) +
  theme_tq() + theme(legend.position = "right") +
  scale_y_continuous(labels = scales::dollar)

growth %>% ungroup() %>% filter(date == max(date)) %>% 
  mutate(rank = row_number()) %>% top_n(5, final_value) %>% 
  select(rank, symbol, final_value)   


#**************************EXAM******************************
#**************************EXAM******************************
#**************************EXAM******************************
#**************************EXAM******************************
#**************************EXAM******************************
#**************************EXAM******************************
#**************************EXAM******************************
#**************************EXAM******************************
#**************************EXAM******************************
#**************************EXAM******************************
#**************************EXAM******************************
#**************************EXAM******************************
#**************************EXAM******************************
#**************************EXAM******************************




# AAPL NFLX FB AMZN 




date <- "2019-3-1"  
tickers <- c("AAPL", "NFLX", "FB", "AMZN")      


# crear tabla dinamicamente pasandole array de empresas/ticker symbol 

portfolioPrices <- NULL
for(ticker in tickers) {
  portfolioPrices <- cbind(portfolioPrices, getSymbols.yahoo(ticker,
                                                             from="2019-3-1", periodicity="daily",auto.assign=FALSE)[,6])  
}


head(portfolioPrices)
aapl <- getSymbols.yahoo("AAPL", from = date)

portfolioReturns <- na.omit(ROC(portfolioPrices))  

portf <- portfolio.spec(colnames(portfolioReturns))

# el 1 hace referencia a que se cumpla para todos los elementos del trade
portf <- add.constraint(portf, type="weight_sum", min_sum=1,
                        max_sum=1)
# este tipo de restricción indica que ninguna acción tendra menos de 10% ni mas de 40% del presupuesto
portf <- add.constraint(portf, type="box", min=.10, max=.40)

# retorno monitoreado basado en los promedios
portf <- add.objective(portf, type="return", name="mean")

# riesgo monitoreado segun la desviación estandar
portf <- add.objective(portf, type="risk", name="StdDev")

library(GenSA)

# optimiza los pesos con el metodo Gensa generalized simulated aniling
optPort <- optimize.portfolio(portfolioReturns, portf, 
                              optimize_method ="GenSA")
optPort  




summary(optPort)










































