# Librerías -----------------------------------------------------------------------------------
library(tidyverse)
theme_set(theme_bw())
theme_update(plot.title = element_text(hjust = 0.5))
library(forecast)
library(tseries)
library(lmtest)
library(MLmetrics)

setwd('C:/Users/Jorge Delgado/OneDrive - Universidad de Chile/Escritorio/Diplomado Data Science/Material complementario/Time_Series')


# Ejercicio 1 ---------------------------------------------------------------------------------

# Cargamos la base de datos
datos <- 
  readxl::read_xlsx("serie_lluvia_1990-2015.xlsx") %>% 
  janitor::clean_names()

# Veamos si es aceptable el formato
glimpse(datos)
str(datos)

# Los meses están en mayúscula así que los dejamos de forma más bonita
# Notamos que los mm_lluvia están en formato carácter, por lo que lo pasamos a número
datos <-
  datos %>% 
  mutate(mes = stringr::str_to_title(mes), 
         mm_lluvia = mm_lluvia %>% as.numeric()) 

# Ahora veamos si se arreglaron
glimpse(datos)

## Notamos que ahora están bien los tipos de las variables

# Ejercicio 2 ---------------------------------------------------------------------------------

# Definimos la serie desde el 1990 al 2014 (esto para luego ver si están correctas las predicciones del 2015)
datos_ts <- ts(data = datos$mm_lluvia, start = c(1990, 1), end = c(2014, 12), frequency = 12)

# Graficamos la serie
plot(datos_ts)
## Se puede ver que hay tendencias al alza al principio, luego se mantiene constante y luego 
## una pequeña tendencia al alza.
## No se nota una estacionalidad muy notoria a simple vista.

# Podemos agregar una recta de regresión (opcional porque no se vio en clases)
abline(reg = lm(datos_ts ~ time(datos_ts)))
## Se pueden hacer regresiones polinomiales que se ajusten mejor a los datos que la r.l.
## Por si quieren seguir usando esto a futuro pueden investigar sobre esto.

# Veamos los gráficos de descomposición temporal:

# Modelo Aditivo:
decom <- decompose(datos_ts)
plot(decom)

## Notamos que la tendencia es parecida a lo comentado y que hay un poco de varianza no constante

# Ejercicio 3 ---------------------------------------------------------------------------------

# Boxcox:
lambda <- forecast::BoxCox.lambda(x = datos_ts)

# Mostramos el lambda óptimo
lambda

## 0.2399751

# Ejercicio 4 ---------------------------------------------------------------------------------

# Transformamos los datos con el lambda obtenido
datos_ts_box <- forecast::BoxCox(datos_ts, lambda = lambda)

# Vemos como quedó la serie y sus componentes
plot(datos_ts_box)
plot(decompose(datos_ts_box))

## Se puede notar que ahora la varianza es un poco más constante, aunque no 100%

# Ejercicio 5 ---------------------------------------------------------------------------------

# Diferenciación regular:
datos_diff_box <- diff(datos_ts_box)
plot(datos_diff_box)

# Diferenciación estacional:
datos_diff_box_final <- diff(datos_diff_box, lag = 2)
plot(datos_diff_box_final)
  
## Ahora se nota con media y varianza constantes. 

# Hacemos el test de Dickey-Fuller:

# H0: La serie de tiempo no es estacionaria
# H1: La serie de tiempo es estacionaria
tseries::adf.test(datos_diff_box_final)

## Como es menor a 0.05 rechazamos la hipótesis de no estacionariedad.
## Por lo tanto concluimos que la serie es estacionaria y seguimos con los modelos.

# Ejercicio 6 ---------------------------------------------------------------------------------

# ACF: Autocorrelación
acf(datos_diff_box_final, lag.max = 100) 
## Repite el comportamiento.
## Se nota estacionalidad cada 9 rezagos aproximadamente.

# PACF: Autocorrelación parcial
pacf(datos_diff_box_final, lag.max = 100)
## No se nota un patrón muy pronunciado.

## Es muy difícil desprender los parámetros del modelo simplemente con los gráficos, así que 
## mejor hagamos un auto.arima para ayudarnos, pues este nos dará el modelo con menor AIC.

# Ejercicio 7 ---------------------------------------------------------------------------------

# Hacemos el modelo
modelo_1 <- forecast::auto.arima(datos_ts_box)

# Veamos como queda el modelo
modelo_1

## Es un ARIMA (1, 0, 0) (1, 1, 0)[12]. Es decir con:
# d: 0 (no se hicieron diferenciaciones en la parte regular)
# D: 1 (se hizo una diferenciación en la parte estacional)
# Con componente autoregresiva para la tendencia. (dependencia regular) (p)
# Hay componente autoregresiva para la dependencia estacional. (P)
# No hay componente de media móvil ni en la parte regular (q), ni en la parte estacional (Q)


# Ejercicio 8 ---------------------------------------------------------------------------------

# Ajustamos el modelo obtenido por auto.arima
modelo_2 <- forecast::Arima(datos_ts_box, order = c(1, 0, 0), seasonal = c(1, 1, 0), include.drift = T)

# Veamos los coeficientes del modelo
lmtest::coeftest(modelo_1)
lmtest::coeftest(modelo_2)
## Notamos que el coeficiente autoregresivo de la dependencia regular es significativo
## El coeficiente autoregresivo de dependencia estacional si es significativo
## El coeficiente del drift no es significativo así que podríamos quitarlo eventualmente

# Veamos si los residuos son independientes o no: Llung-box test
# H0: Los residuos se distribuyen independientes entre si
# H1: Los residuos no se distribuyen de manera independiente
forecast::checkresiduals(modelo_2, plot = T) 
## Rechazamos H0, por lo que los residuos no son independientes con 0.05 de significancia o 95% de confianza

# Veamos si los residuos distribuyen normal o no:

# Graficamos el qqplot
ggplot(data = data.frame(modelo_2$residuals)) +
  stat_qq(mapping = aes(sample = modelo_2$residuals), size = 1, color = "dodgerblue1") +
  stat_qq_line(mapping = aes(sample = modelo_2$residuals), color = "chocolate2") +
  labs(x = "Cuantiles teóricos", y = "Residuos", title = "Q-Q Plot Normal")
## Notamos que los puntos se alejan bastante de las colas, por lo que podría no haber normalidad
## Hagamos los test para verificar:

# H0: Residuos distribuyen normal
# H1: Residuos no distribuyen normal
nortest::lillie.test(modelo_2$residuals)
## No se rechaza el supuesto de normalidad

shapiro.test(modelo_2$residuals)
## Se rechaza el supuesto de normalidad para alpha = 0.05, pero no para alpha = 0.01

## Conclusión:
## No cumplen el supuesto de independencia, pero si el de normalidad. 

# Ejercicio 9 ---------------------------------------------------------------------------------

# Predecimos:
pred <- forecast(modelo_2, level = c(95), h = 12)
autoplot(pred) 
plot(pred)

# Definimos los datos futuros reales que están en la base:
datos_futuros <- 
  datos %>% 
  filter(ano > 2014)

# Los pasamos a una serie de tiempo
datos_ts_futuros <- ts(datos_futuros$mm_lluvia, start = c(2015, 1), end = c(2015, 12), frequency = 12)

# Para compararlos en una gráfica, necesitamos aplicar la transformación de BoxCox inversa
# pues nuestro modelo recibió los datos transformados por BoxCox
pred_no_trans <- InvBoxCox(pred$mean, lambda = lambda)
lower_ci <- InvBoxCox(pred$lower, lambda = lambda)
upper_ci <- InvBoxCox(pred$upper, lambda = lambda)

# Graficamos
autoplot(pred_no_trans, color = "black") + 
  autolayer(lower_ci, lty = 2, lwd = 1.5) + 
  autolayer(upper_ci, lty = 2, lwd = 1.5) + 
  autolayer(datos_ts_futuros)

## Notamos que las predicciones (negro) se parecen a lo real (rojo)
## Aunque los I.C. son bastante grandes

# MAPE: (mean absolute percentage error) (error absoluto medio porcentual)

# Manualmente:
mean(abs(datos_ts_futuros - pred_no_trans) / abs(datos_ts_futuros))

# Con función de R:
MLmetrics::MAPE(pred_no_trans, datos_ts_futuros)

## El MAPE es del 50%. Las predicciones tienen un error del 50% con respecto a los datos reales de 2015
## Lo cual no es muy aceptable (20% o 30% sería más aceptable)

# Definamos los valores reales y los predichos en un dataframe para compararlos visualmente:
comparacion <-
  data.frame(
    predichos = pred_no_trans,
    reales = datos_ts_futuros,
    mes = unique(datos$mes)
  )

# Ahora los vemos
comparacion

## Son bastante distintos en general.
## Podríamos intentar con otros modelos de series de tiempo para ver si mejoran las predicciones.