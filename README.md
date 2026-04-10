# Estadistica en la Investigación 2026
# Repositorio del programa de Doctorado Estadística en la Investigación DCMRN 

# César Aldair Martínez Gauna

# Matrícula: 1591819

####################################################################
# Contenido del curso

**Sesión 1**: Fecha 26/01/26

+ Crear cuenta de Github
+ Crear repositorio del curso
+ Primera sincronización

####################################################################

**Sesión 2**: Fecha 16/02/26

# Contenido del curso

+ Importar datos excel usando el comando read.cv
+ Aplicar función tapply
+ Diseño de boxplot
+ Aplicar prueba de normalidad de datos Saphiro
+ Aplicar prueba de homogeneidad de varianza Bartlett

# EJEMPLO CLASE #################################################

IE <-read.csv("03_Datos/vivero.csv", header = T)
IE$Tratamiento <-as.factor(IE$Tratamiento)

View(IE)

tapply(IE$IE, IE$Tratamiento, mean)
tapply(IE$IE, IE$Tratamiento, sd)
tapply(IE$IE, IE$Tratamiento, var)


# Diseño de gráfico -------------------------------------------------------


boxplot(IE$IE)

boxplot(IE$IE ~ IE$Tratamiento,
        col = "lightblue",
        xlab = "Fertilizante",
        ylab = "IE",
        main = "Vivero FCF",
        ylim = c(0.4, 1.2))
#~ significa en función de o sea cuando tienes varias variables
#para evitar problemas en compilación tener cuidado las carpetas donde se
#trabaja por lo que hay que guardarla de nuevo y ahora si se compila

# Pruebas de normalida "Saphiro y Kormogorof" -----------------------------

shapiro.test(IE$IE)

# Homogeneidad de varianza "Bartlett"--------------------------------------

bartlett.test(IE$IE ~ IE$Tratamiento)

hist(IE$IE,
     col = "red",
     ylim = c(0,12),
     main = "",
     ylab ="Frecuencia",
     xlab = "Variable IE")

####################################################################

**Sesión 3**: Fecha 23/02/26

# Contenido del curso

+ Aplicar prueba no paramétrica "Correlación Spearman"
+ Aplicar prueba no paramétrica "Correlación Kendall"
+ Generar números aleatorios aplicando comando set.seed
+ Utilizar comando sample con set.seed
+ Utilizar comando ifelse para variable dicotómica

# EJEMPLO CLASE #################################################

# Insertar datos ----------------------------------------------------------

resp <- data.frame(
  Tiempo = c(12,15,17,18,20,21,22,26),
  Edad = c(14,25,20,35,45,30,60,95)
)
resp

#Crear nuevas columnas con los rangos (1 a 8)
resp$Rango_Tiempo <-rank(resp$Tiempo, ties.method = "first")
resp$Rango_Edad <-rank(resp$Edad, ties.method = "first")

#Ver resultado
resp

plot(resp$Tiempo, resp$Edad)
plot(resp$Rango_Tiempo, resp$Rango_Edad)


cor.test(resp$Rango_Tiempo, resp$Rango_Edad, method = "spearman")

# Prueba de Kendall -------------------------------------------------------

tau <- data.frame(
  A = c(1,2,3,4,5,6),
  B = c(3,1,4,2,6,5)
)
cor.test(tau$A, tau$B, method = "kendall")

# Correlación punto biserial  ---------------------------------------------

set.seed(123)
#datos dicticios y para que sea siempre los mismos datos, si no sería aleatorio

#Número de observaciones
n <-20

#Generar horas de estudio (entre 1 y 10)
Horas_estudio <- sample(1:10, n, replace = TRUE)

#Asignar probabilidad de aprobar en funcion de horas de estudio
#a mas horas, mas alta probabilidad

Resultado <-sapply(Horas_estudio, function (horas){
  ifelse(runif(1) < (horas / 10), "Aprobado", "Reprobado")
         
  })

# Crear data frame
estudio <-data.frame(
  Estudiante = 1:n,
  Horas_estudio,
  Resultado
)

View(estudio)

# Crear variable dicotómica: 1 = Aprobado, 0 = Reprobado

estudio$Resultado_bin <-ifelse(estudio$Resultado =="Aprobado", 1, 0)
head(estudio)

#correlacion

cor.test(estudio$Horas_estudio, estudio$Resultado_bin, method = "pearson")
estudio$Resultado_bin <- ifelse(estudio$Resultado == "Aprobado", 1, 0)
head(estudio)

####################################################################

**Sesión 4**: Fecha 02/03/26

# Contenido del curso

+ Descargar y activar complemento lm mediante library
+ Aplicar prueba ANOVA
+ Aplicar prueba paramétrica de heterocedasticidad "Breusch - Pragan" 
+ en modelo de regresión
+ Aplicar comandos de estimacion de residuales, varianza, desviación estandar
+ Generar línea de tendencia en plot

# EJEMPLO CLASE #################################################

# ingreso de datos --------------------------------------------------------

x2 <- c(10, 8, 13, 9, 11, 14, 6, 4, 12, 7, 5)
y2 <- c(9.14, 8.14, 8.74, 8.77, 9.26, 8.10, 6.13, 3.10, 9.13, 7.26, 4.74)
  
mean(x2)
mean(y2)

# ejercicio 1 -------------------------------------------------------------
datos <- data.frame(
  Produccion_trigo = c(30, 28, 32, 25, 25, 25, 22, 24, 35, 40),
  Precio_harina    = c(25, 30, 27, 40, 42, 40, 50, 45, 30, 25)
)

plot(datos$Produccion_trigo, datos$Precio_harina)

mean_x <- mean(datos$Produccion_trigo)
mean_y <- mean(datos$Precio_harina)


cor.test(datos$Produccion_trigo, datos$Precio_harina)

#descargar complemento lm

library(lmtest)

fit.lm <- lm(datos$Precio_harina ~ datos$Produccion_trigo)
summary(fit.lm)

anova(fit.lm)

#residuales en el modelo lineal fit.lm
fit.lm$residuals

#valores ajustados Yprima en el modelo lineal
fit.lm$fitted.values


#gráfica de Yprima vs residuales
plot(fit.lm$fitted.values, fit.lm$residuals)

# EJERCICIO 2 -------------------------------------------------------------

x <- c(30, 28, 32, 25,25, 25, 22, 24, 35, 40)

y <- c(25, 30, 27, 40, 42, 40, 50, 45, 30, 25) 

#Prueba paramétrica "Breusch-Pagan"
bptest(fit.lm)
#el resultado de esta prueba de p-value= 0.5641 indica si se cumple o no el 
#criterio

#Agregar columna en la recta utilizadno Bo y B1
datos$recta <- 74.1151 - 1.3537 * datos$Produccion_trigo
datos$residuales <- datos$Precio_harina - datos$recta
datos


#estimar residuales
sum(datos$residuales^2)

#varianza
sum(datos$residuales^2)/8
#25.99

#desviación estandar
sqrt(sum(datos$residuales^2)/8)
##5.05

#hay que correr el lm para la linea de tendencia

plot(datos$Produccion_trigo, datos$Precio_harina,
     col ="indianred",
     xlab ="produccion trigo (Ton/ha",
     ylab ="Precio harina (Euros",
     pch = 19,
      cex = 1.2)
abline(fit.lm, col = "blue")