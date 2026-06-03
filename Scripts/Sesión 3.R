# Estadistica en la Investigación 2026
# Repositorio del programa de Doctorado Estadística en la Investigación DCMRN 

# César Aldair Martínez Gauna

# Matrícula: 1591819

#**Sesión 3**: Fecha 23/02/26
##################################################################


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
# H0 (Hipótesis Nula): No existe correlación entre las dos variables 
# H1 (Hipótesis Alternativa): Existe correlación entre las dos variables
# Nota: Si p-valor < 0.05, se rechaza H0 y se concluye que hay una correlación significativa
# Uso: Ideal para datos ordinales, muestras pequeñas o cuando no hay normalidad

tau <- data.frame(
  A = c(1,2,3,4,5,6),
  B = c(3,1,4,2,6,5)
)
cor.test(tau$A, tau$B, method = "kendall")


# Correlación punto biserial  ---------------------------------------------

set.seed(123)
#utilizar este comando cuando se busca que siempre los mismos datos, si no sería aleatorio

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
# estructura estándar en R que te permite construir una base de datos real sin que los datos se alteren o se corrompan.

estudio <-data.frame(
  Estudiante = 1:n,
  Horas_estudio,
  Resultado
)

View(estudio)


# Crear variable dicotómica: 1 = Aprobado, 0 = Reprobado
## reducir una característica cualitativa a solo dos estados posibles (0 y 1)

estudio$Resultado_bin <-ifelse(estudio$Resultado =="Aprobado", 1, 0)
head(estudio)


#correlacion
## medir la fuerza y la dirección de la relación lineal entre dos variables, permitiéndote saber si los cambios en una variable están asociados con los cambios en la otra.

cor.test(estudio$Horas_estudio, estudio$Resultado_bin, method = "pearson")
estudio$Resultado_bin <- ifelse(estudio$Resultado == "Aprobado", 1, 0)
head(estudio)
