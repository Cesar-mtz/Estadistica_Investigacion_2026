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



