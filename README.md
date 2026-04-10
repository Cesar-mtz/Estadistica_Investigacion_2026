# Estadistica en la Investigación 2026
# Repositorio del programa de Doctorado Estadística en la Investigación DCMRN 

# César Aldair Martínez Gauna

# Matrícula: 1591819

####################################################################
# Contenido del curso

# Sesión 1: Fecha 26/01/26

+ Crear cuenta de Github
+ Crear repositorio del curso
+ Primera sincronización

####################################################################

#Sesión 2: Fecha 16/02/26

# Contenido del curso

+ Importar datos excel usando el comando read.cv
+ Aplicar función tapply
+ Diseño de boxplot
+ Aplicar prueba de normalidad de datos Saphiro
+ Aplicar prueba de homogeneidad de varianza Bartlett

# EJEMPLO #################################################

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











