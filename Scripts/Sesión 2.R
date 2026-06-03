# Estadistica en la Investigación 2026
# Repositorio del programa de Doctorado Estadística en la Investigación DCMRN 

# César Aldair Martínez Gauna

# Matrícula: 1591819

#**Sesión 2**: Fecha 16/02/26


# Importar datos --------------------------------------------------------

# Función read.csv

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



# Pruebas de normalida ----------------------------------------------------
#Saphiro y Kolmogorov

shapiro.test(IE$IE)

# H0 (Hipótesis Nula): Los datos siguen una distribución normal.
# H1 (Hipótesis Alternativa): Los datos NO siguen una distribución normal.
# Nota: Si p-valor > 0.05, se asume normalidad.

# Homogeneidad de varianza ------------------------------------------------

bartlett.test(IE$IE ~ IE$Tratamiento)
# H0 (Hipótesis Nula): Las varianzas son iguales entre los grupos.
# H1 (Hipótesis Alternativa): Al menos un grupo tiene una varianza distinta.
# Nota: Si p-valor > 0.05, se asume homocedasticidad (varianzas iguales).
# Requisito: Los datos de los grupos deben seguir una distribución normal.  

hist(IE$IE,
     col = "red",
     ylim = c(0,12),
     main = "",
     ylab ="Frecuencia",
     xlab = "Variable IE")
