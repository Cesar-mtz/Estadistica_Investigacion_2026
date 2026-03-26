# Estadistica en la Investigación 2026
# Repositorio del programa de Doctorado Estadística en la Investigación DCMRN 

# César Aldair Martínez Gauna

# Matrícula: 1591819

#**Sesión 5**: Fecha 09/03/26

# EJERCICIOS CLASE --------------------------------------------------------

#CLASE DE HOY "COMPARACION DE MEDIAS"
#TIPOS DE MUESTRA: UNA MUESTRA (EXISTE MEDIA DE REFERENCIA "MEDIA TEORETICA" 
#ESTABLECIDA DE ALGUN ESTUDIO) "UN GRUPO DE INDIVIDUOS QUE TIENEN UNA MEDIA Y ES
#LA QUE SE COMPARA

#MUESTRA INDEPENDIENTE: DOS GRUPOS DE INDIVIDUOS CON VARIABLE EN COMUN

#MUESTRA DEPENDIENTE: GRUPO DE INDIVIDUOS QUE SE COMPARA ANTES Y DESPUES (ESCALA
#DE TIEMPO) #EJEMPLO PRODUCCION XAÑO 2024 - 2025 AGRICOLAS, FRUTALES
############################################################################3


#LA PRUEBA DE Tstudent COMPARA DOS MEDIAS Y SEÑALA SI HAY DIFERENCIAS 
#SIGNIFICATIVAS ENTRE ELLAS

#características de las pruebas
#datos normales, homogeneidad, n=30


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
#Safiro y Kormogorof

shapiro.test(IE$IE)

# Homogeneidad de varianza ------------------------------------------------

bartlett.test(IE$IE ~ IE$Tratamiento)


hist(IE$IE,
     col = "red",
     ylim = c(0,12),
     main = "",
     ylab ="Frecuencia",
     xlab = "Variable IE")
###############################################################################
# pruebas para independientes
t.test(IE$IE ~ IE$Tratamiento, var.equal =T)


#INTERVAL DE CONFIANZA: DONDE EXISTE UN VALOR ESTA DENTRO DEL RANGO PARA SABER
#SI ES NORMAL O NO

#var.equal =T (no importa si alguno de tus datos es mayor o no  que el otro)


t.test(IE$IE ~ IE$Tratamiento, var.equal =T,
       alternative= "greater")
#con este comando es para 1 sola cola cuando ya estas diciendo en tu hipotesis
#mayor que


t.test(IE$IE ~ IE$Tratamiento, var.equal =T,
       alternative= "less")

#diferencia greater y less (depende de como lo vayas a plantear en la hipotesis)
##############################################################################

#Prueba de T una muestra
mean(IE$IE)


t.test(mu = 0.85, IE$IE)
#mu media teoretica

#prueba dependiente
Ctrl <- subset(IE$IE, IE$Tratamiento == "Ctrl")
Fert <- subset(IE$IE, IE$Tratamiento != "Ctrl")

t.test(Ctrl, Fert, paired = T)










