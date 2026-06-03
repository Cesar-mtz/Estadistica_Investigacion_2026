# Estadistica en la Investigación Científica 2026

# Repositorio del programa de Doctorado Estadística en la Investigación DCMRN 

# César Aldair Martínez Gauna

# Sesión 2: Fecha 09/02/26

############################

# Gastos mensuales
300+240+1527+400+1500+1833

# Variables de almacén de datos
Celular <- 300
Transporte <- 240
Comestibles <- 1527
Gimnasio <- 400
Alquiler <- 1500
Otros <- 1833
Sumatoria <-5800

Celular
Transporte
Comestibles
Gimnasio
Alquiler
Otros
Sumatoria

# ¿Cuánto gastaría durante un semestre escolar (5 meses)?
Sumatoria*5 ## El gasto sería de 29,000 pesos.

# ¿Cuánto gastaría durante un año escolar (10 meses)?
Sumatoria*10 ## El gasto sería de 58,000 pesos.

# Comprobación de función
sum(Celular+Transporte+Comestibles+Gimnasio+Alquiler+Otros)

# Autoevaluación
# Toma los datos creados de celular, transporte, comestibles etc. para crear
# un vector "gastos".

Gastos <- c(Celular=Celular, Transporte=Transporte, Comestibles=Comestibles, 
            Gimnasio=Gimnasio, Alquiler=Alquiler, Otros=Otros)

# Usar función barplot () para generar un diagrama de barra de gastos
barplot(Gastos)

# Usar función sort () para ordenar los Gastos de manera decreciente
sort(Gastos)

# Usar función sort () y barplot () para generar un diagrama de barra de gastos
# orden decreciente

help("sort")

sort(Gastos, decreasing = TRUE)
sort(Gastos, decreasing = FALSE)

Gastos_decrecientes <- c(sort(Gastos, decreasing = TRUE))

# Genera el gráfico con las barras ordenadas
barplot(Gastos_decrecientes, main = "Gastos Mensuales")

# Parte II Variables

## Problema 1:

### Identifique el tipo de variable (cualitativa o cuantitativa) para la lista
### de preguntas de una encuesta aplicada a estudiantes universitarios en una clase de estadística

# Nombre de estudiante.*Cualitativo*

# Fecha de nacimiento: (21/10/1995) *Cualitativo*

# Edad (en años). *Cuantitativo*

# Dirección de casa: 1234 Ave. Alamo) *Cualitativo*

# Número de teléfono: 510-123-4567). *Cualitativo*

# Área principal de estudio. *Cualitativo*

# Grado de año universitario: primer año, segundo año, tercer año, último año.*Cualitativo*

# Puntaje en la prueba de mitad de período (basado en 100 puntos posibles). *Cuantitativo*

# Calificación general: A, B, C, D, F. *Cualitativo*

# Tiempo (en minutos) para completar la prueba final de MCF 202. *Cuantitativo*

# Número de hermanos. *Cuantitativo*

