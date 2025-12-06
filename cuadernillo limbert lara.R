 6 #cargando la data a trabjar 

data<- read_csv("C:/Users/USER/Downloads/covid_19_diabetes.csv")

6#limpiar columna 

data_limpia <- data[!is.na(data$edad),]

8 #CARGAR PAQUETES 
install.packages("ggplot2")
library(ggplot2)

8#numerica para la edad correccion 
data$edad <- as.character(data$edad)
data$edad <- gsub("[^0-9]", "", data$edad)
data$edad <- as.numeric(data$edad)
8#histograma 
ggplot(data, aes(x= edad)) + geom_histogram(binwidth = 5)
ggplot(data, aes(x=edad)) + geom_histogram(binwidth = 5)

10#MEDIDAS DE TENDENCIA CENTRAL

# MEDIA ARITMETICA 
mean(data$edad, na.rm = TRUE)

#MEDIANA 
median(data$edad, na.rm = TRUE)
#MODA 
moda <- function(x){x <- x[!is.na(x)]; if(length(x)==0)return(NA); tabla <- table(x); as.numeric(names(tabla)[tabla==max(tabla)])}
moda(data$edad)

12# MEDIDAS DE DISPERSIÓN Y POSICIÓN


# Visualizar primeras 6 filas
head(data)

# VARIANZA
var_edad <- var(data$edad, na.rm = TRUE)
var_edad

# DESVIACIÓN ESTÁNDAR
sd_edad <- sd(data$edad, na.rm = TRUE)
sd_edad

# RANGO
rango_edad <- max(data$edad, na.rm = TRUE) - min(data$edad, na.rm = TRUE)
rango_edad

# RANGO INTERCUARTÍLICO (IQR)
iqr_edad <- IQR(data$edad, na.rm = TRUE)
iqr_edad

# CUARTILES
cuartiles_edad <- quantile(data$edad, probs = c(0.25, 0.50, 0.75), na.rm = TRUE)
cuartiles_edad

# RESUMEN COMPLETO
summary(data$edad)


14# ANALISIS DE REGRESION COMPLETO (UNIVARIADO Y MULTIVARIADO)

# Ver primeras filas y nombres
head(data)
names(data)

# Crear variable de edad numérica
edad_num <- data$Edad

# Medidas de dispersión y posición
varianza <- var(edad_num, na.rm = TRUE)
desviacion <- sd(edad_num, na.rm = TRUE)
rango_total <- max(edad_num, na.rm = TRUE) - min(edad_num, na.rm = TRUE)
ric <- IQR(edad_num, na.rm = TRUE)
cuartiles <- quantile(edad_num, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)

varianza
desviacion
rango_total
ric
cuartiles

# Histograma de edad
hist(edad_num, main="Histograma de Edad", xlab="Edad", ylab="Frecuencia")

# Regresión univariada
modelo_simple <- lm(Edad ~ severidad, data = data)
summary(modelo_simple)

# Regresión multivariada
modelo_multiple <- lm(Edad ~ severidad + Temperatura + Saturación_O2, data = data)summary (modelo_multiple)

# Matriz de correlaciones
numeric_vars <- data[sapply(data, is.numeric)]
cor(numeric_vars, use = "complete.obs")

# Pares de gráficos
pairs(numeric_vars)

# Diagnóstico del modelo
par(mfrow=c(2,2))
plot(modelo_multiple)
