
### Ejercicio 3 ############

# X: Resistencia a la rotura del sedal en kilogramos
#### PLANTEAMIENTO DE HIPÓTESIS
# H0: μ = 8 vs Ha: μ ≠ 8

#### Datos
n <- 50                  # tamaño de la muestra
X_bar <- 7.8            # media muestral
desviacion <- 0.5       # desviación estándar poblacional
significancia <- 0.01   # nivel de significancia
miu_0 <- 8              # valor hipotético de la media

### CÁLCULO DEL ESTADÍSTICO DE PRUEBA
Z_c <- (X_bar-miu_0)/(desviacion/sqrt(n)); Z_c

###Punto crítico (valor tabulado)
zz <- qnorm(1-significancia/2)  # Para dos colas dividimos alfa entre 2
cat("Valor crítico: ±", zz, "\n")

### DECISIÓN USANDO CRITERIO DEL PUNTO CRÍTICO
if(abs(Z_c) > zz) {
  cat("Como |", Z_c, "| >", zz, "Rechazar Ho\n")
} else {
  cat("Como |", Z_c, "| ≤", zz, "No rechazar Ho\n")
}

### DECISIÓN USANDO CRITERIO DEL valor-p
#Cálculo del valor-p
valorp <- 2 * pnorm(-abs(Z_c))  # Multiplicamos por 2 para prueba de dos colas
cat("Valor p:", valorp, "\n")

if(valorp < significancia) cat("Rechazar Ho\n") else cat("No rechazar Ho\n")
