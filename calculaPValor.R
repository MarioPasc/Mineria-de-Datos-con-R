

# Variable categórica - categórica
# Chi-cuadrado
# Si tiene alguna instancia menor que 5 -> Fisher

# Variable categórica - numérica
# Dos categorías:
#      Distribución normal: t-test
#      Distribución diferente: wilcox test
# Más de dos categorías:
#      Distribución normal: análisis de la varianza
#      Distribución diferente: Kruskal test

# Variable numérica - numérica
# Distribución normal: correlación de Pearson
# Distribución diferente: correlación de Spearman

variables.categoricas <- function(datos) {
  return(
    colnames(datos)[sapply(datos, function(columna) {
      !is.numeric(columna)
      })]
  )
}

calculaPValor <- function(datos, objetivo, var1, var2) {
  # Calculamos las variables categóricas del dataset
  varCat <- variables.categoricas(datos)
  
  # Comprobamos los tipos de variables que son var1 y var2
  var1.cat <- var1 %in% varCat
  var2.cat <- var2 %in% varCat
  vector.vars <- c(var1, var2)
  
  # Calculamos la tabla de contingencia utilzando xtabs y construyendo una 
  # fórmula dinámica con los argumentos de la función
  formula.text <- paste("~", var1, "+", var2, sep = " ")
  tabla.contingencia <- xtabs(formula = as.formula(formula.text), data = datos)
  
  if (var1.cat && var2.cat) { # Ambas son categóricas
    # Se ha añadido un trycatch ya que hay veces que da error por el tamaño del
    # workspace. Parece que para variables categóricas con demasiados tipos 
    # únicos la carga computacional del test de fisher excede la memoria alojada
    # en el entorno de trabajo. 
    
    tryCatch({
      if (any(tabla.contingencia) < 5) { # Si hay al menos una variable que tiene <5
        
        fish.test <- fisher.test(tabla.contingencia, simulate.p.value=TRUE)
        return(fish.test$p.value)
        
      } else { # Ambas variables tienen más de 5 valores por clase
        
        chi.test <- chisq.test(tabla.contingencia, simulate.p.value=TRUE)
        return(chi.test$p.value)
        
      }
      
    }, error = function(e) {
      message("Se ha producido un error", e)
      return(NA)
    })
    
  } else if (!var1.cat && !var2.cat) { # Ambas son numéricas
    
  } else { # Una de las dos es numérica
    
    # Almacenamos la que es numérica y la que no
    if (var1.cat) {
      numerica <- var2.cat
      categorica <- var1.cat
    } else {
      numerica <- var1.cat
      categorica <- var2.cat
    }
    
    
  }
}




