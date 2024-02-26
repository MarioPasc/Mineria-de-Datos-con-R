# Mario Pascual Gonzalez

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

calculaPValor <- function(datos, var1, var2) {
  # Calculamos las variables categóricas del dataset
  varCat <- variables.categoricas(datos)
  
  # Comprobamos los tipos de variables que son var1 y var2
  var1.cat <- var1 %in% varCat
  var2.cat <- var2 %in% varCat
  vector.vars <- c(var1, var2)
  
  # Calculamos la tabla de contingencia utilzando xtabs y construyendo una 
  # fórmula dinámica con los argumentos de la función
  formula.text <- paste("~", var1, "+", var2, sep = " ")
  tabla.contingencia <- xtabs(formula = as.formula(formula.text), 
                              data = datos)
  
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
    
    if (
      shapiro.test(datos[[var1]])$p.value > 0.05 &&
      shapiro.test(datos[[var2]])$p.value > 0.05
    ) {
      print("PEARSON")
      return(cor.test(datos[[var1]], datos[[var2]], method = "pearson")$p.value)
    } else {
      print("SPEARMAN")
      return(cor.test(datos[[var1]], datos[[var2]], method = "spearman")$p.value)
    }
    
  } else { # Una de las dos es numérica
    
    # Almacenamos la que es numérica y la que no
    if (var1.cat) {
      numerica <- var2
      categorica <- var1
    } else {
      numerica <- var1
      categorica <- var2
    }
    
    formula.text <- paste(var1, "~", var2, sep = " ")
    
    if (length(unique(datos[[categorica]])) > 2) {
      
      if (shapiro.test(datos[[numerica]])$p.value > 0.05) {
        # Más de 2 tipos, distribución normal -> AOV
        aov.test <- summary(aov(formula = as.formula(formula.text), 
                                data = datos))
        print("AOV")
        return(aov.test[[1]][["Pr(>F)"]][1])
        
      } else {
        # Más de dos datos, distribución diferente -> Kruskal
        kruskal.test <- kruskal.test(formula = as.formula(formula.text), 
                                    data = datos)
        print("KURSKAL")
        return(kruskal.test$p.value)
      }
    } else {
      
      if (shapiro.test(datos[[numerica]])$p.value > 0.05) {
        # 2 tipos, distribucion normal -> t test
        ttest.test <- t.test(formula = as.formula(formula.text), 
                             data = datos)
        print("TTEST")
        return(ttest.test$p.value)
      } else {
        # 2 tipos, distribucion diferente -> Wilcoxon test
        wilcoxon.test <- wilcox.test(formula = as.formula(formula.text),
                                     data = datos)
        print("WILCOXON")
        return(wilcoxon.test$p.value)
      }
    }
  }
}

aplicaCalculaPValorATodosLosPares <- function(datos) {
  # Obtén todos los nombres de las variables del dataset
  nombres_variables <- names(datos)
  
  # Inicializa un vector para almacenar los resultados
  resultados_p_valores <- numeric(0) # Vector vacío
  variable_1 <- character(0) 
  variable_2 <- character(0)
  
  # Itera sobre cada combinación única de pares de variables
  for (i in 1:(length(nombres_variables) - 1)) {
    for (j in (i + 1):length(nombres_variables)) {
      # Nombres de las variables actuales
      var1 <- nombres_variables[i]
      var2 <- nombres_variables[j]
      
      # Llama a calculaPValor para el par de variables actual
      resultado <- calculaPValor(datos, var1, var2)
      
      # Almacena el resultado p en el vector
      resultados_p_valores <- c(resultados_p_valores, resultado)
      
      # Guarda el nombre del par de variables
      variable_1 <- c(variable_1, paste(var1))
      variable_2 <- c(variable_2, paste(var2))
      
    }
  }
  
  resultados_df <- data.frame(VariableX = variable_1, VariableY = variable_2, 
                              Valor = resultados_p_valores)
  return(resultados_df)
}





