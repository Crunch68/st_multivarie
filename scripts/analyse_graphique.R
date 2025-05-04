# Librairies utiles
  library(readxl)


# Importation des séries
  series_energie_primaire <- read_excel("data/raw/series_energie_primaire.xlsx", col_names = TRUE, col_types = NULL)

  View(series_energie_primaire)


  
# Création des séries temporelles
  # Production totale d'énergie primaire
    prod_ep <- ts(series_energie_primaire[,2], start = c(1949), end = c(2024))
  
  # Importations d'énergie primaire
    import_ep <- ts(series_energie_primaire[,3], start =c(1949), end = c(2024))
  
  # Consommation totale d'énergie primaire
    cons_ep <- ts(series_energie_primaire[,4], start = c(1949), end = c(2024))
  

    
# Visualisation graphique
  # Graphiques séparés
    layout(matrix(1:3, 1, 3))
    plot(prod_ep, main = "Production totale d'énergie primaire aux USA", xlab = "Années", ylab = "Quadrillons de Btu")
    plot(import_ep, main = "Importations d'énergie primaire aux USA", xlab = "Années", ylab = "Quadrillons de Btu")
    plot(cons_ep, main = "Consommation totale d'énergie primaire aux USA", xlab = "Années", ylab = "Quadrillons de Btu")
    
  # Graphique
    plot(prod_ep, col = "blue", lwd = 2, ylim = range(c(prod_ep, import_ep, cons_ep)),
        xlab = "Années", ylab = "Quadrillons de Btu", main = "Séries - énergie primaire aux USA (1949-2024)")
    
    # Ajout des autres séries
      lines(import_ep, col = "red", lwd = 2)
      lines(cons_ep, col = "green", lwd = 2)
    
    # Légende
      legend("topleft",
             legend = c("prod_ep", "import_ep", "cons_ep"),
             col = c("blue", "red", "green"),
             lty = 1,
             lwd = 2)

  # Séries présentent des tendances avec des variations importantes
    # Transformation en log permettra de lisser ses variations
      
# Transformation des séries en log
  # Nouvelles séries
    l_prod_ep = log(prod_ep)
    l_import_ep = log(import_ep)
    l_cons_ep = log(cons_ep)
    
  # Nouveaux graphiques
    # Graphiques séparés
      layout(matrix(1:3, 1, 3))
      plot(l_prod_ep, main = "Logarithme de la production totale d'énergie primaire aux USA", xlab = "Années", ylab = "Quadrillons de Btu")
      plot(l_import_ep, main = "Logarithme des importations d'énergie primaire aux USA", xlab = "Années", ylab = "Quadrillons de Btu")
      plot(l_cons_ep, main = "Logarithme de la Consommation totale d'énergie primaire aux USA", xlab = "Années", ylab = "Quadrillons de Btu")
    
    # Graphique
      plot(l_prod_ep, col = "blue", lwd = 2, ylim = range(c(l_prod_ep, l_import_ep, l_cons_ep)),
           xlab = "Années", ylab = "Quadrillons de Btu", main = "Log des séries - énergie primaire aux USA (1949-2024)")
      
      # Ajout des autres séries
        lines(l_import_ep, col = "red", lwd = 2)
        lines(l_cons_ep, col = "green", lwd = 2)
        
      # Légende
        legend("bottomright",
               legend = c("l_prod_ep", "l_import_ep", "l_cons_ep"),
               col = c("blue", "red", "green"),
               lty = 1,
               lwd = 2)
    

      
# Corrélogrammes sur le log des séries en niveau
  layout(matrix(1:6,3,2))
  acf(c(l_prod_ep),lag.max= 70, plot = TRUE, col="cyan")
  acf(c(l_import_ep),lag.max= 70,plot = TRUE, col="cyan")
  acf(c(l_cons_ep),lag.max= 70, plot = TRUE, col="cyan")
  pacf(c(l_prod_ep),lag.max= 70,plot = TRUE, col="red", main="")
  pacf(c(l_import_ep),lag.max= 70, plot = TRUE, col="red", main="")
  pacf(c(l_cons_ep),lag.max= 70,plot = TRUE, col="red", main="")
    # Séries non-stationnaires en niveau
      # Présentent des retournements (parfois tardifs), ce qui laisse penser à d'éventuels changements structurels ou dynamiques e long terme

  
  
# Corrélogrammes sur les séries (en log) en différences premières
  # Transformation des séries en différences premières
    l_prod_ep_diff1 <- diff(l_prod_ep)
    l_import_ep_diff1 <- diff(l_import_ep)
    l_cons_ep_diff1 <- diff(l_cons_ep)
  
  # Visualisation
    layout(matrix(1:6,3,2))
    acf(c(l_prod_ep_diff1),lag.max= 70, plot = TRUE, col="cyan")
    acf(c(l_import_ep_diff1),lag.max= 70,plot = TRUE, col="cyan")
    acf(c(l_cons_ep_diff1),lag.max= 70, plot = TRUE, col="cyan")
    pacf(c(l_prod_ep_diff1),lag.max= 70,plot = TRUE, col="red", main="")
    pacf(c(l_import_ep_diff1),lag.max= 70, plot = TRUE, col="red", main="")
    pacf(c(l_cons_ep_diff1),lag.max= 70,plot = TRUE, col="red", main="")
      # Log des séries stationnaires en différences premières
    