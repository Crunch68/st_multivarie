# Librairies utiles
  library(urca)
  library(trend)
  library(CADFtest)
  library(vars)
  library(forecast)

  
    
# Séries utiles
  series_energie_primaire <- read_excel("data/raw/series_energie_primaire.xlsx", col_names = TRUE, col_types = NULL)
  prod_ep <- ts(series_energie_primaire[,2], start = c(1949), end = c(2024))
  import_ep <- ts(series_energie_primaire[,3], start =c(1949), end = c(2024))
  cons_ep <- ts(series_energie_primaire[,4], start = c(1949), end = c(2024))
  prod_ep_diff1 <- diff(prod_ep)
  import_ep_diff1 <- diff(import_ep)
  cons_ep_diff1 <- diff(cons_ep)


  
# Tests - Production d'énergie primaire
  # Série en niveau
    # Test de Cox-Stuart
      cs.test(prod_ep) # Présence d'une tendance (p-value < 0,05)
      # Tests seront effectués avec des modèles en tendance
    
    # Test ADF
      # Sélection du lag optimal
        # Basé sur les critères AIC, MAIC, BIC, HQC
          T <- length(prod_ep)
          pmax <- as.integer(12*(T/100)^(0.25))
          adf <- CADFtest(prod_ep,criterion="HQC",type="trend",max.lag.y=pmax)
          summary(adf) # Convergence vers un lag 4
      
      adftest <- ur.df(prod_ep, type = c("trend"), lags = 4)
      summary(adftest) # Présence d'au moins une racine unitaire (-1.01 > -3.45)
      
    # Test de Pillips-Perron
      pptest <- ur.pp(prod_ep, type="Z-tau", model="trend", lags="short")
      summary(pptest) # Présence d'au moins une racine unitaire (-1.1062 > -3.469623)
      
    # Test KPSS
      testKPSStau <- ur.kpss(prod_ep, type="tau")
      summary(testKPSStau) # Série n'est pas stationnaire (0.2066 > 0.146)
    
    # Tests convergent vers la présence d'au moins une racine unitaire
  
  # Séries en différences premières
    # Test de Cox-Stuart
      cs.test(prod_ep_diff1) # Absence de tendance (p-value > 0,05)
      
      summary(prod_ep_diff1) # Moyenne autour de 1
        # Tests seront effectués avec des modèles en constante
      
    # Test ADF
      # Sélection du lag optimal
        # Basé sur les critères AIC, MAIC, BIC, HQC
          T <- length(prod_ep_diff1)
          pmax <- as.integer(12*(T/100)^(0.25))
          adf <- CADFtest(prod_ep_diff1,criterion="HQC",type="drift",max.lag.y=pmax)
          summary(adf) # Convergence vers un lag 3
        
      adftest <- ur.df(prod_ep_diff1, type = c("drift"), lags = 3)
      summary(adftest) # Présence d'au moins une racine unitaire (-2.8843 > -2.89)
    
    # Test de Pillips-Perron
      pptest <- ur.pp(prod_ep_diff1, type="Z-tau", model="constant", lags="short")
      summary(pptest) # Absence de racine unitaire (-7.8861 > -2.900627)
      
    # Test KPSS
      testKPSSmu <- ur.kpss(prod_ep_diff1, type="mu")
      summary(testKPSSmu) # Série stationnaire (0.2389 < 0.463)
    
    # Tests convergent vers la stationnarité (test ADF du fait des arrondis peut-être considéré comme à la limite)
      # Série est I(1)  

        
  
# Tests - Imoportations d'énergie primaire
  # Série en niveau
    # Test de Cox-Stuart
      cs.test(import_ep) # Présence d'une tendance (p-value < 0,05)
        # Tests seront effectués avec des modèles en tendance
  
    # Test ADF
      # Sélection du lag optimal
        # Basé sur les critères AIC, MAIC, BIC, HQC
          T <- length(import_ep)
          pmax <- as.integer(12*(T/100)^(0.25))
          adf <- CADFtest(import_ep,criterion="BIC",type="trend",max.lag.y=pmax)
          summary(adf) # Convergence vers un lag 1
    
      adftest <- ur.df(import_ep, type = c("trend"), lags = 1)
      summary(adftest) # Présence d'au moins une racine unitaire (-1.3371 > -3.45)

    # Test de Pillips-Perron
      pptest <- ur.pp(import_ep, type="Z-tau", model="trend", lags="short")
      summary(pptest) # Présence d'au moins une racine unitaire (-0.9087 > -3.469623)

    # Test KPSS
      testKPSStau <- ur.kpss(import_ep, type="tau")
      summary(testKPSStau) # Série n'est pas stationnaire (0.2066 > 0.146)

    # Tests convergent vers la présence d'au moins une racine unitaire
      
  # Séries en différences premières
      # Test de Cox-Stuart
        cs.test(import_ep_diff1) # Absence de tendance (p-value > 0,05)
      
        summary(import_ep_diff1) # Moyenne autour de 0
        # Tests seront effectués avec des modèles sans tendance (ni constante pour l'ADF)

      # Test ADF
        # Sélection du lag optimal
          # Basé sur les critères AIC, MAIC, BIC, HQC
            T <- length(import_ep_diff1)
            pmax <- as.integer(12*(T/100)^(0.25))
            adf <- CADFtest(import_ep_diff1,criterion="BIC",type="none",max.lag.y=pmax)
            summary(adf) # Convergence vers un lag 0
      
      adftest <- ur.df(import_ep_diff1, type = c("none"), lags = 0)
      summary(adftest) # Absence racine unitaire (-5.2417 < -1.95)

      # Test de Pillips-Perron
        pptest <- ur.pp(import_ep_diff1, type="Z-tau", model="constant", lags="short")
        summary(pptest) # Absence de racine unitaire (-5.3221 > -2.900627)

      # Test KPSS
        testKPSSmu <- ur.kpss(import_ep_diff1, type="mu")
        summary(testKPSSmu) # Série stationnaire (0.2301 < 0.463)

      # Tests convergent vers la stationnarité
        # Série est I(1)

        
        
# Tests - Consommation d'énergie primaire
  # Série en niveau
    # Test de Cox-Stuart
      cs.test(cons_ep) # Présence d'une tendance (p-value < 0,05)
        # Tests seront effectués avec des modèles en tendance
        
    # Test ADF
      # Sélection du lag optimal
        # Basé sur les critères AIC, MAIC, BIC, HQC
          T <- length(cons_ep)
          pmax <- as.integer(12*(T/100)^(0.25))
          adf <- CADFtest(cons_ep,criterion="HQC",type="trend",max.lag.y=pmax)
          summary(adf) # Convergence vers un lag 2 (seul significatif)
        
        adftest <- ur.df(cons_ep, type = c("trend"), lags = 2)
        summary(adftest) # Présence d'au moins une racine unitaire (-0.343 > -3.45)

    # Test de Pillips-Perron
      pptest <- ur.pp(cons_ep, type="Z-tau", model="trend", lags="short")
      summary(pptest) # Présence d'au moins une racine unitaire (-0.7597 > -3.469623)
        
    # Test KPSS
      testKPSStau <- ur.kpss(cons_ep, type="tau")
      summary(testKPSStau) # Série n'est pas stationnaire (0.4016 > 0.146)
        
      # Tests convergent vers la présence d'au moins une racine unitaire
        
  # Séries en différences premières
    # Test de Cox-Stuart
      cs.test(cons_ep_diff1) # Absence de tendance (p-value > 0,05)
        
      summary(cons_ep_diff1) # Moyenne autour de 0
        # Tests seront effectués avec des modèles sans tendance (ni constante pour l'ADF)

    # Test ADF
      # Sélection du lag optimal
        # Basé sur les critères AIC, MAIC, BIC, HQC
          T <- length(cons_ep_diff1)
          pmax <- as.integer(12*(T/100)^(0.25))
          adf <- CADFtest(cons_ep_diff1,criterion="BIC",type="none",max.lag.y=pmax)
          summary(adf) # Convergence vers un lag 2
        
        adftest <- ur.df(cons_ep_diff1, type = c("none"), lags = 2)
        summary(adftest) # Absence racine unitaire (-3.3005 < -1.95)

    # Test de Pillips-Perron
      pptest <- ur.pp(cons_ep_diff1, type="Z-tau", model="constant", lags="short")
      summary(pptest) # Absence de racine unitaire (-7.9971 > -2.900627)

    # Test KPSS
      testKPSSmu <- ur.kpss((cons_ep_diff1), type="mu")
      summary(testKPSSmu) # Série non-stationnaire (0.5638 > 0.463)
        # Test sévère et sensible à la variance de cette série
          # Test KPSS - série initiale en log
            cs.test(diff(log(cons_ep))) # Présence d'une tendance (p-value < 5%)
            testKPSStau <- ur.kpss(diff(log(cons_ep)), type="tau")
            summary(testKPSSmu) # Série stationnaire (0.0594 < 0.146)
              # Série est stationnaire après transformation en log et réduction de la variance

    # Tests, malgré une apparente contradiction, peuvent être considéres comme convergents vers la stationnarité (quoique faible)
      # Série est I(1)
            
# Les 3 séries sont donc I(1)



# Tests de co-intégration (pour chosir entre VAR ou VECM)
  # Calcul du lag optimal pour les tests de Johansen
    # Estimation d'un VAR sur les séries en niveau
      ep <- cbind(prod_ep, import_ep, cons_ep)
      # Présence de changements structurels avec une inversion de tendance pour les importations et une stabilisation pour la consommation tempérée par des estimations de croissance de 0,2% par an jusqu'à 2050 selon l'EIA (Annual Energy Outlook 2023) et une tendance globale forte, ainsi qu'une croissance constante marquée par des accélérations pour la production, invitent à considérer un modèle avec tendance et constante
      varselect <- VARselect(ep, lag.max = pmax, type = "both", season = NULL, exogen = NULL)
      print(varselect$selection)
        # Absence de convergence nette des critères d'information
          # Cependant Galbraith & Zine-Walde (2004) suggèrent que le SIC/BIC serait le meilleur
            # Mais comme les séries présentent tout de même un certain nombre d'observations et qu'on l'a vu avec les corrélogrammes il existe des dynamiques de long terme, nous privilégieront le HQC (un peu moins parcimonieux)
              # Le lag retenu est 3
  
  # Tests de Johansen
      trace <- ca.jo(ep, ecdet = c("trend"), type = "trace", K = 3, spec = c("longrun"))
      summary(trace)
      valpr <- ca.jo(ep, ecdet = c("trend"), type = "eigen", K=3, spec = c("longrun"))
      summary(valpr)
        # Convergence des 2 tests vers la présence de 1 relation de co-intégration
          # On choisit donc un modèle VECM avec 1 relation de co-intégration
            # Relation de co-intégration : prod_ep.l3 = −1.631 × import_ep.l3 + 1.133 × cons_ep.l3 + 0.106 × trend.l3
              # Chaque relation est conforme à la théorie


      
# Estimation du modèle VECM
  # Modèle VECM
    vecm <- cajorls(trace, r = 1)
    print(vecm$beta) # Relation de co-intégration
    summary(vecm$rlm) # Relations de court-terme
    
  # Transformation du VECM en VAR équivalent (changement de format simplement pour pouvoir utliser certaines fonctions)  
    modele <-vec2var(trace, r = 1)
    print(modele)
    
  # Validation du modèle
      serial.test(modele,lags.pt=36,type="PT.adjusted") # Résidus décorrélés (p-value > 5%)
      arch.test(modele,lags.multi=15) # Résidus homoscédastiques (p-value > 5%)
      normality.test(modele) # Résidus normalement distribués (p-value du test de Jarque-Bera > 5%)
        # Test convergent vers une validation du modèle

      
      
# Analyse et prévision des séries
  # Fonctions de réponse
    prod_to_prod<-irf(modele,impulse="prod_ep",response="prod_ep",n.head=24,boot=TRUE)
    prod_to_import<-irf(modele,impulse="import_ep",response="prod_ep",n.head=24,boot=TRUE)
    prod_to_cons<-irf(modele,impulse="cons_ep",response="prod_ep",n.head=24,boot=TRUE)
    
    import_to_prod<-irf(modele,impulse="prod_ep",response="import_ep",n.head=24,boot=TRUE)
    import_to_import<-irf(modele,impulse="import_ep",response="import_ep",n.head=24,boot=TRUE)
    import_to_cons<-irf(modele,impulse="cons_ep",response="import_ep",n.head=24,boot=TRUE)
    
    cons_to_prod<-irf(modele,impulse="prod_ep",response="cons_ep",n.head=24,boot=TRUE)
    cons_to_import<-irf(modele,impulse="import_ep",response="cons_ep",n.head=24,boot=TRUE)
    cons_to_cons<-irf(modele,impulse="cons_ep",response="cons_ep",n.head=24,boot=TRUE)
    
    # Visualisation graphique
      # Lister les objets IRF
        irf_list <- list(
          prod_to_prod, prod_to_import, prod_to_cons,
          import_to_prod, import_to_import, import_to_cons,
          cons_to_prod, cons_to_import, cons_to_cons
        )
      
      # Titres des graphiques
        titles <- c(
          "Response of prod_ep to prod_ep", 
          "Response of prod_ep to import_ep", 
          "Response of prod_ep to cons_ep",
          "Response of import_ep to prod_ep", 
          "Response of import_ep to import_ep", 
          "Response of import_ep to cons_ep",
          "Response of cons_ep to prod_ep", 
          "Response of cons_ep to import_ep", 
          "Response of cons_ep to cons_ep"
        )
      
      # Initialiser la mise en page 3x3
        layout(matrix(1:9, 3, 3, byrow = TRUE))
        
      # Ajuster les marges
        par(mar = c(3, 3, 3, 2), oma = c(2, 2, 4, 2))
      
      # Boucle sur chaque IRF
        for (i in 1:9) {
          
          irf_obj <- irf_list[[i]]
          
        # Extraire la réponse impulsionnelle
          response <- as.numeric(irf_obj$irf[[1]])
          lower <- as.numeric(irf_obj$Lower[[1]])
          upper <- as.numeric(irf_obj$Upper[[1]])
          
        # Définir les limites y
          ylim_range <- range(c(response, lower, upper), na.rm = TRUE)
        
        # Tracer la réponse
          plot(response, type = "l", col = "blue", lwd = 2,
               main = titles[i],
               ylab = "Response",
               xlab = "Horizon",
               ylim = ylim_range)
        
        # Ajouter la ligne 0
          abline(h = 0, col = "gray", lty = 2)
        
        # Ajouter les IC
          lines(lower, col = "blue", lty = 3)
          lines(upper, col = "blue", lty = 3)
        }
      
      # Titre global
        mtext("Impulse Response Functions (IRF)", side = 3, outer = TRUE, line = 2, cex = 1.5)
        mtext("Horizon", side = 1, outer = TRUE, line = 2)
        mtext("Response", side = 2, outer = TRUE, line = 2)
      
    # Décomposition de la variance
    v_d<-fevd(modele, n.head=24)
    print(v_d)
      # variance de prod_ep expliquée en dernière période à 4% par la variance de import_ep et 2% par celle de cons_ep
      # variance de import_ep expliquée en dernière période à 10% par la variance de prod_ep et 2% par celle de cons_ep
      # variance de cons_ep expliquée en dernière période à 40% par la variance de import_ep et 15% par celle de prod_ep
    plot(v_d)
    
    # Autre représentation graphique
      plot_fevd_custom <- function(fevd_obj, variable, legend_position = "bottomright") {
        fevd_data <- fevd_obj[[variable]]
        
        matplot(fevd_data, type = "l", lwd = 2, col = 1:ncol(fevd_data),
                xlab = "Horizon", ylab = "Variance Explained", main = paste("FEVD -", variable))
        
        legend(legend_position, legend = colnames(fevd_data), col = 1:ncol(fevd_data), lty = 1, bty = "n")
      }
      
      layout(matrix(1:3, 1, 3))
      plot_fevd_custom(v_d, "prod_ep", "center")
      plot_fevd_custom(v_d, "import_ep", "center")
      plot_fevd_custom(v_d, "cons_ep", "bottomleft")
    
    # Prévisions
    forecast<-predict(modele,n.ahead=12,ci=0,95)
    fanchart(forecast,names="prod_ep")
    fanchart(forecast,names="import_ep")
    fanchart(forecast,names="cons_ep")
    