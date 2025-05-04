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
  
  l_prod_ep = log(prod_ep)
  l_import_ep = log(import_ep)
  l_cons_ep = log(cons_ep)
  
  l_prod_ep_diff1 <- diff(l_prod_ep)
  l_import_ep_diff1 <- diff(l_import_ep)
  l_cons_ep_diff1 <- diff(l_cons_ep)

  
  
# 1 - Tests : Production d'énergie primaire
  # Série en niveau
    # Test de Cox-Stuart
      cs.test(l_prod_ep) # Présence d'une tendance (p-value < 0,05)
      # Tests seront effectués avec des modèles en tendance
    
    # Test ADF
      # Sélection du lag optimal
        # Basé sur les critères AIC, MAIC, BIC, HQC
          T <- length(l_prod_ep)
          pmax <- as.integer(12*(T/100)^(0.25))
          adf <- CADFtest(l_prod_ep,criterion="BIC",type="trend",max.lag.y=pmax)
          summary(adf) # Convergence vers un lag 4
      
      adftest <- ur.df(prod_ep, type = c("trend"), lags = 4)
      summary(adftest) # Présence d'au moins une racine unitaire (-1.01 > -3.45)
      
    # Test de Pillips-Perron
      pptest <- ur.pp(l_prod_ep, type="Z-tau", model="trend", lags="short")
      summary(pptest) # Présence d'au moins une racine unitaire (-2.809 > -3.469623)
      
    # Test KPSS
      testKPSStau <- ur.kpss(l_prod_ep, type="tau")
      summary(testKPSStau) # Série n'est pas stationnaire (0.3058 > 0.146)
    
    # Tests convergent vers la présence d'au moins une racine unitaire
  
  # Séries en différences premières
    # Test de Cox-Stuart
      cs.test(l_prod_ep_diff1) # Absence de tendance (p-value > 0,05)
      
      summary(l_prod_ep_diff1) # Moyenne centrée en 0
        # Tests seront effectués avec des modèles sans tendance (ni constante pour l'ADF)
      
    # Test ADF
      # Sélection du lag optimal
        # Basé sur les critères AIC, MAIC, BIC, HQC
          T <- length(l_prod_ep_diff1)
          pmax <- as.integer(12*(T/100)^(0.25))
          adf <- CADFtest(l_prod_ep_diff1,criterion="BIC",type="none",max.lag.y=pmax)
          summary(adf) # Convergence vers un lag 3
        
      adftest <- ur.df(l_prod_ep_diff1, type = c("none"), lags = 3)
      summary(adftest) # Présence d'au moins une racine unitaire (-2.6317 > -1.95)
    
    # Test de Pillips-Perron
      pptest <- ur.pp(l_prod_ep_diff1, type="Z-tau", model="constant", lags="short")
      summary(pptest) # Absence de racine unitaire (-8.206 > -2.900627)

    # Test KPSS
      testKPSSmu <- ur.kpss(l_prod_ep_diff1, type="mu")
      summary(testKPSSmu) # Série stationnaire (0.3296 < 0.463)

    # Tests convergent vers la stationnarité 
      # Série est I(1)  

        
  
# Tests - Imoportations d'énergie primaire
  # Série en niveau
    # Test de Cox-Stuart
      cs.test(l_import_ep) # Présence d'une tendance (p-value < 0,05)
        # Tests seront effectués avec des modèles en tendance
  
    # Test ADF
      # Sélection du lag optimal
        # Basé sur les critères AIC, MAIC, BIC, HQC
          T <- length(l_import_ep)
          pmax <- as.integer(12*(T/100)^(0.25))
          adf <- CADFtest(l_import_ep,criterion="BIC",type="trend",max.lag.y=pmax)
          summary(adf) # Convergence vers un lag 1
    
      adftest <- ur.df(l_import_ep, type = c("trend"), lags = 1)
      summary(adftest) # Présence d'au moins une racine unitaire (-0.8624 > -3.45)

    # Test de Pillips-Perron
      pptest <- ur.pp(l_import_ep, type="Z-tau", model="trend", lags="short")
      summary(pptest) # Présence d'au moins une racine unitaire (-1.3724 > -3.469623)

    # Test KPSS
      testKPSStau <- ur.kpss(l_import_ep, type="tau")
      summary(testKPSStau) # Série n'est pas stationnaire (0.4055 > 0.146)

    # Tests convergent vers la présence d'au moins une racine unitaire
      
  # Séries en différences premières
      # Test de Cox-Stuart
        cs.test(l_import_ep_diff1) # Absence de tendance (p-value > 0,05)
      
        summary(l_import_ep_diff1) # Moyenne autour de 0
        # Tests seront effectués avec des modèles sans tendance (ni constante pour l'ADF)

      # Test ADF
        # Sélection du lag optimal
          # Basé sur les critères AIC, MAIC, BIC, HQC
            T <- length(l_import_ep_diff1)
            pmax <- as.integer(12*(T/100)^(0.25))
            adf <- CADFtest(l_import_ep_diff1,criterion="HQC",type="none",max.lag.y=pmax)
            summary(adf) # Convergence vers un lag 0
      
      adftest <- ur.df(l_import_ep_diff1, type = c("none"), lags = 0)
      summary(adftest) # Absence racine unitaire (-5.5917 < -1.95)

      # Test de Pillips-Perron
        pptest <- ur.pp(l_import_ep_diff1, type="Z-tau", model="constant", lags="short")
        summary(pptest) # Absence de racine unitaire (-6.11 > -2.900627)

      # Test KPSS
        testKPSSmu <- ur.kpss(l_import_ep_diff1, type="mu")
        summary(testKPSSmu) # Série stationnaire (0.7699 < 0.463)

      # Tests convergent vers la stationnarité
        # Série est I(1)

        
        
# Tests - Consommation d'énergie primaire
  # Série en niveau
    # Test de Cox-Stuart
      cs.test(l_cons_ep) # Présence d'une tendance (p-value < 0,05)
        # Tests seront effectués avec des modèles en tendance
        
    # Test ADF
      # Sélection du lag optimal
        # Basé sur les critères AIC, MAIC, BIC, HQC
          T <- length(l_cons_ep)
          pmax <- as.integer(12*(T/100)^(0.25))
          adf <- CADFtest(l_cons_ep,criterion="HQC",type="trend",max.lag.y=pmax)
          summary(adf) # Absence de convergence : méthode GTOS = 8
        
        adftest <- ur.df(cons_ep, type = c("trend"), lags = 8)
        summary(adftest) # Présence d'au moins une racine unitaire (-1.5275 > -3.45)

    # Test de Pillips-Perron
      pptest <- ur.pp(l_cons_ep, type="Z-tau", model="trend", lags="short")
      summary(pptest) # Présence d'au moins une racine unitaire (-1.5437 > -3.469623)
        
    # Test KPSS
      testKPSStau <- ur.kpss(l_cons_ep, type="tau")
      summary(testKPSStau) # Série n'est pas stationnaire (0.4427 > 0.146)
        
      # Tests convergent vers la présence d'au moins une racine unitaire
        
  # Séries en différences premières
    # Test de Cox-Stuart
      cs.test(l_cons_ep_diff1) # Présence de tendance (p-value < 0,05)
        # Tests seront effectués avec des modèles en tendance

    # Test ADF
      # Sélection du lag optimal
        # Basé sur les critères AIC, MAIC, BIC, HQC
          T <- length(l_cons_ep_diff1)
          pmax <- as.integer(12*(T/100)^(0.25))
          adf <- CADFtest(l_cons_ep_diff1,criterion="AIC",type="trend",max.lag.y=pmax)
          summary(adf) # Convergence vers un lag 2
        
        adftest <- ur.df(l_cons_ep_diff1, type = c("trend"), lags = 2)
        summary(adftest) # Absence racine unitaire (-4.9649 < -1.95)

    # Test de Pillips-Perron
      pptest <- ur.pp(l_cons_ep_diff1, type="Z-tau", model="trend", lags="short")
      summary(pptest) # Absence de racine unitaire (-8.3605 > -3.470437)

    # Test KPSS
      testKPSStau <- ur.kpss(l_cons_ep_diff1, type="tau")
      summary(testKPSStau) # Série non-stationnaire (0.0594 > 0.146)

    # Tests convergent vers l'hypothèse de stationnarité
      # Série est I(1)
            
# Les 3 séries en log sont donc I(1)



# Tests de co-intégration (pour chosir entre VAR ou VECM)
  # Calcul du lag optimal pour les tests de Johansen
    # Estimation d'un VAR sur les séries en log
      ep <- cbind(l_prod_ep, l_import_ep, l_cons_ep)
      # Présence de changements structurels avec une inversion de tendance pour les importations et une stabilisation pour la consommation malgré des estimations de croissance de 0,2% par an jusqu'à 2050 selon l'EIA (Annual Energy Outlook 2023) et une tendance globale forte, ainsi qu'une croissance constante marquée par des accélérations pour la production, invitent à considérer un modèle en constante pour ne pas forcer à tort une tendance partagée dans la relation de co-intégration
      varselect <- VARselect(ep, lag.max = pmax, type = "const", season = NULL, exogen = NULL)
      print(varselect$selection)
        # Convergence des critères d'information pour un lag = 2
          
  
  # Tests de Johansen
      trace <- ca.jo(ep, ecdet = c("const"), type = "trace", K = 2)
      summary(trace)
      valpr <- ca.jo(ep, ecdet = c("const"), type = "eigen", K = 2)
      summary(valpr)
        # Convergence des 2 tests vers la présence de 1 relation de co-intégration
          # On choisit donc un modèle VECM avec 1 relation de co-intégration
            # Relation de co-intégration : prod_ep.l2 = -1.941415*l_import_ep.l2 + 5.081609*l_cons_ep.l2 - 12.275468*constant          
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
    prod_to_prod<-irf(modele,impulse="l_prod_ep",response="l_prod_ep",n.head=24,boot=TRUE)
    prod_to_import<-irf(modele,impulse="l_import_ep",response="l_prod_ep",n.head=24,boot=TRUE)
    prod_to_cons<-irf(modele,impulse="l_cons_ep",response="l_prod_ep",n.head=24,boot=TRUE)
    
    import_to_prod<-irf(modele,impulse="l_prod_ep",response="l_import_ep",n.head=24,boot=TRUE)
    import_to_import<-irf(modele,impulse="l_import_ep",response="l_import_ep",n.head=24,boot=TRUE)
    import_to_cons<-irf(modele,impulse="l_cons_ep",response="l_import_ep",n.head=24,boot=TRUE)
    
    cons_to_prod<-irf(modele,impulse="l_prod_ep",response="l_cons_ep",n.head=24,boot=TRUE)
    cons_to_import<-irf(modele,impulse="l_import_ep",response="l_cons_ep",n.head=24,boot=TRUE)
    cons_to_cons<-irf(modele,impulse="l_cons_ep",response="l_cons_ep",n.head=24,boot=TRUE)
    
    # Visualisation graphique
      # Lister les objets IRF
        irf_list <- list(
          prod_to_prod, prod_to_import, prod_to_cons,
          import_to_prod, import_to_import, import_to_cons,
          cons_to_prod, cons_to_import, cons_to_cons
        )
      
      # Titres des graphiques
        titles <- c(
          "Response of l_prod_ep to l_prod_ep", 
          "Response of l_prod_ep to l_import_ep", 
          "Response of l_prod_ep to l_cons_ep",
          "Response of l_import_ep to l_prod_ep", 
          "Response of l_import_ep to l_import_ep", 
          "Response of l_import_ep to l_cons_ep",
          "Response of l_cons_ep to l_prod_ep", 
          "Response of l_cons_ep to l_import_ep", 
          "Response of l_cons_ep to l_cons_ep"
        )
      
      # Initialiser la mise en page 3x3
        layout(matrix(1:9, 3, 3, byrow = TRUE))
      
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
      
    # Décomposition de la variance
    v_d<-fevd(modele, n.head=10)
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
      plot_fevd_custom(v_d, "l_prod_ep", "center")
      plot_fevd_custom(v_d, "l_import_ep", "center")
      plot_fevd_custom(v_d, "l_cons_ep", "bottomleft")
    
    # Prévisions
    forecast<-predict(modele,n.ahead=12,ci=0,95)
    fanchart(forecast,names="l_prod_ep", main = "Prévision de l_prod_ep sur 12 ans")
    fanchart(forecast,names="l_import_ep", main = "Prévision de l_import_ep sur 12 ans")
    fanchart(forecast,names="l_cons_ep", main = "Prévision de l_cons_ep sur 12 ans")
    