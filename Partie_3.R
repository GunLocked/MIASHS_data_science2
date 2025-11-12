# ============================================================
# PARTIE 3.1 – Génération simple & fonction d’évaluation
# ============================================================

set.seed(123)  # pour la reproductibilité
library(MASS)
library(tibble)

# Fonction d'évaluation de performance
getPerformance <- function(X_test, y_test, beta, beta.star) {
  
  nzero <- which(beta != 0)
  zero  <- which(beta == 0)
  
  true.nzero <- which(beta.star != 0)
  true.zero  <- which(beta.star == 0)
  
  TP <- sum(nzero %in% true.nzero)
  TN <- sum(zero %in%  true.zero)
  FP <- sum(nzero %in% true.zero)
  FN <- sum(zero %in%  true.nzero)
  
  recall       <- TP / (TP + FN)       # sensibilité
  specificity  <- TN / (FP + TN)       # spécificité
  precision    <- TP / (TP + FP)       # précision
  
  recall[TP + FN == 0] <- NA
  specificity[TN + FP == 0] <- NA
  precision[TP + FP == 0] <- NA
  
  rmse <- sqrt(mean((beta - beta.star)^2, na.rm = TRUE))
  rerr <- sqrt(mean((y_test - X_test %*% beta)^2))
  
  res <- round(c(precision, recall, specificity, rmse, rerr), 4)
  res[is.nan(res)] <- 0
  names(res) <- c("precision", "recall", "specificity", "rmse", "prediction")
  return(res)
}


# ============================================================
# PARTIE 3.2 – Planning et exécution des simulations
# ============================================================

# Fonction pour générer les données simulées selon rho
simulate_data <- function(n = 100, p = 10, rho = 0, sigma2 = 1) {
  Sigma_X <- outer(1:p, 1:p, function(i, j) rho^abs(i - j))
  X <- mvrnorm(n = n, mu = rep(0, p), Sigma = Sigma_X)
  
  # Vrais coefficients : 5 variables actives
  beta <- c(runif(5, 1, 3), rep(0, p - 5))
  
  y <- X %*% beta + rnorm(n, 0, sqrt(sigma2))
  return(list(X = X, y = y, beta = beta, sigma2 = sigma2))
}

# Fonction principale : exécute plusieurs méthodes et renvoie un tibble
run_simulation <- function(n = 100, p = 10, rho = 0, sigma2 = 1, simu = 1) {
  
  # 1. Génération des données
  data <- simulate_data(n, p, rho, sigma2)
  X <- data$X
  y <- data$y
  beta_true <- data$beta
  
  # 2. Simulation des méthodes (exemples fictifs)
  methods <- c("bestsubset", "stepwiseAIC", "stepwiseBIC") #on va se concentrer sur une seule
  
  results <- lapply(methods, function(met) {
    # 🔧 Exemple : on simule des estimations de beta (à remplacer plus tard)
    beta_hat <- beta_true + rnorm(p, 0, 0.5)
    beta_hat[sample(1:p, 10)] <- 0  # simule une sélection
    
    # 3. Évaluer la performance avec getPerformance()
    perf <- getPerformance(X, y, beta_hat, beta_true)
    
    # 4. Construire le tibble de résultats
    tibble(
      method = met,
      mse = perf["rmse"],                # on peut l’assimiler à MSE
      err = perf["prediction"],          # erreur de prédiction
      acc = perf["precision"],           # précision = accuracy
      sen = perf["recall"],              
      spe = perf["specificity"],         
      n.p = sum(beta_hat != 0) / p,      # proportion de variables sélectionnées
      sigma2 = sigma2,
      simu = simu
    )
  })
  
  res <- do.call(rbind, results)
  return(res)
}

# Exemple d’exécution
res <- run_simulation(n = 100, p = 20, rho = 0, sigma2 = 1, simu = 1)
res

