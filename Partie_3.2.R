
library(MASS)
library(tibble)

# Fonction pour générer les données
simulate_data <- function(n = 100, p = 10, rho = 0, sigma2 = 1) {
  # Créer la matrice de covariance (corrélation AR(1))
  Sigma_X <- outer(1:p, 1:p, function(i, j) rho^abs(i - j))
  X <- mvrnorm(n = n, mu = rep(0, p), Sigma = Sigma_X)
  
  # Coefficients vrais : seules les 5 première variables sont actives
  beta <- c(rep(1, 5), rep(0, p - 5))
  
  # Génération de y
  y <- X %*% beta + rnorm(n, 0, sqrt(sigma2)) 
  return(list(X = X, y = y, beta = beta, sigma2 = sigma2))
}

# Fonction qui exécute les méthodes et renvoie un tibble résultat
run_simulation <- function(n = 100, p = 10, rho = 0, sigma2 = 1, simu = 1) {
  
  # Génération des données
  data <- simulate_data(n, p, rho, sigma2)
  X <- data$X
  y <- data$y
  beta_true <- data$beta
  
  # ⚙️ Ici tu appliques plusieurs méthodes (exemples fictifs)
  # Chaque méthode renvoie des métriques
  methods <- c("bestsubset", "stepwiseAIC", "stepwiseBIC")
  
  results <- lapply(methods, function(met) {
    # ---- REMPLACEZ PAR LES VRAIS FONCTIONS ----
    mse <- runif(1, 0, 2)
    err <- rnorm(1, 0, 0.2)
    acc <- runif(1, 0.8, 1)
    sen <- runif(1, 0.7, 0.95)
    spe <- runif(1, 0.7, 0.95)
    n.p <- sample(1:p, 1)
    # ---------------------------------------------------------------------------
    
    tibble(
      method = met,
      mse = mse,
      err = err,
      acc = acc,
      sen = sen,
      spe = spe,
      n.p = n.p,
      sigma2 = sigma2,
      simu = simu
    )
  })
  
  # Combiner les résultats dans un tibble final
  res <- do.call(rbind, results)
  return(res)
}

# Exemple d’utilisation
res <- run_simulation(n = 100, p = 10, rho = 0, sigma2 = 0.75, simu = 1)
res

