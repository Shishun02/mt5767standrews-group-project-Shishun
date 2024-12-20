library(ggplot2)

#N_t = (N_{t-1} - c_{t-1}) + r_t * (N_{t-1} - c_{t-1}) * [1 - (N_{t-1} - c_{t-1})/K_t]

# parameter setting
alpha_0 <- 0.1  # Growth rate model
alpha_1 <- 0.05 
beta_0 <- 5     # Carrying Capacity
beta_1 <- 0.1   

# initial population size and year
N0 <- 100
years <- 25

# rainfall
set.seed(42)
R <- runif(years, min = 0, max = 1.5) 

# removal
c <- rep(10, years - 1) 

# model 1: removal after growth
N_removal_after_growth <- function(years, N0, R, alpha_0, alpha_1, beta_0, beta_1, c) {
  N <- numeric(years)
  N[1] <- N0
  
  for (t in 2:years) {
    rt <- exp(alpha_0 + alpha_1 * R[t])
    Kt <- exp(beta_0 + beta_1 * R[t])
    N[t] <- (N[t-1] + rt * N[t-1] * (1 - N[t-1] / Kt)) - c[t-1]
    N[t] <- max(N[t], 0) # keep positive
  } 
  return(N)
}

# model 2: removal before growth
N_removal_before_growth <- function(years, N0, R, alpha_0, alpha_1, beta_0, beta_1, c) {
  N <- numeric(years)
  N[1] <- N0
  
  for (t in 2:years) {
    rt <- exp(alpha_0 + alpha_1 * R[t])
    Kt <- exp(beta_0 + beta_1 * R[t])
    N[t] <- (N[t-1] - c[t-1]) + rt * (N[t-1] - c[t-1]) * (1 - (N[t-1] - c[t-1]) / Kt)
    N[t] <- max(N[t], 0) # keep positive
  }
  return(N)
}

# simulate two situations
Nt_removal_after_growth <- N_removal_after_growth(years, N0, R, alpha_0, alpha_1, beta_0, beta_1, c)
Nt_removal_before_growth <- N_removal_before_growth(years, N0, R, alpha_0, alpha_1, beta_0, beta_1, c)

# observation values
set.seed(42)  
sigma_N <- 6   
Nthat_removal_after_growth <- rnorm(years, mean = Nt_removal_after_growth, sd = sigma_N)
Nthat_removal_before_growth <- rnorm(years, mean = Nt_removal_before_growth, sd = sigma_N)



data <- data.frame(
  Year = rep(1:years, 4),  
  Population = c(Nt_removal_after_growth, Nt_removal_before_growth, Nthat_removal_after_growth, Nthat_removal_before_growth),
  Model = rep(c("Removal After Growth", "Removal Before Growth", 
                "Observed Removal After Growth", "Observed Removal Before Growth"), 
              each = years)
)

# visulisation
ggplot(data, aes(x = Year, y = Population, color = Model, shape = Model)) +
  geom_line() +
  geom_point(size = 3) +
  labs(title = "Population Dynamics Comparison",
       x = "Years", y = "Population Size") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "red", "green", "orange")) +
  scale_shape_manual(values = c(19, 17, 16, 15)) 

