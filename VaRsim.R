forecast_year <- max(nfip_state_year$loss_year) + 1

#Get Tweedie parameters
newdata <- nfip_state_year |>
  dplyr::distinct(state) |>
  dplyr::mutate(loss_year = forecast_year)
mu_hat <- predict(twd_re, newdata = newdata, type = "response")
p_hat   <- twd_re$family$getTheta(TRUE)   
phi_hat <- summary(twd_re)$dispersion

#Choose number of simulations
n_sim <- 10000
library(statmod)

sim_losses <- replicate(n_sim, {
  sum(
    rtweedie(
      n = length(mu_hat),
      mu = mu_hat,
      phi = phi_hat,
      power = p_hat
    )
  )
})
#Calculate VaR
VaR_95 <- quantile(sim_losses, 0.95, na.rm = TRUE)
VaR_95

