# Extract fitted values and deviance residuals from the Tweedie GAM
fitted_vals   <- fitted(twd_re)                     # predicted mean for each row
deviance_res  <- residuals(twd_re, type = "deviance")  # deviance residuals

# Plot deviance residuals vs fitted values
plot(
  fitted_vals,
  deviance_res,
  xlab = "Fitted values",
  ylab = "Deviance residuals",
  main = "Deviance Residuals vs Fitted Values",
  pch = 16,
  col = rgb(0,0,0,0.5)   # semi-transparent points
)

# Add horizontal reference line at 0
abline(h = 0, lty = 2, col = "red")
# Log scale to spread low fitted values
plot(
  log(fitted_vals + 1),
  deviance_res,
  xlab = "log(Fitted + 1)",
  ylab = "Deviance residuals",
  main = "Deviance Residuals vs log(Fitted + 1)",
  pch = 16,
  col = rgb(0,0,0,0.5)
)
abline(h = 0, lty = 2, col = "red")

# Absolute deviance residuals to check heteroskedasticity
plot(
  fitted_vals,
  abs(deviance_res),
  xlab = "Fitted values",
  ylab = "|Deviance residuals|",
  main = "Absolute Deviance Residuals vs Fitted Values",
  pch = 16,
  col = rgb(0,0,1,0.5)
)
