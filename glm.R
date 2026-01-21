nfip_state_year <- nfip_state_year %>%
  mutate(
    state = factor(state),
    loss_year = as.numeric(loss_year),
    total_paid = as.numeric(total_paid)
  )
nfip_state_year <- nfip_state_year %>%
  filter(
    is.finite(total_paid),
    total_paid >= 0
  )
twd_fixed <- gam(
  total_paid ~ s(loss_year, k = 5) + state,
  family = tw(link = "log"),
  data = nfip_state_year,
  method = "REML"
)
twd_re <- gam(
  total_paid ~ s(loss_year, k = 5) + s(state, bs = "re"),
  family = tw(link = "log"),
  data = nfip_state_year,
  method = "REML"
)
