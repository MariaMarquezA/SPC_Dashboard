get_utilization <- function(t_dates, c_obs, threshold){
  t_diff <- diff(t_dates$d[c_obs])
  t_active <- sum(t_diff[t_diff < threshold])
  t_total <- sum(t_diff)
  utilization <- as.numeric(t_active)/as.numeric(t_total)
  return(c(t_active, t_total, utilization))
}