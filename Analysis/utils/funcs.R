generateMOI <- function(p_missing = NA, ps = NA, N = NA, N_neg = NA, MOI_max = 20, N_pos_microscopy = NA, replacement = TRUE){
  # p_missing: the proportion of false negatives among all PCR negatives 
  # ps: the probability density function of non-zero MOIs
  # N: the total number of individual sampled within a certain age group
  # N_neg: the total number of individual sampled within a certain age group that are PCR negative
  # N_pos_microscopy: the number of individuals with their var genes sequenced and typed, and MOI estimated  
  # replacement: whether sample with or without replacement from the empirical MOI distribution
  m <- round(N_neg * (1 - p_missing)) # number of true PCR negatives
  n <- N - m - N_pos_microscopy # number of individuals with missing MOI
  if (m == 0) {
    print("The number of true PCR negative individuals is 0 please double check!")
  }
  if (n == 0) {
    print("the number of individuals with missing MOI values is 0!")
  }
  
  inds <- unique(ps$host_id)
  ps_for_missingMOI <- NULL
  inds_sub <- sample(inds, n, replace = replacement)
  if (n > 0) {
    for (i in 1:n) {
      ind <- inds_sub[i]
      ps_ind <- ps %>% filter(host_id == ind)
      ps_for_missingMOI <- rbind(ps_for_missingMOI, ps_ind %>% mutate(host_id = paste0("MOI_missing_ind", i, "_", unique(ps$survey)))) 
    }
  }
  
  ps_for_MOIZero <- NULL
  temp <- as_tibble(data.frame("MOI" = 0:MOI_max, "p" = c(1, rep(0, MOI_max))))
  if (m > 0) {
    for (j in 1:m) {
      ps_for_MOIZero <- rbind(ps_for_MOIZero, temp %>% mutate(host_id = paste0("MOI_zero_ind", j, "_", unique(ps$survey))))
    }
  }
  return(list("true_MOI_zeros" = ps_for_MOIZero, "false_MOI_zeros" = ps_for_missingMOI))
}
