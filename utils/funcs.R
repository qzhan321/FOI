# a two-moment approximation for the GI/G/c queue with finite capacity
suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(ggplot2)
  library(tibble)
})

#### helper function
calc_mu_i <- function(i = NA, a = NA, b = NA, c = NA, r = NA, a_R = NA, b_R = NA) {
  # c servers + r waiting place: 1 to c+r
  # a: expectation of interarrival times
  # b: expectation of service times
  # a_R: time-averaging counterpart for the average residual interarrival time at the departure instant of a customer who leaves behind n customers in the system (0<=n<=c+r)
  # b_R: time-averaging counterpart for the average residual service time of a randomly chosen busy server at the arrival instant of a customer who finds (leaves behind) n customers in the system (1<=n<=c+r).
  # i between 1 and c+r
  if (i >=1 & i <= c) {
    temp <- b-i*(a-a_R)
  } else if (i >= c+1 & i <= c+r) {
    temp <- -c*(a-a_R)+b_R
  }
  return(1/temp)
}

calc_lambda_i <- function(i = NA, a = NA, b = NA, c = NA, r = NA, a_R = NA, b_R = NA) {
  # c servers + r waiting place: 1 to c+r
  # a: expectation of interarrival time
  # b: expectation of service time
  # a_R: time-averaging counterpart for the average residual interarrival time at the departure instant of a customer who leaves behind n customers in the system (0<=n<=c+r)
  # b_R: time-averaging counterpart for the average residual service time of a randomly chosen busy server at the arrival instant of a customer who finds (leaves behind) n customers in the system (1<=n<=c+r).
  # i between 0 and c+r-1
  if (0 <= i & i <= c-2) {
    temp <- (i+1)*a_R
  } else if (c-1 <= i & i <= c+r-2) {
    temp <- c*a_R + b_R - b
  } else if (i==c+r-1) {
    temp <- c*a
  }
  return(1/temp)
}

calc_gamma_i <- function(i = NA, lambda = NA, a = NA, a_R = NA, mu_i_all = NA, lambda_i_all = NA, c = NA, r = NA) {
  # c servers + r waiting place: 0 to c+r
  # lambda: the average arrival rate
  # a: expectation of interarrival time
  # a_R: time-averaging counterpart for the average residual interarrival time at the departure instant of a customer who leaves behind n customers in the system (0<=n<=c+r)
  # mu_i_all: a vector storing values for all mu up to c+r
  # lambda_i_all: a vector storing values for all lambda up to c+r. Note: those lambda_i are different from lambda parameter.
  if (i==0) {
    temp <- lambda * a_R 
  } else if (i >= 1 & i <= c+r-1) {
    temp <- lambda*(mu_i_all[i]*(a-a_R)/lambda_i_all[i]+a_R)
  } else if (i == c+r) {
    temp <- lambda*(mu_i_all[i]*(a-a_R)/lambda_i_all[i]+a)
  }
  return(temp)
}

calc_P_0_to_A <- function(c = NA, r = NA, lambda_i_all = NA, mu_i_all = NA) {
  temp1 <- 0
  for (i in 1:(c+r)) {
    temp2 <- 1
    for (j in 0:(i-1)) {
      temp2 <- temp2 * (lambda_i_all[j+1]/mu_i_all[j+1])  
    }
    temp1 <- temp1 + temp2
  }
  temp <- 1/(1+temp1)
  return(temp)
}

calc_P_n_to_A_part2 <- function(n = NA, lambda_i_all = NA, mu_i_all = NA, c = NA, r = NA) {
  temp <- 1
  for (i in 0:(n - 1)) {
    temp <- temp * lambda_i_all[i+1]/mu_i_all[i+1]
  }
  return(temp)
}

#### actual function returning the probability distribution function of queue length (MOI)
estQLDist <- function(Va, ma, Vs, ms, c, r) {
  # c servers + r waiting place: 1 to c+r
  # the function to estimate the probability distribution function of queue length (MOI).
  # Va: variance of the interarrival time
  # ma: mean of the interarrival time
  # Vs: variance of the service time
  # ms: mean of the service time
  a = ma
  b = ms 
  
  c_A_square = Va/a^2
  c_S_square = Vs/b^2
  a_R = (1+c_A_square)*a/2
  b_R = (1+c_S_square)*b/2
  
  lambda <- 1/a
  
  lambda_i_all <- unlist(lapply(0:(c+r-1), function(x){calc_lambda_i(i = x, a = a, b = b, c = c, r = r, a_R = a_R, b_R = b_R)}))
  
  mu_i_all <- unlist(lapply(1:(c+r), function(x){calc_mu_i(i = x, a = a, b = b, c = c, r = r, a_R = a_R, b_R = b_R)}))
  
  gamma_i_all <- unlist(lapply(0:(c+r), function(x){calc_gamma_i(i = x, lambda = lambda, a = a, a_R = a_R, mu_i_all = mu_i_all, lambda_i_all = lambda_i_all, c = c, r = r)}))
  
  P_i_to_A_part2_all <- unlist(lapply(1:(c+r), function(x){calc_P_n_to_A_part2(n = x, lambda_i_all = lambda_i_all, mu_i_all = mu_i_all, c = c, r = r)}))
  
  P_0_to_A = calc_P_0_to_A(c = c, r = r, lambda_i_all = lambda_i_all, mu_i_all = mu_i_all)
  
  P_i <- P_0_to_A*P_i_to_A_part2_all*gamma_i_all[2:length(gamma_i_all)]
  
  P_0 <- P_0_to_A*gamma_i_all[1]
  
  P_i_all <- c(P_0, P_i)
  return(P_i_all)
}


#### maximize the likelihood 
lh_nlogT2 <- function(p, MOI) {
  minp <- min(p)
  if (any(p < 0)) {
    # print(minp)
    p[p<=0] <- 0
    p <- p/sum(p)
  }
  lh_nlogT <- sum(-log10(p[MOI+1]))
  return(list("lh" = lh_nlogT, "minp" = minp))
}
