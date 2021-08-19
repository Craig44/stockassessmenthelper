#' @title simulate_SPM
#' @description simulate a surplus production model, maybe fletchers?
#' @return Returns a list of model quantiteis and observations that can be used as an operating model
#' @param theta <real> starting exploited biomass
#' @param r <real> intrinsic growth parameter.
#' @param m <real> Shape parameter for the surplus production function
#' @param K <real> carrying capacity parameter.
#' @param exploitation <vector> Exploitation, defines number of years (length = Y - 1), so if you have a period of no catches set values of 0
#' @param obs_cv <vector> CV's (for each year) for simulated observations
#' @param proc_std (optional) time specific time deviations (epsilon_t,p) for each time step
#' @param catchability <vector> a scalar to generate the relative index observation
#' @param proj_years int number of projection years
#' @param proj_u vector<proj_years> (length 1 or proj_years) 
#' @export
simulate_SPM = function(theta, r, m, K, exploitation, obs_cv, catchability, proc_std, proj_years, proj_u, process_error = NULL) {
  ## do some checks up front
  Y = length(exploitation) + 1; ## one less catch than year
  if (length(obs_cv) == 1) 
    obs_cv = rep(obs_cv, Y)
  if (length(catchability) == 1)
    catchability = rep(catchability, Y)
  if (length(obs_cv) != Y) 
    stop(paste0("need the same number of cvs for observations years (catches + 1), you supplied '", length(obs_std), "' observation standard devs, but '", Y,"' years values, please sort this out"))
  if (length(catchability) != Y)
    stop(paste0("need the same number of catchabilities as years (catches + 1), you supplied '", length(obs_std), "' observation standard devs, but '", Y,"' years values, please sort this out"))
  
  observations = actual_catch = predicted_biomass = vector();
  predicted_biomass[1] = K * theta
  Y_tot = Y + proj_years
  if (is.null(process_error)) {
    process_error = rnorm(Y_tot, 0, proc_std);
  } else {
    if (length(process_error) != Y_tot)
      stop(paste0("process_error needs to be for all years including projection = ", Y_tot))
  }
  
  predicted_biomass[1] = predicted_biomass[1] * exp(process_error[1])
  
  for (y in 2:Y) {
    predicted_biomass[y] = ((predicted_biomass[y - 1] + (r / (m - 1.0)) * predicted_biomass[y - 1] * (1.0 - (predicted_biomass[y - 1] / K)^(m - 1.0)))  * (1 - exploitation[y - 1])) * exp(process_error[y])
    actual_catch[y - 1] = (predicted_biomass[y - 1] + (r / (m - 1.0)) * predicted_biomass[y - 1] * (1.0 - (predicted_biomass[y - 1] / K)^(m - 1.0))) * exploitation[y - 1]
  }
  observations = rnorm(Y, predicted_biomass * catchability, predicted_biomass * catchability * obs_cv);
  # Projection period
  for (y in (Y + 1):Y_tot) {
    predicted_biomass[y] = ((predicted_biomass[y - 1] + (r / (m - 1.0)) * predicted_biomass[y - 1] * (1.0 - (predicted_biomass[y - 1] / K)^(m - 1.0)))  * (1 - proj_u)) * exp(process_error[y])
  }
  
  return(list(expected =  predicted_biomass[1:Y] * catchability, obs = observations, biomass = predicted_biomass, catches = actual_catch, process_error = process_error))
}

#' @title project_SPM
#' @description Given an estimated model, will project forward proj_years and simulate data. Uncertainty comes from estimated parameters, as well as the process error which is assumed to be estimated
#' @return Returns a list of model quantiteis and observations that can be used as an operating model
#' @param fixed_effect_estimates <vector> MLE estimates
#' @param covariance_estimates <matrix> MLE standard errors, must match order of parameters fixed_effect_estimates
#' @param proj_years int number of projection years
#' @param n_sims int number random draws 
#' @param B_Y list 'mu' MLE derived quantity and 'sd'
#' @export
#' @importFrom MASS mvrnorm
project_SPM = function(fixed_effect_estimates = vector(), covariance_estimates = matrix(), proj_years,  m, n_sims, B_Y = list()) {
  ## do some checks up front
  if(!all(names(B_Y) %in% c("mu","sd")))
    error("names of B_y needs a name 'mu' and 'sd'")
  start_biomass = rnorm(n_sims, B_Y$mu, B_Y$sd);
  sim_pars = mvrnorm(n_sims, fixed_effect_estimates, covariance_estimates)
  ## projection
  projected_biomass = matrix(NA, nrow = n_sims, ncol = proj_years + 1)
  projected_catches = matrix(NA, nrow = n_sims, ncol = proj_years)
  u_msy = vector()
  for(i in 1:n_sims) {
    u_msy[i] = sim_pars["r",i] / (m - 1) * ( 1 - 1/m)
    projected_biomass[i, 1] = start_biomass[i]
    for (y in 2:(proj_years + 1)) {
      predicted_biomass[i, y] = ((predicted_biomass[i, y - 1] + (sim_pars["r",i] / (m - 1.0)) * predicted_biomass[i, y - 1] * (1.0 - (predicted_biomass[i, y - 1] / sim_pars["K",i])^(m - 1.0)))  * (1 - u_msy[i])) * exp(rnorm(1, 0, sim_pars["pro_std",i]))
      projected_catches[i, y - 1] = (predicted_biomass[i, y - 1] + (sim_pars["r",i] / (m - 1.0)) * predicted_biomass[i, y - 1] * (1.0 - (predicted_biomass[i, y - 1] / sim_pars["K",i])^(m - 1.0))) * u_msy[i]
      
    }
  }
  return(projected_biomass = projected_biomass, projected_catches = projected_catches, u_msy = u_msy)
}


#' @title surplus_production_model_sigma
#' @description simulates a surplus production model can be used as an operating model, Different from the above uses a standard deviation for process error instead of CV
#' @return Returns a list of model quantities and observations that can be used as an operating model
#' @param B1 <real> starting exploited biomass
#' @param r <real> intrinsic growth parameter.
#' @param k <real> carrying capacity parameter.
#' @param catches <vector> observed catches, defines number of years, so if you have a period of no catches set values of 0, number of years = length(catches+ 1)
#' @param obs_std <real> Standard deviation which is used to generate simulated observations.
#' @param process_error (optional) time specific time deviations (epsilon_t,p) for each time step
#' @param process_std (optional) standard deviation for process deviations corrections
#' @param observation_likelihood <int> likelihood type, 1 = lognormal, 2 = normal, 3 = ...
#' @param catchability <vector> a scalar to generate the relative index observation
#' @export
surplus_production_model_sigma = function(seed = 123,B1, r, k, catches, obs_std, catchability, observation_likelihood = 1,process_error = NULL, process_std = NULL) {
  set.seed(seed)
  ## do some checks up front
  if (length(catchability) == 1) {
    catchability = rep(catchability, Y)
  }
  if (length(catchability) != Y) {
    stop(paste0("need the same number of catchabilities as years (catches + 1), you supplied '", length(obs_std), "' observation standard devs, but '", Y,"' years values, please sort this out"))
  }
  
  predicted_biomass = vector();
  observations = vector();
  exploitation = vector();
  predicted_biomass[1] = B1
  surplus_production = vector();
  
  if (!is.null(process_error)) {
    for (t in 1:(Y - 1)) {
      surplus_production[t] = r*(1 - predicted_biomass[t] / k)* predicted_biomass[t]
      predicted_biomass[t+1] = (predicted_biomass[t] + r*(1 - predicted_biomass[t] / k)* predicted_biomass[t] - catches[t]) * exp(process_error[t] - 0.5 * process_std^2)
      exploitation[t] = catches[t] / (predicted_biomass[t] + r * (1 - predicted_biomass[t] / k) * predicted_biomass[t])
      
      if (predicted_biomass[t+1] <= 0) {
        stop(paste0("model generated biomass less than 0 in year ", t, ", sort out parameterisation value"))
      }
    }
  } else {
    for (t in 1:(Y - 1)) {
      surplus_production[t] = r*(1 - predicted_biomass[t] / k)* predicted_biomass[t]
      predicted_biomass[t+1] = predicted_biomass[t] + r * (1 - predicted_biomass[t] / k) * predicted_biomass[t] - catches[t]
      exploitation[t] = catches[t] / (predicted_biomass[t] + r * (1 - predicted_biomass[t] / k) * predicted_biomass[t])
      if (predicted_biomass[t+1] <= 0) {
        stop(paste0("model generated biomass less than 0 in year ", t, ", sort out parameterisation value"))
      }
    }
  }
  
  #exploitation = catches / predicted_biomass[-length(predicted_biomass)]
  sigma = NULL;
  if (observation_likelihood == 1) {
    log_mu = (log(predicted_biomass * catchability) - 0.5 * (obs_std^2))
    observations = exp(rnorm(Y, log_mu, obs_std));
    
  } else if (observation_likelihood == 2) {
    observations = rnorm(Y,predicted_biomass * catchability, obs_std);
    
  }
  return(list(exploitation = exploitation, sd = obs_std, surplus_production = surplus_production, expected =  predicted_biomass * catchability, obs = observations, biomass = predicted_biomass, catches = catches))
}

#' @title general_surplus_production_model_sigma
#' @description that can be used as an operating model
#' Different from the above uses a standard deviation for process error instead of CV and also allow for fishing to be input as both catch and exploitation.
#' @return Returns a list of model quantities and observations 
#' @param B1 <real> starting exploited biomass
#' @param r <real> intrinsic growth parameter.
#' @param k <real> carrying capacity parameter.
#' @param m <real> shape parameter for the surplus production curve (m = 2 = schafer)
#' @param catches <vector> observed catches, defines number of years, so if you have a period of no catches set values of 0, number of years = length(catches+ 1)
#' @param exploitation <vector> observed catches, defines number of years, so if you have a period of no catches set values of 0, number of years = length(catches+ 1
#' @param obs_std <real> Standard deviation which is used to generate simulated observations.
#' @param process_error (optional) time specific time deviations (epsilon_t,p) for each time step
#' @param process_std (optional) standard deviation for process deviations corrections
#' @param observation_likelihood <int> likelihood type, 1 = lognormal, 2 = normal, 3 = ...
#' @param catchability <vector> a scalar to generate the relative index observation
#' @export

general_surplus_production_model_sigma = function(seed = 123,B1, m, r, k, catches = NULL, exploitation = NULL, obs_std, catchability, observation_likelihood = 1,process_error = NULL, process_std = NULL, n_obs_series = 1) {
  F_method = NULL
  set.seed(seed)
  
  ## do some checks up front
  if (length(catchability) == 1) {
    catchability = rep(catchability, Y)
  }
  
  if (is.null(exploitation) & is.null(catches))
    stop("nedd to specify either catches or exploitaiton")
  if (!is.null(exploitation) & !is.null(catches))
    stop("Cannot specify both catches and exploitaiton, one or the other")
  if (!is.null(catches)) {
    F_method = "catch"
  } else {
    F_method = "exploitation"
  }
  if (length(catchability) != Y) {
    stop(paste0("need the same number of catchabilities as years (catches + 1), you supplied '", length(obs_std), "' observation standard devs, but '", Y,"' years values, please sort this out"))
  }
  
  predicted_biomass = vector();
  observations = matrix(NA, ncol  =Y, nrow = n_obs_series);
  actual_exploitation = vector();
  predicted_biomass[1] = B1
  surplus_production = vector();
  actual_catch = vector();
  
  if (!is.null(process_error)) {
    for(y in 2:Y) {
      surplus_production[y - 1] = (r / (m - 1.0)) * predicted_biomass[y - 1] * (1.0 - (predicted_biomass[y - 1] / k)^(m - 1.0));
      if (F_method == "catch") {
        predicted_biomass[y] = ((predicted_biomass[y - 1] + (r / (m - 1.0)) * predicted_biomass[y - 1] * (1.0 - (predicted_biomass[y - 1] / k)^(m - 1.0)))  - catches[y - 1])  * exp(process_error[y - 1] - 0.5 * process_std^2) #* (1 - O1$exploitation[y - 1])# * exp(rnorm(1, -0.5*sd_process*sd_process, sd_process));
        actual_catch[y - 1] = catches[y - 1];
        actual_exploitation[y - 1] = catches[y - 1] / (predicted_biomass[y - 1] + (r / (m - 1.0)) * predicted_biomass[y - 1] * (1.0 - (predicted_biomass[y - 1] / k)^(m - 1.0))) 
      } else {
        predicted_biomass[y] = ((predicted_biomass[y - 1] + (r / (m - 1.0)) * predicted_biomass[y - 1] * (1.0 - (predicted_biomass[y - 1] / k)^(m - 1.0)))  * (1 - exploitation[y - 1]))  * exp(process_error[y - 1] - 0.5 * process_std^2) #* (1 - O1$exploitation[y - 1])# * exp(rnorm(1, -0.5*sd_process*sd_process, sd_process));
        actual_catch[y - 1] = (predicted_biomass[y - 1] + (r / (m - 1.0)) * predicted_biomass[y - 1] * (1.0 - (predicted_biomass[y - 1] / k)^(m - 1.0))) * exploitation[y - 1]
      }
      if (predicted_biomass[y] <= 0) 
        stop(paste0("model generated biomass less than 0 in year ", t, ", sort out parameterisation value"))
    }
  } else {
    for(y in 2:Y) {
      surplus_production[y - 1] = (r / (m - 1.0)) * predicted_biomass[y - 1] * (1.0 - (predicted_biomass[y - 1] / k)^(m - 1.0));
      if (F_method == "catch") {
        predicted_biomass[y] = ((predicted_biomass[y - 1] + (r / (m - 1.0)) * predicted_biomass[y - 1] * (1.0 - (predicted_biomass[y - 1] / k)^(m - 1.0)))  - catches[y - 1]) #* (1 - O1$exploitation[y - 1])# * exp(rnorm(1, -0.5*sd_process*sd_process, sd_process));
        actual_catch[y - 1] = catches[y - 1];
        actual_exploitation[y - 1] = catches[y - 1] / (predicted_biomass[y - 1] + (r / (m - 1.0)) * predicted_biomass[y - 1] * (1.0 - (predicted_biomass[y - 1] / k)^(m - 1.0))) 
      } else {
        predicted_biomass[y] = ((predicted_biomass[y - 1] + (r / (m - 1.0)) * predicted_biomass[y - 1] * (1.0 - (predicted_biomass[y - 1] / k)^(m - 1.0)))  * (1 - exploitation[y - 1])) #* (1 - O1$exploitation[y - 1])# * exp(rnorm(1, -0.5*sd_process*sd_process, sd_process));
        actual_catch[y - 1] = (predicted_biomass[y - 1] + (r / (m - 1.0)) * predicted_biomass[y - 1] * (1.0 - (predicted_biomass[y - 1] / k)^(m - 1.0))) * exploitation[y - 1]
      }
      if (predicted_biomass[y] <= 0) 
        stop(paste0("model generated biomass less than 0 in year ", t, ", sort out parameterisation value"))
    }
  }
  
  #exploitation = catches / predicted_biomass[-length(predicted_biomass)]
  sigma = NULL;
  observations = matrix(NA, ncol  =Y, nrow = n_obs_series);
  for(i in 1:n_obs_series) {
    if (observation_likelihood == 1) {
      log_mu = (log(predicted_biomass * catchability) - 0.5 * (obs_std^2))
      observations[i,] = exp(rnorm(Y, log_mu, obs_std));
      
    } else if (observation_likelihood == 2) {
      observations[i,] = rnorm(Y, predicted_biomass * catchability, obs_std);
      
    }
  }
  if (F_method == "catch")
    exploitation = actual_exploitation
  return(list(exploitation = exploitation, actual_catch = actual_catch, sd = obs_std, surplus_production = surplus_production, expected =  predicted_biomass * catchability, obs = observations, biomass = predicted_biomass, catches = catches))
}

#' @title surplus_production_millar
#' @description Returns a list of model quantities and observations that can be used as an operating model
#' when looking at different estimation models. A main idea here is changing the lognormal distribution to be more
#' traditional no adjustments.
#' @param r <real> intrinsic growth parameter.
#' @param k <real> carrying capacity parameter.
#' @param q <real> relative index catchability.
#' @param P1 <real> initial state relative to K (1 = B_t = K)
#' @param obs_std <real> lognormal standard deviation for relative index
#' @param pro_std <real> lognormal standard deviation for process equation
#' @param seed <int> set.seed for reproducibility
#' @param catch <vector> observed catches, defines number of years, so if you have a period of no catches set values of 0, number of years = length(catches+ 1)
#' @export
surplus_production_millar = function(K, r, q, P1, obs_std, pro_std, catch, seed = 123) {
  set.seed(seed);
  N = length(catch) + 1
  P = vector(); ## state vector
  P[1] = P1
  Pmean = vector();
  Pmean[1] = P1;
  
  for (t in 2:N) {
    Pmean[t] = log(max(P[t-1] + r*P[t-1]*(1-P[t-1]) - catch[t-1] /  K , 0.01));
    P[t] = rlnorm(1, Pmean[t],pro_std);
  }
  
  obs = rlnorm(N, log(q*P*K),obs_std);
  
  return( list(P_state = P, P_mean = Pmean, obs = obs, state = P*K))
}




#' @title surplus_production_model_re_parameterised
#' @description Returns a list of model quantities and observations that can be used as an operating model
#' this is more for helping me debug mu model
#' @param log_jt <real> states specified as J_t = B_t / K
#' @param r <real> intrinsic growth parameter.
#' @param k <real> carrying capacity parameter.
#' @param catches <vector> observed catches, defines number of years, so if you have a period of no catches set values of 0, number of years = length(catches+ 1)
#' @param catchability <vector> a scalar to generate the relative index observation
#' @return a list of OM outputs
#' @export
surplus_production_model_re_parameterised = function(log_jt, r, k, catches, catchability) {
  ## do some checks up front
  if (length(catchability) == 1) {
    catchability = rep(catchability, Y)
  }
  
  if (length(catchability) != Y) {
    stop(paste0("need the same number of catchabilities as years (catches + 1), you supplied '", length(obs_std), "' observation standard devs, but '", Y,"' years values, please sort this out"))
  }
  
  Bt = exp(log_jt) * k;
  
  
  predicted_biomass = vector();
  exploitation = vector();
  predicted_biomass[1] = Bt[1]
  
  for (t in 1:(Y - 1)) {
    predicted_biomass[t+1] = (Bt[t] + r*(1 - Bt[t] / k)* Bt[t] - catches[t])
  }

  exploitation = catches / predicted_biomass[-length(predicted_biomass)]

  return(list(exploitation = exploitation, expected =  predicted_biomass * catchability, biomass = predicted_biomass, catches = catches))
}


#' @title theta_logistic
#' @description log-transformed theta logistic population growth model
#' (Wang, 2007)
#' @param N_t number of time steps
#' @param sig_obs = standard deviation to draw observations
#' @param sig_pro = standard deviation of process error
#' @param r <real> intrinsic growth parameter.
#' @param x0 <real> initial state
#' @param K <real> carrying capacity parameter.
#' @param theta 
#' @return list of simulated data and derived quantities.
#' @export
theta_logistic = function(N_t, sig_obs, sig_pro, r, K, theta, x0) {
  state = vector();
  state[1] = x0;
  for (i in 1:(N_t - 1)) {
    state[i + 1] = state[i] + r * (1 - (exp(state[i])/K)^theta) + rnorm(1,0,sig_pro)
  }
  obs = rnorm(N_t, state, sig_obs);
  return(list(state = state, obs = obs))
}

#' @title theta_logistic_q
#' @description log-transformed theta logistic population growth model
#' (Wang, 2007), with a catchability on index
#' @param N_t number of time steps
#' @param sig_obs = standard deviation to draw observations
#' @param sig_pro = standard deviation of process error
#' @param r <real> intrinsic growth parameter.
#' @param x0 <real> initial state
#' @param K <real> carrying capacity parameter.
#' @param q <real> Catchability coeffecient
#' @param theta 
#' @return list of simulated data and derived quantities.
#' @export
theta_logistic_q = function(q, N_t, sig_obs, sig_pro, r, K, theta, x0) {
  state = vector();
  state[1] = x0;
  for (i in 1:(N_t - 1)) {
    state[i + 1] = state[i] + r * (1 - (exp(state[i])/K)^theta) + rnorm(1,0,sig_pro)
  }
  obs = rnorm(N_t, state * q, sig_obs);
  return(list(state = state, obs = obs))
}
