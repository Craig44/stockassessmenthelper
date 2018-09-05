## Utiltiy function to undertake a randomisation test, without replacement

#@param sample1
#@param sample2
#@param FUN the estimator to compare
#@param N how many resamples you want to carry out

#@return 

rand_test = function(sample1, sample2, FUN = mean, N = 1000) {
  n1 = length(sample1);
  n2 = length(sample2);
  observe_stat = FUN(sample1) / FUN(sample2);
  full_vector = c(sample1,sample2);
  empirical_dist = vector();
  for ( i in 1:N) {
    random_vector = sample(full_vector);
    x1 = random_vector[1:n1];
    x2 = random_vector[n1+1:n2];
    empirical_dist[i] = FUN(x1) / FUN(x2);
  }
  p_val = table(observe_stat < empirical_dist)[2];
  if(is.na(p_val)) {
    p_val=0
  } else {
  p_val = p_val / length(empirical_dist)
  }
  return(list("observed_stat" = observe_stat, "Empirical_dist" = empirical_dist, "P_val" = p_val))
}