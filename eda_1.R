

library(ocp)

ocpd_gaussian <- onlineCPD(covid_daily$cases, 
                  getR = FALSE, 
                  optionalOutputs = TRUE,
                  probModel = list('gaussian'),
                  init_params = list(list(m = 0, k = 0.01, a = 0.01, b = 0.0001)),
                  hazard_func = function(x, lambda){const_hazard(x, lambda = 100)},
                  missPts = 'skip'
                  )

ocpd_poisson <- onlineCPD(covid_daily$cases, 
                           getR = FALSE, 
                           optionalOutputs = TRUE,
                           maxRlength = 14,
                           probModel = list('p'),
                           init_params = list(list(a = 1, b = 1)),
                           hazard_func = function(x, lambda){const_hazard(x, lambda = 100)},
                           missPts = 'skip'
)

plot(ocpd_poisson)



ocpd_poisson$logprobcps
ocpd_poisson$changepoint_lists$threshcps


covid_daily %>%
  slice(25:30)